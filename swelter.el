;;; swelter.el --- A swagger client generator.   -*- lexical-binding: t -*-

;;; Code:

(require 'url)
(require 'dash)
(require 's)
(require 'cl-lib)
(require 'aio)
(require 'yaml)
(require 'plstore)
(require 'jsonp) ;; github.com/joshbax189/jsonp-el
(require 'swelter-oauth)

;; NOTE this is made for Swagger v2 for now
;; one difference is body replaced

;;;###autoload
(defun swelter-generate-from-url (client url)
  "Generate CLIENT from swagger file at URL."
  (interactive "sClient: \nsurl: ")
  (cond
   ((string-suffix-p ".yaml" url)
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (while (looking-at "^.") (delete-line))
      (swelter-generate-from-yaml client nil url)))
   ((string-suffix-p ".json" url)
    (let ((swagger-obj (swelter--get-swagger-json url)))
      (swelter-generate client swagger-obj url)))
   ('t
    (error "Could not determine Swagger file type"))))

;;;###autoload
(defun swelter-generate-from-json (client &optional buffer-or-name url)
  "Generate OpenAPI CLIENT from JSON in a buffer.

BUFFER-OR-NAME contains the JSON, if nil uses `current-buffer'.
URL fallback url if server is not specified in the swagger."
  (interactive "sClient: ")
  (with-current-buffer (or buffer-or-name (current-buffer))
    (swelter--fix-json-big-int)
    (let* ((swagger-obj (json-parse-buffer))
           ;; TODO remote urls?
           (swagger-obj (jsonp-replace-refs swagger-obj)))
      (swelter-generate client swagger-obj (or url "")))))

(defun swelter--print-form (form &optional trailing-newline)
  "Insert FORM in 'current-buffer' updating point for later insertion.
If TRAILING-NEWLINE is set, add a newline after form."
  ;; This fixes the nil => '() issue, but formatting of let blocks is so wacky
  (pp-emacs-lisp-code form)
  (when trailing-newline
    (newline)))

;;;###autoload
(defun swelter-generate-from-yaml (client &optional buffer-or-name url)
  "Generate OpenAPI CLIENT from YAML in a buffer.

BUFFER-OR-NAME contains the YAML, if nil uses `current-buffer'.
URL fallback url if server is not specified in the swagger."
  (interactive "sClient: ")
  (with-current-buffer (or buffer-or-name (current-buffer))
    (let* ((buffer-string (buffer-substring-no-properties (point-min) (point-max)))
           (swagger-obj (yaml-parse-string buffer-string :object-key-type 'string))
           ;; TODO remote urls?
           (swagger-obj (jsonp-replace-refs swagger-obj)))
      (swelter-generate client swagger-obj (or url "")))))

(defun swelter-generate (client swagger-obj url)
  "Generate CLIENT from parsed SWAGGER-OBJ originally at URL."
  (let ((server-root (swelter--get-server-root-url-v2 swagger-obj url)))

    ;; generated code will be output to a buffer
    (pop-to-buffer (concat client ".el"))
    (erase-buffer)
    (emacs-lisp-mode)

    (dolist (x '((require 'url)
                 (require 'json)
                 (require 'cl-lib)
                 (require 'seq)
                 (require 'map)
                 (require 'swelter-oauth)))
      (swelter--print-form x))
    (newline)

    (swelter--print-form
     `(defvar ,(intern (format "%s-api-version" client)) ,(map-nested-elt swagger-obj '("info" "version"))))

    (swelter--print-form
     `(defvar ,(intern (format "%s-swagger-url" client)) ,url))

    (let* ((security-definitions-obj (map-elt swagger-obj "securityDefinitions"))
           (global-security-obj (map-elt swagger-obj "security"))
           (global-consumes (map-elt swagger-obj "consumes"))
           (global-props (make-swelter-global-props
                          :server-root server-root
                          :security global-security-obj
                          :client-name client
                          :consumes global-consumes)))

      (dolist (form (swelter--build-security-definitions client security-definitions-obj))
        (swelter--print-form form t))

      (swelter--print-form
       (swelter--build-version-check-function client)
       t)

      (swelter--print-form
       (swelter--build-authorize-function client security-definitions-obj server-root)
       t)

      (dolist (path-value (map-pairs (map-elt swagger-obj "paths")))
        (-let [(path . endpoint-obj) path-value]
          ;; path is e.g. "/pet/{petId}"
          (dolist (http-verb-value (map-pairs endpoint-obj))
            (-let [(http-verb . path-obj) http-verb-value]
              (swelter--print-form (swelter--build-endpoint
                             http-verb
                             (make-symbol (swelter--make-function-name client http-verb path (map-elt path-obj "operationId")))
                             path
                             path-obj
                             global-props)
                            t))))))

    (swelter--print-form
     `(provide (quote ,(intern client))))))

(defun swelter--fix-json-big-int ()
  "Wrap long ints in string quotes to make valid JSON.

This applies to the current buffer.

Rationale is:
- literal integers in Swagger JSON are probably within example responses
- these are probably just typos"
  ;; Naughty JS developers write bad JSON... fix it
  ;; Although JSON spec says "should be doubles" and IEEE-754 says up to 16 digits in significand
  ;; emacs' json-parse warns after about 19 digits
  (save-excursion
    (save-match-data
      (while (re-search-forward "[[:space:]]\\([0-9]\\{19\\}[0-9]*\\)" nil 't)
        (replace-match "\"\\1\"")))))

(defun swelter--get-swagger-json (url)
  "Get and parse swagger.json from URL.

Throws if missing or not a valid json."
  (let* ((file-buff (url-retrieve-synchronously url))
         (swagger-json (with-current-buffer file-buff
                         ;; skip to body after empty line
                         (goto-char (point-min))
                         (while (looking-at "^.") (delete-line))
                         (swelter--fix-json-big-int)
                         (json-parse-buffer)))
         ;; TODO remote urls?
         (result (jsonp-replace-refs swagger-json)))
    (with-current-buffer (get-buffer-create "*swelter-debug*")
      (cl-prettyprint result))
    result))

(defun swelter--get-swagger-version (swagger-json)
  "Return version string of SWAGGER-JSON.  Nil if not a swagger file."
  (or (map-elt swagger-json "openapi")
      ;; for version 2.0
      (map-elt swagger-json "swagger")))

(defun swelter--get-server-root-url-v2 (swagger-json url)
   "Gets the root url from SWAGGER-JSON.
URL is the original address of the swagger json, used for fallback."
   ;; warn if scheme is present and https is not
   (when-let (schemes (map-elt swagger-json "schemes"))
     (unless (seq-contains-p schemes "https")
       (warn (format "Swagger indicates only these protocols are supported: %s" schemes))))

   (let* ((swagger-url (url-generic-parse-url url))
          (scheme "https")
          (default-host (concat (url-host swagger-url) (when (url-portspec swagger-url) (format ":%s" (url-port swagger-url)))))
          (host (map-elt swagger-json "host"))
          (base-path (map-elt swagger-json "basePath" "")))
     (unless host
       (warn (format "Swagger host not present, using default host %s from Swagger url" default-host)))
     (when (equal "http" (url-type swagger-url))
       (warn "Swagger url used HTTP, assuming HTTPS for client url"))
     (concat scheme "://" (or host default-host) base-path)))

(cl-defstruct swelter-global-props
  "Parsed Swagger objects defined at the top-level of the spec."
  (server-root "Full root URL")
  (security "Contains default auth methods for all endpoints")
  (client-name "Swelter client prefix string")
  (consumes "Optional list of mime types that all endpoints may consume"))

(defun swelter--build-security-definitions (client-name obj)
  "Convert a securityDefinitions OBJ to a list of forms.
CLIENT-NAME is the string name of the client."
  (apply #'append
   (map-apply
    (lambda (scheme-name sec-obj)
      (pcase (map-elt sec-obj "type")
        ("basic" (swelter--build-basic-auth client-name scheme-name sec-obj))
        ("apiKey" (swelter--build-api-key client-name scheme-name sec-obj))
        ("oauth2" (swelter--build-oauth client-name scheme-name sec-obj))))
    obj)))

(defun swelter--get-security-definition-function (client-name scheme-name)
  "Given a SCHEME-NAME get the symbol for the function implementing it.
CLIENT-NAME is the string name of the client."
  (intern (format "%s--authorize-%s" client-name scheme-name)))

(defun swelter--build-oauth (client-name scheme-name sec-obj)
  "Template for OAuth security definition.
CLIENT-NAME string name of client package.
SCHEME-NAME is the name of the security definition.
SEC-OBJ is the security scheme object."
  (let ((oauth-flow (map-elt sec-obj "flow"))
        ;; NOTE: These are actually the available scopes,
        ;;       "scope" from the endpoint security object lists REQUIRED scopes.
        ;;       So could intersect these to fail out of a method that doesn't provide those?
        ;; (oauth-provided-scopes (swelter--swagger-oauth-scopes-to-string (map-elt sec-obj "scopes")))
        (client-id (intern (concat client-name "-" scheme-name "-client-id")))
        (client-secret (intern (concat client-name "-" scheme-name "-client-secret"))))
    `((defvar ,client-id nil)
      (defvar ,client-secret nil)
      (cl-defun ,(swelter--get-security-definition-function client-name scheme-name) (&key scope &allow-other-keys)
        "Authorize using OAuth2 code flow and return the Authorization header."
        (when-let* ((token (swelter-oauth-auth-with-store
                            ',(pcase oauth-flow
                                ("implicit" #'swelter-oauth-implicit-flow)
                                ("password" #'swelter-oauth-password-flow)
                                ("application" #'swelter-oauth-application-flow)
                                ("accessCode" #'swelter-oauth-auth-code-flow)
                                (_
                                 ;; Not an error as there might be other available methods
                                 (warn (format "Swelter does not support OAuth2 %s flow" oauth-flow))
                                 #'ignore))
                            :auth-url
                            ,(map-elt sec-obj "authorizationUrl")
                            :token-url
                            ,(map-elt sec-obj "tokenUrl")
                            :client-id ,client-id
                            :client-secret ,client-secret
                            :scope scope)))
          (cons "Authorization" (format "Bearer %s" token)))))))

(defun swelter--build-api-key (client-name scheme-name sec-obj)
  "Template for API key security definition.
CLIENT-NAME string name of client package.
SCHEME-NAME is the name of the security definition.
SEC-OBJ is the security scheme object."
  (if (equal (map-elt sec-obj "in") "query")
      (prog1 nil
        (warn (format "Ignored query API key security definition in %s" scheme-name)))
      (let ((api-key (intern (concat client-name "-" scheme-name "-api-key"))))
        `((defvar ,api-key nil)
          (defun ,(swelter--get-security-definition-function client-name scheme-name) (&rest _)
            "Authorize using API key in header."
            (when ,api-key
             (cons ,(map-elt sec-obj "name") ,api-key)))))))

(defun swelter--build-basic-auth (client-name scheme-name sec-obj)
  "Template for basic auth security definition.
CLIENT-NAME string name of client package.
SCHEME-NAME is the name of the security definition.
SEC-OBJ is the security scheme object."
  ;; must return a list of defuns to match signature of other sec def methods
  `((cl-defun ,(swelter--get-security-definition-function client-name scheme-name) (&key server-root &allow-other-keys)
      ,(or
        (map-elt sec-obj "description")
        "Authorize using basic authentication and return the Authorization header.")
      (when-let* ((auth-string (url-basic-auth server-root 't)))
        (cons "Authorization" auth-string)))))

(defun swelter--build-authorize-function (client-name security-definitions server-root)
  "Template for authorization function.

CLIENT-NAME string name of client package.
SECURITY-DEFINITIONS alist mapping security method names to functions.
SERVER-ROOT the server url."
  `(defun ,(intern (concat client-name "-authorize")) (security-obj)
     "Return a list of security headers satisfying SECURITY-OBJ."
     (let ((security-definitions ',(map-keys-apply (lambda (name) (cons name (swelter--get-security-definition-function client-name name)))
                                                   security-definitions)))
       (cl-block 'outer
         ;; try each auth method in turn
         (dolist (auth-method-obj (seq--into-list security-obj))
           ;; each one must produce a header
           (let* ((auth-methods-alist (map-pairs auth-method-obj))
                  (sec-headers (map-apply
                                (lambda (auth-method scope)
                                  (let ((method (map-elt security-definitions auth-method)))
                                    (when (fboundp method)
                                      (apply method
                                             (list
                                              :server-root ,server-root
                                              :scope       (swelter-oauth--swagger-oauth-scopes-to-string scope))))))
                                auth-methods-alist)))
             (when (seq-every-p #'identity sec-headers) ;; all non-nil
               (cl-return-from 'outer sec-headers))))
         (cl-return-from 'outer nil)))))

(defun swelter--build-version-check-function (client-name)
  "Template for client version check.

CLIENT-NAME string name of client package."
  `(defun ,(intern (concat client-name "-api-version-check")) ()
      "Signals if client version does not match original Swagger file."
      (let* ((swagger-json (swelter--get-swagger-json ,(intern (concat client-name "-swagger-url"))))
             (upstream-version (map-nested-elt swagger-json '("info" "version"))))
        (unless (equal upstream-version ,(intern (concat client-name "-api-version")))
          (error (format "API version %s did not match client version %s" upstream-version ,(intern (concat client-name "-api-version")))))
        't)))

(defun swelter--make-function-name (client-name http-verb path &optional operation-id)
  "Create a skewer-case function name roughly matching the API endpoint.

If OPERATION-ID is given, the name is {CLIENT-NAME}-{OPERATION-ID}.
Otherwise it is {CLIENT-NAME}-{HTTP-VERB}-{PATH} with path params removed.
In both cases the result is transformed to skewer-case.

Assumes CLIENT-NAME is already skewer-case, and PATH does not include any http:// prefix."
  (if operation-id
      (let ((op-name (s-dashed-words operation-id)))
        (format "%s-%s" client-name op-name))
    ;; fallback to verb-path
    (let* ((path-words (string-split path "/" 't))
           (path-clean (--filter (not (string-prefix-p "{" it)) path-words))
           (path-parts (-map #'s-dashed-words path-clean))
           (path-skewer (string-join path-parts "-")))
      (format "%s-%s-%s" client-name http-verb path-skewer))))

(defun swelter--make-endpoint-function-params (params-obj)
  "Create a list of parameters for an endpoint defun template.

PARAMS-OBJ the swagger object describing the endpoint parameters.
It should be an array of parameter descriptions,
see https://swagger.io/docs/specification/v2_0/describing-parameters/"
  (-let* ((params-by-type (seq-group-by (lambda (x) (map-elt x "in"))
                                        params-obj))
          ((&alist "path"     path-params
                   "query"    query-params
                   "header"   header-params
                   "formData" form-params
                   "body"     body-params)
           params-by-type)
          ;; for these "required" may be :false or nil
          (required-p (lambda (x) (eq 't (map-elt x "required"))))
          ((&alist 't  query-params-req
                   nil query-params-opt)
           (seq-group-by required-p query-params))
          ((&alist 't  form-params-req
                   nil form-params-opt)
           (seq-group-by required-p form-params))
          ((&alist 't  header-params-req
                   nil header-params-opt)
           (seq-group-by required-p header-params))
          ;; these names are used verbatim
          (name-to-symbol (lambda (x) (intern (map-elt x "name"))))
          (required-params
           (-map
            name-to-symbol
            (append path-params
                    query-params-req
                    form-params-req
                    header-params-req
                    body-params)))
          (optional-params
           (-map
            name-to-symbol
            ;; assume all path params required
            (append query-params-opt
                    form-params-opt
                    header-params-opt))))
  (append
   required-params
   (when optional-params
     (cons '&optional optional-params)))))

(defun swelter--path-param-sexp (path)
  "Convert path template PATH to a s-exp over parameters.

E.g. \"/foo/{bar}\" becomes `(format \"/foo/%s\" bar)'"
  (let* ((parts (string-split path "/{\\|}" t))
         (params (-map #'intern (--filter (not (string-prefix-p "/" it)) parts)))
         (format-string (replace-regexp-in-string "{[^}]+}" "%s" path)))

    (if params
        `(format ,format-string ,@params)
      format-string)))

;; NOTE: This is specific to v2 because of new requestBody keyword in v3 replacing in: body param type.
;; TODO there can be parameters shared across all endpoints in a path, should add an optional object here
(defun swelter--build-endpoint (http-verb function-name path obj global-props)
  "Template a client function.

HTTP-VERB e.g. \"get\" \"put\" etc
FUNCTION-NAME full name of the function to generate
PATH relative path of the endpoint from SERVER-ROOT
OBJ the swagger object describing the endpoint
GLOBAL-PROPS a `swelter-global-props' struct with top-level objects"
  (-let* ((server-root (swelter-global-props-server-root global-props))
          (global-security-obj (swelter-global-props-security global-props))
          (client-name (swelter-global-props-client-name global-props))
          (path-sexp (swelter--path-param-sexp path))
          (docstring (or (map-elt obj "summary")
                         (format "%s %s." http-verb path)))
          ((&alist "query"    query-params
                   "formData" form-params
                   "header"   header-params
                   "body"     body-params)
           (seq-group-by (lambda (x) (map-elt x "in"))
                         (map-elt obj "parameters")))
          (params (swelter--make-endpoint-function-params (map-elt obj "parameters")))
          ;; url query string
          (build-query-string-arg (--map (let ((name (map-elt it "name"))) (list 'list name (make-symbol name))) query-params))
          (build-form-string-arg (--map (let ((name (map-elt it "name"))) (list 'list name (make-symbol name))) form-params))
          (build-header-string-arg (--map (let ((name (map-elt it "name"))) (list 'cons name (make-symbol name))) header-params))
          ;; TODO add sanity check warnings, e.g. path params missing from path, cf strava-update-logged-in-athlete

          ;; build headers and body
          ;; see https://swagger.io/docs/specification/v2_0/describing-request-body/
          ;; There can be only one body parameter
          (_ (when (> (length body-params) 1) (error "Multiple parameters for body")))
          (header-and-body (cond
                            (body-params ;; json
                             `((url-request-data (json-encode ,(make-symbol (map-elt (car body-params) "name"))))
                               (url-request-extra-headers '(("Content-Type" . "application/json")))))
                            (form-params ;; form-data
                             `((url-request-data (url-build-query-string (list ,@build-form-string-arg)))
                               (url-request-extra-headers '(("Content-Type" . "application/x-www-form-urlencoded")))))
                            ;; empty body
                            ('t '((url-request-extra-headers nil)))))
          ;; add any headers specified as parameters
          ;; NOTE optional header params are always included but with null value
          (user-headers (when header-params
                          `((url-request-extra-headers (append url-request-extra-headers
                                                               (list ,@build-header-string-arg))))))
          ;; security
          (security-obj (or (map-elt obj "security") global-security-obj)) ;; an array
          (authorize-function (intern (concat client-name "-authorize")))
          (security-header (when (and security-obj
                                      ;; when security-obj is [] security is specifically ignored for this endpoint
                                      (not (seq-empty-p security-obj)))
                             `((url-request-extra-headers (append url-request-extra-headers
                                                                  (,authorize-function ',(--map
                                                                                          (map-into it 'alist)
                                                                                          security-obj)))))
                             )))
    ;; responses

    `(defun ,function-name ,params
       ,docstring
       (let* ((url-request-method ,(upcase http-verb))
              ,@header-and-body
              ,@security-header
              ,@user-headers
              (res (url-retrieve-synchronously (concat ,server-root
                                                       ,path-sexp
                                                       ,@(when query-params `("?" (url-build-query-string (list ,@build-query-string-arg))))))))
         (with-current-buffer res
           (goto-char (point-min))
           (while (looking-at "^.") (forward-line))
           (forward-line)
           ;; Try to parse as JSON and fall back to text
           ;; TODO read content-type header
           (condition-case nil
               (json-parse-string (buffer-substring (point) (point-max)))
             (error (warn "JSON parse error in response")
                    (buffer-substring (point) (point-max))))))))
  )

(provide 'swelter)
;;; swelter.el ends here
