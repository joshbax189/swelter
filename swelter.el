;;; swelter.el --- A swagger client generator.   -*- lexical-binding: t -*-

;;; Code:

(require 'url)
(require 'dash)
(require 's)
(require 'oauth2)
(require 'cl-lib)
(require 'aio)
(require 'yaml)
(require 'plstore)

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
           (swagger-obj (swelter--replace-all-json-refs swagger-obj)))
      (swelter-generate client swagger-obj (or url "")))))

(defun swelter--print-form (form &optional trailing-newline)
  "Insert FORM in 'current-buffer' updating point for later insertion.
If TRAILING-NEWLINE is set, add a newline after form."
  (cl-prettyprint form)
  (if trailing-newline
      (forward-char 1)
    (delete-char 1)))

;;;###autoload
(defun swelter-generate-from-yaml (client &optional buffer-or-name url)
  "Generate OpenAPI CLIENT from YAML in a buffer.

BUFFER-OR-NAME contains the YAML, if nil uses `current-buffer'.
URL fallback url if server is not specified in the swagger."
  (interactive "sClient: ")
  (with-current-buffer (or buffer-or-name (current-buffer))
    (let* ((buffer-string (buffer-substring-no-properties (point-min) (point-max)))
           (swagger-obj (yaml-parse-string buffer-string :object-key-type 'string))
           (swagger-obj (swelter--replace-all-json-refs swagger-obj)))
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
                 (require 'cl-lib)))
      (swelter--print-form x))
    (newline)

    (swelter--print-form
     `(defvar ,(intern (format "%s-api-version" client)) ,(map-nested-elt swagger-obj '("info" "version"))))

    (swelter--print-form
     `(defvar ,(intern (format "%s-swagger-url" client)) ,url))

    (let* ((security-definitions-obj (map-elt swagger-obj "securityDefinitions"))
           (global-security-obj (map-elt swagger-obj "security")))

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
              ;; TODO nil should print as ()?
              (swelter--print-form (swelter--build-endpoint
                             http-verb
                             (make-symbol (swelter--make-function-name client http-verb path (map-elt path-obj "operationId")))
                             path
                             path-obj
                             server-root
                             global-security-obj
                             client)
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

(defun swelter--resolve-json-ref (path root-obj)
  "Lookup JSON pointer PATH in ROOT-OBJ.

Returns nil if node is not found."
  (let* ((clean-path (string-remove-prefix "#/" path))
         (path-components (string-split clean-path "/"))
         (current-loc root-obj))
    (while-let ((current-token (car path-components)))
      ;; map-elt allows indexing into strings, but JSON pointer does not
      ;; return nil in that case
      (when (stringp current-loc)
        (setq current-loc nil))
      ;; vectors must be indexed with numbers
      (if (and (vectorp current-loc)
               ;; because (string-to-number "foo") is 0
               (string-match-p "[0-9]+" current-token))
          (setq current-loc (map-elt current-loc (string-to-number current-token)))
        ;; else treat current-token as a string
        (setq current-loc (map-elt current-loc current-token)))
      (setq path-components (cdr path-components)))
    current-loc))

(defun swelter--replace-json-ref (json-obj root-obj)
  "Given parsed JSON-OBJ expand any $ref.

References are resolved in ROOT-OBJ."
  ;; only use JSON pointers for now, begin with #
  (if-let* ((ref-string (map-elt json-obj "$ref")))
      (if (string-prefix-p "#" ref-string)
          (swelter--resolve-json-ref ref-string root-obj)
        (warn (format "Unknown json $ref %s" ref-string))
        nil)
    json-obj))

(defun swelter--replace-all-json-refs (json-obj &optional root-obj)
  "Given parsed JSON-OBJ expand any $ref.

References are resolved in ROOT-OBJ."
  (setq root-obj (or root-obj json-obj))
  (map-apply
   (lambda (key val)
     (cond
      ((or (not (mapp val))
           (stringp val))
       (cons key val))
      ((vectorp val)
       ;; recurse and rebuild vector
       (cons key (apply #'vector (map-values (swelter--replace-all-json-refs val root-obj)))))
      ('t (if-let ((new-val (swelter--replace-json-ref val root-obj)))
              (cons key (swelter--replace-all-json-refs new-val root-obj))
            (cons key val)))))
   json-obj))

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
         (result (swelter--replace-all-json-refs swagger-json)))
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

;;; OAuth handling
;; following tests from LSP-mode
(defun swelter--port-available (port)
  "Return non-nil if PORT is available."
  (condition-case _err
      (delete-process (open-network-stream "*connection-test*" nil "localhost" port :type 'plain))
    (file-error t)))

(defun swelter--find-available-port (starting-port)
  "Find available port starting from STARTING-PORT."
  (let ((port starting-port))
    (while (not (swelter--port-available port))
      (cl-incf port))
    port))

(defun swelter--oauth-make-state-string (&optional result-length)
  "Generate a random alpha-num string of length RESULT-LENGTH."
  (let ((res))
   (dotimes (_x (or result-length 20))
     (let ((rand-val (+ (cl-random 74) 48)))
       ;; skip symbols
       (when (or (and (>= rand-val 58)
                   (> 65 rand-val))
                 (and (>= rand-val 91)
                      (> 97 rand-val)))
         (setq rand-val (+ 10 rand-val)))
       (push rand-val res)))
   (apply #'string res)))

(cl-defun swelter--oauth-code-flow (&key auth-url token-url client-id client-secret scope &allow-other-keys)
  "Login using auth code flow."
  ;; TODO can it use https?
  ;; network-stream.el can, could make a simple elnode replacement that just reads the HTTP message
  (let* ((promise (aio-promise))
         (port (swelter--find-available-port 8000))
         (redirect-uri (format "http://localhost:%s/foo" port))
         (state (swelter--oauth-make-state-string))
         (query `(("response_type" "code")
                  ("client_id" ,client-id)
                  ("redirect_uri" ,redirect-uri)
                  ("state" ,state)))
         (query (if scope
                    (append query `(("scope" ,(swelter--swagger-oauth-scopes-to-string scope))))
                  query))
         (authorize-url (concat auth-url "?" (url-build-query-string query)))
         (cb (lambda (httpcon)
               (when-let ((code (assoc "code" (elnode-http-params httpcon))))
                 (unless (equal state
                                (cdr (assoc "state" (elnode-http-params httpcon))))
                   (warn "OAuth state invalid")
                   (elnode-send-400 httpcon)
                   (elnode-stop port)
                   (aio-resolve promise (lambda () (error "OAuth state error"))))

                 (message "get token")
                 ;; closure: token-url, client-id, client-secret, redirect-uri
                 (let ((token (oauth2-request-access token-url client-id client-secret (cdr code) redirect-uri)))
                   ;; TODO store token cf oauth2-auth-and-store
                   (message "token retrieved")
                   (print token)
                   (aio-resolve promise (lambda () token))))
               (elnode-http-start httpcon 200 '("Content-type" . "text/html"))
               (elnode-http-return httpcon "<html><p>Token retrieved, you can close this window now.</p></html>")
               (elnode-stop port))))
    (prog1 promise
      (elnode-start cb :port port)
      ;; cleanup elnode listener after 4m
      (run-at-time 240 nil
                   (lambda ()
                     (when (seq-contains-p (elnode-ports) port)
                       (message "shutting down http://localhost:%s" port)
                       (elnode-stop port)
                       (unless (aio-result promise)
                         (aio-resolve promise (lambda () (error "No response to OAuth login attempt")))))))
      (browse-url authorize-url))))

;; FIXME: This will not work because elnode cannot read url fragments (they aren't sent out of the browser).
;;        In general, "implicit" is meant for browser apps and is deprecated, so find a way to not use this.
(cl-defun swelter--oauth-implicit-flow (&key auth-url client-id scope &allow-other-keys)
  "Login using implicit grant flow. Deprecated."
  (error "Implicit flow is not supported"))

(cl-defun swelter--oauth-password-flow (&key auth-url client-id client-secret scope &allow-other-keys)
  "Auth using password. Deprecated.

CLIENT-SECRET If the application is a “confidential client” (not a mobile or JavaScript app), then the secret is included as well.
SCOPE If the application is requesting a token with limited scope, it should provide the requested scopes here."
  ;; The Password grant is one of the simplest OAuth grants and involves only one step:
  ;; the application presents a traditional username and password login form to collect
  ;; the user’s credentials and makes a POST request to the server to exchange the password for an access token.
  (let* ((url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/x-www-form-urlencoded")))
         (form-data `(("grant_type" "password")
                      ;; TODO cache
                      ("username" ,(completing-read "username" nil)) ;; The user’s username that they entered in the application
                      ("password" ,(password-read "password")) ;; The user’s password that they entered in the application
                      ("client_id" ,client-id)))
         (form-data (if client-secret
                        (`("client_secret" ,client-secret) . form-data)
                      form-data))
         (form-data (if scope
                        (`("scope" ,scope) . form-data)
                      form-data))
         (url-request-data (url-build-query-string form-data))
         (promise (aio-promise)))
    (prog1 promise
      (with-current-buffer (url-retrieve-synchronously auth-url)
        (goto-char (point-min))
        (while (looking-at "^.") (delete-line))
        (let ((result (json-parse-buffer)))
          (aio-resolve promise
                       (lambda ()
                         (make-oauth2-token
                          :client-id client-id
                          :client-secret client-secret
                          :access-token (map-elt result "access_token")
                          :refresh-token (map-elt result "refresh_token")
                          :access-response result))))))))

(cl-defun swelter--oauth-application-flow (&key auth-url client-id client-secret scope &allow-other-keys)
  "Auth using client secret. Also called \"client credentials\" flow.

CLIENT-SECRET If the application is a “confidential client” (not a mobile or JavaScript app), then the secret is included as well.
SCOPE If the application is requesting a token with limited scope, it should provide the requested scopes here."
  (let* ((url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/x-www-form-urlencoded")))
         (form-data `(("grant_type" "client_credentials")
                      ("client_id" ,client-id)
                      ("client_secret" ,client-secret)))
         (form-data (if scope
                        (`("scope" ,scope) . form-data)
                      form-data))
         (url-request-data (url-build-query-string form-data))
         (promise (aio-promise)))
    (prog1 promise
      (with-current-buffer (url-retrieve-synchronously auth-url)
        (goto-char (point-min))
        (while (looking-at "^.") (delete-line))
        (let ((result (json-parse-buffer)))
          (aio-resolve promise
                       (lambda ()
                         (make-oauth2-token
                          :client-id client-id
                          :client-secret client-secret
                          :access-token (map-elt result "access_token")
                          :refresh-token (map-elt result "refresh_token")
                          :access-response result))))))))

(defun swelter--get-stored-token (auth-url client-id &optional client-secret token-url)
  "Get and rehydrate stored token for AUTH-URL and CLIENT-ID.

Note CLIENT-SECRET and TOKEN-URL are only used to rehydrate the token."
  (let* ((plstore (plstore-open oauth2-token-file))
         ;; NOTE: see oauth2-compute-id
         (id (secure-hash 'md5 (concat auth-url client-id)))
         (plist (cdr (plstore-get plstore id))))
    (when plist
      ;; TODO tokens created for the same domain but different methods will collide
      ;;      Fix by creating new token struct
      ;; TODO scope may differ, token may be expired
      (message "got token from cache")
      (make-oauth2-token :plstore plstore
                         :plstore-id id
                         :client-id client-id
                         ;; TODO are both of these required?
                         :client-secret client-secret
                         :token-url token-url
                         :access-token (plist-get plist :access-token)
                         :refresh-token (plist-get plist :refresh-token)
                         :access-response (plist-get plist :access-response)))))

(defun swelter--store-token (token auth-url)
  "Store TOKEN against AUTH-URL."
  (let ((id (secure-hash 'md5 (concat auth-url (oauth2-token-client-id token))))
        (plstore (plstore-open oauth2-token-file)))
    ;; Set the plstore in the token
    (setf (oauth2-token-plstore token) plstore)
    (setf (oauth2-token-plstore-id token) id)
    (plstore-put plstore id nil `(:access-token
                                  ,(oauth2-token-access-token token)
                                  :refresh-token
                                  ,(oauth2-token-refresh-token token)
                                  :access-response
                                  ,(oauth2-token-access-response token)))
    (plstore-save plstore)
    token))

(cl-defun swelter--oauth-with-store (method &key auth-url token-url client-id client-secret scope)
 "Auth perhaps with a stored token."
 (or (swelter--get-stored-token auth-url client-id
                                client-secret
                                token-url)
     ;; TODO renew? IIRC only auth-code flow does refresh tokens
     (swelter--store-token
      (aio-wait-for
       (funcall method
                :auth-url auth-url
                :token-url token-url
                :client-id client-id
                :client-secret client-secret
                :scope scope))
      auth-url)))

    (cond

(defun swelter--swagger-oauth-scopes-to-string (scope-obj)
  "Get the scope string from a Swagger scope description.

SCOPE-OBJ may either be an array of scope strings or a map from scope
to description.  In either case returns a string of space-separated scopes.
Returns nil, if SCOPE-OBJ is nil.  The result is not url encoded."
  (cond
   ((not scope-obj)
    nil)
   ;; an array of strings
   ((vectorp scope-obj)
    (funcall #'string-join (seq--into-list scope-obj) " "))
   ;; a map from scope name to description
   ((mapp scope-obj)
    (funcall #'string-join (map-keys scope-obj) " "))))

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
        ;; FIXME: These are actually the available scopes,
        ;;        "scope" from the endpoint security object lists REQUIRED scopes.
        ;;        So could intersect these to fail out of a method that doesn't provide those?
        (oauth-provided-scopes (swelter--swagger-oauth-scopes-to-string (map-elt sec-obj "scopes")))
        (client-id (intern (concat client-name "-" scheme-name "-client-id")))
        (client-secret (intern (concat client-name "-" scheme-name "-client-secret"))))
    `((defvar ,client-id)
      (defvar ,client-secret)
      (cl-defun ,(swelter--get-security-definition-function client-name scheme-name) (&key scope &allow-other-keys)
        "Authorize using OAuth2 code flow and return the Authorization header."
        (when-let* ((token (swelter--oauth-with-store
                            ',(pcase oauth-flow
                                ("implicit" #'swelter--oauth-implicit-flow)
                                ("password" #'swelter--oauth-password-flow)
                                ("application" #'swelter--oauth-application-flow)
                                ("accessCode" #'swelter--oauth-code-flow)
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
                            :scope (or scope ,oauth-provided-scopes))))
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
        `((defvar ,api-key)
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
                                              :scope       (swelter--swagger-oauth-scopes-to-string scope))))))
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
;; TODO collect the global data in an object
(defun swelter--build-endpoint (http-verb function-name path obj server-root global-security-obj client-name)
  "Template a client function.

HTTP-VERB e.g. \"get\" \"put\" etc
FUNCTION-NAME full name of the function to generate
PATH relative path of the endpoint from SERVER-ROOT
OBJ the swagger object describing the endpoint
SERVER-ROOT the overall root url
GLOBAL-SECURITY-OBJ contains default auth methods for all endpoints
CLIENT-NAME string name of the generated client to be used as a prefix"
  (-let* ((path-sexp (swelter--path-param-sexp path))
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
