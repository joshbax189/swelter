;;; swelter.el --- A swagger client generator.   -*- lexical-binding: t -*-

;;; Code:

(require 'url)
(require 'dash)
(require 's)
(require 'oauth2)
(require 'cl-lib)

;; NOTE this is made for Swagger v2 for now
;; one difference is body replaced

(defun swelter-generate (url client)
  "Generate CLIENT from swagger file at URL."
  (interactive)
  ;; download file and convert
  (let* ((swagger-json (swelter--get-swagger-json url))
         (server-root (swelter--get-server-root-url-v2 swagger-json url)))

    ;; generated code will be output to a buffer
    (pop-to-buffer (concat client ".el"))
    (erase-buffer)
    (emacs-lisp-mode)

    (print
     `(defvar ,(intern  (format "%s-api-version" client)) ,(map-nested-elt swagger-json '("info" "version")))
     (current-buffer))

    (print
     `(defvar ,(intern  (format "%s-swagger-url" client)) ,url)
     (current-buffer))

    (let ((security-definitions (swelter--get-security-definitions (map-elt swagger-json "securityDefinitions")))
          (global-security-obj (map-elt swagger-json "security")))

      (cl-prettyprint
       (swelter--build-authorize-function client security-definitions))

      (dolist (path-value (map-pairs (map-elt swagger-json "paths")))
        (-let [(path . endpoint-obj) path-value]
          ;; path is e.g. "/pet/{petId}"
          (dolist (http-verb-value (map-pairs endpoint-obj))
            (-let [(http-verb . path-obj) http-verb-value]
              ;; TODO nil should print as ()?
              (cl-prettyprint (swelter--build-endpoint
                               http-verb
                               (make-symbol (swelter--make-function-name client http-verb path (map-elt path-obj "operationId")))
                               path
                               path-obj
                               server-root
                               global-security-obj
                               client))
              (newline))))))

    (print
     `(provide (quote ,(intern client)))
     (current-buffer))
    )

  ;; also
  ;; - info
  ;; - tags
  ;; - servers

  )

(defun swelter--get-swagger-json (url)
  "Get and parse swagger.json from URL.

Throws if missing or not a valid json."
  ;; TODO handle json parse error?
  (let* ((file-buff (url-retrieve-synchronously url))
         (swagger-json (with-current-buffer file-buff
                         ;; skip to body after empty line
                         (goto-char (point-min))
                         (while (looking-at "^.") (delete-line))
                         (json-parse-buffer))))
    swagger-json))

(defun swelter--get-swagger-version (swagger-json)
  "Return version string of SWAGGER-JSON.  Nil if not a swagger file."
  (or (map-elt swagger-json "openapi")
      ;; for version 2.0
      (map-elt swagger-json "swagger")))

(defun swelter--get-server-root-url-v2 (swagger-json url)
   "Gets the root url from SWAGGER-JSON.
URL is the original address of the swagger json, used for fallback."
   ;; error if scheme is present and https is not
   (when-let (schemes (map-elt swagger-json "schemes"))
     (unless (seq-contains-p schemes "https")
       (error (format "Swagger indicates only these protocols are supported: %s" schemes))))

   (let* ((swagger-url (url-generic-parse-url url))
          (scheme "https")
          (default-host (concat (url-host swagger-url) (when (url-portspec swagger-url) (format ":%s" (url-port swagger-url)))))
          (host (map-elt swagger-json "host" default-host))
          (base-path (map-elt swagger-json "basePath" "")))
     (when (equal "http" (url-type swagger-url))
       (warn "Swagger url used HTTP, assuming HTTPS for client url"))
     (concat scheme "://" host base-path)))

(defun swelter--get-security-definitions (obj)
  "Convert a securityDefinitions OBJ to an alist of name to extra query or headers."
  (map-apply
   (lambda (name method-obj)
     (cons name (swelter--build-security-method-v2 method-obj)))
   obj))

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

(defun swelter--oauth-code-flow (auth-url token-url client-id client-secret &optional scope)
  "Login using auth code flow."
  ;; TODO can it use https?
  (let* ((port (swelter--find-available-port 8000))
         (redirect-uri (format "http://localhost:%s/foo" port))
         (state (swelter--oauth-make-state-string))
         (query `(("response_type" "code")
                  ("client_id" ,client-id)
                  ("redirect_uri" ,redirect-uri)
                  ("state" ,state)))
         (query (if scope
                    (append query `("scope" ,scope))
                  query))
         (authorize-url (concat auth-url "?" (url-build-query-string query)))
         (cb (lambda (httpcon)
               (when-let ((code (assoc "code" (elnode-http-params httpcon))))
                 (unless (equal state
                                (cdr (assoc "state" (elnode-http-params httpcon))))
                   (warn "OAuth state invalid")
                   (elnode-send-400 httpcon))
                 (message "get token")
                 ;; closure: token-url, client-id, client-secret, redirect-uri
                 (print (oauth2-request-access token-url client-id client-secret (cdr code) redirect-uri)))
               ;; TODO store token cf oauth2-auth-and-store
               ;; TODO how to inject token into correct calls?
               (elnode-http-start httpcon 200)
               (elnode-http-return httpcon)
               (elnode-stop port))))
    (elnode-start cb :port port)
    ;; TODO close after timeout too
    (browse-url authorize-url)))

;; FIXME: This will not work because elnode cannot read url fragments (they aren't sent out of the browser).
;;        In general, "implicit" is meant for browser apps and is deprecated, so find a way to not use this.
(defun swelter--oauth-implicit-flow (auth-url client-id &optional scope)
  "Login using implicit grant flow. Deprecated."
  (error "Implicit flow is not supported"))

(defun swelter--oauth-password-flow (url client-id &optional client-secret scope)
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
         (url-request-data (url-build-query-string form-data)))
    (url-retrieve-synchronously url
                                (lambda (_s)
                                  (goto-char (point-min))
                                  (while (looking-at "^.") (delete-line))
                                  (json-parse-buffer)))))

(defun swelter--oauth-application-flow (url client-id client-secret &optional scope)
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
         (url-request-data (url-build-query-string form-data)))
    (url-retrieve-synchronously url
                                (lambda (_s)
                                  (goto-char (point-min))
                                  (while (looking-at "^.") (delete-line))
                                  (json-parse-buffer)))))

;; oauth entrypoint should be like:
;; - check stored token
;; - if valid use it
;; - else if expired use refresh
;; - else renew
;; if token is keyed by client-id and url should work ok

(defun swelter--build-security-method-v2 (obj)
  "Construct a header generating function for a security definition OBJ.

Returns either a form which can be evaluated to get a header cons cell,
or nil if the auth method failed to produce a token."
  ;; NOTE: Expect that they will be eval'd with free vars bound
  (let ((method-type (map-elt obj "type")))
    (cond
     ((equal method-type "basic")
      ;; free var: server-root
      '(-some->> (url-basic-auth server-root 't 't) (list "Authorization")))

     ((equal method-type "apiKey")
      ;; free var: api-key
      (let ((key-location (map-elt obj "in")))
        (cond
         ((equal key-location "header")
          (let ((header-name (map-elt obj "name")))
            `((lambda () (cons ,header-name api-key)))))
         ((equal key-location "query")
          ;; FIXME: the query param is roughly like `(,(map-elt "name") . api-key)
          (warn "Swelter does not support auth by query params")
          nil
          ))))

     ((equal method-type "oauth2")
      ;; free var: client-id, client-secret, scope
      (let ((oauth-flow (map-elt obj "flow"))
            ;; FIXME: These are actually the available scopes,
            ;;        "scope" from the endpoint security object lists REQUIRED scopes.
            ;;        So could intersect these to fail out of a method that doesn't provide those?
            (oauth-provided-scopes (swelter--swagger-oauth-scopes-to-string (map-elt obj "scopes")))
            (oauth-auth-url (map-elt obj "authorizationUrl")))
        (cond
         (;; FIXME: included to support the petstore example, but should be removed later
          (equal oauth-flow "implicit")
          `(-some->>
               (swelter--oauth-implicit-flow ,oauth-auth-url client-id client-secret (or scope ,oauth-provided-scopes))
             (format "Bearer %s" )
             (list "Authorization" )))
         ((equal oauth-flow "password")
          `(-some->>
               (swelter--oauth-password-flow ,oauth-auth-url client-id client-secret (or scope ,oauth-provided-scopes))
             (format "Bearer %s" )
             (list "Authorization" )))
         ((equal oauth-flow "application")
          `(-some->>
               (swelter--oauth-application-flow ,oauth-auth-url client-id client-secret (or scope ,oauth-provided-scopes))
             (format "Bearer %s" )
             (list "Authorization" )))
         ((equal oauth-flow "accessCode")
          `(-some->>
               (swelter--oauth-code-flow
                ,oauth-auth-url
                ,(map-elt obj "tokenUrl")
                client-id
                client-secret
                (or scope ,oauth-provided-scopes))
             (format "Bearer %s" )
             (list "Authorization" )))
         ('t
          ;; Not an error as there might be other available methods
          (warn (format "Swelter does not support OAuth2 %s flow" oauth-flow))
          nil)))))))

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

(defun swelter--build-authorize-function (client-name security-definitions)
  "Template for authorization function.

CLIENT-NAME string name of client package.
SECURITY-DEFINITIONS alist mapping security method names to generating functions."
  `(defun ,(make-symbol (concat client-name "-authorize")) (security-obj)
     "Return a list of security headers satisfying SECURITY-OBJ."
     (let ((security-definitions ',security-definitions))
       (cl-block nil
         ;; try each auth method in turn
         (dolist (auth-method-obj (seq--into-list security-obj))
           ;; each one must produce a header
           (let* ((auth-methods-alist (map-pairs auth-method-obj))
                  (sec-headers (map-apply
                                (lambda (auth-method scopes)
                                  ;; TODO: what if these are not required/present?
                                  (let ((client-id ',(make-symbol (concat client-name "-client-id")))
                                        (client-secret ',(make-symbol (concat client-name "-client-secret")))
                                        (api-key ',(make-symbol (concat client-name "-api-key"))))
                                    (eval (map-elt security-definitions auth-method))))
                                auth-methods-alist)))
             (when (and sec-headers) ;; all non-nil
               (cl-return sec-headers)))))))
  )

(defun swelter--make-function-name (client-name http-verb path &optional operation-id)
  "Create a skewer-case function name roughly matching the API endpoint.

If OPERATION-ID is given, the name is {CLIENT-NAME}-{OPERATION-ID}.
Otherwise it is {CLIENT-NAME}-{HTTP-VERB}-{PATH} with path params removed.
In both cases the result is transformed to skewer-case.

Assumes CLIENT-NAME is already skewer-case, and PATH does not include any http:// prefix."

  (if operation-id
      (let* ((op-name-lower (s-snake-case operation-id))
             (op-name (string-replace "_" "-" op-name-lower)))
        (format "%s-%s" client-name op-name))
    ;; fallback to verb-path
    (let* ((path-words (string-split path "/" 't))
           (path-clean (--filter (not (string-prefix-p "{" it)) path-words))
           (path-lower (-map #'s-snake-case path-clean))
           (path-skewer (string-replace "_" "-" (string-join path-lower "-"))))
      (format "%s-%s-%s" client-name http-verb path-skewer))))

(defun swelter--path-param-sexp (path)
  "Convert path template PATH to a s-exp over parameters.

E.g. \"/foo/{bar}\" becomes `(format \"/foo/%s\" bar)'"
  (let* ((parts (string-split path "/{\\|}" t))
         (params (-map #'intern (--filter (not (string-prefix-p "/" it)) parts)))
         (format-string (replace-regexp-in-string "{[^}]+}" "%s" path)))
    `(format ,format-string ,@params)))

;; NOTE: This is specific to v2 because of new requestBody keyword in v3 replacing in: body param type.
(defun swelter--build-endpoint (http-verb function-name path obj server-root global-security-obj client-name)
  (-let* ((path-sexp (swelter--path-param-sexp path))
          (docstring (or (map-elt obj "summary")
                         (format "%s %s." http-verb path)))
          (parameters (map-elt obj "parameters"))
          ;; assume all path params required
          ((&alist "path" path-params
                   "query" query-params
                   "formData" form-params
                   "body" body-params)
           (seq-group-by (lambda (x) (map-elt x "in"))
                         parameters))
          ((&alist 't query-params-req
                   :false query-params-opt)
           (seq-group-by (lambda (x) (map-elt x "required"))
                         query-params))
          ((&alist 't form-params-req
                   :false form-params-opt)
           (seq-group-by (lambda (x) (map-elt x "required"))
                         form-params))
          (required-params (--map (make-symbol (map-elt it "name")) (append path-params query-params-req form-params-req body-params)))
          (optional-params (--map (make-symbol (map-elt it "name")) (append query-params-opt form-params-opt)))
          ;; function args
          (params `(,@required-params
                    ,@(when optional-params (cons '&optional optional-params))))
          (build-query-string-arg (--map (let ((name (map-elt it "name"))) (list 'list name (make-symbol name))) query-params))
          (build-form-string-arg (--map (let ((name (map-elt it "name"))) (list 'list name (make-symbol name))) form-params))
          ;; build headers and body
          ;; TODO can body params be described individually?
          (_ (when (> (length body-params) 1) (error "Multiple parameters for body")))
          (header-and-body (if body-params
                               ;; json
                               `((url-request-data (json-encode ,(make-symbol (map-elt (car body-params) "name"))))
                                 (url-request-extra-headers '(("Content-Type" . "application/json"))))
                             ;; form
                             `((url-request-data (url-build-query-string (list ,@build-form-string-arg)))
                               (url-request-extra-headers '(("Content-Type" . "application/x-www-form-urlencoded"))))))
          ;; security
          (security-obj (or (map-elt obj "security") global-security-obj)) ;; an array
          (authorize-function (make-symbol (concat client-name "-authorize")))
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
              ;; TODO can simplify url when no parameters given to "format"
              (res (url-retrieve-synchronously (concat ,server-root ,path-sexp ,@(when query-params `("?" (url-build-query-string (list ,@build-query-string-arg))))))))
         (with-current-buffer res
           (goto-char (point-min))
           (while (looking-at "^.") (delete-line))
           (json-parse-buffer)))))
  )

(provide 'swelter)
;;; swelter.el ends here
