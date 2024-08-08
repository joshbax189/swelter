;;; swelter.el --- A swagger client generator.   -*- lexical-binding: t -*-

;;; Code:

;; is this the best choice?
(require 'url)
(require 'dash)
(require 's)
(require 'oauth2)

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
                             server-root))
            (newline)))))

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

(defun swelter--oauth-code-flow (auth-url token-url client-id client-secret &optional scope)
  "Login using auth code flow."
  ;; TODO can it use https?
  (let* ((port (swelter--find-available-port 8000))
         (redirect-uri (format "http://localhost:%s/foo" port))
         (state "aaaaaa") ;; TODO state should be generated
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
  "Convert PATH to a s-exp over parameters.

E.g. \"/foo/{bar}\" becomes `(format \"/foo/%s\" bar)'"
  ;; TODO the path params might have names that are captured, maybe give them a prefix?
  ;; TODO assumes path is absolute

  (let* ((parts (string-split path "/{\\|}" t))
         (params (-map #'intern (--filter (not (string-prefix-p "/" it)) parts)))
         (format-string (replace-regexp-in-string "{[^}]+}" "%s" path)))

    `(format ,format-string ,@params)))

;; TODO this is specific to v2 because of new requestBody keyword in v3 replacing in: body param type.
(defun swelter--build-endpoint (http-verb function-name path obj server-root)
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
                               (url-request-extra-headers '(("Content-Type" . "application/x-www-form-urlencoded")))))))
   ;; responses
   ;; security

    `(defun ,function-name ,params
       ,docstring
       (let* ((url-request-method ,(upcase http-verb))
              ,@header-and-body
              ;; TODO can simplify url when no parameters given to "format"
              (res (url-retrieve-synchronously (concat ,server-root ,path-sexp ,@(when query-params `("?" (url-build-query-string (list ,@build-query-string-arg))))))))
         (with-current-buffer res
           (goto-char (point-min))
           (while (looking-at "^.") (delete-line))
           (json-parse-buffer)))))
    )

(provide 'swelter)
;;; swelter.el ends here
