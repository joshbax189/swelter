;;; swelter.el --- A swagger client generator.

;;; Code:

;; is this the best choice?
(require 'url)
(require 'dash)
(require 's)

(defcustom swelter-client-dir "~/.swelter-clients"
  "Where to store generated clients."
  :type 'string)

(defun swelter-generate (url client)
  "Generate CLIENT from swagger file at URL."
  (interactive)
  ;; download file and convert
  (let ((swagger-json (swelter--get-swagger-json url)))
    ;; create client dir
    (make-directory (concat swelter-client-dir "/" client) 't)

    ;; TODO don't open but oh well
    (find-file (concat swelter-client-dir "/" client "/" "my-foo-client.el"))

    ;; TODO account for host and basePath

    (dolist (path-value (map-pairs (map-elt swagger-json "paths")))
      (-let [(path . endpoint-obj) path-value]
        ;; path is e.g. "/pet/{petId}"
        (dolist (http-verb-value (map-pairs endpoint-obj))
          (-let [(http-verb . path-obj) http-verb-value]
            (cond
             ((equal http-verb "get")
              ;; TODO nil should print as ()?
              (print (swelter--build-get-endpoint
                      (make-symbol (swelter--make-function-name client http-verb path (map-elt path-obj "operationId")))
                      path
                      path-obj)
                     (current-buffer)))
             ;; ((equal http-verb "post")
             ;;  () ;; TODO
             ;;  )
             ((equal http-verb "delete")
              (print (swelter--build-delete-endpoint
                      (make-symbol (swelter--make-function-name client http-verb path (map-elt path-obj "operationId")))
                      path
                      path-obj)
                     (current-buffer)))
             )
            ;; ((equal http-verb "put")
            ;;  () ;; TODO
            ;;  )
            ;; ((equal http-verb "patch")
            ;;  () ;; TODO
            ;;  )
            )))))

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

;; TODO can this be modified by the "servers" prop?
(defun swelter--get-server-root-url (url)
   "Gets the root url from the swagger URL."
   (url-basepath url))

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

(defun swelter--build-get-endpoint (name path obj)
  (-let* ((path-sexp (swelter--path-param-sexp path))
         (docstring (or (map-elt obj "summary")
                        (format "GET %s." path)))
         (parameters (map-elt obj "parameters"))
         ;; assume all path params required
         ((&alist "path" path-params "query" query-params) (seq-group-by
                     (lambda (x) (map-elt x "in"))
                     parameters))
         ((&alist 't query-params-req :false query-params-opt) (seq-group-by
              (lambda (x) (map-elt x "required"))
              query-params))
         ;; function args
         (params `(,@(--map (make-symbol (map-elt it "name")) path-params)
                   ,@(--map (make-symbol (map-elt it "name")) query-params-req)
                   ,@(when query-params-opt
                       (cons '&optional (--map (make-symbol (map-elt it "name")) query-params-opt)))))
         (build-query-string-arg (--map (let ((name (map-elt it "name"))) (list 'list name (make-symbol name))) query-params)))

  ;; optional
  ;; operationId ??
   ;; responses
   ;; tags
   ;; security

  `(defun ,name ,params
     ,docstring
     (let ((res (url-retrieve-synchronously (concat server-root ,path-sexp ,@(when query-params `("?" (url-build-query-string (list ,@build-query-string-arg))))))))
       (with-current-buffer res
         (goto-char (point-min))
         (while (looking-at "^.") (delete-line))
         (json-parse-buffer))))
   ))

(defun swelter--build-delete-endpoint (name path obj)
  (-let* ((path-sexp (swelter--path-param-sexp path))
         (docstring (or (map-elt obj "summary")
                        (format "DELETE %s." path)))
         (parameters (map-elt obj "parameters"))
         ;; assume all path params required
         ((&alist "path" path-params "query" query-params) (seq-group-by
                     (lambda (x) (map-elt x "in"))
                     parameters))
         ((&alist 't query-params-req :false query-params-opt) (seq-group-by
              (lambda (x) (map-elt x "required"))
              query-params))
         ;; function args
         (params `(,@(--map (make-symbol (map-elt it "name")) path-params)
                   ,@(--map (make-symbol (map-elt it "name")) query-params-req)
                   ,@(when query-params-opt
                       (cons '&optional (--map (make-symbol (map-elt it "name")) query-params-opt)))))
         (build-query-string-arg (--map (let ((name (map-elt it "name"))) (list 'list name (make-symbol name))) query-params)))

  `(defun ,name ,params
     ,docstring
     (let* ((url-request-method "DELETE")
            (res (url-retrieve-synchronously (concat server-root ,path-sexp ,@(when query-params `("?" (url-build-query-string (list ,@build-query-string-arg))))))))
       (with-current-buffer res
         (goto-char (point-min))
         (while (looking-at "^.") (delete-line))
         ;; TODO may not always be json!
         (json-parse-buffer))))
   ))

(provide 'swelter)
;;; swelter.el ends here
