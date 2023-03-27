;;; swelter.el --- A swagger client generator.

;; is this the best choice?
(require 'url)

(defcustom swelter-client-dir "~/.swelter-clients/"
  "Where to store generated clients.")

(defun swelter-generate (url client)
  "Generate CLIENT from URL."
  (interactive)
  ;; download file and convert
  ;; TODO may be nil
  ;; TODO may 404
  (let* ((file-buff (url-retrieve-synchronously url))
         (swagger-json (with-current-buffer file-buff
                         ;; skip to body after empty line
                         (while (looking-at "^.") (delete-line))
                         (json-parse-buffer))))
    ;; (print swagger-json)
    (swelter--build-get-endpoint "test" "/form-type/{id}" (gethash "get" (gethash "/form-type/{id}" (gethash "paths" swagger-json))))
    ;; create client dir
    ;; create skeleton
    ;; - paths
    ;; - info
    ;; - tags
    ;; - servers
  ;; map each endpoint to a fn
  ))

(defun swelter--swagger-json-p (args)
   "Tests if ARG is a swagger file."
   ;; has top level key openapi: 3.0.0
   ())

;; TODO can this be modified by the "servers" prop?
(defun swelter--get-server-root-url (url)
   "Gets the root url from the swagger URL."
   (url-basepath url))

(defun swelter--path-param-sexp (path)
  "Convert PATH to a s-exp over parameters."
  ;; TODO the path params might have names that are captured, maybe give them a prefix?
  `(string-join
    (list ,(mapcar
            (lambda (x) (if (string-prefix-p "/" x)
                            (string-trim x "/")
                          (make-symbol x)))
     (string-split path "/{\\|}" t)))
    "/")
  )

(defun swelter--build-get-endpoint (client path obj)
  ;; check for path params, e.g. /foo/{id}
  (let* ((path-sexp (swelter--path-param-sexp path))
         ;; summary -> docstring
         (parameters (gethash "parameters" obj))
         ;; assume all path params required
         (path-params (mapcar (lambda (x) (gethash "name" x)) (seq-filter
                       (lambda (x) (string-equal (gethash "in" x) "path"))
                       parameters)))
         (query-params (seq-filter
                        (lambda (x) (string-equal (gethash "in" x) "query"))
                        parameters))
         (query-params-split (seq-group-by
                              (lambda (x) (gethash "required" x))
                              query-params))
         (query-params-req (mapcar (lambda (x) (gethash "name" x)) (cdr (car query-params-split))))
         (query-params-opt (mapcar (lambda (x) (gethash "name" x)) (cdr (nth 1 query-params-split))))
         (params (seq-concatenate 'list (mapcar 'make-symbol path-params) (mapcar 'make-symbol query-params-req) '('&optional) (mapcar 'make-symbol query-params-opt))))

    ;; (url-build-query-string
    ;;  '((key1 val1)
    ;;    (key2 val2)))                     ;
  ;; optional
  ;; operationId ??
   ;; responses
   ;; tags
   ;; security

  `(defun ,(make-symbol (string-join '("client" "get" "foo") "-")) ,params ,(gethash "summary" obj)
          (let ((res (url-retrieve-synchronously (concat server-root path-sexp "?" query1 "=" q1 "&" so))))
            (check-errors)
            (jump-to-body)
            (with-current-buffer res (json-parse-buffer))
          ))
   ))

(provide 'swelter)
;;; swelter.el ends here
