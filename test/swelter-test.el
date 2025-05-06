;; -*- lexical-binding: t -*-

(require 'ert)
(require 'map)
(require 'el-mock)
(require 'swelter)

(ert-deftest swelter--path-param-sexp/test ()
  "Path strings should be transformed to format expressions."
  (should (equal (swelter--path-param-sexp "/foo") "/foo"))
  (should (equal (swelter--path-param-sexp "/foo/{barId}") '(format "/foo/%s" barId)))
  (should (equal (swelter--path-param-sexp "/foo/{barId}/subaz") '(format "/foo/%s/subaz" barId)))
  (should (equal (swelter--path-param-sexp "/foo/{barId}/subaz/{id}") '(format "/foo/%s/subaz/%s" barId id))))

(ert-deftest swelter--make-function-name/test ()
  "Should handle path params."
  (should (equal (swelter--make-function-name "my-foo" "get" "/pet/{petId}") "my-foo-get-pet"))
  (should (equal (swelter--make-function-name "my-foo" "post" "/pet/{petId}/uploadImage") "my-foo-post-pet-upload-image"))
  (should (equal (swelter--make-function-name "my-foo" "get" "/pet/findByStatus") "my-foo-get-pet-find-by-status"))
  (should (equal (swelter--make-function-name "my-foo" "get" "/pet/{petId}" "getPetById") "my-foo-get-pet-by-id")))

(ert-deftest swelter--fix-json-big-int/test ()
  "Replace big integers with strings."
  (with-temp-buffer
    (insert "{
  \"foo\": {
    \"id\": 12341234123412341234,
    \"name\": \"baz\",
    \"ok\": \"abc12341234123412341234\"
  },
  \"bar\": {
    \"id\": \"12341234123412341234\",
    \"name\": \"bar\",
    \"num\": 1234123412341234
  }
}")
    (goto-char (point-min))
    (swelter--fix-json-big-int)
    (let ((result (json-parse-buffer)))
      ;; bad number is replaced
      (should (equal "12341234123412341234" (map-nested-elt result '("foo" "id"))))
      ;; number with prefix not replaced
      (should (equal "abc12341234123412341234" (map-nested-elt result '("foo" "ok"))))
      ;; already quoted number is not replaced
      (should (equal "12341234123412341234" (map-nested-elt result '("bar" "id"))))
      ;; short number is not replaced
      (should (equal 1234123412341234 (map-nested-elt result '("bar" "num")))))))

(ert-deftest swelter--get-server-root-url-v2/test ()
  "Test cases"
  ;; assume only HTTPS for now -- error if not supported
  (let ((json (json-parse-string "{
  \"schemes\": [
    \"http\",
    \"ws\"
  ]
}" :object-type 'hash-table)))
    (should-error (swelter--get-server-root-url-v2 json "http://foo.com/bar/apiDocs.json")))

  ;; construct from object
  (let ((json (json-parse-string "{
  \"schemes\": [ \"https\" ],
  \"host\": \"spoon.com:8081\",
  \"basePath\": \"/path\"
}" :object-type 'hash-table)))
      (should (equal (swelter--get-server-root-url-v2 json "http://foo.com/bar/apiDocs.json")
                     "https://spoon.com:8081/path")))

  ;; fallback to url
  (let ((json (json-parse-string "{}" :object-type 'hash-table)))
    (should (equal (swelter--get-server-root-url-v2 json "https://foo.com/bar/apiDocs.json")
                   "https://foo.com")))

  ;; default host
  (let ((json (json-parse-string "{
  \"schemes\": [ \"https\" ],
  \"basePath\": \"/path\"
}" :object-type 'hash-table)))
      (should (equal (swelter--get-server-root-url-v2 json "http://foo.com:8081/bar/apiDocs.json")
                     "https://foo.com:8081/path")))

  ;; default basePath
  (let ((json (json-parse-string "{
  \"schemes\": [ \"https\" ],
  \"host\": \"spoon.com:8081\"
}" :object-type 'hash-table)))
      (should (equal (swelter--get-server-root-url-v2 json "http://foo.com/bar/apiDocs.json")
                     "https://spoon.com:8081"))))

(ert-deftest swelter--build-version-check-function/test ()
  "Tests output of template."
  (should (equal (swelter--build-version-check-function "foo")
                 '(defun foo-api-version-check ()
                    "Signals if client version does not match original Swagger file."
                    (let* ((swagger-json (swelter--get-swagger-json foo-swagger-url))
                           (upstream-version (map-nested-elt swagger-json '("info" "version"))))
                      (unless (equal upstream-version foo-api-version)
                        (error (format "API version %s did not match client version %s" upstream-version foo-api-version)))
                      't)))))

(ert-deftest swelter--make-endpoint-function-params/test ()
  "Tests parameter lists can be generated from example objects."
  (let ((query-params
         (json-parse-string "{\"parameters\":[{\"in\":\"query\",\"description\":\"The number of items to skip before starting to collect the result set.\",\"type\":\"integer\",\"name\":\"offset\"},{\"in\":\"query\",\"description\":\"The numbers of items to return.\",\"type\":\"integer\",\"name\":\"limit\"}]}")))
    (should (equal (swelter--make-endpoint-function-params (map-elt query-params "parameters"))
                   '(&optional offset limit))))

  (let ((path-params (json-parse-string "{\"parameters\":[{\"in\":\"path\",\"description\":\"The user ID.\",\"minimum\":1,\"type\":\"integer\",\"required\":true,\"name\":\"id\"}]}")))
    ;; assume all path params are required
    (should (equal (swelter--make-endpoint-function-params (map-elt path-params "parameters"))
                   '(id))))

  (let ((form-params (json-parse-string "{\"parameters\":[{\"in\":\"formData\",\"description\":\"A person's name.\",\"type\":\"string\",\"name\":\"name\"},{\"in\":\"formData\",\"description\":\"A person's favorite number.\",\"type\":\"number\",\"name\":\"fav_number\"}]}")))
    (should (equal (swelter--make-endpoint-function-params (map-elt form-params "parameters"))
                   '(&optional name fav_number))))

  (let ((header-params (json-parse-string "{\"parameters\":[{\"in\":\"header\",\"required\":true,\"type\":\"string\",\"name\":\"X-Request-ID\"}]}")))
    (should (equal (swelter--make-endpoint-function-params (map-elt header-params "parameters"))
                   '(X-Request-ID))))

  (let ((body-param
         (json-parse-string "{\"parameters\":[{\"in\":\"body\",\"schema\":{\"type\":\"string\"},\"required\":true,\"name\":\"status\"}]}")))
    (should (equal (swelter--make-endpoint-function-params (map-elt body-param "parameters"))
                   '(status))))

  ;; combo of both rerquired and optional
  (let ((query-params
         (json-parse-string "{\"parameters\":[{\"in\":\"query\",\"description\":\"The number of items to skip before starting to collect the result set.\",\"type\":\"integer\",\"name\":\"offset\",\"required\": true},{\"in\":\"query\",\"description\":\"The numbers of items to return.\",\"type\":\"integer\",\"name\":\"limit\"}]}")))
    (should (equal (swelter--make-endpoint-function-params (map-elt query-params "parameters"))
                   '(offset &optional limit))))

  ;; required false
  (let ((query-params
         (json-parse-string "{\"parameters\":[{\"in\":\"query\",\"description\":\"The number of items to skip before starting to collect the result set.\",\"type\":\"integer\",\"name\":\"offset\",\"required\": false}]}")))
    (should (equal (swelter--make-endpoint-function-params (map-elt query-params "parameters"))
                   '(&optional offset)))))

(ert-deftest swelter--build-api-key/test ()
  "Tests normal behavior of header API key function."
  (let* ((sec-obj (json-parse-string "{
  \"type\": \"apiKey\",
  \"name\": \"api_key\",
  \"in\": \"header\"
}"))
         (client-name "45")
         (scheme-name "56")
         (result (swelter--build-api-key client-name scheme-name sec-obj)))
    ;; should return a list of lists
    (should (equal (type-of result)
                   'cons))
    (should (equal (type-of (car result))
                   'cons))

    ;; eval the generated function
    (dolist (form result)
      (eval form))

    ;; should create a header
    (setq 45-56-api-key "foobar123")
    (should (equal (apply (swelter--get-security-definition-function client-name scheme-name) ())
                   (cons "api_key" "foobar123")))

    ;; should be nil if var is nil
    (setq 45-56-api-key nil)
    (should-not (apply (swelter--get-security-definition-function client-name scheme-name) ()))))

(ert-deftest swelter--build-api-key/test-query-key ()
  "Should ignore API keys for the query string."
  (let* ((sec-obj (json-parse-string "{
  \"type\": \"apiKey\",
  \"name\": \"api_key\",
  \"in\": \"query\"
}"))
         (client-name "45")
         (scheme-name "56")
         (result (swelter--build-api-key client-name scheme-name sec-obj)))
    (should-not result)))

(ert-deftest swelter--build-basic-auth/test ()
  "Tests normal behavior of basic auth function."
  (let* ((sec-obj (json-parse-string "{
  \"type\": \"basic\"
}"))
         (client-name "101")
         (scheme-name "102")
         (result (swelter--build-basic-auth client-name scheme-name sec-obj)))
    ;; should return a list of lists
    (should (equal (type-of result)
                   'cons))
    (should (equal (type-of (car result))
                   'cons))

    ;; eval the generated function
    (dolist (form result)
      (eval form))

    ;; should create a header
    (with-mock
      (stub url-basic-auth => "foobar123")
      (should (equal (apply (swelter--get-security-definition-function client-name scheme-name) '(:server-root "http://foobar.com/"))
                     (cons "Authorization" "foobar123"))))

    ;; should be nil if result is nil
    (with-mock
      (stub url-basic-auth => nil)
      (should-not (apply (swelter--get-security-definition-function client-name scheme-name) '(:server-root "http://foobar.com/"))))))

(ert-deftest swelter--build-oauth/test ()
  "Tests normal behavior of header API key function."
  (let* ((sec-obj (json-parse-string "{
  \"type\": \"oauth2\",
  \"authorizationUrl\": \"http://swagger.io/api/oauth/dialog\",
  \"flow\": \"implicit\",
  \"scopes\": {
    \"write:pets\": \"modify pets in your account\",
    \"read:pets\": \"read your pets\"
  }
}"))
         (client-name "123")
         (scheme-name "456")
         (result (swelter--build-oauth client-name scheme-name sec-obj)))
    ;; should return a list of lists
    (should (equal (type-of result)
                   'cons))
    (should (equal (type-of (car result))
                   'cons))

    ;; eval the generated function
    (dolist (form result)
      (eval form))

    ;; should create a header
    (with-mock
      (mock
       (swelter--oauth-with-store
        * ;; implicit flow, but symbol doesn't match
        :auth-url "http://swagger.io/api/oauth/dialog"
        :token-url nil
        :client-id "id"
        :client-secret "secret"
        :scope "write:pets read:pets")
            => "foobar123")
      (setq 123-456-client-id "id"
            123-456-client-secret "secret")
      (should (equal (apply (swelter--get-security-definition-function client-name scheme-name) ())
                     (cons "Authorization" "Bearer foobar123"))))

    ;; should be nil if oauth result is nil
    (with-mock
      (stub swelter--oauth-with-store => nil)
      (should-not (apply (swelter--get-security-definition-function client-name scheme-name) ())))))

;; these integration tests also cover swelter--build-authorize-function
(ert-deftest swelter--authorize-fn/test-api-key ()
  "Generated auth function should handle API keys."
  (let* ((sec-defs-obj (json-parse-string "{
    \"api_key\": {
      \"type\": \"apiKey\",
      \"name\": \"api_key\",
      \"in\": \"header\"
    }}"))
         (client "401")
         (defs (swelter--build-security-definitions client sec-defs-obj))
         (auth (swelter--build-authorize-function client sec-defs-obj "http://foo.com")))

    (dolist (form defs)
      (eval form))
    (eval auth)

    (setq 401-api_key-api-key "foo123")
    (should (equal '(("api_key" . "foo123"))
                   (401-authorize (json-parse-string "[{ \"api_key\": [] }]"))))))

(ert-deftest swelter--authorize-fn/test-query-api-key ()
  "Generated auth function should ignore API keys in query."
  (let* ((sec-defs-obj (json-parse-string "{
    \"api_key\": {
      \"type\": \"apiKey\",
      \"name\": \"api_key\",
      \"in\": \"query\"
    }}"))
         (client "402")
         (defs (swelter--build-security-definitions client sec-defs-obj))
         (auth (swelter--build-authorize-function client sec-defs-obj "http://foo.com")))

    (should-not defs)
    (eval auth)

    ;; even though key is set it should not appear as a header
    (setq 402-api_key-api-key "foo123")
    (should-not (402-authorize (json-parse-string "[{ \"api_key\": [] }]")))))

(ert-deftest swelter--authorize-fn/test-oauth-scopes ()
  "Required scopes should be passed."
  (let* ((sec-defs-obj (json-parse-string "{
\"oauth\": {
  \"type\": \"oauth2\",
  \"authorizationUrl\": \"http://swagger.io/api/oauth/dialog\",
  \"flow\": \"implicit\",
  \"scopes\": {
    \"write:pets\": \"modify pets in your account\",
    \"read:pets\": \"read your pets\"
  }
}}"))
         (client "11")
         (defs (swelter--build-security-definitions client sec-defs-obj))
         (auth (swelter--build-authorize-function client sec-defs-obj "http://foo.com")))

    (dolist (form defs)
      (eval form))
    (eval auth)

    (setq 11-oauth-client-id "id"
          11-oauth-client-secret "secret")

    ;; empty scope should be passed thru
    (with-mock
      (mock (swelter--oauth-with-store
             *
             :auth-url "http://swagger.io/api/oauth/dialog"
             :token-url nil
             :client-id "id"
             :client-secret "secret"
             :scope "")
            => "foo123")
      (should (equal '(("Authorization" . "Bearer foo123"))
                     (11-authorize (json-parse-string "[{ \"oauth\": [] }]")))))

    ;; single required scope
    (with-mock
      (mock (swelter--oauth-with-store
             *
             :auth-url "http://swagger.io/api/oauth/dialog"
             :token-url nil
             :client-id "id"
             :client-secret "secret"
             :scope "write:pets")
            => "foo123")
      (should (equal '(("Authorization" . "Bearer foo123"))
                     (11-authorize (json-parse-string "[{ \"oauth\": [\"write:pets\"] }]")))))

    ;; required scope is not in securityDefinitions
    (with-mock
      (mock (swelter--oauth-with-store
             *
             :auth-url "http://swagger.io/api/oauth/dialog"
             :token-url nil
             :client-id "id"
             :client-secret "secret"
             :scope "admin")
            => "foo123")
      (should (equal '(("Authorization" . "Bearer foo123"))
                     (11-authorize (json-parse-string "[{ \"oauth\": [\"admin\"] }]")))))

    ;; multiple scopes
    (with-mock
      (mock (swelter--oauth-with-store
             *
             :auth-url "http://swagger.io/api/oauth/dialog"
             :token-url nil
             :client-id "id"
             :client-secret "secret"
             :scope "write:pets read:pets")
            => "foo123")
      (should (equal '(("Authorization" . "Bearer foo123"))
                     (11-authorize (json-parse-string "[{ \"oauth\": [\"write:pets\", \"read:pets\"] }]")))))))

(ert-deftest swelter--authorize-fn/test-no-auth ()
  "Authorize with empty security obj should not pass any headers."
  (let* ((sec-defs-obj (json-parse-string "{
    \"api_key\": {
      \"type\": \"apiKey\",
      \"name\": \"api_key\",
      \"in\": \"header\"
    }}"))
         (client "403")
         (defs (swelter--build-security-definitions client sec-defs-obj))
         (auth (swelter--build-authorize-function client sec-defs-obj "http://foo.com")))


    (dolist (form defs)
      (eval form))
    (eval auth)

    (setq 403-api_key-api-key "foo123")
    (should-not (403-authorize (json-parse-string "[]")))))

(ert-deftest swelter--authorize-fn/test-no-match ()
  "Authorize with non-matching security obj should not pass any headers."
  (let* ((sec-defs-obj (json-parse-string "{
    \"api_key\": {
      \"type\": \"apiKey\",
      \"name\": \"api_key\",
      \"in\": \"header\"
    }}"))
         (client "404")
         (defs (swelter--build-security-definitions client sec-defs-obj))
         (auth (swelter--build-authorize-function client sec-defs-obj "http://foo.com")))


    (dolist (form defs)
      (eval form))
    (eval auth)

    (setq 404-api_key-api-key "foo123")
    (should-not (404-authorize (json-parse-string "[{ \"scrog\": [] }]")))))

(ert-deftest swelter--authorize-fn/test-both ()
  "Authorize with two required schemes."
  (let* ((sec-defs-obj (json-parse-string "{
    \"key-a\": {
      \"type\": \"apiKey\",
      \"name\": \"key-a\",
      \"in\": \"header\"
    },
    \"key-b\": {
      \"type\": \"apiKey\",
      \"name\": \"key-b\",
      \"in\": \"header\"
    }
}"))
         (client "103")
         (defs (swelter--build-security-definitions client sec-defs-obj))
         (auth (swelter--build-authorize-function client sec-defs-obj "http://foo.com")))


    (dolist (form defs)
      (eval form))
    (eval auth)

    (setq 103-key-a-api-key "a")
    (setq 103-key-b-api-key "b")
    (should (equal '(("key-a" . "a") ("key-b" . "b"))
                   (103-authorize (json-parse-string "[{ \"key-a\": [], \"key-b\": [] }]"))))))

(ert-deftest swelter--authorize-fn/test-either ()
  "Authorize with two optional schemes."
  (let* ((sec-defs-obj (json-parse-string "{
    \"key-a\": {
      \"type\": \"apiKey\",
      \"name\": \"key-a\",
      \"in\": \"header\"
    },
    \"key-b\": {
      \"type\": \"apiKey\",
      \"name\": \"key-b\",
      \"in\": \"header\"
    }
}"))
         (client "104")
         (defs (swelter--build-security-definitions client sec-defs-obj))
         (auth (swelter--build-authorize-function client sec-defs-obj "http://foo.com")))


    (dolist (form defs)
      (eval form))
    (eval auth)

    ;; only one should be set
    (setq 104-key-a-api-key nil)
    (setq 104-key-b-api-key "b")
    (should (equal '(("key-b" . "b"))
             (104-authorize (json-parse-string "[{ \"key-a\": [] }, { \"key-b\": [] }]"))))))

(ert-deftest swelter--authorize-fn/test-nil ()
  "Authorize when securityDefinitions are nil."
  (let* ((sec-defs-obj nil)
         (client "100")
         (defs (swelter--build-security-definitions client sec-defs-obj))
         (auth (swelter--build-authorize-function client sec-defs-obj "http://foo.com")))

    (dolist (form defs)
      (eval form))
    (eval auth)

    ;; assume security object will be nil too
    (should-not (100-authorize (json-parse-string "[]")))))

;;; swelter-test.el ends here
