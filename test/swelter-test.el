;; -*- lexical-binding: t -*-

(require 'ert)
(require 'map)
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

(ert-deftest swelter--get-security-definitions/test ()
  "Should work on petstore example."
  (let ((json (json-parse-string "{
  \"api_key\": {
    \"type\": \"apiKey\",
    \"name\": \"api_key\",
    \"in\": \"header\"
  },
  \"petstore_auth\": {
    \"type\": \"oauth2\",
    \"authorizationUrl\": \"http://swagger.io/api/oauth/dialog\",
    \"flow\": \"implicit\",
    \"scopes\": {
      \"write:pets\": \"modify pets in your account\",
      \"read:pets\": \"read your pets\"
    }
  }
}" :object-type 'hash-table)))
    (should (equal
             (swelter--get-security-definitions json)
             '(("api_key" :header ("api_key" . api-key))
               ("petstore_auth" :header ("Authorization" (format "Bearer %s" (swelter-oauth-implicit "http://swagger.io/api/oauth/dialog" client-id client-secret scopes)))))
             ))))

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

(ert-deftest swelter--resolve-json-ref/test ()
  "Should work on petstore example."
  (let ((json (json-parse-string "{
  \"api_key\": {
    \"$ref\": \"#/keys/foo\"
  },
  \"keys\": {
    \"foo\": {
      \"x\": 1,
      \"y\": 2
    },
    \"bar\": [ \"x\", \"y\", \"z\" ]
  }
}" :object-type 'hash-table)))
   (should (equal
            (map-pairs (swelter--resolve-json-ref "#/keys/foo" json))
            '(("x" . 1) ("y" . 2))))
   (should (equal
            (swelter--resolve-json-ref "#/keys/bar/1" json)
            "y"))))

(ert-deftest swelter--replace-all-json-refs/test ()
  "Should work on petstore example."
  (let ((json (json-parse-string "{
  \"api_key\": {
    \"$ref\": \"#/keys/foo\"
  },
  \"keys\": {
    \"foo\": {
      \"x\": 1,
      \"y\": 2
    }
  }
}" :object-type 'hash-table)))
    (should (equal
             (swelter--replace-all-json-refs json)
             '(("api_key" . (("x" . 1) ("y" . 2))) ("keys" . (("foo" . (("x" . 1) ("y" . 2))))))
             ))))

(ert-deftest swelter--replace-all-json-refs/test-unknown ()
  "Should work on petstore example."
  (let ((json (json-parse-string "{
  \"api_key\": {
    \"$ref\": \"https://example.com/foo/bar/baz#Bam\"
  },
  \"keys\": {
    \"foo\": {
      \"x\": 1,
      \"y\": 2
    }
  }
}" :object-type 'hash-table)))
    (should (equal
             (swelter--replace-all-json-refs json)
             '(("api_key" . (("$ref" . "https://example.com/foo/bar/baz#Bam"))) ("keys" . (("foo" . (("x" . 1) ("y" . 2))))))
             ))))

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
  ;; TODO
  ;; (let ((header-params (json-parse-string "{\"parameters\":[{\"in\":\"header\",\"required\":true,\"type\":\"string\",\"name\":\"X-Request-ID\"}]}")))
  ;;   (should (equal (swelter--make-endpoint-function-params (map-elt header-params "parameters"))
  ;;                  '(x-request-id))))

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

(ert-deftest swelter--store-token/test ()
  "Should store tokens without collision."
  (let ((original-plstore oauth2-token-file))
    (unwind-protect
        (progn
          (setq oauth2-token-file "./test-tokens")
          (let* ((auth-url "https://example.com/authorize")
                 (client-id "a-client")
                 (client-secret "a-secret")
                 (scope "read:profile write:profile")
                 (token (make-oauth2-token
                         :client-id client-id
                         :client-secret client-secret
                         :access-token "foobar"
                         :refresh-token "baz123"
                         :access-response `(("scope" . ,scope))))
                 result)
            ;; store a token
            (swelter--store-token token auth-url)
            ;; can get the same token back
            (setq result (swelter--get-stored-token auth-url client-id scope client-secret))
            (should result)
            (should (equal "foobar" (oauth2-token-access-token result)))))
      (setq oauth2-token-file original-plstore))))
