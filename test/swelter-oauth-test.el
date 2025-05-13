;; -*- lexical-binding: t -*-

;;; Code:

(require 'ert)
(require 'map)
(require 'el-mock)
(require 'json)
(require 'swelter-oauth)

(defun swelter-oauth-test--make-jwt (obj)
  "Make a mock JWT that encodes OBJ."
  (let* ((jwt-payload (json-encode obj))
         (jwt-encoded (base64-encode-string jwt-payload)))
    (concat "header." jwt-encoded ".signature")))

(ert-deftest swelter-oauth--make-state-string/test ()
  "Test basic behavior."
  (let* ((n 5)
         (result (swelter-oauth--make-state-string n)))
    (should result)
    (should (stringp result))
    (should (equal (length result)
                   n))))

(ert-deftest swelter-oauth--token-scope-difference/test-empty ()
  "Tests empty scope edge cases."
  (let* ((scope "read:profile write:profile")
         (token (make-swelter-oauth-token
                 :client-id "a"
                 :client-secret "b"
                 :access-token "foobar"
                 :refresh-token "baz123"
                 :access-response `((scope . ,scope)))))
    (should-not (swelter-oauth--token-scope-difference token nil))
    (should-not (swelter-oauth--token-scope-difference token ""))
    (setf (swelter-oauth-token-access-response token) '((scope . "")))
    (should-not (swelter-oauth--token-scope-difference token nil))))

(ert-deftest swelter-oauth--token-scope-difference/test-basic ()
  "Tests normal behavior."
  (let* ((scope "read:profile write:profile")
         (token (make-swelter-oauth-token
                 :client-id "a"
                 :client-secret "b"
                 :access-token "foobar"
                 :refresh-token "baz123"
                 :scope scope)))
    (should-not (swelter-oauth--token-scope-difference token scope))
    ;; read:profile is in token, so ok
    (should-not (swelter-oauth--token-scope-difference token '("read:profile")))
    (should (equal (swelter-oauth--token-scope-difference token '("foo"))
                   '("foo")))
    ;; out of order
    (should-not (swelter-oauth--token-scope-difference token '("write:profile" "read:profile")))))

(ert-deftest swelter-oauth--store-token/test ()
  "Should store tokens without collision."
  (let ((original-plstore swelter-oauth-token-file))
    (unwind-protect
        (progn
          (setq swelter-oauth-token-file "./test-tokens")
          (let* ((auth-url "https://example.com/authorize")
                 (client-id "a-client")
                 (client-secret "a-secret")
                 (scope "read:profile write:profile")
                 (token (make-swelter-oauth-token
                         :client-id client-id
                         :client-secret client-secret
                         :access-token "foobar"
                         :refresh-token "baz123"
                         :scope scope
                         :access-response `((scope . ,scope))))
                 result)
            ;; store a token
            ;; TODO this triggers a manual keyring unlock?
            (swelter-oauth--store-token token auth-url)
            ;; can get the same token back
            (setq result (swelter-oauth--get-stored-token auth-url client-id client-secret "" '("read:profile")))
            (should result)
            (should (equal "foobar" (swelter-oauth-token-access-token result)))))
      (setq swelter-oauth-token-file original-plstore))))

(ert-deftest swelter-oauth--token-time-until-expiry/jwt-exp ()
  "It should use the JWT exp claim when present."
  (let* ((now (time-convert (current-time) 'integer))
         (jwt-exp (+ now 3600))
         (jwt-payload `(("exp" . ,jwt-exp)))
         (token (make-swelter-oauth-token :access-token (swelter-oauth-test--make-jwt jwt-payload))))
    (should (eq (- jwt-exp now)
                (swelter-oauth--token-time-until-expiry token)))))

(ert-deftest swelter-oauth--token-time-until-expiry/jwt-iat ()
  "It should use the JWT iat claim when exp is not present."
  (let* ((now (time-convert (current-time) 'integer))
         (jwt-iat (- now 3600))
         (expires-in 7200)
         (jwt-payload `(("iat" . ,jwt-iat)))
         (token (make-swelter-oauth-token :access-token (swelter-oauth-test--make-jwt jwt-payload)
                                   :access-response `((expires_in . ,expires-in)))))
    (should (eq (- (+ jwt-iat expires-in) now)
                (swelter-oauth--token-time-until-expiry token)))))

(ert-deftest swelter-oauth--token-time-until-expiry/expired ()
  "It should be negative when token is expired."
  (let* ((now (time-convert (current-time) 'integer))
         (jwt-exp (- now 3600)) ;; expiry in the past
         (jwt-payload `(("exp" . ,jwt-exp)))
         (token (make-swelter-oauth-token :access-token (swelter-oauth-test--make-jwt jwt-payload))))
    (should (< (swelter-oauth--token-time-until-expiry token) 0))))

(ert-deftest swelter-oauth--token-time-until-expiry/non-jwt ()
  "Non-JWT tokens can have an expires_at header."
  (let* ((now (time-convert (current-time) 'integer))
         (expires-at (+ now 3600))
         (token (make-swelter-oauth-token :access-token "00000000"
                                   :access-response `((expires_at . ,expires-at)))))
    (should (> (swelter-oauth--token-time-until-expiry token) 0))))

(ert-deftest swelter-oauth--token-time-until-expiry/no-expiry-info ()
  (let ((token (make-swelter-oauth-token)))
    (should-not (swelter-oauth--token-time-until-expiry token))))

;;; swelter-oauth-test.el ends here
