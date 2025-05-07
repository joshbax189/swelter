;; -*- lexical-binding: t -*-

;;; Code:

(require 'ert)
(require 'map)
(require 'el-mock)
(require 'swelter-oauth)

(ert-deftest swelter-oauth--token-scope-difference/test-empty ()
  "Tests empty scope edge cases."
  (let* ((scope "read:profile write:profile")
         (token (make-oauth2-token
                 :client-id "a"
                 :client-secret "b"
                 :access-token "foobar"
                 :refresh-token "baz123"
                 :access-response `((scope . ,scope)))))
    (should-not (swelter-oauth--token-scope-difference token nil))
    (should-not (swelter-oauth--token-scope-difference token ""))
    (setf (oauth2-token-access-response token) '((scope . "")))
    (should-not (swelter-oauth--token-scope-difference token nil))))

(ert-deftest swelter-oauth--token-scope-difference/test-basic ()
  "Tests normal behavior."
  (let* ((scope "read:profile write:profile")
         (token (make-oauth2-token
                 :client-id "a"
                 :client-secret "b"
                 :access-token "foobar"
                 :refresh-token "baz123"
                 :access-response `((scope . ,scope)))))
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
                 (token (make-oauth2-token
                         :client-id client-id
                         :client-secret client-secret
                         :access-token "foobar"
                         :refresh-token "baz123"
                         :access-response `(("scope" . ,scope))))
                 result)
            ;; store a token
            (swelter-oauth--store-token token auth-url)
            ;; can get the same token back
            (setq result (swelter-oauth--get-stored-token auth-url client-id scope client-secret))
            (should result)
            (should (equal "foobar" (oauth2-token-access-token result)))))
      (setq swelter-oauth-token-file original-plstore))))

;;; swelter-oauth-test.el ends here
