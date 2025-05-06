;; -*- lexical-binding: t -*-

;;; Code:

(require 'ert)
(require 'map)
(require 'el-mock)
(require 'swelter-oauth)

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
