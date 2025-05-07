;;; swelter-oauth.el --- OAuth functions for Swelter clients -*- lexical-binding: t -*-

;; Author: Josh Bax
;; Maintainer: Josh Bax
;; Version: 0.0.1
;; Package-Requires: (dependencies)
;; Homepage: https://github.com/joshbax189/swelter


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; Standalone OAuth functions that can be called by Swelter clients

;;; Code:

(require 'cl-lib)
(require 'url)
(require 'seq)
(require 'map)
(require 'aio)
(require 'elnode)

;;;; oauth2 copied
;; see e.g. https://sourcegraph.com/github.com/syl20bnr/spacelpa/-/blob/packages/oauth2-0.11.el

(defcustom swelter-oauth-token-file (concat user-emacs-directory "swelter-oauth.plstore")
  "Token storage for Swelter clients."
  :group 'swelter
  :type 'file)

;; TODO update to something useful
;; TODO namespace to this package

(cl-defstruct oauth2-token
  client-id
  client-secret
  token-url
  access-token
  refresh-token
  access-response
  plstore
  plstore-id)

;; NOTE unlike the original oauth2 method, redirect-uri is not optional
(defun oauth2-request-access (token-url client-id client-secret code redirect-uri)
  "Request OAuth access at TOKEN-URL.
The CODE should be obtained with `oauth2-request-authorization'.
Return an `oauth2-token' structure."
  (when code
    (let* ((query (url-build-query-string `(("client_id" ,client-id)
                                            ("client_secret" ,client-secret)
                                            ("code" ,code)
                                            ("redirect_uri" ,redirect-uri)
                                            ("grant_type" "authorization_code"))))
           (result (oauth2-make-access-request
                    token-url
                    query)))
      (make-oauth2-token :client-id client-id
                         :client-secret client-secret
                         :access-token (map-elt result 'access_token)
                         :refresh-token (map-elt result 'refresh_token)
                         :token-url token-url
                         :access-response result))))

(defun oauth2-make-access-request (url data)
  "Make an access request to URL using DATA in POST."
  (let ((url-request-method "POST")
        (url-request-data data)
        (url-request-extra-headers
         '(("Content-Type" . "application/x-www-form-urlencoded"))))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (when (search-forward-regexp "^$" nil t)
        ;; TODO which JSON method here?
        ;; It appears as oauth2-token-access-response
        (let ((data (json-read)))
          (prog1 data
            (kill-buffer (current-buffer))))))))

;; following tests from LSP-mode
(defun swelter-oauth--port-available (port)
  "Return non-nil if PORT is available."
  (condition-case _err
      (delete-process (open-network-stream "*connection-test*" nil "localhost" port :type 'plain))
    (file-error t)))

(defun swelter-oauth--find-available-port (starting-port)
  "Find available port starting from STARTING-PORT."
  (let ((port starting-port))
    (while (not (swelter-oauth--port-available port))
      (cl-incf port))
    port))

;;;; Helpers
(defun swelter-oauth--make-state-string (&optional result-length)
  "Generate a random alpha-num string of length RESULT-LENGTH (default 20)."
  (let (res)
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

(defun swelter-oauth--swagger-oauth-scopes-to-string (scope-obj)
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

;;;; Flows
(cl-defun swelter-oauth-auth-code-flow (&key auth-url token-url client-id client-secret scope &allow-other-keys)
  "Login using auth code flow."
  ;; TODO can it use https?
  ;; network-stream.el can, could make a simple elnode replacement that just reads the HTTP message
  (let* ((promise (aio-promise))
         (port (swelter-oauth--find-available-port 8000))
         (redirect-uri (format "http://localhost:%s/foo" port))
         (state (swelter-oauth--make-state-string))
         (query `(("response_type" "code")
                  ("client_id" ,client-id)
                  ("redirect_uri" ,redirect-uri)
                  ("state" ,state)))
         (query (if scope
                    (append query `(("scope" ,(swelter-oauth--swagger-oauth-scopes-to-string scope))))
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
(cl-defun swelter-oauth-implicit-flow (&key auth-url client-id scope &allow-other-keys)
  "Login using implicit grant flow. Deprecated."
  (error "Implicit flow is not supported"))

;; TODO check that it requires auth-url not token-url
(cl-defun swelter-oauth-password-flow (&key auth-url client-id client-secret scope &allow-other-keys)
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

;; TODO check that it requires auth-url not token-url
(cl-defun swelter-oauth-application-flow (&key auth-url client-id client-secret scope &allow-other-keys)
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

(cl-defun swelter-oauth-refresh-flow (&key token-url token client-id client-secret scope &allow-other-keys)
  "Auth using refresh token TOKEN (a string).

CLIENT-SECRET If the application is a “confidential client” (not a mobile or JavaScript app),
              then the secret is included as well.
SCOPE Optional, must match scope of original token."
  (let* ((url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/x-www-form-urlencoded")))
         (form-data `(("grant_type" "refresh_token")
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
      (with-current-buffer (url-retrieve-synchronously token-url)
        (goto-char (point-min))
        (while (looking-at "^.") (delete-line))
        (let ((result (json-parse-buffer)))
          (aio-resolve promise
                       (lambda ()
                         (make-oauth2-token
                          :client-id client-id
                          :client-secret client-secret
                          :token-url token-url
                          :access-token (map-elt result "access_token")
                          ;; Assume current refresh token remains valid
                          :refresh-token (or (map-elt result "refresh_token") token)
                          :access-response result))))))))

;;;; Stored Tokens
(defun swelter-oauth--get-stored-token (auth-url client-id &optional client-secret token-url)
  "Get and rehydrate stored token for AUTH-URL and CLIENT-ID.

Note CLIENT-SECRET and TOKEN-URL are only used to rehydrate the token."
  (let* ((plstore (plstore-open swelter-oauth-token-file))
         ;; NOTE: see oauth2-compute-id
         (id (secure-hash 'md5 (concat auth-url client-id)))
         (plist (cdr (plstore-get plstore id))))
    (when plist
      ;; TODO tokens created for the same domain but different methods will collide
      ;;      Fix by creating new token struct
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

(defun swelter-oauth--store-token (token auth-url)
  "Store TOKEN against AUTH-URL."
  (let ((id (secure-hash 'md5 (concat auth-url
                                      (oauth2-token-client-id token))))
        (plstore (plstore-open swelter-oauth-token-file)))
    ;; Set the plstore in the token
    (setf (oauth2-token-plstore token) plstore)
    (setf (oauth2-token-plstore-id token) id)
    (message "storing token for %s" auth-url)
    (plstore-put plstore id nil `(:access-token
                                  ,(oauth2-token-access-token token)
                                  :refresh-token
                                  ,(oauth2-token-refresh-token token)
                                  :access-response
                                  ,(oauth2-token-access-response token)))
    (plstore-save plstore)
    token))

(defun swelter-oauth--token-scope (token)
  "Get TOKEN scope as a list of strings."
  ;; FIXME: actually store scopes in the main token
  (when-let* ((scope (map-elt
                      (oauth2-token-access-response token) 'scope)))
   (string-split scope " " t)))

(defun swelter-oauth--token-scope-difference (token scope)
  "Return list of scopes in SCOPE but not in TOKEN struct's scopes.

SCOPE may be a sequence of strings or a single string with space-delimited scopes."
  (when (stringp scope)
    (setq scope (string-split scope " " t)))
  (let ((token-scopes
         (swelter-oauth--token-scope token)))
    (seq-difference scope token-scopes)))

;; NOTE assumes TOKEN has access-response that includes expires_in
(defun swelter-oauth--token-time-until-expiry (token)
  "Seconds remaining before TOKEN expires.

Result is negative if TOKEN is already expired, positive if still valid,
and nil if expiry time could not be determined."
  ;; if there is no access token, then return nil
  (when-let* ((access-token (oauth2-token-access-token token))
              ;; TODO perhaps replace with jwt lib
              (jwt-payload (nth 1 (string-split access-token "\\.")))
              (jwt-payload (json-parse-string (base64-decode-string jwt-payload 't))))

    (let* ((expiry (map-elt (oauth2-token-access-response token) 'expires_in))
           (jwt-iat (map-elt jwt-payload "iat"))
           (jwt-exp (map-elt jwt-payload "exp"))
           (time-seconds (time-convert (current-time) 'integer)))
      (cond
       ;; A JWT "exp" claim gives the absolute expiry time
       (jwt-exp
        (- jwt-exp time-seconds))
       ;; Otherwise the response may give a lifetime in seconds
       ;; This needs the JWT "iat" claim to work out the actual age
       ((and jwt-iat expiry)
        (- (+ jwt-iat expiry) time-seconds))))))

;; TODO scope may be empty string or nil
(cl-defun swelter-oauth-auth-with-store (method &key auth-url token-url client-id client-secret scope)
  "Auth perhaps with a stored token."
  (let* ((token (swelter-oauth--get-stored-token auth-url
                                    client-id
                                    client-secret
                                    token-url))
         (expiry (when token
                   (swelter-oauth--token-time-until-expiry token)))
         (missing-scopes (when token
                           (swelter-oauth--token-scope-difference token scope))))
    (cond
     ;; cache miss
     ((not token)
      (swelter-oauth--store-token
       (aio-wait-for
        (funcall method
                 :auth-url auth-url
                 :token-url token-url
                 :client-id client-id
                 :client-secret client-secret
                 :scope scope))
       auth-url))
     ;; if there are missing scopes then refresh will not help
     (missing-scopes
      (swelter-oauth--store-token
       (aio-wait-for
        (funcall method
                 :auth-url auth-url
                 :token-url token-url
                 :client-id client-id
                 :client-secret client-secret
                 ;; NOTE: extends stored scope with missing scope
                 :scope (append missing-scopes (swelter-oauth--token-scope token))))
       auth-url))
     ;; expired
     ((or (not expiry) (<= expiry 0))
      (if-let* ((refresh-token (oauth2-token-refresh-token token)))
          (swelter-oauth--store-token
           (aio-wait-for
            (swelter-oauth-refresh-flow
             :token-url token-url
             :token refresh-token
             :client-id client-id
             :client-secret client-secret
             :scope scope))
           auth-url)
        ;; else get a new token
        (swelter-oauth--store-token
         (aio-wait-for
          (funcall method
                   :auth-url auth-url
                   :token-url token-url
                   :client-id client-id
                   :client-secret client-secret
                   :scope scope))
         auth-url)))
     ;; otherwise use stored token
     (t token))))

(provide 'swelter-oauth)

;;; swelter-oauth.el ends here
