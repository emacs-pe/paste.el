;;; paste.el --- Paste bin services integration  -*- lexical-binding: t -*-

;; Copyright (C) 2021 Mario Rodas <marsam@users.noreply.github.com>

;; Author: Mario Rodas <marsam@users.noreply.github.com>
;; URL: https://github.com/emacs-pe/paste.el
;; Keywords: convenience, web
;; Version: 0.1
;; Package-Requires: ((emacs "28.1"))

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Integration with pastebin services.
;;
;; Currently supported:
;; - <https://paste.centos.org/>
;; - <https://dpaste.org/>
;; - Wastebin instances:
;;   + <https://bin.bloerg.net/>
;; - Pinnwand instances:
;;   + <https://bpa.st/>
;;   + <https://rpa.st/>

;; -------------------------------------------------------------------
;; Israel is committing genocide of the Palestinian people.
;;
;; The population in Gaza is facing starvation, displacement and
;; annihilation amid relentless bombardment and suffocating
;; restrictions on life-saving humanitarian aid.
;;
;; As of March 2025, Israel has killed over 50,000 Palestinians in the
;; Gaza Strip – including 15,600 children – targeting homes,
;; hospitals, schools, and refugee camps.  However, the true death
;; toll in Gaza may be at least around 41% higher than official
;; records suggest.
;;
;; The website <https://databasesforpalestine.org/> records extensive
;; digital evidence of Israel's genocidal acts against Palestinians.
;; Save it to your bookmarks and let more people know about it.
;;
;; Silence is complicity.
;; Protest and boycott the genocidal apartheid state of Israel.
;;
;;
;;                  From the river to the sea, Palestine will be free.
;; -------------------------------------------------------------------

;;; Code:
(eval-when-compile
  (defvar url-http-end-of-headers)
  (defvar url-http-response-status))

(require 'transient)
(require 'url-http)

(autoload 'url-expand-file-name "url-expand")

(define-error 'paste-error "Unknown paste error")

(defgroup paste nil
  "Integration with patebin services."
  :prefix "paste-"
  :group 'tools)

(defcustom paste-pinnwand-instance "https://bpa.st/"
  "Default Pinnwand instance to use."
  :type '(choice
          (const :tag "rpa.st" "https://rpa.st/")
          (const :tag "bpa.st" "https://bpa.st/")
          string)
  :group 'paste)

(defcustom paste-dpaste-instance "https://dpaste.org/"
  "Default DPaste instance to use."
  :type '(choice
          (const :tag "dpaste.org" "https://dpaste.org/")
          string)
  :group 'paste)

(defcustom paste-wastebin-instance "https://bin.bloerg.net/"
  "Default Wastebin instance to use."
  :type '(choice
          (const :tag "bin.bloerg.net" "https://bin.bloerg.net/")
          string)
  :group 'paste)

(defalias 'paste--json-read
  (if (fboundp 'json-parse-buffer)
      (lambda ()
        (json-parse-buffer :object-type 'plist
                           :null-object nil
                           :false-object :json-false))
    (require 'json)
    (defvar json-object-type)
    (declare-function json-read "json" ())
    (lambda ()
      (let ((json-object-type 'plist))
        (json-read))))
  "Read JSON object in buffer, move point to end of buffer.")

(defalias 'paste--json-encode
  (if (fboundp 'json-serialize)
      (lambda (object)
        (json-serialize object
                        :false-object :json-false
                        :null-object nil))
    (require 'json)
    (defvar json-false)
    (defvar json-null)
    (declare-function json-encode "json" (object))
    (lambda (object)
      (let ((json-false :json-false)
            (json-null nil))
        (json-encode object))))
  "Encode OBJECT into a JSON string.")

(defvar paste-language-alist
  '((c++         . "cpp")
    (conf-toml   . "toml")
    (fundamental . "text")
    (makefile    . "make")
    (nxml        . "xml")
    (sh          . "bash"))
  "Language alist.")

(defun paste--collect-args (args &optional function)
  "Convert transient ARGS using FUNCTION."
  (cl-loop for arg in args
           for (key value) = (split-string arg "=" t)
           collect (funcall (or function #'list) (string-remove-prefix "--" key) (or value "1"))))

(defun paste--text ()
  "Generate the text to send it to the paste-bin service."
  (apply #'buffer-substring-no-properties
         (if (use-region-p)
             (list (region-beginning) (region-end))
           (list (point-min) (point-max)))))

(defun paste-current-language (&optional override)
  "Get the paste language from the current `major-mode' with OVERRIDE."
  (let ((lang (let ((mode (symbol-name
                           (pcase (get major-mode 'derived-mode-parent)
                             ((or 'nil 'magit-mode 'tabulated-list-mode 'special-mode) 'text-mode)
                             ((or 'text-mode 'prog-mode) major-mode)
                             (parent parent)))))
                (string-match "\\`\\(.+\\)-mode\\'" mode)
                (thread-last
                  (match-string 1 mode)
                  (string-remove-suffix "-base")
                  (string-remove-suffix "-data")
                  (string-remove-suffix "-ts")))))
    (or (cdr (assq (intern lang) (append override paste-language-alist))) lang)))

(cl-defun paste-request (url
                         &key data headers (reader 'raw) (method "GET")
                         &aux (parser (pcase reader
                                        (`json #'paste--json-read)
                                        (`raw  (lambda () (buffer-substring (point) (point-max))))
                                        ((pred functionp) (lambda () (funcall reader (point) (point-max))))
                                        (_ (user-error "Unsupported reader: %s" reader)))))
  "Send request to URL with METHOD, HEADERS, and sending DATA."
  (declare (indent 1))
  (let ((url-request-data data)
        (url-request-method method)
        (url-request-extra-headers headers))
    (with-current-buffer (url-retrieve-synchronously url 'silent)
      (goto-char (1+ url-http-end-of-headers))
      (pcase url-http-response-status
        ((or 200 201) (funcall parser))
        (_   (signal 'paste-error (list (buffer-substring (point) (point-max)))))))))

;;;###autoload (autoload 'paste-centos "paste" nil t)
(transient-define-prefix paste-centos (&optional transient args)
  "Create a paste in CentOS paste service.

See: <https://paste.centos.org/>"
  :value (lambda () (list "--private" "--expire=1440" (concat "--lang=" (paste-current-language))))
  ["Arguments"
   ("-t" "Title"          "--title=")
   ("-n" "Author"         "--name=")
   ("-l" "Language"       "--lang=")
   ("-p" "Private"        "--private")
   ("-e" "Expire minutes" "--expire=" transient-read-number-N+)]
  [:description
   (lambda ()
     (propertize (if (use-region-p) "Paste Selected Region" "Paste Entire Buffer") 'face 'transient-heading))
   ("C" "Create" paste-centos)]
  (interactive (if (eq transient-current-command 'paste-centos)
                   (list nil (transient-args 'paste-centos))
                 (list t)))
  (if transient
      (transient-setup 'paste-centos)
    (message "Creating paste of %s..." (if (use-region-p) "Selected Region" "Entire Buffer"))
    (paste-request "https://paste.centos.org/api/create?apikey=5uZ30dTZE1a5V0WYhNwcMddBRDpk6UzuzMu-APKM38iMHacxdA0n4vCqA34avNyt"
      :method "POST"
      :headers '(("Content-Type" . "application/x-www-form-urlencoded"))
      :data (url-build-query-string (cons (list "text" (paste--text)) (paste--collect-args args)))
      :reader (lambda (start _end)
                (kill-new (message "%s" (buffer-substring start (line-end-position))))))))

;;;###autoload (autoload 'paste-pinnwand "paste" nil t)
(transient-define-prefix paste-pinnwand (&optional transient args)
  "Create a paste in a Pinnwand instance.

See: <https://github.com/supakeen/pinnwand>."
  :value (lambda () (list "--expire=1day" (concat "--language=" (paste-current-language '((nix . "nixos"))))))
  ["Arguments"
   ("-n" "Name"      "--name=")
   ("-l" "Language"  "--language=")
   ("-e" "Expire"    "--expire=")]
  [:description
   (lambda ()
     (propertize (if (use-region-p) "Paste Selected Region" "Paste Entire Buffer") 'face 'transient-heading))
   ("C" "Create" paste-pinnwand)]
  (interactive (if (eq transient-current-command 'paste-pinnwand)
                   (list nil (transient-args 'paste-pinnwand))
                 (list t)))
  (if transient
      (transient-setup 'paste-pinnwand)
    (message "Creating paste of %s..." (if (use-region-p) "Selected Region" "Entire Buffer"))
    (let-alist (paste--collect-args args (lambda (k v) (cons (intern k) v)))
      (let* ((url (url-expand-file-name "/api/v1/paste" paste-pinnwand-instance))
             (response (paste-request url
                         :method "POST"
                         :headers '(("Content-Type" . "application/json"))
                         :data (paste--json-encode
                                (list
                                 :expiry .expire
                                 :files (vector `((name . ,.name) (content . ,(paste--text)) (lexer . ,(or .language "text"))))))
                         :reader 'json)))
        (message "Paste created, removal link: %s" (plist-get response :removal))
        (kill-new (message "%s" (plist-get response :link)))))))

;;;###autoload (autoload 'paste-dpaste "paste" nil t)
(transient-define-prefix paste-dpaste (&optional transient args)
  "Create a paste in a DPaste instance.

See: <https://dpaste.org/>."
  :value (lambda () (list "--expires=604800" (concat "--lexer=" (paste-current-language))))
  ["Arguments"
   ("-n" "Filename"       "--filename=")
   ("-l" "Language"       "--lexer=")
   ("-e" "Expire seconds" "--expires=" :choices ("onetime" "never""3600" "604800" "2592000"))]
  [:description
   (lambda ()
     (propertize (if (use-region-p) "Paste Selected Region" "Paste Entire Buffer") 'face 'transient-heading))
   ("C" "Create" paste-dpaste)]
  (interactive (if (eq transient-current-command 'paste-dpaste)
                   (list nil (transient-args 'paste-dpaste))
                 (list t)))
  (if transient
      (transient-setup 'paste-dpaste)
    (message "Creating paste of %s..." (if (use-region-p) "Selected Region" "Entire Buffer"))
    (paste-request (url-expand-file-name "/api/" paste-dpaste-instance)
      :method "POST"
      :headers '(("Content-Type" . "application/x-www-form-urlencoded"))
      :data (url-build-query-string (cons (list "content" (paste--text)) (paste--collect-args args)))
      :reader (lambda (start _end)
                (kill-new (message "%s" (substring (substring (buffer-substring start (line-end-position)) 0 -1) 1)))))))

;;;###autoload (autoload 'paste-wastebin "paste" nil t)
(transient-define-prefix paste-wastebin (&optional transient args)
  "Create a paste in a Wastebin instance.

See: <https://github.com/matze/wastebin>."
  :value (lambda () (append '("--expires=604800") (and buffer-file-name (list (concat "--extension=" (file-name-extension buffer-file-name))))))
  ["Arguments"
   ("-t" "Title"          "--title=")
   ("-E" "Extension"      "--extension=")
   ("-P" "Password"       "--password=")
   ("-b" "Burn after reading" "--burn_after_reading")
   ("-e" "Expire seconds" "--expires=" transient-read-number-N+)]
  [:description
   (lambda ()
     (propertize (if (use-region-p) "Paste Selected Region" "Paste Entire Buffer") 'face 'transient-heading))
   ("C" "Create" paste-wastebin)]
  (interactive (if (eq transient-current-command 'paste-wastebin)
                   (list nil (transient-args 'paste-wastebin))
                 (list t)))
  (if transient
      (transient-setup 'paste-wastebin)
    (message "Creating paste of %s..." (if (use-region-p) "Selected Region" "Entire Buffer"))
    (let ((response (paste-request paste-wastebin-instance
                      :method "POST"
                      :headers '(("Content-Type" . "application/json"))
                      :data (let ((params (paste--collect-args args (lambda (k v) (cons (intern k) v)))))
                              (when (assq 'expires params)
                                (setf (cdr (assq 'expires params)) (string-to-number (cdr (assq 'expires params)))))
                              (when (assq 'burn_after_reading params)
                                (setf (cdr (assq 'burn_after_reading params)) t))
                              (paste--json-encode (cons (cons 'text (paste--text)) params)))
                      :reader 'json)))
      (kill-new (message "%s" (url-expand-file-name (plist-get response :path) paste-wastebin-instance))))))

(provide 'paste)
;;; paste.el ends here
