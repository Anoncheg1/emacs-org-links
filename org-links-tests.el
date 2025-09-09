;;; org-links-tests.el --- Search line and fallback to number [[PATH::NUM::LINE]] -*- lexical-binding: t -*-

;; Author: <github.com/Anoncheg1,codeberg.org/Anoncheg>
;; Keywords: org, text, hypermedia, url
;; URL: https://github.com/Anoncheg1/emacs-org-links
;; Version: 0.1
;; Created: 30 Aug 2025
;; Package-Requires: ((emacs "27.1") (compat "30.1"))
;; > (Emacs 26+) for negative regex
;; "27.1" for ol.el
;; SPDX-License-Identifier: AGPL-3.0-or-later
;; Copyright (c) 2025 github.com/Anoncheg1,codeberg.org/Anoncheg


;;; License

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; Licensed under the GNU Affero General Public License, version 3 (AGPLv3)
;; <https://www.gnu.org/licenses/agpl-3.0.en.html>

;;; Commentary
;; (eval-buffer) or (load-file "path/to/async-tests.el")
;; Running Tests: Load the test file and run:
;; (eval-buffer)
;; (ert t)
;; to execute all tests. Individual tests can be run with (ert 'test-name).


;;; Code:
(require 'ert)
(require 'org-links)

;;; - Help functions
(defun set-major-mode (mode)
  (funcall mode))

(defun stub-fn (name value)
  (fset name (lambda (&rest _args) value)))

(defun stub-org-store-link (val)
  (fset 'org-store-link (lambda (&optional _arg) val)))

(defun with-temp-file-buffer (filename thunk)
  "Create temp buffer visiting FILENAME, call THUNK, then kill."
  (let ((buf (generate-new-buffer "*test*")))
    (unwind-protect
         (with-current-buffer buf
           (set-visited-file-name filename t t)
           (set-buffer-modified-p nil)
           (funcall thunk))
      (kill-buffer buf))))

;;; - org-links-store-link-fallback
;; Helper usage already defined above
(ert-deftest org-links-tests-store-link-fallback--thumbnail-mode ()
  (let ((kill-ring nil))
    (with-temp-buffer
      (set-major-mode 'image-dired-thumbnail-mode)
      (cl-letf (((symbol-function 'image-dired-original-file-name)
                 (lambda () "foo.png")))
        (org-links-store-link-fallback nil)
        (should (equal (car kill-ring) "[[file:foo.png]]"))))))

(ert-deftest org-links-tests-store-link-fallback--image-mode ()
  (let ((kill-ring nil))
    (with-temp-file-buffer "/bar/image.jpg"
      (lambda ()
        (set-major-mode 'image-dired-image-mode)
        (cl-letf (((symbol-function 'buffer-base-buffer) (lambda (&optional _buf) (current-buffer))))
          (org-links-store-link-fallback nil)
          (should (equal (car kill-ring) "[[file:/bar/image.jpg]]")))))))

(ert-deftest org-links-tests-store-link-fallback--prog-mode-with-arg ()
  (let ((kill-ring nil))
    (with-temp-buffer
      (set-major-mode 'prog-mode)
      (goto-char (point-max))
      (cl-letf (((symbol-function 'org-store-link)
                 (lambda (&optional _arg) #("[[file:~/.emacs::substring-no-properties (org-store-link nil))))))]]" 17 35 (fontified t) 35 40 (fontified t) 41 56 (fontified t) 57 66 (fontified t))))) ; "[[file:~/sources/emacs-org-links/org-links.el]]"
        (org-links-store-link-fallback t)
        (should (equal (car kill-ring) "[[file:~/.emacs::substring-no-properties (org-store-link nil))))))]]"))))))

(ert-deftest org-links-tests-store-link-fallback--fundamental-mode-no-arg ()
  (let ((kill-ring nil))
    (with-temp-buffer
      (set-major-mode 'fundamental-mode)
      ;; what org-store-link returns
      (cl-letf (((symbol-function 'org-store-link)
                                  (lambda (&optional _arg) "[[file:/fundamental.txt]]")))
        (org-links-store-link-fallback nil)
        ;; (print (car kill-ring)))))
        (should (equal (car kill-ring) "[[file:/fundamental.txt::1]]"))))))

(ert-deftest org-links-tests-store-link-fallback--text-mode-non-org ()
  (let ((kill-ring nil))
    (with-temp-buffer
      (set-major-mode 'text-mode)
      (cl-letf (((symbol-function 'org-store-link)
                 (lambda (&optional _arg) "[[file:/fundamental.txt]]")))
        (org-links-store-link-fallback nil)
        ;; (print (car kill-ring)))))
        (should (string= (car kill-ring) "[[file:/fundamental.txt::1]]"))))))

(ert-deftest org-links-tests-store-link-fallback--org-mode-default ()
  (let ((kill-ring nil))
    (with-temp-buffer
      (set-major-mode 'org-mode)
      (cl-letf (((symbol-function 'org-store-link)
                 (lambda (&optional _arg) "[[file:/org.org::]]")))
        (org-links-store-link-fallback nil)
        (should (string= (car kill-ring) "[[file:/org.org::]]"))))))

;;; - org-links-create-link
(ert-deftest org-links-tests-create-link ()
  (should (string-equal (org-links-create-link "file:.././string") "[[file:~/sources/string]]")))
;;; provide
(provide 'org-links-tests)

;;; org-links-tests.el ends here
