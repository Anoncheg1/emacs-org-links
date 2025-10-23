;;; org-links-tests.el --- Search line and fallback to number [[PATH::NUM::LINE]] -*- lexical-binding: t -*-


;; Author: <github.com/Anoncheg1,codeberg.org/Anoncheg>
;; Keywords: org, text, hypermedia, url
;; URL: https://github.com/Anoncheg1/emacs-org-links
;; Version: 0.2
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
(require 'org)
(require 'org-links)
(require 'image-dired)
;;; - Help functions
(defun set-major-mode (mode)
  (funcall mode))

(defun stub-fn (name value)
  (fset name (lambda (&rest _args) value)))

(defun stub-org-store-link (val)
  (fset 'org-store-link (lambda (&optional _arg) val)))

(defun with-temp-file-buffer (filename thunk)
  "Create temp buffer visiting FILENAME, call THUNK, then kill."
  (with-temp-buffer
    (set-visited-file-name filename t t)
    (set-buffer-modified-p nil)
    (funcall thunk)))

;;; - org-links-store-link-fallback
;; Helper usage already defined above
(ert-deftest org-links-tests-store-link-fallback--thumbnail-mode ()
  (with-temp-buffer
    (let ((kill-ring nil))
      (set-major-mode 'image-dired-thumbnail-mode)
      (cl-letf (((symbol-function 'image-dired-original-file-name)
                 (lambda () "foo.png")))
        (org-links-store-link-fallback nil)
        (should (equal (car kill-ring) "file:foo.png"))))))

(ert-deftest org-links-tests-store-link-fallback--image-mode ()
  (if (display-graphic-p)
      (let ((kill-ring nil))
        (with-org-link-config
         (with-temp-file-buffer "/bar/image.jpg"
                                (lambda ()
                                  (set-major-mode 'image-dired-image-mode)
                                  (cl-letf (((symbol-function 'buffer-base-buffer) (lambda (&optional _buf) (current-buffer))))
                                    (org-links-store-link-fallback nil)
                                    (should (equal (car kill-ring) "[[file:/bar/image.jpg]]")))))))))


(ert-deftest org-links-tests-store-link-fallback--prog-mode-no-file ()
  (with-temp-buffer
    (let ((kill-ring nil))
      (set-major-mode 'prog-mode)
      (goto-char (point-max))
      (org-links-store-link-fallback)
      (should (equal (car kill-ring)
                     "[[file:::1]]")))))

(ert-deftest org-links-tests-store-link-fallback--prog-mode-with-file ()
  (with-temp-buffer
    (let ((kill-ring nil))
      (set-major-mode 'prog-mode)
      (setq buffer-file-name "/mock/p.py")
      (goto-char (point-max))
      (org-links-store-link-fallback)
      (should (equal (car kill-ring)
                     "[[file:/mock/p.py::1]]")))))


(ert-deftest org-links-tests-store-link-fallback--org-mode-no-file-no-arg ()
  (with-temp-buffer
    (let ((kill-ring nil))
      (set-major-mode 'org-mode)
      (org-links-store-link-fallback nil)
      (should (string= (car kill-ring) "[[file:::1]]")))))

(ert-deftest org-links-tests-store-link-fallback--org-mode-with-file-arg ()
  (with-temp-buffer
    (let ((kill-ring nil))
      (set-major-mode 'org-mode)
      (setq buffer-file-name "/mock/org.org")
      (org-links-store-link-fallback 1)
      (should (string= (car kill-ring) "[[file:/mock/org.org::1]]")))))

(ert-deftest org-links-tests-store-link-fallback--org-mode-with-file-no-arg1 ()
  (with-temp-buffer
    (let ((kill-ring nil))
      (set-major-mode 'org-mode)
      (setq buffer-file-name "/mock/org.org")
      (org-links-store-link-fallback nil)
      (should (string= (car kill-ring) "[[file:/mock/org.org]]")))))

(ert-deftest org-links-tests-store-link-fallback--org-mode-with-file-no-arg2 ()
  (with-temp-buffer
    (let ((kill-ring nil))
      (set-major-mode 'org-mode)
      (setq buffer-file-name "/mock/org.org")
      (insert "* headline")
      (org-links-store-link-fallback nil)
      (should (string= (car kill-ring) "[[file:/mock/org.org::*headline][headline]]")))))
;; (kill-buffer buf))))
;;; - advices activation
;; ;; opening
;; (add-hook 'org-execute-file-search-functions #'org-links-additional-formats)
;; (advice-add 'org-open-file :around #'org-links-org-open-file-advice)
;; ;; copying
;; (global-set-key (kbd "C-c w") #'org-links-store-extended)
(defmacro with-org-link-config (&rest body)
  `(let ((org-execute-file-search-functions
          (cons #'org-links-additional-formats
                org-execute-file-search-functions)))
     (advice-add 'org-open-file :around #'org-links-org-open-file-advice)
     (unwind-protect
         ,@body
       (advice-remove 'org-open-file #'org-links-org-open-file-advice))))
;;; - org-links-create-link
(ert-deftest org-links-tests-create-link ()
  (if (file-exists-p "~/sources/") ; local
      (should (string-equal (org-links-create-link "file:.././string") "[[file:~/sources/string]]"))
    ;; else - melpaziod
    (should (string-equal (org-links-create-link "file:.././string") "[[file:~/work/emacs-org-links/string]]"))))
;;; - org-links-org--unnormalize-string
;; Utility for printable test output:
(defun org-links--print-fail (desc val expected)
  (format "Failed: %s\nGot: %S\nExpected: %S" desc val expected))

;; Improved ERT tests:
(ert-deftest org-links-string-full-match-tests ()
  "Test coverage for org-links-string-full-match with different boundary cases."
  ;; Empty string & empty regexp
  (should (org-links-string-full-match "^$" ""))
  ;; Partial matches rejected
  (should-not (org-links-string-full-match "foo" "barfoo"))
  ;; Multi-line handling (should be considered failed unless matching)
  (should-not (org-links-string-full-match "^foo$" "foo\nbar"))
  ;; Non-ascii
  (should (org-links-string-full-match "^ümlaut$" "ümlaut"))
  ;; Only matches if both start and end align
  (should-not (org-links-string-full-match "^foo$" " foo "))
  ;; String is all whitespace, should not match non-empty
  (should-not (org-links-string-full-match "^foo$" "   "))
  ;; Should allow empty target
  (should (org-links-string-full-match "^$" "")))

(ert-deftest org-links-org-link--normalize-string-tests ()
  "Thorough whitespace and input coverage for normalization."
  (should (equal (org-links-org-link--normalize-string "    foo   ") "foo"))
  (should (equal (org-links-org-link--normalize-string "foo    bar\tbaz") "foo bar baz"))
  ;; (should (equal (org-links-org-link--normalize-string "\tfoo\nbar\rbaz\t") "foo bar baz"))
  ;; String with only whitespace
  ;; (should (equal (org-links-org-link--normalize-string "   \t \r\n ") ""))
  ;; Already normal string (should return unchanged)
  (should (equal (org-links-org-link--normalize-string "foo bar baz") "foo bar baz"))
  ;; ;; Unicode, combining marks, etc.
  ;; (should (equal (org-links-org-link--normalize-string "café bar") "café bar"))
  ;; Multiple whitespace types between words.
  ;; (should (equal (org-links-org-link--normalize-string "foo\t\tbar\n\nbaz") "foo bar baz"))
  ;; Empty string
  (should (equal (org-links-org-link--normalize-string "") "")))

(ert-deftest org-links-org--unnormalize-string-tests ()
  "Test generating regex with edge cases and non-standard inputs."
  (let ((norm "foo bar baz")
        (spaces "    foo    bar\tbaz"))
    (let ((rx (org-links-org--unnormalize-string norm)))
      (should (string-match rx spaces))
      (should (string-match rx norm))
      ;; Should allow excessive leading whitespace
      (should (string-match rx "           foo bar baz"))
      ;; Should not match words out of order
      (should-not (string-match rx "baz foo bar"))
      ;; Should not match missing word
      (should-not (string-match rx "foo bar"))
      ;; Should not match extra trailing
      ;; (should-not (string-match rx "foo bar baz quux"))
      )
    ;; Unicode words
    (let ((rx (org-links-org--unnormalize-string "café bar")))
      (should (string-match rx "   café    bar"))
      (should-not (string-match rx "cafe bar")))))

(ert-deftest org-links-roundtrip-property-tests ()
  "Test round-trip: original → normalize → regex → matches original (robust property)."
  (dolist (input '("   foo bar baz   "
                   "foo\tbar\tbaz"
                   " \tfoo\tbar \tbaz\t "
                   "café bar"
                   "" ;; empty
                   "   ")) ;; just whitespace
    (let* ((norm (org-links-org-link--normalize-string input))
           (rx (org-links-org--unnormalize-string norm)))
      ;; If input was just whitespace, normal is "", and rx should match empty or whitespace input
      (when (string-empty-p norm)
        (should (string-match rx input)))
      ;; Otherwise, round-trip should match input after normalization
      (should (string-match rx input))
      ;; Should match normalized string too
      (should (org-links-string-full-match rx norm))
      ;; Should not match a string missing a word
      ;; (when (> (length norm) 0)
      ;;   (should-not (string-match rx (concat norm " extra"))))
      )))

(ert-deftest org-links-negatives-and-edge ()
  "Test negative cases for robustness."
  ;; Random non-matching input
  (let ((norm "foo bar"))
    (should-not (string-match (org-links-org--unnormalize-string norm) "foobar"))
    (should-not (string-match (org-links-org--unnormalize-string norm) "foo   baz"))
    ;; Should not match if words overlap improperly
    (should-not (string-match (org-links-org--unnormalize-string norm) "bar foo"))))

;;; - search line
;; Function 1: org-links--line-number-at-string-pos
(ert-deftest org-links--line-number-at-string-pos-basic ()
  (should (= (org-links--line-number-at-string-pos "foo\nbar\nbaz" 0) 1)) ;; start
  (should (= (org-links--line-number-at-string-pos "foo\nbar\nbaz" 4) 2)) ;; after first \n
  (should (= (org-links--line-number-at-string-pos "foo\nbar\nbaz" 8) 3)) ;; after second \n
  (should (= (org-links--line-number-at-string-pos "foo\nbar\nbaz" 11) 3)) ;; end boundary
  )

;; Function 2: org-links-find-first-two-exact-lines-in-buffer-optimized
(ert-deftest org-links-find-first-two-exact-lines-in-buffer-optimized-basic ()
  (with-temp-buffer
    (insert "apple\nbanana\nbanana\ncarrot\nBANANA\nbanana\n")
    ;; Find line numbers with regex "banana"
    (should (equal (org-links-find-first-two-exact-lines-in-buffer-optimized "^banana$") '(2 3)))
    ;; Find only first match
    (should (equal (org-links-find-first-two-exact-lines-in-buffer-optimized "^banana$" nil 1) '(2)))
    ;; Get buffer positions of matches
    (should (equal  (org-links-find-first-two-exact-lines-in-buffer-optimized "^banana$" t 2)'(6 13)))
    ;; No match
    (should (equal (org-links-find-first-two-exact-lines-in-buffer-optimized "^pear$") '()))
    ))

;; (with-temp-buffer
;;     (insert "apple\nbanana\nbanana\ncarrot\nBANANA\nbanana\n")
;;     (org-links-find-first-two-exact-lines-in-buffer-optimized "^banana$" t 2))
;;     (org-links-find-first-two-exact-lines-in-buffer-optimized "^banana$" nil 1))

;; Function 3: org-links--find-line
(ert-deftest org-links--find-line-basic ()
  (with-temp-buffer
    (insert "alpha\nlink1\nlink2\nlink1\nlink3\n")
    ;; Should return line number only if exactly one match
    (should (equal (org-links--find-line "link2") 3)) ; line number 3
    ;; Multiple matches => nil
    (should (equal (org-links--find-line "link1") nil))
    ;; No match => nil
    (should (equal (org-links--find-line "foo") nil))
    )
  )
;;; - org-open-file advice to other file
(ert-deftest org-links-jump-num-line-test ()
  (let ((kill-buffer-query-functions))
    (with-temp-buffer
      (with-org-link-config
       (org-mode)
       (setq buffer-file-name "/mock/org.org")
       (insert "some-text above\n")
       (insert "* headline")
       (org-links-store-extended nil)
       (forward-line -1)
       (insert "some-text above2\n")
       (forward-line 1)
       (insert "\nsome-text below\n")
       ;; (require 'xref)
       ;; (xref-push-marker-stack)
       (setq buffer-read-only t)
       (with-temp-buffer
         ;; (print (car kill-ring))
         (insert (car kill-ring)) ; [[file:/mock/org.org::2::* headline]]
         ;; (print (list "org-execute-file-search-functions" org-execute-file-search-functions))
         ;; (setq buffer-read-only t)
         (read-only-mode 1)
         (org-open-at-point-global)
         (should (string-equal "* headline" (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
         ;; (xref-go-back) ;; (xref-go-back)
         ;; (xref-go-forward)
         (set-buffer-modified-p nil))
       (setq kill-ring nil)
       (set-buffer-modified-p nil)))))
;;; - store link
;; Mocking necessary dependencies
(defmacro with-mocks (&rest body)
  `(cl-letf (((symbol-function 'image-dired-original-file-name)
              (lambda () "/mock/pic.jpg"))
             ;; ((symbol-function 'org-links-create-link)
             ;;  (lambda (x) (concat "[[" x "]]")))
             ;; ((symbol-function 'org-store-link)
             ;;  (lambda (_) "[[file:/mock/test.txt]]"))
             ;; ((symbol-function 'org-links-org-link--normalize-string)
             ;;  (lambda (x) x))
             )
     ,@body))

(ert-deftest org-links-store-extended-image-thumbnail-test ()
  (with-temp-buffer
    (with-mocks
     ;; Simulate mode
     (setq major-mode 'image-dired-thumbnail-mode)
     (setq kill-ring nil)
     (org-links-store-extended nil)
     (should (string= (car kill-ring) "file:/mock/pic.jpg"))(set-buffer-modified-p nil))))

(ert-deftest org-links-store-extended-image-mode-test ()
  (with-temp-buffer
    (with-mocks
     (setq major-mode 'image-dired-image-mode)
     (setq buffer-file-name "/mock/image.png")
     (setq kill-ring nil)
     (org-links-store-extended nil)
     (should (string= (car kill-ring) "file:/mock/image.png"))(set-buffer-modified-p nil))))

(ert-deftest org-links-store-extended-region-test ()
  (let ((kill-buffer-query-functions))
    (with-temp-buffer
      (with-org-link-config
       (setq major-mode 'text-mode)
       (setq buffer-file-name "/mock/test.txt")
       (insert "foo\nbar\nbaz\nqux")
       (set-mark (point-min))
       (goto-char (point-max))
       (setq kill-ring nil)
       (org-links-store-extended nil)
       (should (string-match-p
                "[[file:/mock/test.txt::1-4]]" (car kill-ring))))
      (set-buffer-modified-p nil))))

(ert-deftest org-links-store-extended-prog-mode--no-arg-test ()
  (let ((kill-buffer-query-functions))
    (with-temp-buffer
      (setq major-mode 'prog-mode)
      (setq buffer-file-name "/mock/code.el")
      (insert "myline")
      (setq kill-ring nil)
      (goto-char (point-min))
      (org-links-store-extended 1)
      (should (string-match-p "\\[\\[file:/mock/code.el::1\\]\\]" (car kill-ring)))
      (set-buffer-modified-p nil))))

(ert-deftest org-links-store-extended-prog-mode-arg-test ()
  (let ((kill-buffer-query-functions))
    (with-temp-buffer
      (setq major-mode 'prog-mode)
      (setq buffer-file-name "/mock/code.el")
      (insert "myline")
      (setq kill-ring nil)
      (goto-char (point-min))
      (org-links-store-extended nil)
      (should (string= (car kill-ring) "[[file:/mock/code.el::1::myline]]" ))
      (set-buffer-modified-p nil))))

(ert-deftest org-links-store-extended-org-mode-test ()
  (let ((kill-buffer-query-functions))
    (with-temp-buffer
      (org-mode)
      (setq buffer-file-name "/mock/org.org")
      (insert "* headline")
      (setq kill-ring nil)
      (goto-char (point-min))
      (org-links-store-extended nil)
      ;; (print (car kill-ring))
      (should (string= (car kill-ring) "[[file:/mock/org.org::1::* headline]]"))
      (set-buffer-modified-p nil))))

(ert-deftest org-links-store-extended-org-mode-arg-test ()
  (let ((kill-buffer-query-functions))
    (with-temp-buffer
      (org-mode)
      (setq buffer-file-name "/mock/org.org")
      (insert "* headline")
      (setq kill-ring nil)
      (goto-char (point-min))
      (org-links-store-extended 1)
      ;; (print (car kill-ring))
      (should (string= (car kill-ring) "[[file:/mock/org.org::*headline][headline]]"))
      (set-buffer-modified-p nil))))
;;; provide
(provide 'org-links-tests)

;;; org-links-tests.el ends here
