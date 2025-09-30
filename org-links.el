;;; org-links.el --- Copy link with numer of current line in all modes. -*- lexical-binding: t -*-

;; Author: <github.com/Anoncheg1,codeberg.org/Anoncheg>
;; Keywords: org, text, hypermedia, url
;; URL: https://github.com/Anoncheg1/emacs-org-links
;; Version: 0.1
;; Created: 30 Aug 2025
;; Package-Requires: ((emacs "27.1"))
;; > (Emacs 26+) for negative regex
;; (compat "30.1")
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

;;; Commentary:

;; *About*:
;; 1) add function for copying link to kill ring clipboard, faster
;; search for small files.
;; 2) extend links with new types of more robust links, especially
;; useful for creating links to files with programming code.
;; 3) add warning at opening link if two targets was found for classic
;; old Org link.
;;
;;
;; Additional link formats:
;; - [[PATH::NUM::LINE]]
;; - [[PATH::NUM-NUM::LINE]]
;; - [[PATH::NUM-NUM]]
;; - [[PATH::NUM]] creating

;; [[PATH::NUM::LINE]] - At opening we search for LINE first, if not
;; found exactly one, we use NUM line number.

;; [[NUM-NUM]] - is position of cursor at copying or region begin and end.

;; Org support opening links PATH::NUM with line number but don't
;; implement creation of them. Implemeted here.

;; *Features provided*:
;; - respect org-link-context-for-files, if not set store only number.
;; - correctly store file in image-dired-thumbnail-mode
;; - Add support for image-dired-thumbnail-mode and image-dired-image-mode

;; *Configuration*:
;; (require 'org-links)
;; (add-hook 'org-execute-file-search-functions #'org-links-additional-formats)
;; (advice-add 'org-open-file :around #'org-links-org-open-file-advice)
;; (global-set-key (kbd "C-c w") #'org-links-store-extended)

;; You may advanced configuration in README.md file.

;; *How this works*:
;; We provide new function `org-links-store-extended' that use
;;  standard ol.el function and we add additional format for
;;  programming modes.
;;
;; For opening links we add hook to org-execute-file-search-functions
;;  that called from `org-link-search' function, used by Org function
;;  for oppening files: `org-open-at-point' (that bound to C-c C-o by
;;  default in Org mode.)  and `org-open-at-point-global'.

;; *DONATE MONEY*:
;; You can sponsor author directly with crypto currencies:
;; - BTC (Bitcoin) address: 1CcDWSQ2vgqv5LxZuWaHGW52B9fkT5io25
;; - USDT (Tether) address: TVoXfYMkVYLnQZV3mGZ6GvmumuBfGsZzsN
;; - TON (Telegram) address: UQC8rjJFCHQkfdp7KmCkTZCb5dGzLFYe2TzsiZpfsnyTFt9D
;;
;;; Code:
;;; - Code
(require 'ol)

(defvar org-links-threshold-search-link-optimization-max-file (* 30 1024 1024) ; 30MB, adjustable
  "If lower we create copy of file in memory.
If size of file larger than threshold we will process file line by line
instead creating of copy.")

(defsubst org-links-string-full-match (regexp string)
  "Return t if REGEXP fully match STRING."
  (and (string-match regexp string)
       (zerop (match-beginning 0))
       (= (match-end 0) (length string))))

(defun org-links-create-link (string &optional description)
  "Format path of link according to `org-link-file-path-type' variable.
We use `org-insert-link' function that have required logic.
Argument STRING is a org link of file: type.
DESCRIPTION not used."
  (setq description description) ;; noqa: unused
  (with-temp-buffer
    (org-insert-link nil string description)
    (buffer-substring-no-properties (point-min) (point-max))))

;; (if (not (string-equal (org-links-create-link "file:.././string") "[[file:~/sources/string]]"))
;;     (error "Org-links"))

;;; - Copy to clipboard
(defun org-links--create-simple-at-point (arg)
  "Link builder for Fundamental mode.
ARG is universal argument, if non-nil"
  (if arg
      ;; else - just LINE - will work if `org-link-search-must-match-exact-headline' is nil
      (org-links-org-link--normalize-string (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
      ;; (concat (number-to-string (line-number-at-pos)) "-" (number-to-string (line-number-at-pos)))
    ;; store in NUM::LINE format
    (concat (number-to-string (line-number-at-pos))
            "::" (org-links-org-link--normalize-string (buffer-substring-no-properties (line-beginning-position) (line-end-position))))))

(defun org-links--create-org-default-at-point ()
  "Wrap `org-store-link' to extract main parts of link."
  (let ((link-string (substring-no-properties (org-store-link nil))))
    ;; - [[ ]]  links
    (if (string-match org-link-bracket-re link-string) ; 1: file::search-option 2: decription
        (let ((path (match-string 1 link-string))
              (desc (match-string 2 link-string)))

          (print (list path desc (org-links-create-link path desc)))
         (org-links-create-link path desc))
         ;; else - other types
         (org-links-create-link link-string))))

;; (let* (
;; 	 (file-name (if (not option) path
;; 		      (substring path 0 (match-beginning 0)))))
;;;###autoload
(defun org-links-store-extended (arg)
  "Store link to `kill-ring' clipboard.
ARG is universal argument.
Count lines from 1 like `line-number-at-pos' function does.
For usage with original Org `org-open-at-point-global' function."
  (interactive "P\n")
  (let ((link (cond
              ;; - Images mode 1
              ((derived-mode-p (intern "image-dired-thumbnail-mode"))
               (concat "file:" (funcall (intern "image-dired-original-file-name"))))
              ;; - Images mode 2
              ((derived-mode-p (intern "image-dired-image-mode"))
               (concat "file:" (buffer-file-name (buffer-base-buffer))))
              ;; - Buffer menu
              ((derived-mode-p 'Buffer-menu-mode)
               (concat "file:" (or (buffer-file-name (Buffer-menu-buffer t))
                                   (with-current-buffer (Buffer-menu-buffer t)
                                     default-directory))))

              ;; - NUM-NUM
              ((use-region-p)
               (prog1 (let ((path (org-links-create-link (concat "file:" (buffer-file-name (buffer-base-buffer))))))
                        (concat (substring path 0 (- (length path) 2)) "::"
                                (number-to-string (line-number-at-pos (region-beginning))) "-" (number-to-string (line-number-at-pos (region-end)))
                                (when arg
                                  (save-excursion
                                    (concat "::" (org-links-org-link--normalize-string
                                                  (save-excursion
                                                    (goto-char (region-beginning))
                                                    (buffer-substring-no-properties
                                                     (line-beginning-position)
                                                     (line-end-position)))))))
                                "]]"))
                 (deactivate-mark)))

              ;; - PATH::NUM::LINE - for Programming modes and fundamental
              ;; store without fuzzy content and add line number."
              ((or (derived-mode-p 'prog-mode)
                   (and (not (derived-mode-p 'org-mode)) (derived-mode-p 'text-mode))
                   (derived-mode-p 'fundamental-mode))

               (if (bound-and-true-p buffer-file-name)
                   (if arg
                       ;; store in PATH::NUM::LINE format
                       (org-links-create-link (concat
                                               "file:"
                                               (buffer-file-name (buffer-base-buffer))
                                               ;; (substring link 2 (- (length link) 2)) ; path
                                               "::" (number-to-string (line-number-at-pos))
                                               "::" (org-links-org-link--normalize-string (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))
                     ;; else
                     (org-links-create-link (concat
                                             "file:"
                                             (buffer-file-name (buffer-base-buffer))
                                             ;; (substring link 2 (- (length link) 2))
                                             "::" (number-to-string (line-number-at-pos)))))
                 ;; else - *scratch* buffer
                 (setq link (org-links-create-link (org-links--create-simple-at-point arg)))))
              ;; - PATH::NUM::LINE -  all modes
              (t
               (if (bound-and-true-p buffer-file-name)
                   (if arg
                       (org-links-create-link (concat
                                               "file:"
                                               (buffer-file-name (buffer-base-buffer))
                                               ;; (substring link 2 (- (length link) 2))
                                               "::" (number-to-string (line-number-at-pos))
                                               "::" (org-links-org-link--normalize-string (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))
                     ;; else - default
                     (org-links--create-org-default-at-point))

                 ;; else - *scratch* buffer
                 (setq link (org-links-create-link (org-links--create-simple-at-point arg))))))))
    (kill-new link)
    (message (concat link "\t- copied to clipboard"))))

;;; - Fallback "Save to clipboard" without requirement of org-links this package
(defun org-links-store-link-fallback (arg)
  "Copy Org-mode link to kill ring and clipboard from any mode.
Without a  prefix argument  ARG, copies a  link PATH::NUM  (current line
number).
Count lines from 1 like `line-number-at-pos' function does.
With a universal argument C - u, copies a link in the form PATH::LINE.
Support `image-dired-thumbnail-mode' and `image-dired-image-mode' modes."
  (interactive "P")
  (let ((link
         (if (derived-mode-p 'image-dired-thumbnail-mode)
             (concat "[[file:" (funcall (intern "image-dired-original-file-name")) "]]")
           ;; - else
           (if (derived-mode-p 'image-dired-image-mode)
               (concat "[[file:" (buffer-file-name (buffer-base-buffer)) "]]")
             ;; - else - programming, text and fundamental
             (if (and (not arg)
                      (or (derived-mode-p 'prog-mode)
                          (and (not (derived-mode-p 'org-mode)) (derived-mode-p 'text-mode))
                          (derived-mode-p 'fundamental-mode)))
                 (let* (
                        ;; (org-link-context-for-files)
                        (link (substring-no-properties (org-store-link nil))))
                   (concat (substring link 0 (- (length link) 2)) "::" (number-to-string (line-number-at-pos)) "]]"))
               ;; else - prog with argument or Org - with line for fuzzy search
               (substring-no-properties (org-store-link nil)))))))
    (kill-new link)
    (message  "%s\t- copied to clipboard" link)))

;;; - help functions: unnormalize link

(defun org-links-org-link--normalize-string (string &optional context)
  "Compact spaces and trim leading to make link more compact.
Modified version of `org-link--normalize-string'.
Instead of much of removal we only compact spaces and remove leading.
Instead of removing [1/3], [50%], leading ( and trailing ), spaces at
the end of STRING, we just compress spaces in line and remove leading
spaces from STRING.  CONTEXT ignored."
  (setq context context) ;; noqa: unused
  (string-trim
   (replace-regexp-in-string
    (rx (one-or-more (any " \t")))
    " "
    string) "[ \t\n\r]+"))

(defun org-links-org--unnormalize-string (string)
  "Create regex matching STRING with arbitrary whitespace.
Reverse of `org-links-org-link--normalize-string.
Add spaces at begin of line and replace spaces with any number of spaces
or tabs in the middle.
To create proper regex, string should be first be processed with
`regexp-quote'."
  (concat "[ \t]*" (replace-regexp-in-string " " "[ \t]+" string) "[ \t]*"))


;; small tests:

(if (not (string-match (let ((string "    ;;     	    (setq string (org-trim (substring string 1 -1))))"))
                         (org-links-org-link--normalize-string string))
                       ";; (setq string (org-trim (substring string 1 -1))))"))
    (error "Assert failed"))

(let ((string "    ;;     	    (setq string (org-trim (substring string 1 -1))))"))
  (if (not (org-links-string-full-match
                (org-links-org--unnormalize-string
                 (regexp-quote
                  (org-links-org-link--normalize-string string)))
                string))
    (error "Assert failed")))

;;; - find LINE
(defun org-links--line-number-at-string-pos (string pos)
  "Return the line number at position POS in STRING."
  (1+ (cl-count ?\n (substring string 0 pos))))


(defun org-links-find-first-two-exact-lines-in-buffer-optimized (search-string-regex &optional get-positions n)
  "Find first N or two exactly matching lines to SEARCH-STRING-REGEX.
Search in current buffer.
Returns list of line numbers or empty list.
Count lines from 1 like `line-number-at-pos' function does.
If GET-POSITIONS is  non-nil, returns list of buffer  positions for each
match otherwisde line numbers."
  (let* ((threshold org-links-threshold-search-link-optimization-max-file)
         (bufsize (- (point-max) (point-min)))
         (n (or n 2)))
    (if (< bufsize threshold)
      ;; - Fast approach: whole buffer as a string
      (let ((buf-str (buffer-substring-no-properties (point-min) (point-max)))
            (start 0)
            (results1 '()))
        (while (and (< (length results1) n)
                    (string-match search-string-regex buf-str start))
          ;; convert pos to line number
          (push (if get-positions (match-beginning 0)
                  ;; else
                  (org-links--line-number-at-string-pos buf-str (match-beginning 0)))
                  results1)
          (setq start (match-end 0)))
        (nreverse results1))
      ;; - Large buffer fallback: per-line traversal without copying whole buffer.
      (save-excursion
        (goto-char (point-min))
        (let ((results2 '())
              (ln 1))
          (while (and (< (length results2) n)
                      (not (eobp)))

            (let ((line (buffer-substring-no-properties
                         (line-beginning-position)
                         (line-end-position))))
              (when (and (not (string-empty-p line)) ; skip empty lines
                     (org-links-string-full-match search-string-regex line))
                (push (if get-positions (line-beginning-position) ln) results2))
              (forward-line 1)
              (setq ln (1+ ln))))
            (nreverse results2))))))

(defun org-links--find-line (link-org-string &optional get-position)
  "Return line number that match LINK-ORG-STRING in buffer or nil.
If GET-POSITION is non-nil, then return position instead of line
numbner."
  (let* ((link (concat "^" (org-links-org--unnormalize-string (regexp-quote link-org-string)) "$"))
         (re (org-links-find-first-two-exact-lines-in-buffer-optimized link get-position)))
    (if (eq (length re) 1) ;; found exactly one
        (car re)
      ;; else
      nil)))


;;; - Open link - help functions and variablses

(defvar org-links-num-num-regexp "^\\([0-9]+\\)-\\([0-9]+\\)$"
  "Links ::NUM-NUM.")

;; (defvar org-links-num-num-re (rx (seq "[["
;; 	           ;; URI part: match group 1.
;; 	           (group (+ digit)) "-" (group (+ digit))
;; 		   ;; Description (optional): match group 2.
;; 		   (opt "[" (group (+? anything)) "]")
;; 		   "]")))

(defvar org-links-num-num-line-regexp "^\\([0-9]+\\)-\\([0-9]+\\)::\\(.*\\)$"
  "Links ::NUM-NUM::LINE.")
(defvar org-links-num-line-regexp "^\\([0-9]+\\)::\\(.+\\)$"
  "Links ::NUM::LINE.")

(defun org-links-num-num-enshure-num2-visible (num2)
  "For NUM-NUM format, we enshure that NUM is visible when jump.
NUM2 is number of line or string with number.
Recenter screen and Two times check visibility."
  (let ((num2 (if (stringp num2)
                  (string-to-number num2)
                num2)))
    (when (not (pos-visible-in-window-p (save-excursion
                                          (goto-char (point-min))
                                          (forward-line (1- num2))
                                          (point))))
      (recenter)
      (when (not (pos-visible-in-window-p (save-excursion
                                            (goto-char (point-min))
                                            (forward-line (1- num2))
                                            (point))))
        (recenter 1)))))

;;; - Open link - for [[PATH::NUM-NUM]] - org-execute-file-search-functions +  advice

(defun org-links--local-get-target-position-for-link (link)
  "For LINK string return (line-num-beg line-num-end) or (line-num-beg) or nil.
Use current buffer for search line.
LINK is plain link without []."
  (cond
   ;; NUM-NUM
   ((when-let* ((num1 (and (string-match org-links-num-num-regexp link)
	                   (match-string 1 link)))
	        (num2 (match-string 2 link)))
      (list (string-to-number num1) (string-to-number num2))))
   ;; NUM-NUM::LINE
   ((when-let* ((num1 (and (string-match org-links-num-num-line-regexp link)
	                   (match-string 1 link)))
	        (num2 (match-string 2 link))
                (num1 (string-to-number num1))
                (num2 (string-to-number num2))
                (line (match-string 3 link)))
      ;; find line
      (if-let* ((n1 (org-links--find-line line))
                (n2 (+ n1 (- num2 num1))))
          (list n1 n2)
        ;; else num1-num2
        (list num1 num2))))
   ;; NUM::LINE
   ((when-let* ((num1 (and (string-match org-links-num-line-regexp link)
	                   (match-string 1 link)))
	        (line (match-string 2 link)))
      (if-let* ((n1 (org-links--find-line line)))
          (list n1 nil)
        ;; else
        (list (string-to-number num1) nil))))))

;; (org-links--get-target-position-for-link "1-2::asd")


;;;###autoload
(defun org-links-additional-formats (link)
  "Jump to link position in current buffer.
Return t if link was processed or nil.
Called from `org-link-search', which always called for link targets in
current buffer.
LINK is string after :: or was just in [[]].
`org-execute-file-search-in-bibtex' as example."
  ;; 1) get line numbers for link
  (if-let ((nums (org-links--local-get-target-position-for-link link)))
      (let ((num1 (car nums))
            (num2 (cadr nums)))
        ;; 2) jump
        (org-goto-line num1)
        (when num2
          (org-links-num-num-enshure-num2-visible num2))
        t)))


;; (add-hook 'org-execute-file-search-functions #'org-links-additional-formats)
;; (remove-hook 'org-execute-file-search-functions #'org-links-additional-formats)
;;; - Approach 1) org-open-file advice - based on fuzzy links. Fix probles caused by org-open-file.
;;;###autoload
(defun org-links-org-open-file-advice (orig-fun &rest args)
  "Support for additional formats.
Argument ORIG-FUN is `org-open-file' that breaks at NUM-NUM,
NUM-NUM::LINE, NUM::LINE formats.
Use current buffer for search line.
Optional argument ARGS is `org-open-file' arguments."
  (print (list "org-links-org-open-file-advice" args))
  (seq-let (path in-emacs string search) args
    (setq string string) ;; noqa: unused
    (if search ; part after ::
        ;; (if-let ((repos (org-links--get-target-position-for-link search)))
        ;;     (let ((pos1 (car repos))
        ;;           (pos2 (cadr repos)))
        ;;       (apply orig-fun (list path in-emacs (string-to-number num1)))


        (cond
         ;; NUM-NUM
         ((when-let* ((num1 (and (string-match org-links-num-num-regexp search)
	                         (match-string 1 search)))
	              (num2 (match-string 2 search)))
            (apply orig-fun (list path in-emacs (string-to-number num1)))
            (org-links-num-num-enshure-num2-visible num2)
            t))
         ;; NUM-NUM::LINE
         ((when-let* ((num1 (and (string-match org-links-num-num-line-regexp search)
	                         (match-string 1 search)))
	              (num2 (match-string 2 search))
                      (line (match-string 3 search)))
            (apply orig-fun (list path in-emacs))
            (if-let ((line-position (org-links--find-line line)))
                (org-goto-line line-position)
              ;; else
              (org-goto-line (string-to-number num1))
              (org-links-num-num-enshure-num2-visible num2))
            t))
         ;; NUM::LINE
         ((when-let* ((num1 (and (string-match org-links-num-line-regexp search)
	                         (match-string 1 search)))
	              (line (match-string 2 search)))
            (if-let ((line-position (org-links--find-line line)))
                (org-goto-line line-position)
              ;; else
              (org-goto-line (string-to-number num1)))
            t))
         (t ;; else - classic Org format
          ;; Addon to Org logic: signal if two targets exist
          (apply orig-fun args)
          (save-excursion
	    (save-restriction
          ;; (with-restriction (line-end-position) (point-max)
          ;;   (save-excursion
              (condition-case nil
                  ;; (with-restriction (line-end-position) (point-max)
                    (let ((org-link-search-must-match-exact-headline t))
                      (when (org-link-search search nil t)
                        (message "Warning: Two targets exist for this link.")))
                (error nil)
                (user-error nil))))))
      ;; else - no part after ::
      (apply orig-fun args))))


;; (advice-add 'org-open-file :around #'org-links-org-open-file-advice)
;; (advice-remove 'org-open-file #'org-links-org-open-file-advice)


;;; provide
(provide 'org-links)

;;; org-links.el ends here
