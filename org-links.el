;;; org-links.el --- Better manage line numbers in links of Org mode -*- lexical-binding: t -*-

;; Author: <github.com/Anoncheg1,codeberg.org/Anoncheg>
;; Keywords: org, text, hypermedia, url
;; URL: https://github.com/Anoncheg1/emacs-org-links
;; Version: 0.2
;; Created: 30 Aug 2025
;; Package-Requires: ((emacs "27.1"))
;; SPDX-License-Identifier: AGPL-3.0-or-later
;; Copyright (c) 2025 github.com/Anoncheg1,codeberg.org/Anoncheg

;;; License

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU Affero General Public License
;; as published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

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
;; org-mode supports  file links  with line numbers  and line  via the
;;  following syntax:
;; [[PATH::NUM][Link description]]
;; [[PATH::LINE][Link description]]

;; This package (org-links) The syntax above is extended with
;;  following formats and facilities provided to help create and
;;  manage them all.
;; - [[PATH::NUM::LINE]]
;; - [[PATH::NUM-NUM::LINE]]
;; - [[PATH::NUM-NUM]]

;; For ex.  `[[file:./notes/warehouse.el::23::(defun alina (pic))]]`
;;
;; Known issue: Org export not working properly with new formats.

;; Also provided:
;; 1) The command `org-links-store-extended' copies a link to the
;;    current file, at the current point.
;; 2) A helpful warning is triggered when a link has an ambiguous target
;;    (e.g., in the case where two targets are found).

;; Why? Allow to point more flexible to use links with AI.

;; How  [[PATH::NUM::LINE]] links works?
;;   First, we search for LINE, if not found we use NUM line number.

;; [[NUM-NUM]] - used for region selection.

;; Also, Emacs have `find-file-at-point` functions globally (ffap.el)
;; to follow PATHs.

;; *Configuration*:
;; (require 'org-links)
;; (add-hook 'org-execute-file-search-functions #'org-links-additional-formats)
;; (add-hook 'org-open-link-functions #'org-links-fix-open-target-not-org)
;; (advice-add 'org-open-file :around #'org-links-org-open-file-advice)
;; (global-set-key (kbd "C-c w") #'org-links-store-extended)

;; You may advanced configuration in README.md file.

;; *Features provided*:
;; - respect org-link-context-for-files, if not set store only number.
;; - correctly store file in image-dired-thumbnail-mode
;; - Add support for image-dired-thumbnail-mode and image-dired-image-mode
;; - fuzzy search by the begining of ::LINE, not full match

;; I recommend to set those Org ol.el options for clarity:
;; (setopt org-link-file-path-type 'absolute) ; create links with full path
;; (setopt org-link-search-must-match-exact-headline nil) ; fuzzy search
;; (setopt org-link-descriptive nil) ; show links in raw, don't hide

;; *How this works*:
;; We provide new function `org-links-store-extended' that use
;;  standard ol.el function and we add additional format for
;;  programming modes.
;;
;; For opening links we add hook to org-execute-file-search-functions
;;  that called from `org-link-search' function, used by Org function
;;  for oppening files: `org-open-at-point' (that bound to C-c C-o by
;;  default in Org mode.)  and `org-open-at-point-global'.

;; Other packages:
;; - Modern navigation in major modes https://github.com/Anoncheg1/firstly-search
;; - Search with Chinese	https://github.com/Anoncheg1/pinyin-isearch
;; - Ediff no 3-th window	https://github.com/Anoncheg1/ediffnw
;; - Dired history		https://github.com/Anoncheg1/dired-hist
;; - Selected window contrast	https://github.com/Anoncheg1/selected-window-contrast
;; - Copy link to clipboard	https://github.com/Anoncheg1/emacs-org-links
;; - Solution for "callback hell"	https://github.com/Anoncheg1/emacs-async1
;; - Restore buffer state	https://github.com/Anoncheg1/emacs-unmodified-buffer1
;; - outline.el usage		https://github.com/Anoncheg1/emacs-outline-it
;; - ai_block for Org mode for chat.  https://github.com/Anoncheg1/emacs-oai

;; *DONATE MONEY* to sponsor author directly with crypto currencies:
;; - BTC (Bitcoin) address: 1CcDWSQ2vgqv5LxZuWaHGW52B9fkT5io25
;; - USDT (Tether) address: TVoXfYMkVYLnQZV3mGZ6GvmumuBfGsZzsN
;; - TON (Telegram) address: UQC8rjJFCHQkfdp7KmCkTZCb5dGzLFYe2TzsiZpfsnyTFt9D

;;; TODO:
;; - each file: link should be generated with some description. (for export)
;; - provide option FOR NUM::FUZZY: if several lines found jump to
;;   closes to NUM, not to exact NUM.
;; - when at <<some>> generate special link without universal arg.
;; - support multiline links separated at ::
;; - compare org-links-org-open-file-advice and
;;  org-links--local-get-target-position-for-link

;;; Code:
;; -=  includes
(require 'ol)
(require 'org-element)

;; -=  variables

(defcustom org-links-silent nil
  "Don't spawn messages."
  :type 'boolean
  :group 'org-links)

(defcustom org-links-threshold-search-link-optimization-max-file (* 50 1024 1024) ; 50MB, adjustable
  "If file size is lower we create copy of file in memory.
If size of file larger than threshold process file line by line instead."
  :type 'integer
  :group 'org-links)

(defcustom org-links-find-exact-flag nil
  "Non-nil means we search lines that exact match ::LINE.
Oterwise, by default, we search for lines that begin with ::LINE.
Used in `org-links--find-line'.
Search ignore first empty first characters in all case."
  :type 'boolean
  :group 'org-links)

(defvar org-links--debug-flag nil)

;; -=  functions
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
    (org-insert-link nil string description) ; have logic to manage path
    (buffer-substring-no-properties (point-min) (point-max))))

(defun org-links-get-type (string)
  "Format path of link according to `org-link-file-path-type' variable.
We use `org-insert-link' function that have required logic.
Argument STRING is a org link of file: type.
DESCRIPTION not used."
  (with-temp-buffer
    (org-insert-link nil string)
    (goto-char 1)
    (when-let* ((link (org-element-link-parser))
                (type (org-element-property :type link))
	        (path (org-element-property :path link)))
      type)))

;; (if (not (string-equal (org-links-create-link "file:.././string") "[[file:~/sources/string]]"))
;;     (error "Org-links"))

;; -=  functions: Copy to clipboard
;; old
(defun org-links--create-simple-at-point (arg)
  "Link builder for Fundamental mode.
ARG is universal argument, if non-nil.
Bad handling of [ character, such links should be avoided."
  (if arg
      ;; else - just LINE - will work if `org-link-search-must-match-exact-headline' is nil
      (org-links-org-link--normalize-string (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
      ;; (concat (number-to-string (line-number-at-pos)) "-" (number-to-string (line-number-at-pos)))
    ;; store in NUM::LINE format
    (concat (number-to-string (line-number-at-pos))
            "::" (org-links-org-link--normalize-string (buffer-substring-no-properties (line-beginning-position) (line-end-position))))))
; old
;; (defun org-links--create-org-default-at-point ()
;;   "Wrap `org-store-link' to extract main parts of link.
;; Return string in format [[file:path::fuzzy][desc]].
;; Used code from `org-babel-tangle--unbracketed-link'."
;;   (let ((link-string (substring-no-properties
;;                       (cl-letf (((symbol-function 'org-store-link-functions)
;;                                  (lambda () nil)))
;;                         (org-store-link nil)))))
;;     ;; - [[ ]]  links
;;     (if (string-match org-link-bracket-re link-string) ; 1: file::search-option 2: decription
;;         (let ((path (match-string 1 link-string))
;;               (desc (match-string 2 link-string)))

;;           (org-links-create-link path desc))
;;          ;; else - other types
;;          (org-links-create-link link-string))))



;; -=  Copy to clipboard - create link - main
(defun org-links--create-link-for-region (arg)
  "Create link based on current position for any mode.
If universal argument ARG provided, then links create with full path for
sharing between documents.
Without ARG liks are shorter for working in current document.
Return link or nil if line begin and end are equal for region"
  (interactive "P")
  ;; - 1) Check
  (let ((r-end (region-end))
        (r-beg (region-beginning))
        path)
    (unless (= (line-number-at-pos r-end)
               (line-number-at-pos r-beg))
      (when org-links--debug-flag
        (print (format "org-links--create-link-for-region N0 %s %s" (line-number-at-pos r-beg) (line-number-at-pos r-end))))
      (prog1
          ;; - 2) Preparation
          (let (cline desc first-line-pos)
            (save-excursion
              (goto-char r-beg)
              ;; Skip empty lines
              (beginning-of-line)
              (re-search-forward "[^ \t\n]" r-end t)
              ;; skip comments
              (when (derived-mode-p 'prog-mode)
                (while (and (comment-only-p (line-beginning-position) (line-end-position))
                            (< (point) r-end))
                  (forward-line)))
              (when (> (point) r-end)
                (goto-char r-beg))
              (setq cline (org-links-org-link--normalize-string))
              (setq desc (org-link--normalize-string (org-links-org-link--normalize-string) t))
              (setq first-line-pos (point)))
            (when org-links--debug-flag
              (print (format "org-links--create-link-for-region N1 %s" desc)))
            ;; - 3) Create link
            (if (and arg (bound-and-true-p buffer-file-name)) ; same as: `org-link--file-link-to-here'
                (org-links-create-link (concat
                                        "file:"
                                        (buffer-file-name (buffer-base-buffer)) ; path
                                        "::"
                                        (number-to-string (line-number-at-pos first-line-pos)) "-" (number-to-string (line-number-at-pos r-end))
                                        (if (string-empty-p cline) "" (concat "::" cline)))
                                       desc)
              ;; else - short
              (org-link-make-string
               (concat (number-to-string (line-number-at-pos first-line-pos)) "-" (number-to-string (line-number-at-pos r-end))
                       "::"
                       cline))))
        (deactivate-mark)))))

  ;;   (setq path (org-links-create-link (if (not arg)
  ;;                                         ;; path: one level upper and relative
  ;;                                         (file-relative-name (buffer-file-name (buffer-base-buffer))
  ;;                                                             (file-name-directory (directory-file-name default-directory)))
  ;;                                       ;; else - path: full
  ;;                                       (buffer-file-name (buffer-base-buffer))))))
  ;; ;; Skip empty lines and comments at beginning of region
  ;;       (save-excursion
  ;;         (goto-char r-beg)
  ;;         (when (not arg)
  ;;           ;; Skip empty lines
  ;;           (beginning-of-line)
  ;;           (re-search-forward "[^ \t\n]" r-end t)
  ;;           ;; skip comments
  ;;           (when (derived-mode-p 'prog-mode)
  ;;             (while (comment-only-p (line-beginning-position) (line-end-position))
  ;;               (forward-line)
  ;;               ;; (re-search-forward "[^ \t\n]" r-end t)
  ;;               )))
  ;;         ;; make link
  ;;         (concat (substring path 0 (- (length path) 2)) "::"
  ;;                 (number-to-string (line-number-at-pos (point))) "-" (number-to-string (line-number-at-pos r-end))
  ;;                 (when (not arg)
  ;;                   (concat "::"
  ;;                           ;; get first line
  ;;                           (org-links-org-link--normalize-string
  ;;                            (buffer-substring-no-properties
  ;;                             (line-beginning-position)
  ;;                             (line-end-position)))))
  ;;                 "]]")))


;; (defcustom org-links--get-path 'one-level-upper
;;   ""
;;     :type 'symbol
;;     :group 'org-links)

;; (defun org-links--get-path ()
;;   ; one level upper
;;   (abbreviate-file-name (expand-file-name path))
;;   (cond
;;     ((eq org-links--get-path 'one-level-upper)
;;      (let* ((dfn (directory-file-name default-directory))
;;             (file-name-directory


;;   "../" (file-relative-name (buffer-file-name (buffer-base-buffer))
;;                             (file-name-directory (directory-file-name default-directory)))

;;;###autoload
(defun org-links-store-extended (arg)
  "Store link to `kill-ring' clipboard.
If universal argument ARG provided, then links create with full path for
sharing between documents and description
Without ARG liks are shorter for working in current document and without
 description.
`org-link-make-string' is used to surround with [] and quote them inside.
Count lines from 1 like `line-number-at-pos' function does.
`org-links-create-link' do same, but also apply change path.
For programming  modes we create link  with path by default,  because it
 more oftenly used.
For usage with original Org `org-open-at-point-global' function."
  (interactive "P\n")
  ;; (abbreviate-file-name (buffer-file-name - ?
  (org-with-limited-levels
   (let ((link
          (cond
           ;; - Images mode 1
           ((derived-mode-p (intern "image-dired-thumbnail-mode"))
            (concat "file:" (funcall (intern "image-dired-original-file-name"))))
           ;; - Images mode 2
           ((derived-mode-p (intern "image-dired-image-mode"))
            (concat "file:" (buffer-file-name (buffer-base-buffer))))
           ;; - Images mode 3
           ((derived-mode-p (intern "image-mode"))
            (concat "file:" (buffer-file-name (buffer-base-buffer))))
           ;; - Dired
           ((derived-mode-p (intern "dired-mode"))
            (string-join (mapcar (lambda (x) (concat "file:" x))
                                   (funcall (intern "dired-get-marked-files") arg)) " "))
           ;; - Buffer menu
           ((derived-mode-p 'Buffer-menu-mode)
            (concat "file:" (or (buffer-file-name (Buffer-menu-buffer t))
                                (with-current-buffer (Buffer-menu-buffer t)
                                  default-directory))))
           ;; - Any mode - region
           ;; - format: NUM-NUM::LINE
           ;; - format: NUM-NUM - with argument
           ((and (use-region-p)
                 (org-links--create-link-for-region arg)))

           ;; all modes - for cursor at <<target>>
           ;; [[target]]
           ((org-in-regexp "[^<]<<\\([^<>]+\\)>>[^>]" 1)
            (org-links-create-link (concat
                                    (when arg
                                      (concat "file:"
                                              (abbreviate-file-name
                                               (buffer-file-name (buffer-base-buffer)))
                                              "::"))
                                      (match-string 1))))

           ;; - Programming, text, fundamental modes, not Org.
           ;; - format: PATH::NUM::LINE
           ((or (derived-mode-p 'prog-mode)
                (and (not (derived-mode-p 'org-mode)) (derived-mode-p 'text-mode))
                ;; (derived-mode-p 'fundamental-mode)
                )
            ;; store without fuzzy content and add line number."
            (if (and (bound-and-true-p buffer-file-name) arg)
                (org-link-make-string
                 (concat (number-to-string (line-number-at-pos)) "::"
                         (org-links-org-link--normalize-string)))
              ;; else - with argument - longer PATH::NUM::LINE format
              (let ((cline (org-links-org-link--normalize-string)))
                (org-links-create-link
                 (concat "file:" (buffer-file-name (buffer-base-buffer))
                         "::" (number-to-string (line-number-at-pos))
                         (if (string-empty-p cline) "" (concat "::" cline)))))))
           ;; - Org mode - at header
           ;; format: [[* header]]
           ((and (derived-mode-p 'org-mode)
                 (org-at-heading-p)
                 (if (and arg (bound-and-true-p buffer-file-name)) ; used: `org-link--file-link-to-here'
                     (let ((desc (org-link--normalize-string
                                  (org-get-heading t t t t)))
                           (path (buffer-file-name (buffer-base-buffer))))
                     (org-links-create-link
                      (concat "file:"
                              path
                              "::" (number-to-string (line-number-at-pos))
                              "::" (org-links-org-link--normalize-string))))
                     ;; else - short - "[[233::*ai <<asd>>]]"
                   (org-link-make-string
                    (concat (number-to-string (line-number-at-pos)) "::"
                            (substring-no-properties
                             (org-link-heading-search-string)))))))
           ;; ;; - Org mode
           ;; ((derived-mode-p 'org-mode)
           ;;  (let ((cline (org-links-org-link--normalize-string)))
           ;;    (if (and arg (bound-and-true-p buffer-file-name)) ; used: `org-link--file-link-to-here'
           ;;        ;; long
           ;;        (let ((desc (substring-no-properties (org-link--normalize-string (org-current-line-string) t)))
           ;;              (path (buffer-file-name (buffer-base-buffer))))
           ;;          (org-links-create-link
           ;;           (concat "file:"
           ;;                   path
           ;;                   "::" (number-to-string (line-number-at-pos))
           ;;                   (if (string-empty-p cline) "" (concat "::" cline)))
           ;;           desc))
           ;;      ;; else - short
           ;;      (org-link-make-string
           ;;       (concat (number-to-string (line-number-at-pos))
           ;;               "::"
           ;;               cline)))))

           ;; all modes - any line [[../emacs-org-links/org-links.el::367]]
           ;; format: - NUM::LINE and PATH::NUM::LINE
           (t ; for Org-mode normal line and  for any mode
            (let ((cline (org-links-org-link--normalize-string)))
              (if (and arg (bound-and-true-p buffer-file-name)) ; same as: `org-link--file-link-to-here'
                  (let ((desc (org-link--normalize-string (org-links-org-link--normalize-string) t))
                        (path (buffer-file-name (buffer-base-buffer))))
                    (when org-links--debug-flag
                      (print (format "org-links-store-extended %s" desc)))
                    (org-links-create-link (concat
                                            "file:"
                                            path
                                            "::" (number-to-string (line-number-at-pos))
                                            (if (string-empty-p cline) "" (concat "::" cline)))
                                           desc))
                ;; else - short
                (org-link-make-string
                 (concat (number-to-string (line-number-at-pos))
                         "::"
                         cline))))))))
     ;; let: link
     (kill-new link)
     (princ link))))

;; (let ((org-link-file-path-type 'relative))
;;   (org-links--create-org-default-at-point))

;; -=  Fallback "Save to clipboard" without requirement of org-links

(defun org-links-store-link-fallback (&optional arg)
  "Copy Org-mode link to kill ring and clipboard from any mode.
Without a universal argument C - u, copies a link in the form
PATH::LINE.
With a universal argument ARG, copies a link as PATH::NUM (current line
number).  Count lines from 1 like `line-number-at-pos' function does.
Support `image-dired-thumbnail-mode', `image-dired-image-mode' and
`image-mode' modes."
  (interactive "P")
  ;; (require 'org)
  (let ((link
         (cond
          ((derived-mode-p (intern "image-dired-thumbnail-mode"))
           (concat "file:" (funcall (intern "image-dired-original-file-name"))))

          ((or (derived-mode-p (intern "image-dired-image-mode"))
               (derived-mode-p (intern "image-mode")))
           (concat "[[file:" (buffer-file-name (buffer-base-buffer)) "]]"))

          ((not (buffer-file-name (buffer-base-buffer))) ; buffer with no file
           (concat "[[file:::" (number-to-string (line-number-at-pos)) "]]"))

          ((derived-mode-p (intern "org-mode"))
           (require 'org) ; hence we are in org anyway
           (if arg ; - ::NUM
               (let* ((org-link-context-for-files) ; set to nil to replace fuzzy links with line numbers
                      (link (substring-no-properties (org-store-link nil))))
                 (concat (substring link 0 (- (length link) 2)) "::" (number-to-string (line-number-at-pos)) "]]"))
             ;; else - ::LINE
             (substring-no-properties (org-store-link nil))))

          ;; - else - programming, text and fundamental
          ;;          (or (derived-mode-p 'prog-mode)
          ;;              (and (not (derived-mode-p 'org-mode)) (derived-mode-p 'text-mode))
          ;;              (derived-mode-p 'fundamental-mode)))
          (t
           (concat "[[file:" (buffer-file-name (buffer-base-buffer)) "::" (number-to-string (line-number-at-pos)) "]]")))))
    (kill-new link)
    (unless org-links-silent
      (message  "%s\t- copied to clipboard" link))))

;; -=  help functions: unnormalize link

(defun org-links-org-link--normalize-string (&optional string)
  "Compact spaces and trim leading to make link more compact.
Works in not Org modes.
Modified version of `org-link--normalize-string'.
Dont escape [] characters, this is done with futher
 `org-link-make-string' or `org-links-create-link'.
Instead of much of removal we only compact spaces and remove leading.
Instead of removing [1/3], [50%], leading ( and trailing ), spaces at
the end of STRING, we just compress spaces in line and remove leading
spaces from STRING.  CONTEXT ignored."
  (let ((string (or string (buffer-substring-no-properties
                            (line-beginning-position)
                            (line-end-position)))))
    (replace-regexp-in-string
     (rx (one-or-more (any " \t")))
     " "
     (string-trim
      ;; (org-link-escape ; add two slashed, but onle one needed. [
      ;; (replace-regexp-in-string "\\([][]\\)"
      ;;                           "\\\\1"
      string)
     "[ \t\n\r]+")))

(defun org-links-org--unnormalize-string (string)
  "Create regex matching STRING with arbitrary whitespace.
Reverse of `org-links-org-link--normalize-string.
Add spaces at begin of line and replace spaces with any number of spaces
or tabs in the middle.
To create proper regex, string should be first be processed with
`regexp-quote'."
  (concat "[ \t]*" (replace-regexp-in-string " " "[ \t]+" (string-trim string)) "[ \t]*"))


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

;; -=  find LINE
(defun org-links--line-number-at-string-pos (string pos)
  "Return the line number at position POS in STRING."
  (1+ (cl-count ?\n (substring string 0 pos))))


(defun org-links-find-first-two-exact-lines-in-buffer-optimized (search-string-regex &optional get-positions n)
  "Find first N or two exactly matching lines to SEARCH-STRING-REGEX.
Search in current buffer.
Count lines from 1 like `line-number-at-pos' function does.
If GET-POSITIONS is  non-nil, returns list of buffer  positions for each
match otherwisde line numbers.
Returns list of line numbers or empty list."
  (when org-links--debug-flag
      (print (format "first-two-exact-lines-in-buffer-optimized %s %s" search-string-regex get-positions)))
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
          (push (if get-positions (1+ (match-beginning 0))
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

(defun org-links--find-line (link-org-string &optional only-line get-position)
  "Return line number that match LINK-ORG-STRING in buffer or nil.
Search for target <<>> first, unless ONLY-LINE is set, if not found
 search for whole line.
If GET-POSITION is non-nil, then return position instead of line
numbner."
  (when org-links--debug-flag
    (print (format "org-links--find-line1 %s" link-org-string)))
  (let ((link (concat "<<"
                      (org-links-org--unnormalize-string (regexp-quote link-org-string))
                      ">>")) ; 1): target
        res)
    (when org-links--debug-flag
      (print (format "org-links--find-line2 %s" link)))
    ;; search <<target>>
    (unless only-line
      (setq res (org-links-find-first-two-exact-lines-in-buffer-optimized link get-position)))
    (unless res
      ;; 2): full line
      (setq link (concat "^"
                         (org-links-org--unnormalize-string (regexp-quote link-org-string))
                         (when org-links-find-exact-flag "$")))
      ;; search LINE
      (setq res (org-links-find-first-two-exact-lines-in-buffer-optimized link get-position)))
    (when org-links--debug-flag
      (print (format "org-links--find-line3 %s" res)))
    (if (eq (length res) 1) ;; found exactly one
        (car res)
      ;; else
      (unless org-links-silent
        (if  (> (length res) 1)
            (message "More than one line found, NUM is used. %s" res)
          ;; else
          (message "Line not found, NUM is used.")))
      nil)))

(defun org-lnd-target (target-string)
  "Return line number that match TARGET-STRING in buffer or nil.
If GET-POSITION is non-nil, then return position instead of line
numbner."
  (let* ((link (concat "<<" (org-links-org--unnormalize-string (regexp-quote target-string)) ">>"))
         (re (org-links-find-first-two-exact-lines-in-buffer-optimized link t)))
    (if (eq (length re) 1) ;; found exactly one
        (car re)
      ;; else
      (unless org-links-silent
        (if  (> (length re) 1)
            (message "More than one line found, NUM is used. %s" re)
          ;; else
          (message "Line not found, NUM is used.")))
      nil)))


;; -=  Open link - help functions and variablses

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
(defvar org-links-num-line-regexp "^\\([0-9]+\\)::\\(.?+\\)$"
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
      (when (eq (window-buffer) (current-buffer)) ; if showed
        (recenter))
      (when (not (pos-visible-in-window-p (save-excursion
                                            (goto-char (point-min))
                                            (forward-line (1- num2))
                                            (point))))
        (when (eq (window-buffer) (current-buffer)) ; if showed
          (recenter 1))))))

;; -=  Open link - for [[PATH::NUM-NUM]] - org-execute-file-search-functions
;; +  advice

(defun org-links--local-get-target-position-for-link (link)
  "For LINK string return (line-num-beg line-num-end) or (line-num-beg) or nil.
Use current buffer for search line.
Also profide fix for not-Org modes to able to search fuzzy.
LINK is plain link without []."
  (when org-links--debug-flag
    (print (format "org-links--local-get-target-position-for-link %s %s"
                   link (derived-mode-p 'org-mode))))
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
      (if-let* ((n1 (org-links--find-line line t)) ; skip search <<target>>
                (n2 (+ n1 (- num2 num1))))
          (list n1 n2)
        ;; else num1-num2
        (list num1 num2))))
   ;; NUM::LINE
   ((when-let* ((num1 (and (string-match org-links-num-line-regexp link)
	                   (match-string 1 link)))
	        (line (match-string 2 link))) ; may be ""
      (if-let* ((n1 (and (not (string-empty-p line))
                         (org-links--find-line line))))
          (list n1 nil)
        ;; else
        (list (string-to-number num1)))))
   ;; here: link may be a target (without <<>>) or  fuzzy link
    ((and
      (not (derived-mode-p 'org-mode))
      (when-let ((num1 (org-links--find-line link)))
        (list num1 nil))))))

;; (org-links--get-target-position-for-link "1-2::asd")
;; (org-links--get-target-position-for-link "480::")
;; (progn (string-match org-links-num-line-regexp "480::") (match-string 2 "480::"))

;;;###autoload
(defun org-links-fix-open-target-not-org (link-content)
  "Implementation of hook `org-open-link-functions'.
LINK-CONTENT is a string of link without open and close square brackets.
Fix for case for not Org mode to find <<target1>> links.
Execution path that cause error: `org-link-open-from-string' ->
`org-link-open' (hook here) -> `org-link-search' (cause error).
Hook requirement: When the function does handle the link, it must return
a non-nil value.  Don't called for file: type of links."
  (when org-links--debug-flag
    (print (list "org-links-fix-open-target-not-org"  link-content)))
  (if (and (not (derived-mode-p 'org-mode))
           (not current-prefix-arg))
      (progn (when-let ((pos (org-links--find-target link-content)))
               (push-mark nil t)
               (goto-char pos))
               t)
    ;; else
    nil))

;; (add-hook 'org-open-link-functions #'org-links-fix-open-target-not-org)



;;;###autoload
(defun org-links-additional-formats (link)
  "Jump to link position in current buffer.
Called from `org-link-search', which always called for link targets in
current buffer.
LINK is string after :: or was just in [[]].
Used for  `org-execute-file-search-functions'.
`org-execute-file-search-in-bibtex' as example.
Return t if link was processed or nil."
  (when org-links--debug-flag
    (print (list "org-links-additional-formats" link)))
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
;; -=  Approach 1) org-open-file advice - based on fuzzy links. Fix probles caused by org-open-file.
;;;###autoload
(defun org-links-org-open-file-advice (orig-fun &rest args)
  "Support for additional formats.
Argument ORIG-FUN is `org-open-file' that breaks at NUM-NUM,
NUM-NUM::LINE, NUM::LINE formats.
Support file::LINE and file:LINE formats.
Use current buffer for search line.
Optional argument ARGS is `org-open-file' arguments."
  ;; [[file::#+ or  [[file:#+
  (when org-links--debug-flag
    (print (list "org-links-org-open-file-advice" args)))
  ;; (if (= (length args) 2)
  ;;     (seq-let (search _) args
  ;;       ;; (if search
  ;;       ;;     (progn
  ;;       ;;       (when (string-prefix-p ":" search)
  ;;       ;;         (setq search (substring search 1)))
  ;;       ;;       (if-let ((line-position (org-links--find-line search)))
  ;;       ;;           (org-goto-line line-position)))
  ;;       ;;   ;; else - no search
  ;;         (apply orig-fun args))
  ;;       )
      ;; [[file::LINE]]
      ;; (seq-let (search _) args
      ;;   (if-let ((line-position (org-links--find-line saerch)))
      ;;           (org-goto-line line-position)
      ;;   )
      ;; else 4
  (seq-let (path in-emacs string search) args
    (setq string string) ;; noqa: unused
    (if search ; part after ::
        ;; (if-let ((repos (org-links--get-target-position-for-link search)))
        ;;     (let ((pos1 (car repos))
        ;;           (pos2 (cadr repos)))
              ;; (apply orig-fun (list path in-emacs (string-to-number num1)))
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
            (if-let ((line-position (org-links--find-line line t))) ; skip search <<target>>
                (org-goto-line line-position)
              ;; else
              (org-goto-line (string-to-number num1))
              (org-links-num-num-enshure-num2-visible num2))
            t))
         ;; NUM::LINE
         ((when-let* ((num1 (and (string-match org-links-num-line-regexp search)
	                         (match-string 1 search)))
	              (line (match-string 2 search))) ; may be ""
            (apply orig-fun (list path 'emacs)) ; in emacs
            (if-let ((line-position (and (not (string-empty-p line))
                                         (org-links--find-line line))))
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

;; -=  Better `org-open-at-point-global' to open link from string
(defun org-links-org-open-at-point-global (&optional arg)
  "Enhancements for `org-open-at-point-global' function.
- ask for link if not found.
- if file not exist, raise error, not open empty file.
If universal argument ARG is non-nil, then skip additionals."
  (interactive "P")
  (if arg
      (call-interactively #'org-open-at-point-global)
    ;; else
    ;; - raise error if check that file exist
    (save-excursion
      (save-match-data
        (when (org-in-regexp org-link-any-re)
          (goto-char (match-beginning 0))
          (when-let* ((link (org-element-link-parser))
                      (type (org-element-property :type link))
	              (path (org-element-property :path link)))
            (when (and (string= type "file")
                       (not (file-readable-p path)))
              (user-error "File does not exist"))))))

    ;; - call with catching error
    (condition-case nil
        (call-interactively #'org-open-at-point-global)
      ;; user-error: "No link found"
      (user-error
       (when kill-ring
         (let ((link (read-buffer "link: " (when (string-match org-link-any-re (car kill-ring-yank-pointer))
                                             (car kill-ring-yank-pointer)))))
           (unless (string-empty-p link)
             (with-temp-buffer
               (insert link)
               (call-interactively #'org-open-at-point-global)))))))))
;;; provide
(provide 'org-links)

;;; org-links.el ends here
