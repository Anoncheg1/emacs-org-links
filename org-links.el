;;; org-links.el --- Better manage line numbers in links of Org mode -*- lexical-binding: t -*-

;; Author: <github.com/Anoncheg1,codeberg.org/Anoncheg>
;; Keywords: org, text, hypermedia, url
;; URL: https://github.com/Anoncheg1/emacs-org-links
;; Version: 0.4
;; Created: 30 Aug 2025
;; Package-Requires: ((emacs "27.2"))
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

;; This package (org-links) provides facilities to help create and
;;  manage Org-mode links, add new link types, have speed optimization
;;  for large files.  Have facility to message if two lines was found,
;;  control wheter to search for full line or by the begining of it.

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

;; [[::LINE]] is equal to [[LINE]]

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
;; Provided new function `org-links-store-extended' to create and copy
;;  link to clipboard kill-ring and we add advices and hooks to handle
;;  opeing of new links types.
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
;; - provide option FOR NUM::FUZZY: if several lines found jump to
;;   closes to NUM, not to exact NUM.

;;; Code:
;; -=  includes
(require 'ol)
(require 'org-element)

;; -=  variables

(defgroup org-links nil
  "Org-links package customization."
  :group 'org-links)

(defcustom org-links-silent nil
  "Don't spawn messages."
  :type 'boolean
  :group 'org-links)

(defcustom org-links-threshold-search-link-optimization-max-file (* 50 1024 1024) ; 50MB, adjustable
  "If file size is lower we create copy of file in memory.
If size of file larger than threshold process file line by line instead."
  :type 'integer
  :group 'org-links)

(defcustom org-links-find-exact-flag t
  "Non-nil means we search lines that exact match ::LINE.
Oterwise, by default, we search for lines that begin with ::LINE.
Used in `org-links--find-line'.
Search ignore space characters at the begining of lines in all case."
  :type 'boolean
  :group 'org-links)

(defcustom org-links-on-several-halt-flag t
  "Non-nil means we don't jump to first found result.
Instead, we signal error and don't jump.
Used in `org-links--find-line'."
  :type 'boolean
  :group 'org-links)

(defcustom org-links-debug-flag nil
  "Non-nil means activate debug output with `print' function."
  :type 'boolean
  :group 'org-links)

(define-error 'org-links-on-several-error "Two results was fould for line in link.")



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
  (ignore description) ; noqa: unused
  (with-temp-buffer
    (org-insert-link nil string description) ; have logic to manage path
    (buffer-substring-no-properties (point-min) (point-max))))


(defun org-links-get-type (string) ; not used
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
;; (defun org-links--create-simple-at-point (arg)
;;   "Link builder for Fundamental mode.
;; ARG is universal argument, if non-nil.
;; Bad handling of [ character, such links should be avoided."
;;   (if arg
;;       ;; else - just LINE - will work if `org-link-search-must-match-exact-headline' is nil
;;       (org-links-org-link--normalize-string (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
;;       ;; (concat (number-to-string (line-number-at-pos)) "-" (number-to-string (line-number-at-pos)))
;;     ;; store in NUM::LINE format
;;     (concat (number-to-string (line-number-at-pos))
;;             "::" (org-links-org-link--normalize-string (buffer-substring-no-properties (line-beginning-position) (line-end-position))))))
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
  "Create link for transient-mode region selection.
Works for any mode.
If universal argument ARG provided, then links create with full path for
sharing between documents.
Without ARG liks are shorter for working in current document.
Return link or nil if line begin and end are equal for region"
  (interactive "P")
  ;; - 1) Check
  (let ((r-end (region-end))
        (r-beg (region-beginning)))
    (unless (= (line-number-at-pos r-end)
               (line-number-at-pos r-beg))
      (when org-links-debug-flag
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
            (when org-links-debug-flag
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

(defun org-links-store-extended-universal (line arg desc)
  "If ARG is non-nil we try to create long link, otherwise short.
Short link we create without DESC-description.
LINE is #+name: or <<name>> or full line normalized.
DESC is strictly normalized LINE.
Same as `org-link--file-link-to-here'.
Return string with Org line surounded with [[]] characters."
  (if (and (bound-and-true-p buffer-file-name) arg)
      ;; PATH::NUM::LINE format
      (org-links-create-link
       (concat "file:"
               (buffer-file-name (buffer-base-buffer))
               "::" (number-to-string (line-number-at-pos))
               (if (string-empty-p line) "" (concat "::" line)))
       desc)
    ;; else - NUM::LINE
    (org-link-make-string
     (concat (number-to-string (line-number-at-pos)) "::" line))))

;;;###autoload
(defun org-links-store-extended (arg)
  "Store link to `kill-ring' clipboard.
Same to `org-store-link' but copy link to `kill-ring'.
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
  (when org-links-debug-flag
    (print (format "org-links-store-extended %s" arg)))
  ;; (abbreviate-file-name (buffer-file-name - ?
  (org-with-limited-levels
   (let* ((case-fold-search t)
          (link
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
             (string-join (mapcar (lambda (x) (concat "\"" x "\""))
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
            ((org-in-regexp "[^<]<<\\([^<>]+\\)>>[^>]?$?" 1) ; works in any mode
             (when org-links-debug-flag
               (print (format "org-links-store-extended at <<target>>")))
             (org-links-store-extended-universal (match-string 1)
                                                 arg
                                                 (org-link--normalize-string (match-string 1) t)))

               ;; (org-links-create-link (concat
               ;;                         (when arg
               ;;                           (concat "file:"
               ;;                                   (abbreviate-file-name
               ;;                                    (buffer-file-name (buffer-base-buffer)))
               ;;                                   "::"))
               ;;                         (match-string 1)))


            ;; - Programming, text, not Org.
            ;; - format: PATH::NUM::LINE
            ((or (derived-mode-p 'prog-mode)
                 (and (not (derived-mode-p 'org-mode)) (derived-mode-p 'text-mode)))
             (when org-links-debug-flag
               (print (format "org-links-store-extended in programming mode")))
             ;; store without fuzzy content and add line number."
             (org-links-store-extended-universal (org-links-org-link--normalize-string)
                                                 (not arg) ; reversal
                                                 nil)) ; without descrition
           ;; (if (and (bound-and-true-p buffer-file-name) arg)
           ;;     (org-link-make-string
           ;;      (concat (number-to-string (line-number-at-pos)) "::"
           ;;              cline))
           ;;   ;; else - with argument - longer PATH::NUM::LINE format
           ;;   (org-links-create-link
           ;;    (concat "file:" (buffer-file-name (buffer-base-buffer))
           ;;            "::" (number-to-string (line-number-at-pos))
           ;;            (if (string-empty-p cline) "" (concat "::" cline)))))))

           ;; - Org mode - at header. Format: [[* header]], same to `org-link-heading-search-string'
            ((and (derived-mode-p 'org-mode)
                  (org-at-heading-p))

             (when org-links-debug-flag
               (print (format "org-links-store-extended at Org-header")))
             (org-links-store-extended-universal (concat "*"
                                                         (let* ((s (org-links-org-link--normalize-string)) ; or (substring-no-properties (org-link-heading-search-string))
                                                                (pos (string-match " +" s)))
                                                           (if pos
                                                               (substring s (match-end 0))
                                                             s)))
                                                 arg
                                                 (org-link--normalize-string
                                                  (org-get-heading t t t t))))

           ;; - Org #+name
           ((and (derived-mode-p 'org-mode)
                 (save-excursion
                   (beginning-of-line)
                   (and
                    (or (looking-at org-babel-src-name-regexp)
                        (looking-at "^[ \t]*#\\+\\(begin\\|end\\)_.*$")) ; header or end of some block
                    ;; goto begin if at end
                    (progn
                      (when org-links-debug-flag
                        (print (format "org-links-store-extended at name1")))
                      (when (looking-at "^[ \t]*#\\+end_.*$")
                        (goto-char (org-element-property :begin (org-element-at-point))))
                      (when-let ((name (or (org-element-property :name (org-element-at-point))
                                           (org-links-org-link--normalize-string))))
                        (when org-links-debug-flag
                          (print (format "org-links-store-extended at name2 %s" (org-link--normalize-string name t))))
                        (org-links-store-extended-universal name
                                                            arg
                                                            (org-link--normalize-string name t))))))))
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
            (when org-links-debug-flag
              (print (format "org-links-store-extended at t - any")))
            (let ((cline (org-links-org-link--normalize-string)))
              (org-links-store-extended-universal cline
                                                  arg
                                                  (org-link--normalize-string cline t)))))))

            ;; (let ((cline (org-links-org-link--normalize-string)))
            ;;   (if (and arg (bound-and-true-p buffer-file-name)) ; same as: `org-link--file-link-to-here'
            ;;       (let ((desc (org-link--normalize-string (org-links-org-link--normalize-string) t))
            ;;             (path (buffer-file-name (buffer-base-buffer))))

            ;;         (org-links-create-link (concat
            ;;                                 "file:"
            ;;                                 path
            ;;                                 "::" (number-to-string (line-number-at-pos))
            ;;                                 (if (string-empty-p cline) "" (concat "::" cline)))
            ;;                                desc))
            ;;     ;; else - short
            ;;     (org-link-make-string
            ;;      (concat (number-to-string (line-number-at-pos))
            ;;              "::"
            ;;              cline))))))))
     ;; let: link
     (kill-new link)
     (princ "\n")
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
                      (link (substring-no-properties (car (call-interactively #'org-store-link nil))))) ; may be with description
                 (concat "[[" link "::" (number-to-string (line-number-at-pos)) "]]"))
             ;; else - ::LINE
             (substring-no-properties (concat "[[" (car (call-interactively #'org-store-link nil)) "]]"))))

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
spaces from STRING.  CONTEXT ignored.
If string is header, we replace it with one * character without space
 after.
Return string"
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
STRING should not contain regex expressions, be quoted with
 `org-link-make-string', `regexp-quote' quote * also, that is not
 appropriate.
Do 1) replace spaces with regex for spaces of any lenght.
2) for header make regex for  proper Org headers, for line surround with
 possible spaces.
Reverse of `org-links-org-link--normalize-string' and to
 [[422::(org-at-heading-p)]].  Add spaces at begin of line and replace
 spaces with any number of spaces or tabs in the middle.
If STRING starts with * character without space after, it is header
 search."
  ;; if it is a heading line "*word" - replace with regex to find headers
  ;; (print (list "org-links-org--unnormalize-string1" string))
  ;; spaces may be any lenghth
  (setq string (string-trim string))

  ;; header or not
  (if (string-match "^[\\]?\\*+ ?[^ *]" string) ; quoted or not
      ;; (progn
        (setq string (concat org-outline-regexp
                             (replace-regexp-in-string " +" "[ \t]+" string nil nil nil (1- (match-end 0)))))
                             ;; (substring string (1- (match-end 0)))
        ;; (setq string (replace-regexp-in-string " +" "[ \t]+" string nil nil nil 4))) ; (1+ (length org-outline-regexp))
    ;; else
    ;; (print (list "org-links-org--unnormalize-string2 not header" string))
    (concat "[ \t]*" (replace-regexp-in-string " +" "[ \t]+" string) "[ \t]*")))

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
  (when org-links-debug-flag
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

(defun org-links--find-line (link-org-string &optional enable-target get-position)
  "Return line number that match LINK-ORG-STRING in buffer or nil.
When ENABLE-TARGET, then Search <<>> and #+NAME keywrord first, if not
 found search for whole line.
If GET-POSITION is non-nil, then return position instead of line number.
Return one number or nil."
  (when org-links-debug-flag
    (print (format "org-links--find-line1 %s" link-org-string)))
  ;; repare search regex
  (let ((link-line (concat "^"
                           (org-links-org--unnormalize-string (regexp-quote link-org-string)) ; header or line
                           (when org-links-find-exact-flag "$")))
        (link-target (concat "<<"
                             (org-links-org--unnormalize-string (regexp-quote link-org-string))
                             ">>"))
        (link-name (format "^[ \t]*#\\+NAME: +%s[ \t]*$" (regexp-quote link-org-string)))
        res)

    (when enable-target
      ;; search <<target>>
      (setq res (org-links-find-first-two-exact-lines-in-buffer-optimized
                 link-target get-position))
      (when org-links-debug-flag
        (print (format "org-links--find-line2\n%s\n%s" link-target res)))
      ;; search #+NAME:
      (setq res (org-links-find-first-two-exact-lines-in-buffer-optimized
                 link-name get-position))
      (when org-links-debug-flag
        (print (format "org-links--find-line3\n%s\n%s" link-name res))))

    (unless res
      ;; search LINE
      (setq res (org-links-find-first-two-exact-lines-in-buffer-optimized
                 link-line get-position))
      (when org-links-debug-flag
        (print (format "org-links--find-line4\n%s\n%s" link-line res))))

    ;; (unless (or res only-line)
    ;;   ;; search LINE with Org
    ;;   (let ((ln-before (line-number-at-pos)))
    ;;     (save-excursion
    ;;       (org-with-wide-buffer
    ;;        (let ((found (org-link-search link-org-string)))
    ;;          (when org-links-debug-flag
    ;;            (print (format "org-links--find-line3 %s" found)))
    ;;          (when (and (eq found 'dedicated)
    ;;                     (not (= (line-number-at-pos) ln-before))) ; found?
    ;;            (setq res (list (if get-position
    ;;                      (point)
    ;;                    ;; else
    ;;                    (line-number-at-pos))))))))))
    ;; - return first NUM from res, signal erro
    (if res
        (prog1
            (car res) ; return
          (when (> (length res) 1)
            (when org-links-on-several-halt-flag
              (signal 'org-links-on-several-error res))
            (unless org-links-silent
              (message "More than one line found, NUM is used. %s" res))))
      ;; else - no res
      (unless org-links-silent
        (message "Line not found, NUM is used."))
      nil))) ; return

;; (defun org-lnd-target (target-string)
;;   "Return line number that match TARGET-STRING in buffer or nil.
;; If GET-POSITION is non-nil, then return position instead of line
;; numbner."
;;   (let* ((link (concat "<<" (org-links-org--unnormalize-string (regexp-quote target-string)) ">>"))
;;          (re (org-links-find-first-two-exact-lines-in-buffer-optimized link t)))
;;     (if (eq (length re) 1) ;; found exactly one
;;         (car re)
;;       ;; else
;;       (unless org-links-silent
;;         (if  (> (length re) 1)
;;             (message "More than one line found, NUM is used. %s" re)
;;           ;; else
;;           (message "Line not found, NUM is used.")))
;;       nil)))


;; -=  Open link - help functions and variablses

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

(defun org-links--local-get-target-position-for-link (link)
  "For LINK string return (line-num-beg line-num-end) or (line-num-beg) or nil.
Use current buffer for search line.
Also profide fix for not-Org modes to able to search fuzzy.
LINK is plain link without [].
::LINK not handled.
Return t if success or nil if failed"
  (when org-links-debug-flag
    (print (format "org-links--local-get-target-position-for-link N1 %s %s"
                   link (derived-mode-p 'org-mode))))
  (condition-case err
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
          (if-let* ((n1 (org-links--find-line line)) ; search <<target>>
                    (n2 (+ n1 (- num2 num1))))
              (list n1 n2)
            ;; else num1-num2
            (list num1 num2))))
       ;; NUM::LINE
       ((when-let* ((num1 (and (string-match org-links-num-line-regexp link)
	                       (match-string 1 link)))
	            (line (match-string 2 link))) ; may be ""
          (when org-links-debug-flag
            (print (format "org-links--local-get-target-position-for-link N2 %s %s"
                           link (derived-mode-p 'org-mode))))
          (if-let* ((n1 (and (not (string-empty-p line))
                             (org-links--find-line line t)))) ; search target also
              (progn
                (when org-links-debug-flag
                  (print (format "org-links--local-get-target-position-for-link N3 %s"
                                 n1
                                 )))
                (list n1 nil))
            ;; else - fail to find line, return NUM
            (list (string-to-number num1)))))
       ;; ;; LINE - may be a target (without <<>>) or fuzzy link
       ;; ((and (not (derived-mode-p 'org-mode))
       ;;       (when-let ((num1 (org-links--find-line link t)))
       ;;         (list num1 nil))))
       ;; t - debug
       ((when org-links-debug-flag
          (print (format "org-links--local-get-target-position-for-link failed"))
          nil)))
    (org-links-on-several-error ; exception name
     (user-error "More than one lnes was found at lines: %s" (cdr err))
     nil)))

;; (org-links--get-target-position-for-link "1-2::asd")
;; (org-links--get-target-position-for-link "480::")
;; (progn (string-match org-links-num-line-regexp "480::") (match-string 2 "480::"))

;; ;; (add-hook 'org-open-link-functions #'org-links-fix-open-target-not-org)
;; ;;;###autoload
;; (defun org-links-fix-open-target-not-org (link-content)
;;   "Implementation of hook `org-open-link-functions'.
;; LINK-CONTENT is a string of link without open and close square brackets.
;; Fix for case for not Org mode to find <<target1>> links.
;; Execution path that cause error: `org-link-open-from-string' ->
;; `org-link-open' (hook here) -> `org-link-search' (cause error).
;; Hook requirement: When the function does handle the link, it must return
;; a non-nil value.  Don't called for file: type of links."
;;   (when org-links-debug-flag
;;     (print (list "org-links-fix-open-target-not-org"  link-content)))
;;   (if (and (not (derived-mode-p 'org-mode))
;;            (not current-prefix-arg))
;;       (progn (when-let ((pos (org-links--find-line link-content)))
;;                (push-mark nil t)
;;                (goto-char pos))
;;                t)
;;     ;; else
;;     nil))

;; (add-hook 'org-open-link-functions #'org-links-fix-open-target-not-org)


;;;###autoload
(defun org-links-additional-formats (link)
  "Jump to link position in current buffer.
Called from `org-link-search', which always called for link targets in
current buffer.
Continue with `org-link-search' if link was not found.
LINK is string after :: or was just in [[]].
Used for  `org-execute-file-search-functions'.
`org-execute-file-search-in-bibtex' as example.
Return t if link was processed to stop `org-link-search' or nil to
 fallback to `org-link-search'."
  (when org-links-debug-flag
    (print (list "org-links-additional-formats N1" link)))
  ;; 1) get line numbers for link
  (when-let ((nums (org-links--local-get-target-position-for-link link)))
    (when org-links-debug-flag
      (print (list "org-links-additional-formats N2" nums)))
    (let ((num1 (car nums))
          (num2 (cadr nums)))
      ;; 2) jump
      (org-goto-line num1)
      (when num2
        (org-links-num-num-enshure-num2-visible num2))
      t)))

(defun org-links-org-link-search (search)
  "Check if `org-link-search' can found result again after current line.
We apply `org-link-search' for area after current line to the end of the
 buffer, suppress errors and give warning for two resutls.
Argument SEARCH is link to search."
  (save-excursion
    (save-restriction
      (narrow-to-region (line-end-position) (point-max)) ;  (with-restriction (line-end-position) (point-max)
      (condition-case nil
          ;; (with-restriction (line-end-position) (point-max)
          (let ((org-link-search-must-match-exact-headline t))
            (org-link-search search nil t))
        (error nil)
        (user-error nil)))))


;; (add-hook 'org-execute-file-search-functions #'org-links-additional-formats)
;; (remove-hook 'org-execute-file-search-functions #'org-links-additional-formats)
;; -=  Approach 1) org-open-file advice - based on fuzzy links. Fix probles caused by org-open-file.
;;;###autoload
(defun org-links-org-open-file-advice (orig-fun &rest args)
  "Fix `org-open-file' for NUM-NUM, NUM-NUM::LINE, NUM::LINE formats.
Open file before pass control to `org-link-search' with our hook.
Argument ORIG-FUN is `org-open-file'.
Support file::LINE and file:LINE formats.
Use current buffer for search line.
We apply original function to open file and then find in it.
Optional argument ARGS is `org-open-file' arguments."
  ;; [[file::#+ or  [[file:#+
  (when org-links-debug-flag
    (print (list "org-links-org-open-file-advice N1" args)))
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
    (ignore string) ; noqa: unused
    (when org-links-debug-flag
      (print (list "org-links-org-open-file-advice N2" path search)))
    (cond
     ((and search ; part after ::
           (or (string-match org-links-num-num-regexp search) ; NUM-NUM
               (string-match org-links-num-line-regexp search) ; NUM::LINE
               (string-match org-links-num-num-line-regexp search)) ; NUM-NUM::LINE
           (progn
             (when org-links-debug-flag
               (print (list "org-links-org-open-file-advice N3" "")))
             (let ((cbfn (buffer-file-name (buffer-base-buffer))) ;no scratch
                   (tbf (get-file-buffer path)))
               (if (and tbf
                        (or (and cbfn (not (file-equal-p cbfn path))) ; if not scratch and different
                            (not cbfn))) ; scratch
                   (switch-to-buffer-other-window tbf)
                 ;; else
                 (apply orig-fun (list path in-emacs))))
             ;; (when-let ((cbfn (buffer-file-name (buffer-base-buffer))))
             ;;   (print "OOOOOpen2")
             ;;   ;; (unless (file-equal-p cbfn path)
             ;;     (apply orig-fun (list path in-emacs)))
             (org-links-additional-formats search))))
     ;; (search ; PATH::LINE - fallback to `org-open-file' + `org-link-search'
     ;;  ;; current buffer may be *scratch*
     ;;  ;; link may be to current buffer with path

     ;;  (let ((cbfn (buffer-file-name (buffer-base-buffer))) ; no scratch
     ;;        (tbf (get-file-buffer path)))
     ;;    (when org-links-debug-flag
     ;;      (print (list "org-links-org-open-file-advice N4" "")))
     ;;    (if (and tbf
     ;;             (or (and cbfn (not (file-equal-p cbfn path))) ; if not scratch and different
     ;;                 (not cbfn))) ; scratch
     ;;        (switch-to-buffer-other-window tbf)
     ;;      ;; else
     ;;      (apply orig-fun args)))

     ;;  ;; (when (org-links-org-link-search search)
     ;;  ;;   (if org-links-on-several-halt-flag
     ;;  ;;       (user-error "Two targets exist for this link")
     ;;  ;;     ;; else
     ;;  ;;     (unless org-links-silent
     ;;  ;;       (message "Warning: Two targets exist for this link."))))
     ;;  )
         ;; ;; NUM-NUM
         ;; ((when-let* ((num1 (and (string-match org-links-num-num-regexp search)
	 ;;                         (match-string 1 search)))
	 ;;              (num2 (match-string 2 search)))
         ;;    (apply orig-fun (list path in-emacs (string-to-number num1)))
         ;;    (org-links-num-num-enshure-num2-visible num2)
         ;;    t))
         ;; ;; NUM-NUM::LINE
         ;; ((when-let* ((num1 (and (string-match org-links-num-num-line-regexp search)
	 ;;                         (match-string 1 search)))
	 ;;              (num2 (match-string 2 search))
         ;;              (line (match-string 3 search)))
         ;;    (apply orig-fun (list path in-emacs))
         ;;    (if-let ((line-position (org-links--find-line line t))) ; skip search <<target>>
         ;;        (org-goto-line line-position)
         ;;      ;; else
         ;;      (org-goto-line (string-to-number num1))
         ;;      (org-links-num-num-enshure-num2-visible num2))
         ;;    t))
         ;; ;; NUM::LINE
         ;; ((when-let* ((num1 (and (string-match org-links-num-line-regexp search)
	 ;;                         (match-string 1 search)))
	 ;;              (line (match-string 2 search))) ; may be ""
         ;;    (apply orig-fun (list path 'emacs)) ; in emacs
         ;;    (if-let ((line-position (and (not (string-empty-p line))
         ;;                                 (org-links--find-line line))))
         ;;        (org-goto-line line-position)
         ;;      ;; else
         ;;      (org-goto-line (string-to-number num1)))
         ;;    t))
     (t ;; else - PATH::LINE - fallback to `org-open-file' + `org-link-search'
      ;; (old) Addon to Org logic: signal if two targets exist
      ;; else - no part after ::
      (when org-links-debug-flag
        (print (list "org-links-org-open-file-advice N5" orig-fun args)))
      (apply orig-fun (list path t))
      ;; (apply orig-fun args)
      ))))


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
    ;; - raise error if check that file exist, dont check if buffer with file path exist
    (save-excursion
      (save-match-data
        (when (org-in-regexp org-link-any-re)
          (goto-char (match-beginning 0))
          (when-let* ((link (org-element-link-parser))
                      (type (org-element-property :type link))
	              (path (org-element-property :path link)))
            (when (and (string= type "file")
                       (not (file-readable-p path))
                       (not (get-file-buffer path)))
              (user-error "File does not exist"))))))

    ;; - call with catching error
    (when org-links-debug-flag
      (print (list "org-links-org-open-at-point-global" "")))
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
