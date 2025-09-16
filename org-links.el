;;; org-links.el --- Configuration and some new links: [[PATH::NUM::LINE]] -*- lexical-binding: t -*-

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

;;; Commentary:

;; Provided function for copying link to kill ring with additional
;; formats, useful for programming modes.
;; - [[PATH::NUM::LINE]]
;; - [[PATH::NUM-NUM::LINE]]
;; - [[PATH::NUM-NUM]]
;; - [[PATH::NUM]] creating

;; Copying links to clipboard kill ring. Warning at opening if two targets exist.

;; [[PATH::NUM::LINE]] - At opening we search for LINE first, if not
;; found exactly one, we use NUM line number.

;; For opening links there is advice that extend standard
;; org-open-at-point-global and org-open-at-point function used to
;; follow link to support new additional format of link.

;; Org support opening links PATH::NUM with line number but don't
;; implement creation of them.

;; Configuration:
;; (require 'org-links)
;; (add-hook 'org-execute-file-search-functions #'org-links-additional-formats)
;; (advice-add 'org-open-file :around #'org-links-org-open-file-advice)
;; (global-set-key (kbd "C-c w") #'org-links-store-extended)

;; You may advanced configuration in README.md file.

;; Terms and Org default behavior:
;; - Org link: [[link][description]]
;; - sads -> <<<sads>>> # radio-target
;; - [[ads]] -> <<ads>> # target
;; - "::*text" - fuzzy link to header
;; - "::asd" - link to target or fuzzy search
;; - "::234" - number of line supported only with "file:"
;;
;; Org links: ol.el store links to headers if it have one otherwise it
;;   use fuzzy search if you use:
;; (setq org-link-search-must-match-exact-headline nil)

;; Features:
;; - respect org-link-context-for-files, if not set store only number.
;; - correctly store file in image-dired-thumbnail-mode
;; - Add support for image-dired-thumbnail-mode and image-dired-image-mode

;; How this works:
;; We provide new function `org-links-store-extended' that use
;;  standard ol.el function and we add additional format for
;;  programming modes.
;;
;; We  add  advice  around   `org-link-open-as-file'  that  called  by
;;  `org-open-at-point-global' and by `org-open-at-point' that bound to
;;  C-c C-o by default in Org mode.

;; ==== How Org links works: ====
;; https://orgmode.org/guide/Hyperlinks.html
;; Classification according to `org-element-link-parser':
;; - radio - any text to <<<target>>>
;; - bracket - [[link]]
;;   - file:
;;   - coderef, custom-id, fuzzy
;; - Plain link - type:...
;; - Angular link <type:...>


;; Storing: `org-store-link' store link to org-stored-links variable
;;  `org-stored-links', functions `org-insert-link' and
;;  `org-insert-link-global' put link to buffer.

;; Opening 1): `org-open-at-point'
;; -> `org-link-open' ; org.el
;;   - for "files:" `org-link-open-as-file' -> `org-open-file' (handle
;;        "::23", cause troubles) -> `org-link-search'
;;   - for local links `org-link--search-radio-target' and `org-link-search' used

;; Opening 2): `org-open-at-point-global' ; org.el
;; -> `org-link-open-from-string' -> `org-link-open' (element)

;; `org-link-search' (for curret buffer) call  `org-execute-file-search-functions' or search link.

;; Org configurable variables:
;; - org-link-context-for-files - default t, store fuzzy text
;; - org-link-search-must-match-exact-headline - if nil search fuzzy

;; How links readed:
;; org-open-at-point use cache, `org-open-at-point-global' uses org-element-link-parser

;; - Simple solution
;; Store without fuzzy only PATH:
;; (require 'ol)
;; - Store:
;; (let ((org-link-context-for-files))
;;    (kill-new (org-store-link nil)))
;; - Open:
;; (let ((org-link-search-must-match-exact-headline))
;;    (org-open-at-point-global))
;;
;; Simple solution problems
;; - links sotred without number
;; - targets in Org mode: stored same as a lines
;; - Opening links  with fuzzy search  will match any first  line with
;;   fuzzy      substrings,       not      full       line      match,
;;   (org-link-search-must-match-exact-headline = nil required).

;; ==== Name: as referece ====
;; `org-babel-find-named-block'  - for  source-code  block only,  uses
;;    org-babel-src-block-regexp (try to replace with org-block-regexp)

;; (let ((org-babel-src-block-regexp org-block-regexp))
;;   (org-babel-find-named-block "asd"))
;;

;; `org-store-link'     and     `org-open-at-point'     works     with
;;   [[file:~/a.org::nname]]  and [[nname]]  -  look  for <<target>>  or #+NAME:
;;
;; Documentation used https://orgmode.org/manual/Adding-Hyperlink-Types.html
;; But this works for links types defined as prefix: "man:".

;;; TODO:
;; - add advice for  org-element-link-parser and add own  link type to
;;   simplify `org-link-open'

;; DONATE MONEY, SPONSOR AUTHOR:
;; You can sponsor me directly with crypto currencies:
;; - BTC (Bitcoin) address: 1CcDWSQ2vgqv5LxZuWaHGW52B9fkT5io25
;; - USDT (Tether) address: TVoXfYMkVYLnQZV3mGZ6GvmumuBfGsZzsN
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
    (org-insert-link nil string nil)
    (buffer-substring-no-properties (point-min) (point-max))))

;; (if (not (string-equal (org-links-create-link "file:.././string") "[[file:~/sources/string]]"))
;;     (error "Org-links"))

;;; - Copy to clipboard
;;;###autoload
(defun org-links-store-extended (arg)
  "Store link to `kill-ring' clipboard.
ARG is universal argument.
Count lines from 1 like `line-number-at-pos' function does.
For usage with original Org `org-open-at-point-global' function."
  (interactive "P\n")
  (let ((org-link-context-for-files nil)
        link)
    (cond
     ;; - Images mode 1
     ((derived-mode-p (intern "image-dired-thumbnail-mode"))
      (setq link
            ;; (org-links-create-link
                  (concat "file:" (funcall (intern "image-dired-original-file-name")))))
     ;; - Images mode 2
     ((derived-mode-p (intern "image-dired-image-mode"))
      (setq link
            ;; (org-links-create-link
             (concat "file:" (buffer-file-name (buffer-base-buffer)))))

     ((use-region-p)
      (let ((path (org-links-create-link (concat "file:" (buffer-file-name (buffer-base-buffer))))))
        (setq link (concat (substring path 0 (- (length path) 2)) "::"
                (number-to-string (line-number-at-pos (region-beginning))) "-" (number-to-string (line-number-at-pos (region-end)))
                "]]"))))

     ;; - PATH::NUM::LINE - for Programming modes and fundamental
     ;; store without fuzzy content and add line number."
     ((or (derived-mode-p 'prog-mode)
          (and (not (derived-mode-p 'org-mode)) (derived-mode-p 'text-mode))
          (derived-mode-p 'fundamental-mode))
      (setq link (org-store-link nil))
      (when link
        (setq link (substring-no-properties link))
        (setq link (if arg
                       ;; store in PATH::NUM::LINE format
                       (org-links-create-link (concat (substring link 2 (- (length link) 2)) ; path
                                                      "::" (number-to-string (line-number-at-pos))
                                                      "::" (org-links-org-link--normalize-string (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))
                     ;; else
                     (org-links-create-link (concat (substring link 2 (- (length link) 2)) "::" (number-to-string (line-number-at-pos))))))))
     ;; - Org mode
     (t
      (setq link (org-store-link nil))
      (when link
        (setq link (substring-no-properties link))
        (setq link (if arg
                       ;; store in PATH::NUM::LINE format
                       (org-links-create-link (concat (substring link 2 (- (length link) 2))
                                                      "::" (number-to-string (line-number-at-pos))
                                                      "::" (org-links-org-link--normalize-string (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))
                     ;; else
                     (setq org-link-context-for-files t) ; local in let
                     (substring-no-properties (org-store-link nil)))))))
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
                 (let* ((org-link-context-for-files)
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
  (concat "[ \t]*" (string-replace " " "[ \t]+" string) "[ \t]*"))


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

(defun org-links--find-line (link-org-string)
  "Return position that match LINK-ORG-STRING in buffer or nil."
  (let* ((link (concat "^" (org-links-org--unnormalize-string (regexp-quote link-org-string)) "$"))
         (re (org-links-find-first-two-exact-lines-in-buffer-optimized link)))
    (if (eq (length re) 1) ;; found exactly one
        (car re)
      ;; else
      nil)))

;; (org-links--find-line (regexp-quote "(ln 1))"))
;; (string-match "^[ 	]*))$" "ssd\n))\n vv")

;; (let ((search-string ";; (list name"))
;;   (let ((buf-str (substring-no-properties ";;                 (list name"))
;;         (regexp (concat "^" ((regexp-quote search-string) "$"))
;;         (start 0)
;;         (results1 '()))
;;     (print regexp)
;;     (string-full-match regexp buf-str))))

;;; - Open link - help functions and variablses

;; (let ((path  "234-444"))
;;                  (string-match "^\\([0-9]+\\)-\\([0-9]+\\)$" path )
;;                  (list (match-string 1 path) (match-string 2 path))) ; => ("234" "444")

;; not used
;; (defun org-links--get-position-for-line-number (N)
;;   "N is line number."
;;   (save-excursion
;;     (goto-char (point-min))
;;     (forward-line (1- N))
;;     (point)))

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

(defun org-links-num-num-enshure-num2-visible (num2-str)
  "For NUM-NUM format, we enshure that NUM is visible when jump.
NUM2-STR is number of line.
Recenter screen and Two times check visibility."
  (let ((num2 (string-to-number num2-str)))
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
;;;###autoload
(defun org-links-additional-formats (link)
  "Local search for additional formats in current buffer.
Called from `org-link-search'.
LINK is string after :: or was just in [[]].
`org-execute-file-search-in-bibtex' as example."
  ;; from `org-link-open-as-file'
  (print "org-links-additional-formats")
  (cond
   ;; NUM-NUM
   ((when-let* ((num1 (and (string-match org-links-num-num-regexp link)
	                   (match-string 1 link)))
	        (num2 (match-string 2 link)))

      (org-goto-line (string-to-number num1))
      (org-links-num-num-enshure-num2-visible num2)
      t))
   ;; NUM-NUM::LINE
   ((when-let* ((num1 (and (string-match org-links-num-num-line-regexp link)
                           (match-string 1 link)))
                (num2 (match-string 2 link))
                (line (match-string 3 link)))
      ;; use line
      (let ((n1 (string-to-number num1))
            (n2 (string-to-number num2)))
        (if-let ((line-position (org-links--find-line line)))
            (progn
              (org-goto-line line-position)
              (if (> n2 n1) (org-links-num-num-enshure-num2-visible (+ line-position (- n2 n1)))))
          ;; else - use NUM-NUM
          (org-goto-line n1)
          (org-links-num-num-enshure-num2-visible num2))
        t)))
   ;; NUM::LINE
   ((when-let* ((num1 (and (string-match org-links-num-line-regexp link)
	                   (match-string 1 link)))
	        (line (match-string 2 link)))
      ;; use line
      (if-let ((line-position (org-links--find-line line)))
          (org-goto-line line-position)
        ;; else - use NUM
        (org-goto-line (string-to-number num1)))
      t))))

;; (add-hook 'org-execute-file-search-functions #'org-links-additional-formats)
;; (remove-hook 'org-execute-file-search-functions #'org-links-additional-formats)
;;; - Approach 1) org-open-file advice - based on fuzzy links. Fix probles caused by org-open-file.
;;;###autoload
(defun org-links-org-open-file-advice (orig-fun &rest args)
  "Support for additional formats.
Argument ORIG-FUN is `org-open-file' that breaks at NUM-NUM,
NUM-NUM::LINE, NUM::LINE formats.
Optional argument ARGS is `org-open-file' arguments."
  (print (list "org-links-org-open-file-advice" args))
  (seq-let (path in-emacs string search) args
    (setq string string) ;; noqa: unused
    (if search ; part after ::
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
          (with-restriction (line-end-position) (point-max)
            (save-excursion
              (condition-case nil
                  (with-restriction (line-end-position) (point-max)
                    (let ((org-link-search-must-match-exact-headline t))
                      (when (org-link-search search nil t)
                        (message "Warning: Two targets exist for this link."))))
                (error nil)
                (user-error nil))))))
      ;; else - no part after ::
      (apply orig-fun args))))


;; (advice-add 'org-open-file :around #'org-links-org-open-file-advice)
;; (advice-remove 'org-open-file #'org-links-org-open-file-advice)

;;; - Approach 2) Alternative implementation by advice to link parse for [[NUM-NUM]], NUM::LINK, etc
;; (defun org-links--org-element-link-parser-advice (orig-fun &rest args)
;;   "Advice to recognize NUM-NUM links.
;; For `org-element-link-parser'."
;;   (print "org-links--org-element-link-parser-advice")
;;   (catch 'no-object
;;     (let ((begin (point))
;; 	  end contents-begin contents-end link-end post-blank path type format
;; 	  raw-link search-option application
;;           (explicit-type-p nil))
;;       (cond
;;        ;; Type 1: Text targeted from a radio target.
;;        ((and org-target-link-regexp
;; 	     (save-excursion (or (bolp) (backward-char))
;;                              (if org-target-link-regexps
;;                                  (org--re-list-looking-at org-target-link-regexps)
;;                                (looking-at org-target-link-regexp))))
;;         (setq type "radio")
;;         (setq format 'plain)
;;         (setq link-end (match-end 1))
;;         (setq path (match-string-no-properties 1))
;;         (setq contents-begin (match-beginning 1))
;;         (setq contents-end (match-end 1)))
;;        ;; Type 2: Standard link, i.e. [[https://orgmode.org][website]]
;;        ((looking-at org-link-bracket-re)
;;         (setq format 'bracket)
;;         (setq contents-begin (match-beginning 2))
;;         (setq contents-end (match-end 2))
;;         (setq link-end (match-end 0))
;;         ;; RAW-LINK is the original link.  Decode any encoding.
;;         ;; Expand any abbreviation in it.
;;         ;;
;;         ;; Also treat any newline character and associated
;;         ;; indentation as a single space character.  This is not
;;         ;; compatible with RFC 3986, which requires ignoring
;;         ;; them altogether.  However, doing so would require
;;         ;; users to encode spaces on the fly when writing links
;;         ;; (e.g., insert [[shell:ls%20*.org]] instead of
;;         ;; [[shell:ls *.org]], which defeats Org's focus on
;;         ;; simplicity.
;;         (setq raw-link (org-link-expand-abbrev
;; 		        (org-link-unescape
;; 			 (replace-regexp-in-string
;; 			  "[ \t]*\n[ \t]*" " "
;; 			  (match-string-no-properties 1)))))
;;         ;; Determine TYPE of link and set PATH accordingly.  According
;;         ;; to RFC 3986, remove whitespaces from URI in external links.
;;         ;; In internal ones, treat indentation as a single space.

;;         ;; (print (list "raw-link" raw-link (string-match org-links-num-num-regexp raw-link))) ;; "414-2344"

;;         (cond
;; 	 ;; File type.
;;          ((or (string-match org-links-num-num-regexp raw-link)
;;               (string-match org-links-num-line-regexp raw-link)
;;               (string-match org-links-num-num-line-regexp raw-link))
;;           (setq type "num")
;; 	  (setq path raw-link ))
;;          ;;  (setq type "num-num")
;; 	 ;;  (setq path raw-link ))
;;          ;; ((string-match org-links-num-line-regexp raw-link)
;;          ;;  (setq type "num-line")
;; 	 ;;  (setq path raw-link ))
;;          ;; ((string-match org-links-num-num-line-regexp raw-link)
;;          ;;  (setq type "num-num-line")
;; 	 ;;  (setq path raw-link ))
;;          (t
;;           ;; (print "throw1")
;;           (throw 'no-object nil))))
;;        (t
;;         ;; (print "throw2")
;;         (throw 'no-object nil)))

;;       ;; In any case, deduce end point after trailing white space from
;;       ;; LINK-END variable.
;;       (save-excursion
;;         (setq post-blank
;; 	      (progn (goto-char link-end) (skip-chars-forward " \t")))
;;         (setq end (point)))
;;       ;; Special "file"-type link processing.  Extract opening
;;       ;; application and search option, if any.  Also normalize URI.
;;       (when (string-match "\\`file\\(?:\\+\\(.+\\)\\)?\\'" type)
;;         (setq application (match-string-no-properties 1 type))
;;         (setq type "file")
;;         (when (string-match "::\\(.*\\)\\'" path)
;; 	  (setq search-option (match-string-no-properties 1 path))
;; 	  (setq path (replace-match "" nil nil path)))
;;         (setq path (replace-regexp-in-string "\\`///*\\(.:\\)?/" "\\1/" path)))
;;       ;; Translate link, if `org-link-translation-function' is set.
;;       (let ((trans (and (functionp org-link-translation-function)
;; 		        (funcall org-link-translation-function type path))))
;;         (when trans
;; 	  (setq type (car trans))
;;           (setq explicit-type-p t)
;; 	  (setq path (cdr trans))))

;;       (org-element-create
;;        'link
;;        (list :type (org-element--get-cached-string type)
;;              :type-explicit-p explicit-type-p
;; 	     :path path
;; 	     :format format
;; 	     :raw-link (or raw-link path)
;; 	     :application application
;; 	     :search-option search-option
;; 	     :begin begin
;; 	     :end end
;; 	     :contents-begin contents-begin
;; 	     :contents-end contents-end
;; 	     :post-blank post-blank
;;              ))))
;;   ;; (print "org-links--org-element-link-parser-advice call origin")
;;   (apply orig-fun args))

;; (defun org-links-follow (search _)
;;   "Open a \"help\" type link.
;; PATH is a symbol name, as a string."
;;   (print (list "vvvbas" search))
;;   (let ((orig-fun 'org-open-file)
;;         (path "")
;;         in-emacs)
;;     (when-let* ((num1 (and (string-match org-links-num-num-regexp search)
;; 	                   (match-string 1 search)))
;; 	        (num2 (match-string 2 search)))
;;       ;; (print (list "vvvbas" "num1" num1 "num2" num2))
;;       (apply orig-fun (list path in-emacs (string-to-number num1)))
;;       (org-links-num-num-enshure-num2-visible num2)
;;       t)))

;; (org-link-set-parameters "num" :follow #'org-links-follow)
;; test:
;; (org-link-open-from-string "[[414-453][s]]")
;; [[414-2344][s]]
;; [[file:org-links.el::644-646]]
;; (advice-add 'org-element-link-parser :around #'org-links--org-element-link-parser-advice)
;;; provide
(provide 'org-links)

;;; org-links.el ends here
