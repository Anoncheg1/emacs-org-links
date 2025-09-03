;;; org-links.el --- Links [[file:PATH::NUM::LINE]] that search for line and then for number -*- lexical-binding: t -*-

;; Copyright (c) 2025 github.com/Anoncheg1,codeberg.org/Anoncheg
;; SPDX-License-Identifier: AGPL-3.0-or-later
;; Author: github.com/Anoncheg1,codeberg.org/Anoncheg
;; Keywords: org, links, link, jump, jumping
;; URL: https://github.com/Anoncheg1/emacs-org-links
;; Version: 0.1
;; Created: 30 Aug 2025
;; Package-Requires: ((emacs "27.1") (compat "30.1"))
;; > (Emacs 26+) for negative regex
;; "27.1" for ol.el

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
;; format for programming mode.

;; For opening links there is advice that extend standard
;; org-open-at-point-global and org-open-at-point function used to
;; follow link to support new additional format of link.

;; Org support opening links PATH::NUM with line number but don't
;; implement creation of them.

;; Provided configuration for confirtable usage of Org links.
;; Provided additional format of links PATH::NUM::LINE for programming modes.


;; Configuration simple:
;; (require 'org-links)
;; (global-set-key (kbd "C-c w") #'org-links-store-extended)
;; (advice-add 'org-link-open-as-file :around #'org-links--org-link-open-as-file))

;; Advanced configuration in README.md

;; Terms and Org default behavior:
;; - Org link: [[link][description]]
;; - sads -> <<<sads>>> # radio-target
;; - [[ads]] -> <<ads>> # target
;; - "::*text" - fuzzy link to header
;; - "::asd" - link to target or fuzzy search
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
;; standard ol.el function and we add additional format for
;; programming modes.
;;
;; We  add  advice  around   `org-link-open-as-file'  that  called  by
;; `org-open-at-point-global' and by `org-open-at-point' that bound to
;; C-c C-o by default in Org mode.

;; ==== How Org links works: ====
;; https://orgmode.org/guide/Hyperlinks.html
;;
;; Storing: `org-store-link' store link to org-stored-links variable
;; Org: `org-stored-links' and `org-insert-link-global' put link to buffer.

;; Opening: `org-open-at-point' -> `org-link-open' ; org.el
;; or everywhere: `org-open-at-point-global' ; org.el
;; -> `org-link-open-from-string' -> `org-link-open' (element)
;; -> `org-link-open-as-file' -> `org-open-file' -> `org-link-search' for fuzzy

;; Org config:
;; - org-link-context-for-files - default t, store fuzzy text
;; - org-link-search-must-match-exact-headline - if nil search fuzzy

;; - Simple solution
;; store without fuzzy only PATH:
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
;; - opening links with fuzzy search will match any first line with fuzzy substrings, not full line match, (org-link-search-must-match-exact-headline = nil required).

;; ==== Name: as referece ====
;; `org-babel-find-named-block' - for source-code block only, uses org-babel-src-block-regexp (try to replace with org-block-regexp)
;; (let ((org-babel-src-block-regexp org-block-regexp))
;;   (org-babel-find-named-block "asd"))
;;
;; `org-store-link' and `org-open-at-point' works with [[file:~/a.org::nname]] and [[nname]] - look for <<target>> or #+NAME:

;; DONATE MONEY, SPONSOR AUTHOR
;; You can give me crypto money directly with crypto currencies:
;; - BTC (Bitcoin) address: 1CcDWSQ2vgqv5LxZuWaHGW52B9fkT5io25
;; - USDT (Tether) address: TVoXfYMkVYLnQZV3mGZ6GvmumuBfGsZzsN

;;; Code:
;;; - code
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

;;; - Store
(defun org-links--begins-with-single-asterisk-p (s)
  "For checking that S link was not created for Org header.
That have format *header-text."
  (and (string-prefix-p "*" s)
       (> (length s) 1)
       (not (eq (aref s 1) ?*))))

;; (defun org-links--org-store-link (func-call &rest args)
;;   "From [[PATH::LINE]] make [[PATH::NUM::LINE]].
;; Abbreviate path used."
;;   (let ((org-stored-links) ; replace with local one
;;         (link
;;          (cond
;;           ;; - Images mode 1
;;           ((derived-mode-p 'image-dired-thumbnail-mode)
;;            ;; create link by self
;;            (concat "file:" (abbreviate-file-name (image-dired-original-file-name))))
;;           ;; - Images mode 2
;;           ((derived-mode-p 'image-dired-image-mode)

;;            (buffer-file-name (buffer-base-buffer)))
;;           ;; - in Org - <<target>>
;;           ;; ((and (buffer-file-name (buffer-base-buffer))
;;           ;;       (derived-mode-p 'org-mode)
;;           ;;       (org-in-regexp "[^<]<<\\([^<>]+\\)>>[^>]" 1))
;;           ;;  (match-string 1)
;;           ;;  )

;;           ;; else
;;           ;; - Call `org-store-link'
;;           (t
;;            (apply func-call args)
;;            ;; - link-stored returned by `org-store-link'
;;            (let ((link-stored (substring-no-properties (car (car org-stored-links))))
;;                  (all-prefixes (org-link-types)))
;;              (print (list "link-stored" org-stored-links))
;;              ;; - Split link-stored
;;              (let ((desc (apply #'mapconcat #'identity (cdr (string-split link-stored "::")) '("::")))
;;                    (before-desc (car (string-split link-stored "::")))
;;                    ;; detect type, like in `org-insert-link'
;;                    (type
;;                     (cond
;;                      ((and all-prefixes
;;                            (string-match (rx-to-string `(: string-start (submatch (or ,@all-prefixes)) ":")) link-stored))
;;                       (match-string 1 link-stored))
;;                      ((file-name-absolute-p link-stored) "file")
;;                      ((string-match "\\`\\.\\.?/" link-stored) "file"))))
;;                ;; (print (list "type" type))
;;                (print (list "desc" desc))
;;                ;; (link-path (string-split link "::")
;;                ;; (let* ((link-element (with-temp-buffer
;;                ;;                (let ((org-inhibit-startup nil))
;;                ;;                  (insert link)
;;                ;;                  (org-mode)
;;                ;;                  (goto-char (point-min))
;;                ;;                  (org-element-link-parser))))
;;                ;;        (type (org-element-property :type link-element))
;;                ;;        (path (org-element-property :path link-element))
;;                ;;        (follow (org-link-get-parameter type :follow))
;;                ;;        (option (org-element-property :search-option link-element))) ;; after ::
;;                ;;   (print (list type path option follow))
;;                ;;   (print link-element))
;;                (if (and (string-equal type "file")
;;                         (or (derived-mode-p 'prog-mode)
;;                             (derived-mode-p 'text-mode)
;;                             (derived-mode-p 'fundamental-mode))
;;                         (not (org-links--begins-with-single-asterisk-p desc))) ; not header
;;                    ;; if pointer at <<target>>
;;                    (if (and (not (string-empty-p desc)) (org-in-regexp "[^<]<<\\([^<>]+\\)>>[^>]" 1))
;;                        (concat before-desc "::" (number-to-string (line-number-at-pos)) "::<<" (match-string 1) ">>")
;;                      ;; else - file links
;;                      (let* ((desc (if (not (string-empty-p desc)) (concat "::" desc)))
;;                             (link (concat before-desc "::" (number-to-string (line-number-at-pos))
;;                                           (if org-link-context-for-files desc) )))
;;                        link))
;;                  ;; else - original
;;                  link-stored)))))))
;;     ;; - Final link preparation
;;     (let ((link2 (concat "[[" link "]]")))
;;       (kill-new link2)
;;       (message (concat link2 "\t- copied to clipboard"))
;;       link2)))

;;; - Open link
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
        ;; (print (list start (string-match search-string-regex buf-str start)))
        (while (and (< (length results1) n)
                    (string-match search-string-regex buf-str start))
          ;; (print (list (< (length results1) n) (string-match search-string-regex buf-str start)))
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
              ;; (print (list (string-full-match search-string-regex line) search-string-regex line))
              (when (and (not (string-empty-p line)) ; skip empty lines
                     (org-links-string-full-match search-string-regex line))
                (push (if get-positions (line-beginning-position) ln) results2))
              (forward-line 1)
              (setq ln (1+ ln))))
            (nreverse results2))))))

(defun org-links--find-line (link-org-string)
  "Return empty list or with numbers that match LINK-ORG-STRING."
  (let ((link (concat "^" (org-links-org--unnormalize-string (regexp-quote link-org-string)) "$")))
    (org-links-find-first-two-exact-lines-in-buffer-optimized link)))

;; (org-links--find-line (regexp-quote "(ln 1))"))
;; (string-match "^[ 	]*))$" "ssd\n))\n vv")

;; (let ((search-string ";; (list name"))
;;   (let ((buf-str (substring-no-properties ";;                 (list name"))
;;         (regexp (concat "^" ((regexp-quote search-string) "$"))
;;         (start 0)
;;         (results1 '()))
;;     (print regexp)
;;     (string-full-match regexp buf-str))))

;; (defvar org-links--target-re "\\(^\\|[^<]\\)\\(<<\\([^<][^>]*\\)>>\\)"
;;   "Find Org link type called target.
;; This [^<]<<\\([^<>]+\\)>>[^>] from `org-store-link' require symbols before and after target.")

;; ;; (let ((s "vvd<<asd>>vv"))
;; ;;   (when (string-match "[^<]<<\\([^<>]+\\)>>[^>]" s)
;; ;;     (match-string 1 s))) ;; => "asd"

;; ;; (let ((s "<<asd>>"))
;; ;;   (when (string-match "[^<]<<\\([^<>]+\\)>>[^>]" s)
;; ;;     (match-string 1 s))) ;; => nil


;; (if (not (let ((s "<<asd>>"))
;;            (org-links-string-full-match org-links--target-re s)))
;;     (error "<<asd>>1"))

;; (if (not (string-equal (let ((s "<<asd>>"))
;;       (when (string-match org-links--target-re s)
;;         (match-string 3 s)))
;;               "asd"))
;;     (error "<<asd>>2"))

(defun org-links--org-link-open-as-file (orig-fun &rest args)
  "Extend `org-link-open-as-file' that is ORIG-FUN with ARGS.
For file:/path/file::NUM::DESC.
Advice for `org-link-open-as-file'.
We look for TEXT in file and if found and only one go there.
Otherwise, go to NUM.
Return True, if we identify and follow a link of el."
  (if-let* ((path (car args))
            (in-emacs (cdr args))
            (num (and (string-match "::\\([0-9]+\\)::\\(.*\\)" path)
                      (match-string 1 path)))
            (line (match-string 2 path))) ;; may be ""

      ;; (print (list "org-links--org-link-open-as-file" num line (string-empty-p line)))
      (if
          ;; - PATH::NUM::
          (string-empty-p line)
          ;; - Call with ("PATH::NUM" in-emacs)
          (apply orig-fun (list (substring path 0 (- (match-beginning 2) 2)) in-emacs))
        ;; - else - Num and text
        ;; (print (list "aaaaaaaaaaaaa" (list (substring path 0 (match-beginning 0)) in-emacs)))
        ;; open file
        (apply orig-fun (list (substring path 0 (match-beginning 0)) in-emacs))
        (let ((line-position (org-links--find-line line)))
          ;; (print (list "line-position" line-position (eq (length line-position) 1)))
          (goto-char (point-min)) ; move to begining of buffer for call of `forward-line' after
          (if (eq (length line-position) 1) ;; found exactly one
              (forward-line (1- (car line-position)))
            ;; else - not found or many of them, we use  num to jump
            (forward-line (1- (string-to-number num)))))
        )
    ;; else
    (apply orig-fun args)))

    ;; ;; (print desc)
    ;; ;; (print el)
    ;; ;; (string-match "^\\([0-9]+\\)::\\(.*\\)" "232::") ; => 0
    ;; ;; (string-match "^\\([0-9]+\\)::\\(.*\\)" "::aasd") ; = nil

    ;; ;; ;; (let ((s "vvd<<asd>>vv"))
    ;; ;; ;;   (when (string-match "[^<]<<\\([^<>]+\\)>>[^>]" s)
    ;; ;; ;;     (match-string 1 s))) ;; => "asd"
    ;; ;; ;; (substring-no-properties (match-string 0))
    ;; (when (and desc (string-match "^\\([0-9]+\\)::\\(.*\\)" desc))
    ;;   (print (list "desc111111111" desc (zerop (match-beginning 0)) (= (match-end 0) (length desc))))
    ;;   (let ((num (match-string 1 desc))
    ;;         (text (match-string 2 desc))
    ;;         ;; (tagrget-m (string-match "\\(^\\|[^<]\\)\\(<<\\([^<][^>]*\\)>>\\)" desc))
    ;;         ) ; may be "" empty
    ;;     ;; FILE::NUM::DESC
    ;;     (print (list "desc here" desc))
    ;;     (cond
    ;;      ;; - PATH::NUM
    ;;      ((string-empty-p text)
    ;;       (print "ww")
    ;;       ;; Modify and open link:
    ;;       ;; (let ((el (org-element-context)))
    ;;       (org-element-put-property el :search-option num)
    ;;       ;; (org-element-put-property el :raw-link "file:~/tmp/emacs-file2025-08-26.org::23")
    ;;       (org-link-open-from-string (org-element-interpret-data el))
    ;;       (goto-line (number-to-string num))
    ;;       )

    ;;      ;; - FILE::NUM::<<target>>
    ;;      ((org-links-string-full-match org-links--target-re text)
    ;;       (print "target")
    ;;       (match-string 3 text)
    ;;       )
    ;;      ;; - Num and text
    ;;      (t
    ;;       (print "vvvv")
    ;;       ;; open path
    ;;       (org-link-open-as-file path
    ;;                              (pcase (org-element-property :application el)
    ;;                                ("emacs" 'emacs)
    ;;                                ("sys" 'system))) ; org-link-open
    ;;       ;; In path buffer:
    ;;       (print (list "aaaaaaaaaaaaa" text))
    ;;       (if (org-links-string-full-match org-links--target-re text)


    ;;           ;; else -
    ;;       (let ((line-position (org-links--find-line text)))
    ;;         (print (list "line-position" line-position (eq (length line-position) 1)))
    ;;         (if (eq (length line-position) 1) ;; found exactly one
    ;;             (goto-line (car line-position))
    ;;           ;; else - not found or many of them, we use  num to jump
    ;;           (goto-line (string-to-number num)))))))))))

;; (defun my/org-open-at-point-functions-impl ()
;;   "Activation for opening links support.
;; Hook implementation, that added to `org-open-at-point-functions'.
;; Don't work for `org-open-at-point-global'. :-("
;;   (if-let* ((el (org-element-context))
;;             (el-type (org-element-type el))
;;             (type-str (org-element-property :type el)))
;;       (print (list "my/org-open-at-point-functions-impl" type-str))
;;     (if (and (eql el-type 'link) (string-equal type-str "file"))
;;         (org-links--open el))))


;;; - alternative solution
;; (defun my/org-link--file-link-to-here ()
;;   "Return as (LINK . DESC) a file link with search string to here.
;; Called only from `org-store-link'."
;;   (let ((link (concat "file:"
;;                       (abbreviate-file-name
;;                        (buffer-file-name (buffer-base-buffer)))))
;;         desc)

;;       (pcase (org-link-precise-link-target)
;;         (`nil nil)
;;         (`(,search-string ,search-desc ,_position)
;;          (if org-link-context-for-files
;;              (progn
;;                (setq link (format "%s::%s::%s" link (line-number-at-pos) search-string))
;;                (setq desc search-desc))
;;            ;; else - no context but we save line number at least
;;            (progn
;;                (setq link (format "%s::%s" link (line-number-at-pos)))
;;                (setq desc search-desc))
;;            )))
;;     (cons link desc)))

;; (advice-add 'org-link--file-link-to-here :override #'my/org-link--file-link-to-here)

;; (defun my/org-create-file-search-functions-imp ()
;;   " Called from `org-store-link'.
;; `org-link--file-link-to-here' reimplementation.
;; Return part after \"file:path::\" of link."
;; (if (derived-mode-p 'image-dired-thumbnail-mode)
;; ;;         ;; create link by self
;; ;;         (setq link (concat "file:" (image-dired-original-file-name)))
;;   (let ((ret
;;          ((cond
;;            ((derived-mode-p 'image-dired-thumbnail-mode)


;;          (pcase (org-link-precise-link-target)
;;            (`nil nil)
;;            (`(,search-string ,search-desc ,_position)
;;             (if org-link-context-for-files
;;                 (format "%s::%s" (line-number-at-pos) search-string)
;;               ;; else - no context but we save line number at least
;;               (number-to-string (line-number-at-pos)))))))
;;       ;; save to buffer
;;       (kill-new (concat "[[file:" (abbreviate-file-name buffer-file-name)
;;                         "::" ret "]]"))
;;       ret))

;;; - Fix for creation of link, disable removing of leading and ending "(" and ")"
;; (defun org-link-precise-link-target ()
;;   "Determine search string and description for storing a link.

;; If a search string (see `org-link-search') is found, return
;; list (SEARCH-STRING DESC POSITION).  Otherwise, return nil.

;; If there is an active region, the contents (or a part of it, see
;; `org-link-context-for-files') is used as the search string.

;; In Org buffers, if point is at a named element (such as a source
;; block), the name is used for the search string.  If at a heading,
;; its CUSTOM_ID is used to form a search string of the form
;; \"#id\", if present, otherwise the current heading text is used
;; in the form \"*Heading\".

;; If none of those finds a suitable search string, the current line
;; is used as the search string.

;; The description DESC is nil (meaning the user will be prompted
;; for a description when inserting the link) for search strings
;; based on a region or the current line.  For other cases, DESC is
;; a cleaned-up version of the name or heading at point.



;; POSITION is the buffer position at which the search string
;; matches."
;;   (let* ((region (org-link--context-from-region))
;;          (result
;;           (cond
;;            (region
;;             (list (org-link--normalize-string region t)
;;                   nil
;;                   (region-beginning)))

;;            ((derived-mode-p 'org-mode)
;;             (let* ((element (org-element-at-point))
;;                    (name (org-element-property :name element))
;;                    (heading (org-element-lineage element '(headline inlinetask) t))
;;                    (custom-id (org-entry-get heading "CUSTOM_ID")))
;;               (cond
;;                (name
;;                 (list name
;;                       name
;;                       (org-element-begin element)))
;;                ((org-before-first-heading-p)
;;                 (list (org-link--normalize-string (org-current-line-string) t)
;;                       nil
;;                       (line-beginning-position)))
;;                (heading
;;                 (list (if custom-id (concat "#" custom-id)
;;                         (org-link-heading-search-string))
;;                       (org-link--normalize-string
;;                        (org-get-heading t t t t))
;;                       (org-element-begin heading))))))

;;            ;; Not in an org-mode buffer, no region
;;            (t
;;             (list (org-link--normalize-string (org-current-line-string) nil) ;; <------ Fix: without = nil
;;                   nil
;;                   (line-beginning-position))))))

;;     ;; Only use search option if there is some text.
;;     (when (org-string-nw-p (car result))
;;       result)))

(defun org-links-org-link--normalize-string (string &optional context)
  "Modified version of `org-link--normalize-string'.
Instead of much of removal we only compact spaces and remove leading.
Instead of removing [1/3], [50%], leading ( and trailing ), spaces at
the end of STRING, we just compress spaces in line and remove leading
spaces from STRING.  CONTEXT ignored."
  (setq context context) ;; noqa for Warning: Unused lexical argument ‘context’
  (let ((string
	 ;; (org-trim
          (string-trim
	  (replace-regexp-in-string
	   (rx (one-or-more (any " \t")))
	   " "
	   ;; (replace-regexp-in-string
	   ;;  ;; Statistics cookie regexp.
	   ;;  (rx (seq "[" (0+ digit) (or "%" (seq "/" (0+ digit))) "]"))
	   ;;  " "
	   string) "[ \t\n\r]+" nil ))) ; trim only left
    ;; (when context
    ;;   (while (cond ((and (string-prefix-p "(" string)
    ;;     		 (string-suffix-p ")" string))
    ;;     	    (setq string (org-trim (substring string 1 -1))))
    ;;     	   ((string-match "\\`[#*]+[ \t]*" string)
    ;;     	    (setq string (substring string (match-end 0))))
    ;;     	   (t nil))))
    string))

(defun org-links-org--unnormalize-string (string)
  "Convert STRING to regex.
Reverse of `org-links-org-link--normalize-string.
Add spaces at begin of line and replace spaces with any number of spaces
or tabs in the middle.
To create proper regex, string should be first be processed with
`regexp-quote'."
  (concat "[ \t]*" (string-replace " " "[ \t]+" string)))


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

;;; - simple store link

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
     ((derived-mode-p 'image-dired-thumbnail-mode)
      (setq link (concat "file:" (abbreviate-file-name (image-dired-original-file-name)))))
     ;; - Images mode 2
     ((derived-mode-p 'image-dired-image-mode)
      (setq link (buffer-file-name (buffer-base-buffer))))

     ;; - Programming mode store without fuzzy content and add line number."
     ((or (derived-mode-p 'prog-mode)
          (and (not (derived-mode-p 'org-mode)) (derived-mode-p 'text-mode))
          (derived-mode-p 'fundamental-mode))
      (setq link (org-store-link nil))
      (setq link (if arg
                     ;; store in PATH::NUM::LINE format
                     (concat (substring link 0 (- (length link) 2))
                             "::" (number-to-string (line-number-at-pos))
                             "::" (org-links-org-link--normalize-string (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
                             "]]")
                   ;; else
                   (concat (substring link 0 (- (length link) 2)) "::" (number-to-string (line-number-at-pos)) "]]"))))
     ;; - Org mode
     (t
      (setq link (org-store-link nil))
      (setq link (if arg
                     ;; store in PATH::NUM::LINE format
                     (concat (substring link 0 (- (length link) 2))
                             "::" (number-to-string (line-number-at-pos))
                             "::" (org-links-org-link--normalize-string (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
                             "]]")
                   ;; else
                   (setq org-link-context-for-files t)
                   (org-store-link nil)
                   ))
      ))
    (kill-new link)
    (message (concat link "\t- copied to clipboard"))))

;;; - Fallback simple function without requirement of org-links this package
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
             (concat "file:" (abbreviate-file-name (image-dired-original-file-name)))
           ;; - else
           (if (derived-mode-p 'image-dired-image-mode)
                (buffer-file-name (buffer-base-buffer))
             ;; - else
             (if (and (not arg)
                      (or (derived-mode-p 'prog-mode)
                          (and (not (derived-mode-p 'org-mode)) (derived-mode-p 'text-mode))
                          (derived-mode-p 'fundamental-mode)))
                 (let* ((org-link-context-for-files)
                        (link (org-store-link nil)))
                   (concat (substring link 0 (- (length link) 2)) "::" (number-to-string (line-number-at-pos)) "]]"))
               ;; else - with line for fuzzy search
               (org-store-link nil))))))
    (kill-new link)
    (message  "%s\t- copied to clipboard" link)))

;;; provide
(provide 'org-links)

;;; org-links.el ends here
