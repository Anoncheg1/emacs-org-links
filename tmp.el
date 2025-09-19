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


;; (org-links--find-line (regexp-quote "(ln 1))"))
;; (string-match "^[ 	]*))$" "ssd\n))\n vv")

;; (let ((search-string ";; (list name"))
;;   (let ((buf-str (substring-no-properties ";;                 (list name"))
;;         (regexp (concat "^" ((regexp-quote search-string) "$"))
;;         (start 0)
;;         (results1 '()))
;;     (print regexp)
;;     (string-full-match regexp buf-str))))


;; ;;;###autoload
;; (defun org-links-additional-formats (link)
;;   "Local search for additional formats in current buffer.
;; Called from `org-link-search', which always called for link targets in
;; current buffer.
;; LINK is string after :: or was just in [[]].
;; `org-execute-file-search-in-bibtex' as example."
;;   ;; from `org-link-open-as-file'
;;   (print "org-links-additional-formats")
;;   (cond
;;    ;; NUM-NUM
;;    ((when-let* ((num1 (and (string-match org-links-num-num-regexp link)
;; 	                   (match-string 1 link)))
;; 	        (num2 (match-string 2 link)))

;;       (org-goto-line (string-to-number num1))
;;       (org-links-num-num-enshure-num2-visible num2)
;;       t))
;;    ;; NUM-NUM::LINE
;;    ((when-let* ((num1 (and (string-match org-links-num-num-line-regexp link)
;;                            (match-string 1 link)))
;;                 (num2 (match-string 2 link))
;;                 (line (match-string 3 link)))
;;       ;; use line
;;       (let ((n1 (string-to-number num1))
;;             (n2 (string-to-number num2)))
;;         (if-let ((line-position (org-links--find-line line)))
;;             (progn
;;               (org-goto-line line-position)
;;               (when (> n2 n1)
;;                   (org-links-num-num-enshure-num2-visible (+ line-position (- n2 n1)))))
;;           ;; else - use NUM-NUM
;;           (org-goto-line n1)
;;           (org-links-num-num-enshure-num2-visible num2))
;;         t)))
;;    ;; NUM::LINE
;;    ((when-let* ((num1 (and (string-match org-links-num-line-regexp link)
;; 	                   (match-string 1 link)))
;; 	        (line (match-string 2 link)))
;;       ;; use line
;;       (if-let ((line-position (org-links--find-line line)))
;;           (org-goto-line line-position)
;;         ;; else - use NUM
;;         (org-goto-line (string-to-number num1)))
;;       t))))


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
