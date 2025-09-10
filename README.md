# emacs-org-links
And new formats of links:
- [[PATH::NUM::LINE]] - At opening we search for LINE first, if not found exactly one, we use NUM line number.
- [[PATH::NUM-NUM::LINE]]
- [[PATH::NUM-NUM]]
- [[PATH::NUM]] creating

For ex. [[file:./notes/warehouse.el::23::(defun alina (pic))]]

Also, provide configuration for using standard ol.el without requirement of this package. Copying links to clipboard kill ring.

## Why?

LLMs and fuzzy search will be more effective with additional information, if you want link that point to block of code you will need a range of line numbers

This is the solution to some Org links problems:
- links sotred without number
- targets in Org mode: stored same as a lines
- opening links with fuzzy search will match any first line with fuzzy substrings, not full line match, (org-link-search-must-match-exact-headline = nil required).

## How?

### Simple configuration

```elisp
(require 'org-links)
;; opening for C-c C-o
(add-hook 'org-execute-file-search-functions #'org-links-additional-formats)
(advice-add 'org-open-file :around #'org-links-org-open-file-advice)
;; copying
(global-set-key (kbd "C-c w") #'org-links-store-extended)
```

### Advanced configuration

```elisp
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

(add-to-list 'load-path "/home/g/sources/emacs-org-links")
(if (not (require 'org-links nil 'noerror))
    (global-set-key (kbd "C-c w") #'org-links-store-link-fallback)

  ;; org-links configuration
  ;; opening
  (add-hook 'org-execute-file-search-functions #'org-links-additional-formats)
  (advice-add 'org-open-file :around #'org-links-org-open-file-advice)
  ;; copying
  (global-set-key (kbd "C-c w") #'org-links-store-extended))

(require 'ol)
(global-set-key (kbd "C-c C-o") #'org-open-at-point-global) ; optional

```

### How Org links works?

https://orgmode.org/guide/Hyperlinks.html

Storing: `org-store-link' store link to org-stored-links variable  `org-stored-links', functions `org-insert-link' and `org-insert-link-global' put link to buffer.

Opening 1): `org-open-at-point' -> `org-link-open' ; org.el
- for "files:" `org-link-open-as-file' -> `org-open-file' (handle "::23", cause troubles) -> `org-link-search'
- for local links `org-link--search-radio-target' and `org-link-search' used

Opening 2): `org-open-at-point-global' ; org.el
- -> `org-link-open-from-string' -> `org-link-open' (element)

`org-link-search' (for curret buffer) call `org-execute-file-search-functions' or search link.

Org configurable variables:
- org-link-context-for-files - default t, store fuzzy text
- org-link-search-must-match-exact-headline - if nil search fuzzy

Simple storing links to kill ring:

```elisp
(require 'ol)
;; - Store:
(let ((org-link-context-for-files))
    (kill-new (org-store-link nil)))
;; - Open:
(let ((org-link-search-must-match-exact-headline))
   (org-open-at-point-global))
```

### How this package works

Provided function for copying link to kill ring with additional format for programming mode.

For opening links there is advice that extend standard org-open-at-point-global and org-open-at-point function used to follow link to support new additional format of link.

### Donate, sponsor author
You can give me crypto money directly with crypto currencies:
- BTC (Bitcoin) address: 1CcDWSQ2vgqv5LxZuWaHGW52B9fkT5io25
- USDT (Tether) address: TVoXfYMkVYLnQZV3mGZ6GvmumuBfGsZzsN
