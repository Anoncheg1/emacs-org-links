# emacs-org-links
Configuration for using Org links function from ol.el.

And new supported form of links [[PATH::NUM::LINE]]. At opening we search for LINE first, if not found exactly one, we use NUM.

For ex. [[file:./notes/warehouse.el::23::(defun alina (pic))]]

## Why?

Show you how to use links right, and more advanced "not strict" link.

This is the solution to some Org links problems:
- links sotred without number
- targets in Org mode: stored same as a lines
- opening links with fuzzy search will match any first line with fuzzy substrings, not full line match, (org-link-search-must-match-exact-headline = nil required).

## How?

### Simple configuration

```elisp
(require 'org-links)
(global-set-key (kbd "C-c w") #'org-links-store-extended)
(advice-add 'org-link-open-as-file :around #'org-links--org-link-open-as-file))
```

### Advanced configuration

```elisp
(require 'ol)
(global-set-key (kbd "C-c C-o") #'org-open-at-point-global) ; optional

(defun my/org-store-link-fallback (arg)
  "Copy link to kill-ring clipboard.
In  form  PATH::NUM,  if  with  universal argument  C-u,  then  in  form
PATH::LINE."
  (interactive "P\n")
  (let ((link (if (and (not arg)
                       (or (derived-mode-p 'prog-mode)
                           (and (not (derived-mode-p 'org-mode)) (derived-mode-p 'text-mode))
                           (derived-mode-p 'fundamental-mode)))
                   (let* ((org-link-context-for-files)
                          (link (org-store-link nil)))
                     (concat (substring link 0 (- (length link) 2)) "::" (number-to-string (line-number-at-pos)) "]]"))
                 ;; else - with line for fuzzy search
                 (org-store-link nil))))
    (kill-new link)
    (message (concat link "\t- copied to clipboard"))))

(add-to-list 'load-path "/home/g/sources/emacs-org-links")
(if (not (require 'org-links nil 'noerror))
    (global-set-key (kbd "C-c w") #'my/org-store-link-fallback)
  ;; else - org-links have been loaded
  (global-set-key (kbd "C-c w") #'org-links-store-extended)
  ;; For support opening links in format file:PATH::NUM::line
  (advice-add 'org-link-open-as-file :around #'org-links--org-link-open-as-file))
```

### How Org links works?

https://orgmode.org/guide/Hyperlinks.html

Storing or copying: `org-store-link' store link to org-stored-links variable.

Inserting: `org-stored-links' and `org-insert-link-global' put link to buffer.

Opening in Org: `org-open-at-point' -> `org-link-open' ; org.el

Opening everywhere: `org-open-at-point-global' ; org.el
- -> `org-link-open-from-string' -> `org-link-open' (element)
- -> `org-link-open-as-file' -> `org-open-file' -> `org-link-search' for fuzzy

Org config variables for links in ol.el:
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
