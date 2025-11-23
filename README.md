![build](https://github.com/Anoncheg1/emacs-org-links/workflows/melpazoid/badge.svg)
[![MELPA](https://melpa.org/packages/org-links-badge.svg)](http://melpa.org/#/org-links)
[![MELPA Stable](https://stable.melpa.org/packages/org-links-badge.svg)](https://stable.melpa.org/#/org-links)

# emacs-org-links

Org mode supports file links with line numbers and line via the following syntax:
- `[[PATH::NUM][Link description]]`
- `[[PATH::LINE][Link description]]`

There is `find-file-at-point` functions from ffap.el.

This package (org-links) provides facilities to help create and manage these links:
1) The command `org-links-store-extended' copies a link to the current file, at the current point.
2) The syntax above is extended to include a few variants that are useful for linking into source code:
- `[[PATH::NUM::LINE]]`
- `[[PATH::NUM-NUM::LINE]]`
- `[[PATH::NUM-NUM]]`

3) A helpful warning is triggered when a link has an ambiguous target (e.g., in the case where two targets are found).

For ex. `[[file:./notes/warehouse.el::23::(defun alina (pic))]]`

You just copy link with *C-c C-w* and insert with *C-y* in any mode.

## How  [[PATH::NUM::LINE]] links works?
First, we search for LINE, if not found we use NUM line number.

`[[NUM-NUM]]` - used for region selection.

## Why?

LLMs and fuzzy search will be more effective with additional information, if you want link that point to block of code you will need a range of line numbers

This is the solution to some Org links problems:
- links sotred without number
- targets in Org mode: stored same as a lines
- opening links with fuzzy search will match any first line with fuzzy substrings, not full line match, (org-link-search-must-match-exact-headline = nil required).

## How?
### Installation - from MELPA
```elisp
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
```
Install via `M-x package-install RET org-links RET`

### Installation - With `use-package`
If your package is available on MELPA, add this to your init file:

```elisp
(use-package org-links
  :ensure t)
```

If installing from a GitHub repo (not yet in MELPA), specify the source:
```elisp
(use-package org-links
  :straight (org-links :host github :repo "Anoncheg1/emacs-org-links"))
;; Requires straight.el.
```


## Simple configuration

```elisp
(require 'org-links)
;; opening for C-c C-o
(add-hook 'org-execute-file-search-functions #'org-links-additional-formats)
(advice-add 'org-open-file :around #'org-links-org-open-file-advice)
;; copying
(global-set-key (kbd "C-c w") #'org-links-store-extended)
```

## Advanced configuration

```elisp
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
           (concat "file:" (buffer-file-name (buffer-base-buffer))))

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
    (message  "%s\t- copied to clipboard" link)))

(add-to-list 'load-path "/home/g/sources/emacs-org-links")
(if (not (require 'org-links nil 'noerror))
    (progn
      ;; falback
      (global-set-key (kbd "C-c w") #'org-links-store-link-fallback)
      (require 'ol)
      (global-set-key (kbd "C-c C-o") #'org-open-at-point-global)) ; optional
  ;; - else
  ;; org-links configuration
  ;; opening
  (add-hook 'org-execute-file-search-functions #'org-links-additional-formats)
  ;; (advice-add 'org-element-link-parser :around #'org-links--org-element-link-parser-advice)
  (advice-add 'org-open-file :around #'org-links-org-open-file-advice)
  ;; copying
  (global-set-key (kbd "C-c w") #'org-links-store-extended)
  ;; opening
  (global-set-key (kbd "C-c C-o") #'org-links-org-open-at-point-global))

;; recommended:
(setopt org-link-file-path-type 'absolute) ; create links with full path
(setopt org-link-search-must-match-exact-headline nil) ; use fuzzy search of Org links
(setopt org-link-descriptive nil) ; show links in raw, don't hide
```

### Copy link to ring instead of opening
```elisp
(add-hook 'org-mode-hook (lambda ()
                           (make-variable-buffer-local 'org-link-parameters)
                           (dolist (scheme '("http" "https"))
                             (org-link-set-parameters scheme
                                          :follow
                                          (lambda (url arg)
                                              (setq-local url (concat "http:" url arg))
                                              (kill-new url))))))
```

## How this package works

Provided function for copying link to kill ring with additional format for programming mode.

For opening links we add hook to org-execute-file-search-functions that called from `org-link-search' function, used by Org function for oppening files: `org-open-at-point' (bound to C-c C-o by default in Org mode.) and `org-open-at-point-global'.

## Other packages
- Navigation in Dired, Packages, Buffers modes https://github.com/Anoncheg1/firstly-search
- Search with Chinese	https://github.com/Anoncheg1/pinyin-isearch
- Ediff fix		https://github.com/Anoncheg1/ediffnw
- Dired history	https://github.com/Anoncheg1/dired-hist
- Selected window contrast	https://github.com/Anoncheg1/selected-window-contrast
- Copy link to clipboard	https://github.com/Anoncheg1/org-links
- Solution for "callback hell"	https://github.com/Anoncheg1/emacs-async1
- Call LLMs and AI agents from Org-mode ai block. https://github.com/Anoncheg1/emacs-oai

## Donate, sponsor author
You can sponsor author crypto money directly with crypto currencies:
- BTC (Bitcoin) address: 1CcDWSQ2vgqv5LxZuWaHGW52B9fkT5io25
- USDT (Tether) address: TVoXfYMkVYLnQZV3mGZ6GvmumuBfGsZzsN
- TON (Telegram) address: UQC8rjJFCHQkfdp7KmCkTZCb5dGzLFYe2TzsiZpfsnyTFt9D