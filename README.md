# emacs-org-links
And new formats of links:
- [[PATH::NUM::LINE]] - At opening we search for LINE first, if not found exactly one, we use NUM line number.
- [[PATH::NUM-NUM::LINE]]
- [[PATH::NUM-NUM]]
- [[PATH::NUM]] creating

For ex. [[file:./notes/warehouse.el::23::(defun alina (pic))]]

Also, provide:
- configuration for using standard ol.el without requirement of this package. Copying links to clipboard kill ring.
- Warning if two Org targets exist in one file.

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
(use-package async1
  :straight (async1 :host github :repo "Anoncheg1/emacs-org-links"))
;; Requires straight.el.
```


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

### Copy link to ring instead of opening
```elisp
(add-hook 'org-mode-hook (lambda ()
                           (make-variable-buffer-local 'org-link-parameters)
                           (dolist (scheme '("http" "https"))
                             (org-link-set-parameters scheme
                                          :follow
                                          (lambda (url arg)
                                              (setq-local url (concat "http:" url arg))
                                              (kill-new url)
                                              )))))
```

### How this package works

Provided function for copying link to kill ring with additional format for programming mode.

For opening links we add hook to org-execute-file-search-functions that called from `org-link-search' function, used by Org function for oppening files: `org-open-at-point' (bound to C-c C-o by default in Org mode.) and `org-open-at-point-global'.

### Donate, sponsor author
You can give me crypto money directly with crypto currencies:
- BTC (Bitcoin) address: 1CcDWSQ2vgqv5LxZuWaHGW52B9fkT5io25
- USDT (Tether) address: TVoXfYMkVYLnQZV3mGZ6GvmumuBfGsZzsN
