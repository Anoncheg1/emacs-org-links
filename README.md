![build](https://github.com/Anoncheg1/emacs-org-links/workflows/melpazoid/badge.svg)
[![MELPA](https://melpa.org/packages/org-links-badge.svg)](http://melpa.org/#/org-links)
[![MELPA Stable](https://stable.melpa.org/packages/org-links-badge.svg)](https://stable.melpa.org/#/org-links)

# emacs-org-links

This package (org-links) provides facilities to help create and manage links that have both line number and line itself.

## TL;DR
Org mode:
- C-c w
```[[180::asd]]```

- C-u C-c w
```[[file:~/file.org::13::asd][asd]]```

Programming mode:
- C-c w
```[[file:~/file.org::13::asd][asd]]```

- C-u C-c w
```[[180::asd]]```

Region selected:
- C-c w
```[[180-190::asd]]```

- C-u C-c w
```[[file:~/file.org::13-17::asd][asd]]```


## Features

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

Known issue: Org export not working properly with new formats.

## Org mode provide by default

Org mode supports file links with line numbers and line via the following syntax:
- `[[PATH::NUM][Link description]]`
- `[[PATH::LINE][Link description]]`

There is `find-file-at-point` functions from ffap.el for opening FILENAME.

## Behavior

If links with link number we use own search algo.

Functions for storing links copy shortest links wihtout universtal and linkest links with it.

First we search for target with <<>> and then for full line.

## Why?

LLMs and fuzzy search will be more effective with additional information, if you want link that point to block of code you will need a range of line numbers

This is the solution to some Org links problems:
- links stored without line number
- targets in Org mode: stored same way as a lines
- opening links with fuzzy search will match any first line with fuzzy substrings, not full line match, (org-link-search-must-match-exact-headline = nil required).
- fuzzy always match full line exactly (we search for first lines that begins with link)

## How?
### Installation - from MELPA
```elisp
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
```
Install via `M-x package-install RET org-links RET` or `M-x package-list-packages`

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
  "Copy this function from org-links.el file.
Used, when org-links package is not installed.")

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

Org use:
1) org-open-at-point
2) org-links-org-open-at-point-global

Those functions (C-c C-o) call `org-open-file` and `org-link-open`, we modify behavior of the last one.
- we directy advice `org-open-file` - called for "file:" links.
- add hook to `org-execute-file-search-functions` that called from `org-link-search` function that called for short links.
- and to `org-open-link-functions`, called from `org-link-open` - Fix for case for not Org mode to find <<target1>> links.


- org-links-org-open-file-advice - called for :file
- org-links-additional-formats -> org-links--local-get-target-position-for-link - called for short links
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
You can sponsor author directly with crypto currencies:

- BTC (Bitcoin) address: 1CcDWSQ2vgqv5LxZuWaHGW52B9fkT5io25

![BTC](https://raw.githubusercontent.com/Anoncheg1/public-share/refs/heads/main/BTC-1CcDWSQ2vgqv5LxZuWaHGW52B9fkT5io25.png)

- USDT (Tether TRX-TRON) address: TVoXfYMkVYLnQZV3mGZ6GvmumuBfGsZzsN

![BTC](https://raw.githubusercontent.com/Anoncheg1/public-share/refs/heads/main/USDT-TVoXfYMkVYLnQZV3mGZ6GvmumuBfGsZzsN.png)

- TON (Telegram) address: UQC8rjJFCHQkfdp7KmCkTZCb5dGzLFYe2TzsiZpfsnyTFt9D