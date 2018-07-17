;;;; General emacs settings ;;;;
;; Disabling some ui elements
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(setq inhibit-startup-screen t)
;; Disable alarm noise
(setq ring-bell-function 'ignore)
;; Increase line spacing a bit
(setq-default line-spacing 4)
;; Disabling automatic backup and autosaving
(setq make-backup-files nil)
(setq auto-save-default nil)
;; Setting ibuffer as the default buffer list
(global-set-key (kbd "C-x C-b") 'ibuffer)

; ;; Margin settings (WIP)
; ; Colour at bottom of config to avoid overwriting it
; ; Size
; (defun my-set-margins ()
;   "Set margins in current buffer"
;   (setq left-margin-width  4)
;   (setq right-margin-width 4))
; ; Enable for org mode
; (add-hook 'org-mode-hook 'my-set-margins)
; ; Colour
; (set-face-attribute 'fringe nil :background "#EFF0F1" :foreground "#EFF0F1")

;; Setting font
(set-face-attribute 'default nil :family "Fira Code" :height 160)

;; Org mode settings for line wrapping and indentation
;; If org is installed via a package manager wrap with with-eval-after-load
(setq org-startup-indented t)
(add-hook 'org-mode-hook #'visual-line-mode)
;; Change number of separator lines (default 2)
(setq org-cycle-separator-lines 1)
;; Enabling font application to entire heading line (for leuven theme)
(setq org-fontify-whole-heading-line t)


;;;; Package Installation and Settings ;;;;
;; Boostrapping straight.el package manager
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 4))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Installing and configuring packages
(straight-use-package 'use-package)
(straight-use-package 'zenburn-theme)
(straight-use-package 'all-the-icons)
(straight-use-package 'smart-mode-line)

;; Auctex (doesn't seem to work with use-package, indented to match formatting)
(straight-use-package 'auctex)
  ;; Enable auto saving and document parsing
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  ;; Make auctex aware of multi-file document structures
  (setq-default TeX-master nil)

(use-package exec-path-from-shell
  :straight t
  :config
  (when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize)))

(use-package evil
  :straight t
  :init
  (setq evil-want-abbrev-expand-on-insert-exit nil)
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode t))

(use-package helm
  :straight t
  :config
  (helm-mode 1))

(use-package leuven-theme
  :straight t
  :init
  ;; Changing leuven theme org mode heading scaling
  (setq leuven-scale-outline-headlines 1.2))

(use-package evil-leader
  :straight t
  ;; I know it says below this should be enabled before but it seems to break C-u otherwise
  :after evil
  :config
  ;; Note: should be enabled before evil mode
  (global-evil-leader-mode)
  ;; Evil-leader combination bindings
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key "w" 'save-buffer))

(use-package evil-org
  :straight t
  :after evil
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package evil-snipe
  :straight t
  :after evil
  :config
  (evil-snipe-mode +1)
  (evil-snipe-override-mode +1)
  (setq evil-snipe-scope 'visible))

(use-package neotree
  :straight t
  :after evil
  :config
  ;; Rebinding evil-mode keys for neotree compatibility
  (evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
  (evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
  (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
  ;; Enabling icons for neotree
  (setq neo-autorefresh nil)
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

(use-package org-bullets
  :straight t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; NOTE: Some packages are placed after the theme specification by necessity
;; Menu set options (currently just theme)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(custom-enabled-themes (quote (leuven)))
 '(custom-safe-themes
   (quote
    ("9a155066ec746201156bb39f7518c1828a73d67742e11271e4f24b7b178c4710" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "420459d6eeb45aadf5db5fbcc3d6990b65141c104911f7359454fc29fa9d87a0" default)))
 '(fci-rule-color "#383838")
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))

;; Setting up smart mode line (complains if before the menu set theme
(setq sml/theme 'light)
(sml/setup)
