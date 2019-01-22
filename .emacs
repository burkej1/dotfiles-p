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
(global-set-key (kbd "C-x b") 'ibuffer)

;; Setting font
; (set-face-attribute 'default nil :family "Fira Code" :height 160)
(add-to-list 'default-frame-alist
             '(font . "Fira Code-16"))

; Fixing weird problem where bold text was smaller in org mode
(setq org-emphasis-alist
 (quote
  (
   ("*" (bold :height 170))
   )))

;; Org mode settings for line wrapping and indentation
;; If org is installed via a package manager wrap with with-eval-after-load
(setq org-startup-indented t)
(add-hook 'org-mode-hook #'visual-line-mode)
;; Change number of separator lines (default 2)
(setq org-cycle-separator-lines 1)
;; Enabling font application to entire heading line (for leuven theme)
(setq org-fontify-whole-heading-line t)


;; Function to call org-time-stamp-inactive with a prefix argument
;; Defined here so it can be bound to a leader key combination using evil leader
(defun org-time-stamp-inactive-prefixarg ()
  (interactive)
  (let ((current-prefix-arg '(4))) (call-interactively 'org-time-stamp-inactive)))


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
(straight-use-package 'spacemacs-theme)

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

; (use-package leuven-theme
;   :straight t
;   :init
;   ;; Changing leuven theme org mode heading scaling
;   (setq leuven-scale-outline-headlines 1.2))

(use-package evil-leader
  :straight t
  ;; I know it says below this should be enabled before but it seems to break C-u otherwise
  :after evil
  :config
  ;; Note: should be enabled before evil mode
  (global-evil-leader-mode)
  ;; Evil-leader combination bindings
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key "s" 'save-buffer)
  (evil-leader/set-key "v" 'org-brain-visualize)
  (evil-leader/set-key "t" 'org-time-stamp-inactive-prefixarg))

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

(use-package org-brain
  :straight t
  :after evil
  :init
  (setq org-brain-path "~/Dropbox/Writing_Sync/Writing/map")
  (evil-set-initial-state 'org-brain-visualize-mode 'emacs)
  (setq org-brain-visualize-default-choices 'all)
  (setq org-brain-title-max-length 12))

;; Theme Settings
; (load-theme 'zenburn t)
; (load-theme 'leuven t)
(load-theme 'spacemacs-dark t)

;; Setting up smart mode line (complains if before the menu set theme
(setq sml/no-confirm-load-theme t)
; (setq sml/theme 'light)
(setq sml/theme 'dark)
(sml/setup)
