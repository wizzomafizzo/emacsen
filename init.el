;;;; wizzomafizzo

;; don't want a delay for gui stuff
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode 1)

;; some custom functions
(add-to-list 'load-path "~/.emacs.d/lisp")
(require 'my-functions)

;; start emacs server
(load "server")
(unless (server-running-p) (server-start))

;; config and load package stuff
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
						 ("marmalade" . "http://marmalade-repo.org/packages/")
						 ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

;; FIXME: are all these really necessary?
(require 'smartparens-config)
(require 'recentf)
(require 'color-theme)
(require 'csv-mode)
(require 'asp-mode)
(require 'powershell-mode)
(require 'smart-tab)
(require 'magit)
(require 'helm-config)
(require 'powerline)
(require 'uniquify)
(require 'auto-complete)
;(require 'pretty-mode)
(require 'popwin)
(require 'projectile)
;(require 'minimap)

(load-theme 'base16-default)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 100 :family "Droid Sans Mono")))))

;; mac stuff
(when (eq system-type 'darwin)
  (exec-path-from-shell-initialize)
  (setq ns-command-modifier 'meta)
  (setq flymake-gui-warnings-enabled nil)) ; fix bug on mac

;; core emacs settings
(add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))
(fset 'yes-or-no-p 'y-or-n-p)
(setq-default indicate-empty-lines t)
(setq-default fill-column 80)
(setq-default cursor-type 'bar)
(setq-default tab-width 4)
(setq visible-bell t)
(setq ring-bell-function (lambda () (message "*ding*")))
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
(setq jit-lock-defer-time 0.05)

;;; custom keybindings
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c g") 'org-iswitchb)
(global-set-key (kbd "C-c f") 'projectile-find-file)
(global-set-key (kbd "C-c p") 'projectile-switch-project)
(global-set-key (kbd "C-c r") 'rotate-windows)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "C-c h") 'helm-projectile)
(global-set-key (kbd "C-c m") 'magit-status)
(global-set-key (kbd "C-c n") 'minimap-create)
(global-set-key (kbd "C-c N") 'minimap-kill)
(global-set-key (kbd "C-x d") 'diredp-dired-files)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key [(control z)] 'create-shell)
(global-set-key [M-left] 'windmove-left)
(global-set-key [M-right] 'windmove-right)
(global-set-key [M-up] 'windmove-up)
(global-set-key [M-down] 'windmove-down)
(global-set-key (kbd "<C-S-up>") 'buf-move-up)
(global-set-key (kbd "<C-S-down>") 'buf-move-down)
(global-set-key (kbd "<C-S-left>") 'buf-move-left)
(global-set-key (kbd "<C-S-right>") 'buf-move-right)
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-c i") 'imenu)
(global-set-key "\C-l" 'whack-whitespace)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;;; mode settings
;; shell
(setq comint-scroll-to-bottom-on-input 1)
(setq comint-scroll-show-maximum-output 1)
;; python
(setq python-shell-interpreter "/usr/local/bin/python3")
(setq python-shell-interpreter-args "")
;(setq jedi:server-command '("python3 jediepcserver.py"))
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)
(add-hook 'python-mode-hook
          (lambda () (define-key python-mode-map (kbd "C-c c")
				  (lambda () (interactive) (python-shell-send-buffer)
					(python-shell-switch-to-shell)))))
;; dired
(setq-default dired-listing-switches "-alhv")
;; org-mode
(setq org-startup-indented t)
;; tramp
(setq tramp-default-method "ssh")
;; recentf
(setq recentf-max-menu-items 250)
;; ido
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
;; slime
(setq inferior-lisp-program "sbcl")
;; clojure
(setq nrepl-hide-special-buffers t)

;; enable global modes
(show-paren-mode t)
(recentf-mode 1)
(global-smart-tab-mode 1)
(column-number-mode 1)
(icomplete-mode 1)
(ido-mode 1)
(powerline-default-theme)
(global-auto-complete-mode 1)
(global-yascroll-bar-mode 1)
(global-rainbow-delimiters-mode 1)
(electric-pair-mode 1)
(global-git-gutter-mode 1)
;(global-pretty-mode 1)
(delete-selection-mode t)
(winner-mode 1)
(popwin-mode 1)
(window-numbering-mode 1)
(which-function-mode 1)

;; mode hooks
(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook #'enable-paredit-mode)
(add-hook 'lisp-mode-hook #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook #'enable-paredit-mode)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'cider-mode-hook 'enable-paredit-mode)
;; file association
(push '("\\.ps1$" . powershell-mode) auto-mode-alist)
(push '("\\.asp$" . asp-mode) auto-mode-alist)

;; "natural scrolling"
(setq mouse-wheel-scroll-amount '(4 ((shift) . t)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 't)
(unless (and (boundp 'chee/mouse-wheel-smooth-scroll) chee/mouse-wheel-smooth-scroll)
  (global-set-key [wheel-down] (lambda () (interactive) (scroll-up-command 1)))
  (global-set-key [wheel-up] (lambda () (interactive) (scroll-down-command 1)))
  (global-set-key [double-wheel-down] (lambda () (interactive) (scroll-up-command 2)))
  (global-set-key [double-wheel-up] (lambda () (interactive) (scroll-down-command 2)))
  (global-set-key [triple-wheel-down] (lambda () (interactive) (scroll-up-command 3)))
  (global-set-key [triple-wheel-up] (lambda () (interactive) (scroll-down-command 3))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-safe-themes (quote ("1affe85e8ae2667fb571fc8331e1e12840746dae5c46112d5abb0c3a973f5f5a" default)))
 '(show-paren-mode t)
 '(tool-bar-mode nil))
