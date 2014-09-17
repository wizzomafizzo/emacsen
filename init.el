;;;; wizzomafizzo

;; don't want a delay for gui stuff
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode 1)

(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "~/.emacs.d/slime")
(add-to-list 'load-path "~/.emacs.d/helm")

(require 'my-functions)

;; config and load package stuff
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
						 ("marmalade" . "http://marmalade-repo.org/packages/")
						 ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

(require 'csv-mode)
(require 'asp-mode)
(require 'powershell-mode)
(require 'helm-config)
(require 'auto-complete)
(require 'popwin)
(require 'slime-autoloads)

(load-theme 'monokai t)
(custom-set-faces '(default ((t (:height 100 :family "Droid Sans Mono")))))

;; mac stuff
(when (eq system-type 'darwin)
  (exec-path-from-shell-initialize)
  (setq ns-command-modifier 'meta)
  (setq flymake-gui-warnings-enabled nil))

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
;(setq jit-lock-defer-time 0.02)
(setq redisplay-dont-pause t)

;;; custom keybindings
(global-set-key (kbd "C-c f") 'projectile-find-file)
(global-set-key (kbd "C-c p") 'projectile-switch-project)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-b") 'helm-mini)
(global-set-key (kbd "C-c h") 'helm-projectile)
(global-set-key (kbd "C-c m") 'magit-status)
(global-set-key (kbd "C-x d") 'diredp-dired-files)
(global-set-key (kbd "C-z") 'create-shell)
(global-set-key (kbd "M-<left>") 'windmove-left)
(global-set-key (kbd "M-<right>") 'windmove-right)
(global-set-key (kbd "M-<up>") 'windmove-up)
(global-set-key (kbd "M-<down>") 'windmove-down)
(global-set-key (kbd "<C-S-up>") 'buf-move-up)
(global-set-key (kbd "<C-S-down>") 'buf-move-down)
(global-set-key (kbd "<C-S-left>") 'buf-move-left)
(global-set-key (kbd "<C-S-right>") 'buf-move-right)
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-c i") 'imenu)
(global-set-key (kbd "C-l") 'whack-whitespace)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c M-x") 'execute-extended-command)
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)
(global-set-key (kbd "C-c u") 'revert-buffer)

;;; mode settings
;; shell
(setq comint-scroll-to-bottom-on-input 1)
(setq comint-scroll-show-maximum-output 1)
;; python
(setq python-shell-interpreter "/usr/local/bin/python3")
(setq python-shell-interpreter-args "")
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)
;; org-mode
(setq org-startup-indented t)
;; tramp
(setq tramp-default-method "ssh")
;; recentf
(setq recentf-max-menu-items 250)
;; ido
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
;; common lisp
(setq inferior-lisp-program "sbcl")
(setq slime-contribs '(slime-fancy slime-autodoc))
;; clojure
(setq nrepl-hide-special-buffers t)
;; js
(setq js2-highlight-level 3)
;; whitespace
(setq whitespace-style '(face empty tabs lines-tail trailing))

;;; enable global modes
(show-paren-mode 1)
(recentf-mode 1)
(column-number-mode 1)
(ido-mode 1)
(global-auto-complete-mode 1)
(global-yascroll-bar-mode 1)
(electric-pair-mode 1)
(global-git-gutter-mode 1)
(delete-selection-mode 1)
(winner-mode 1)
(popwin-mode 1)
(window-numbering-mode 1)
(which-function-mode 1)
(helm-mode 1)
(global-whitespace-mode 1)

;;; mode hooks
(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook #'eldoc-mode)

(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)

(add-hook 'lisp-mode-hook #'disable-eldoc-mode)
(add-hook 'lisp-mode-hook #'slime-autodoc-mode)
(add-hook 'lisp-mode-hook #'rainbow-delimiters-mode)
(add-hook 'lisp-mode-hook #'enable-paredit-mode)

(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'rainbow-delimiters-mode)

(add-hook 'scheme-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook #'rainbow-delimiters-mode)

(add-hook 'cider-mode-hook #'cider-turn-on-eldoc-mode)
(add-hook 'cider-mode-hook #'enable-paredit-mode)
(add-hook 'cider-mode-hook #'rainbow-delimiters-mode)

(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)

(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'python-mode-hook
          (lambda () (define-key python-mode-map (kbd "C-c c")
				  (lambda () (interactive) (python-shell-send-buffer)
					(python-shell-switch-to-shell)))))

;;; file association
(add-to-list 'auto-mode-alist '("\\.ps1$" . powershell-mode))
(add-to-list 'auto-mode-alist '("\\.asp$" . asp-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;;; natural scrolling
(setq mouse-wheel-scroll-amount '(4 ((shift) . t)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 't)
(global-set-key [wheel-down] (lambda () (interactive) (scroll-up-command 1)))
(global-set-key [wheel-up] (lambda () (interactive) (scroll-down-command 1)))
(global-set-key [double-wheel-down] (lambda () (interactive) (scroll-up-command 2)))
(global-set-key [double-wheel-up] (lambda () (interactive) (scroll-down-command 2)))
(global-set-key [triple-wheel-down] (lambda () (interactive) (scroll-up-command 3)))
(global-set-key [triple-wheel-up] (lambda () (interactive) (scroll-down-command 3)))
