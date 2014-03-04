;; Callan

(setq inhibit-startup-message t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tooltip-mode -1)
(setq ns-command-modifier 'meta)
(fset 'yes-or-no-p 'y-or-n-p)
(setq transient-mark-mode t)
(show-paren-mode t)
(set-default 'indicate-empty-lines t)
(set-default 'fill-column 80)
(add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))
(setq visible-bell t)
(setq ring-bell-function (lambda () (message "*beep* *beep*")))
(setq-default cursor-type 'bar)
(setq default-directory "~/")
(setq-default tab-width 4)
(setq indent-tabs-mode nil)
(setq-default dired-listing-switches "-alhv")
(setq org-startup-indented t)
(setq tramp-default-method "ssh")
(setq recentf-max-menu-items 250)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)

(setq explicit-shell-file-name "/bin/bash")
(setq comint-scroll-to-bottom-on-input 1)
(setq comint-scroll-show-maximum-output 1)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(setq python-shell-interpreter "/usr/bin/ipython3")
(setq python-shell-interpreter-args "")

(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)
;(add-hook 'python-mode-hook 'flycheck-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-safe-themes (quote ("73fe242ddbaf2b985689e6ec12e29fab2ecd59f765453ad0e93bc502e6e478d6" "97a2b10275e3e5c67f46ddaac0ec7969aeb35068c03ec4157cf4887c401e74b1" "f3ceb7a30f6501c1093bc8ffdf755fe5ddff3a85437deebf3ee8d7bed8991711" "70cf411fbf9512a4da81aa1e87b064d3a3f0a47b19d7a4850578c8d64cac2353" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "4cb3034cbb7fd36bf0989fad19cac0beb818472854a7cbc8d2a597538b1f2cf0" "9cc993013dc78c24f44ea4a34e5f868f8674bbfe1501f12e233c5cb14cdd38df" default)))
 '(helm-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil))

(load "server")
(unless (server-running-p) (server-start))

(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(package-initialize)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
						 ("marmalade" . "http://marmalade-repo.org/packages/")
						 ("melpa" . "http://melpa.milkbox.net/packages/")))

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
;(require 'python-mode)
(require 'auto-complete)
(require 'pretty-mode)
(require 'popwin)
(require 'projectile)
(require 'minimap)

(require 'my-functions)

(load-theme 'inkpot)

;(autopair-global-mode 0)
(recentf-mode 1)
(global-smart-tab-mode 1)
(column-number-mode 1)
(global-hl-line-mode 1)
(icomplete-mode 1)
(ido-mode 1)
(powerline-default-theme)
(global-auto-complete-mode 1)
(global-yascroll-bar-mode 1)
(global-rainbow-delimiters-mode 1)
(electric-pair-mode 1)
(global-git-gutter-mode 1)
(helm-mode 0)
(global-pretty-mode 1)
(delete-selection-mode t)
(winner-mode 1)
(popwin-mode 1)
(window-numbering-mode 1)
(which-function-mode 1)

(push '("\\.ps1$" . powershell-mode) auto-mode-alist)
(push '("\\.asp$" . asp-mode) auto-mode-alist)

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c g") 'org-iswitchb)
(global-set-key (kbd "C-c f") 'projectile-find-file)
(global-set-key (kbd "C-c r") 'rotate-windows)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "C-c h") 'helm-projectile)
(global-set-key (kbd "C-c m") 'magit-status)
(global-set-key (kbd "C-c n") 'minimap-create)
(global-set-key (kbd "C-c N") 'minimap-kill)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key [(control z)] 'create-shell)
(global-set-key [M-left] 'windmove-left)
(global-set-key [M-right] 'windmove-right)
(global-set-key [M-up] 'windmove-up)
(global-set-key [M-down] 'windmove-down)
(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-c i") 'imenu)
(global-set-key "\C-l" 'whack-whitespace)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(add-hook 'python-mode-hook
          (lambda () (define-key python-mode-map (kbd "C-c c") (lambda ()
								(interactive)
								(python-shell-send-buffer)
								(python-shell-switch-to-shell)))))

(defun whack-whitespace ()
  "Delete all white space from point to the next word."
  (interactive nil)
  (when (re-search-forward "[ \t\n]+" nil t)
    (replace-match "" nil nil)))

(defun create-shell ()
    "creates a shell with a given name"
    (interactive);; "Prompt\n shell name:")
    (let ((shell-name (read-string "shell name: " nil)))
    (shell (concat "*" shell-name "*"))))

(defun ipython ()
  (interactive)
  (term "/usr/bin/ipython3"))

;; thx chee. idk what half this shit does
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

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Terminus" :foundry "xos4" :slant normal :weight normal :height 90 :width normal)))))
