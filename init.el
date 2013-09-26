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
(setq org-mode-directory "~/org")
(setq org-mobile-directory "~/org/mobile")
(setq org-startup-indented t)
(setq tramp-default-method "ssh")
(setq inferior-lisp-program "/usr/bin/sbcl")
(setq recentf-max-menu-items 250)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq whitespace-style '(face empty tabs lines-tail trailing))

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(add-to-list 'load-path "~/.emacs.d/lisp")

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
(require 'autopair)
(require 'helm-config)
(require 'powerline)
(require 'uniquify)

(require 'my-functions)

(add-hook 'term-mode-hook
  #'(lambda () (setq autopair-dont-activate t)))

;(autopair-global-mode 1)
(recentf-mode 1)
(global-smart-tab-mode 1)
(column-number-mode 1)
(global-hl-line-mode 1)
(icomplete-mode 1)
(ido-mode 1)
;(global-whitespace-mode t)
(powerline-default-theme)

(push '("\\.ps1$" . powershell-mode) auto-mode-alist)
(push '("\\.asp$" . asp-mode) auto-mode-alist)

(global-set-key (kbd "<C-S-down>") 'move-line-down)
(global-set-key (kbd "<C-S-up>") 'move-line-up)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c g") 'org-iswitchb)
(global-set-key (kbd "C-c r") 'rotate-windows)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key [(control z)] 'create-shell)
(global-set-key [M-left] 'windmove-left)          ; move to left windnow
(global-set-key [M-right] 'windmove-right)        ; move to right window
(global-set-key [M-up] 'windmove-up)              ; move to upper window
(global-set-key [M-down] 'windmove-down)          ; move to downer window

(global-set-key (kbd "M-j")
            (lambda ()
                  (interactive)
                  (join-line -1)))

(global-set-key (kbd "C-S-n")
                (lambda ()
                  (interactive)
                  (ignore-errors (next-line 5))))

(global-set-key (kbd "C-S-p")
                (lambda ()
                  (interactive)
                  (ignore-errors (previous-line 5))))

(global-set-key (kbd "C-S-f")
                (lambda ()
                  (interactive)
                  (ignore-errors (forward-char 5))))

(global-set-key (kbd "C-S-b")
                (lambda ()
                  (interactive)
                  (ignore-errors (backward-char 5))))

(defun create-shell ()
    "creates a shell with a given name"
    (interactive);; "Prompt\n shell name:")
    (let ((shell-name (read-string "shell name: " nil)))
    (shell (concat "*" shell-name "*"))))

(defun my-irc ()
  (interactive)
  (erc :server "bounce.home.local" :port 6667 :nick "wizzo" :password "wizzo:wizzo"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "ProggyCleanTTSZ" :foundry "unknown" :slant normal :weight normal :height 120 :width normal)))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-mobile-files (quote (org-agenda-files "~/org/todo.org"))))
