;; Callan

(setq inhibit-startup-message t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
;(menu-bar-mode -1)
(tooltip-mode -1)
(setq ns-command-modifier 'meta)
(fset 'yes-or-no-p 'y-or-n-p)
(setq transient-mark-mode t)

(show-paren-mode t)
(set-default 'indicate-empty-lines t)
(set-default 'fill-column 80)

(add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))

(setq visible-bell t)
(setq ring-bell-function (lambda () (message "*beep*")))

(setq-default cursor-type 'bar)
(setq default-directory "~/")
(setq-default tab-width 4)
(setq indent-tabs-mode nil)
(setq delete-by-moving-to-trash t)

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
;(require 'magit)
(require 'autopair)
;(require 'slime)
(require 'deft)
(require 'anything)
(require 'zencoding-mode)

(require 'color-theme-tomorrow)
(color-theme-tomorrow--define-theme night)

(require 'my-functions)

(setq-default dired-listing-switches "-alhv")
(setq ord-mode-directory "~/Org")
(setq org-startup-indented t)
(setq deft-directory "~/Org")
(setq deft-extension "org")
(setq deft-text-mode 'org-mode)
(setq tramp-default-method "ssh")
(setq inferior-lisp-program "/usr/bin/sbcl")

(add-hook 'term-mode-hook
  #'(lambda () (setq autopair-dont-activate t)))

;(slime-setup)
(autopair-global-mode 1)
(recentf-mode 1)
(global-smart-tab-mode 1)
(column-number-mode 1)
(global-hl-line-mode 1)
(icomplete-mode 1)
(ido-mode 1)

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)

(setq whitespace-style '(face empty tabs lines-tail trailing))
;(global-whitespace-mode t)

(push '("\\.ps1$" . powershell-mode) auto-mode-alist)
(push '("\\.asp$" . asp-mode) auto-mode-alist)

(global-set-key (kbd "<C-S-down>") 'move-line-down)
(global-set-key (kbd "<C-S-up>") 'move-line-up)
(global-set-key (kbd "C-x g") 'webjump)
(global-set-key (kbd "C-c g") 'org-iswitchb)
(global-set-key (kbd "<f11>") 'anything)
(global-set-key (kbd "<f12>") 'deft)
(global-set-key [(control z)] 'shell)
(global-set-key (kbd "C-c l") 'org-store-link)

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

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Inconsolata" :slant normal :weight normal :height 150 :width normal)))))
