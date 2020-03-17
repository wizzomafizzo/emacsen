;;;; zoop!

(setq inhibit-startup-message t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode 1)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/lib")

(require 'utils)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Fira Code Retina" :height 120)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (doom-themes solarized-theme indium prettier-js rjsx-mode shx use-package buffer-move web-mode tide typescript-mode flycheck yasnippet yascroll window-numbering smex projectile powerline paredit osx-trash magit helm git-gutter exec-path-from-shell dracula-theme diminish company beacon))))

(load-theme 'doom-vibrant t)

(powerline-default-theme)

(setq-default indicate-empty-lines t
              fill-column 80
              cursor-type 'bar
              indent-tabs-mode nil
              tab-width 4
              save-place t)

(setq visible-bell t
	  ring-bell-function #'(lambda () (message "*ding*"))
	  backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
	  auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
	  tramp-auto-save-directory (concat user-emacs-directory "tramp-autosave")
	  default-directory "~"
	  save-place-file (concat user-emacs-directory "places")
	  delete-by-moving-to-trash t
	  initial-scratch-message ";; ಠ_ಠ\n\n")

(fset 'yes-or-no-p 'y-or-n-p)

(add-hook 'before-save-hook #'(lambda () (delete-trailing-whitespace)))
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; mac stuff
(exec-path-from-shell-initialize)
(osx-trash-setup)
(mac-auto-operator-composition-mode)

(setq ns-command-modifier 'meta
      mac-mouse-wheel-smooth-scroll nil
      use-dialog-box nil
      python-shell-interpreter-args ""
      python-shell-interpreter "/usr/local/bin/python3"
      ispell-program-name "aspell")

;; keybindings
(dolist (x '(("C-c f" . projectile-find-file)
			 ("C-c p" . projectile-switch-project)
			 ("C-x b" . helm-mini)
			 ("C-x C-b" . helm-mini)
			 ("C-c h" . helm-projectile)
			 ("C-c m" . magit-status)
			 ("C-z" . create-shell)
			 ("M-<left>" . windmove-left)
			 ("M-<right>" . windmove-right)
			 ("M-<up>" . windmove-up)
			 ("M-<down>" . windmove-down)
			 ("<C-S-up>" . buf-move-up)
			 ("<C-S-down>" . buf-move-down)
			 ("<C-S-left>" . buf-move-left)
			 ("<C-S-right>" . buf-move-right)
			 ("C-l" . whack-whitespace)
			 ("M-x" . smex)
			 ("M-X" . smex-major-mode-commands)
			 ("C-c M-x" . execute-extended-command)
			 ("C-c u" . revert-buffer)
			 ("C-c a" . org-agenda)
			 ("C-c l" . org-store-link)
			 ("C-c w" . whitespace-mode)
			 ("C-x F" . find-file-literally)
			 ("C-;" . comment-current-line)
             ("C-M-f" . indent-buffer)))
  (global-set-key (kbd (car x)) (cdr x)))

;; disable seconday selection
(global-unset-key (kbd "<M-drag-mouse-1>"))
(global-unset-key (kbd "<M-down-mouse-1>"))
(global-unset-key (kbd "<M-mouse-1>"))
(global-unset-key (kbd "<M-mouse-2>"))
(global-unset-key (kbd "<M-mouse-3>"))

;; mode settings
(setq org-startup-indented t
	  tramp-default-method "ssh"
	  recentf-max-menu-items 50
	  ido-enable-flex-matching t
	  ido-everywhere t
	  inferior-lisp-program "sbcl"
	  powerline-default-separator 'contour
	  whitespace-line-column 80
	  ispell-local-dictionary "british"
	  projectile-mode-line-prefix "P"
      shell-file-name "/usr/local/bin/bash"
      company-tooltip-align-annotations t)

;; global modes
(let ((on '(show-paren-mode
			recentf-mode
			column-number-mode
			ido-mode
			global-git-gutter-mode
			delete-selection-mode
			window-numbering-mode
			electric-indent-mode
			electric-pair-mode
			projectile-global-mode
            global-hl-line-mode
            yas-global-mode
            global-company-mode
            shx-global-mode))
	  (off '())
	  (hide '(yas-minor-mode
              company-mode
              shx-mode)))
  (dolist (x on) (funcall x 1))
  (dolist (x off) (funcall x -1))
  (dolist (x hide) (diminish x)))


(defun setup-tide-mode ()
  (when (and (stringp buffer-file-name)
             (string-match "\\.[tj]sx?\\'" buffer-file-name))
    (tide-setup)
    (tide-hl-identifier-mode)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (company-mode +1)))

(add-hook 'js-mode-hook #'setup-tide-mode)
(add-hook 'before-save-hook 'tide-format-before-save)
(add-hook 'typescript-mode-hook #'setup-tide-mode)

(add-to-list 'auto-mode-alist '("\\.[tj]sx?\\'" . rjsx-mode))
