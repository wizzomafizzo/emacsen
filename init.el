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
(require 'uniquify)

(load-theme 'doom-dracula t)
(powerline-default-theme)

;; some sane defaults
(setq-default indicate-empty-lines t
              fill-column 80
              cursor-type 'bar
              indent-tabs-mode nil
              tab-width 4)

(setq visible-bell t
      ring-bell-function #'(lambda () (message "*ding*"))
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      tramp-auto-save-directory (concat user-emacs-directory "tramp-autosave")
      default-directory "~"
      save-place-file (concat user-emacs-directory "places")
      delete-by-moving-to-trash t
      uniquify-buffer-name-style 'forward
      initial-scratch-message ";; ಠ_ಠ\n\n")

(fset 'yes-or-no-p 'y-or-n-p)

;; mac stuff
(when (eq system-type 'darwin)
  (menu-bar-mode 1)
  (exec-path-from-shell-initialize)
  (osx-trash-setup)
  (setq ns-command-modifier 'meta
	    flymake-gui-warnings-enabled nil
	    use-dialog-box nil
	    python-shell-interpreter-args ""
	    python-shell-interpreter "/usr/local/bin/python3"
	    ispell-program-name "aspell"))

;; windows stuff
(when (eq system-type 'windows-nt)
  (menu-bar-mode 1)
  (let ((home "C:/Users/calla"))
    (setq default-directory home
	      org-agenda-files (list (concat home "/org/todo.org"))
	      python-shell-interpreter "C:/Python39/python.exe"
	      magit-git-executable "C:/Program Files (x86)/Git/bin/git.exe"
	      ispell-program-name "C:/Program Files (x86)/Aspell/bin/aspell.exe"
	      package-check-signature nil ; no gpg installed
	      tramp-default-method "plink")
    (setenv "GIT_ASKPASS" "git-gui--askpass")
    (add-to-list 'exec-path "C:/Program Files (x86)/PuTTy")
    (add-to-list 'exec-path "C:/Program Files/Git/bin")
    (add-to-list 'exec-path "c:/Python39/Scripts")
    (add-to-list 'exec-path "c:/Python39")
    (add-to-list 'exec-path (concat home "/bin"))))

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
             ("C-M-f" . indent-buffer)
	         ("M-/" . hippie-expand)
	         ("M-z" . zap-to-char)
	         ("C-s" . isearch-forward-regexp)
	         ("C-r" . isearch-backward-regexp)
	         ("C-M-s" . isearch-forward)
	         ("C-M-r" . isearch-backward)))
  (global-set-key (kbd (car x)) (cdr x)))

;; disable seconday selection
(global-unset-key (kbd "<M-drag-mouse-1>"))
(global-unset-key (kbd "<M-down-mouse-1>"))
(global-unset-key (kbd "<M-mouse-1>"))
(global-unset-key (kbd "<M-mouse-2>"))
(global-unset-key (kbd "<M-mouse-3>"))

;; mode settings
(setq org-startup-indented t
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
            shx-global-mode
            save-place-mode))
      (off '())
      (hide '(yas-minor-mode
              company-mode
              shx-mode)))
  (dolist (x on) (funcall x 1))
  (dolist (x off) (funcall x -1))
  (dolist (x hide) (diminish x)))
