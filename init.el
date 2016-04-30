;;;; wizzomafizzo

(setq inhibit-startup-message t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

;;; config and load package stuff
(add-to-list 'load-path "~/.emacs.d/lisp")

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
						 ("melpa" . "https://melpa.milkbox.net/packages/")))
(package-initialize)

(require 'my-functions)
(require 'helm-config)
(require 'slime-autoloads)
(require 'popwin)
(require 'smart-tab)
(require '6502-mode)

;;; looks

(custom-set-faces
 '(default ((t (:family "Source Code Pro" :height 120)))))
(load-theme 'material t)
(powerline-default-theme)

;;; core settings
(setq-default indicate-empty-lines t
			  fill-column 80
			  cursor-type 'bar
			  tab-width 4
			  save-place t)
(setq visible-bell t
	  ring-bell-function #'(lambda () (message "*ding*"))
	  backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
	  auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
	  tramp-auto-save-directory (concat user-emacs-directory "tramp-autosave")
	  ; jit-lock-defer-time 0.02
	  redisplay-dont-pause t
	  default-directory "~"
	  save-place-file (concat user-emacs-directory "places")
	  delete-by-moving-to-trash t
	  initial-scratch-message ";; ಠ_ಠ\n\n")
(put 'downcase-region 'disabled nil)
(fset 'yes-or-no-p 'y-or-n-p)
(add-hook 'before-save-hook #'(lambda () (delete-trailing-whitespace)))
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;;; mode settings
(setq comint-scroll-to-bottom-on-input 1
	  comint-scroll-show-maximum-output 1
	  jedi:setup-keys t
	  jedi:complete-on-dot t
	  org-startup-indented t
	  ; org-log-done t
	  tramp-default-method "ssh"
	  recentf-max-menu-items 250
	  ido-enable-flex-matching t
	  ido-everywhere t
	  inferior-lisp-program "sbcl"
	  slime-contribs '(slime-fancy)
	  nrepl-hide-special-buffers t
	  js2-highlight-level 3
	  powerline-default-separator 'contour
	  whitespace-line-column 80
	  ispell-local-dictionary "british"
	  projectile-mode-line "P"
	  elfeed-feeds '("https://news.ycombinator.com/rss")
	  c-default-style "linux"
	  c-basic-offset 8
	  magit-last-seen-setup-instructions "1.4.0"
	  js2-highlight-level 3
	  js2-basic-offset 4
	  web-mode-markup-indent-offset 2
	  asm-comment-char 35)

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
  (let ((home "C:/Users/callan"))
	(setq default-directory home
		  org-agenda-files (list (concat home "/org/todo.org")
								 (concat home "/org/finance.org"))
		  cider-lein-command (concat home "/bin/lein.bat")
		  lua-default-application (concat home "/bin/lua/bin/lua.exe")
		  python-shell-interpreter "C:/Python34/python.exe"
		  magit-git-executable "C:/Program Files (x86)/Git/bin/git.exe"
		  ispell-program-name "C:/Program Files (x86)/Aspell/bin/aspell.exe"
		  inferior-lisp-program "wx86cl"
		  package-check-signature nil ; no gpg installed
		  tramp-default-method "plink")
	(setenv "GIT_ASKPASS" "git-gui--askpass")
	(add-to-list 'exec-path "C:/Program Files (x86)/PuTTy")
	(add-to-list 'exec-path "C:/Program Files (x86)/Git/bin")
	(add-to-list 'exec-path "C:/Python34/Scripts")
	(add-to-list 'exec-path "C:/Python34")
	(add-to-list 'exec-path "C:/Program Files/Steel Bank Common Lisp (x86)/1.2.7")
	(add-to-list 'exec-path "C:/msys/1.0/bin")
	(add-to-list 'exec-path "C:/MinGW/bin")
	(add-to-list 'exec-path "C:/Rust/bin")
	(add-to-list 'exec-path (concat home "/bin/ccl"))
	(add-to-list 'exec-path (concat home "/bin/ctags58"))
	(add-to-list 'exec-path (concat home "/bin"))))

;;; keybindings
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
			 ("C-c SPC" . ace-jump-mode)
			 ("C-S-c C-S-c" . mc/edit-lines)
			 ("C-l" . whack-whitespace)
			 ("M-x" . smex)
			 ("M-X" . smex-major-mode-commands)
			 ("C-c M-x" . execute-extended-command)
			 ("C-S-<mouse-1>" . mc/add-cursor-on-click)
			 ("C-c u" . revert-buffer)
			 ("C-c a" . org-agenda)
			 ("C-c l" . org-store-link)
			 ("M-y" . browse-kill-ring)
			 ("C-c r" . reset-erc-track-mode)
			 ("C-c w" . whitespace-mode)
			 ("C-x F" . find-file-literally)
			 ("C-;" . comment-current-line)))
  (global-set-key (kbd (car x)) (cdr x)))

;; go away seconday selection
(global-unset-key (kbd "<M-drag-mouse-1>"))
(global-unset-key (kbd "<M-down-mouse-1>"))
(global-unset-key (kbd "<M-mouse-1>"))
(global-unset-key (kbd "<M-mouse-2>"))
(global-unset-key (kbd "<M-mouse-3>"))

;;; global modes
(let ((on '(show-paren-mode
			recentf-mode
			column-number-mode
			ido-mode
			global-yascroll-bar-mode
			global-git-gutter-mode
			delete-selection-mode
			popwin-mode
			window-numbering-mode
			electric-indent-mode
			global-auto-complete-mode
			global-smart-tab-mode
			yas-global-mode
			electric-pair-mode
			winner-mode
			projectile-global-mode
			beacon-mode))
	  (off '(which-function-mode))
	  (hide '(smart-tab-mode
			  auto-complete-mode)))
  (dolist (x on) (funcall x 1))
  (dolist (x off) (funcall x -1))
  (dolist (x hide) (diminish x)))

;;; mode hooks
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'rainbow-delimiters-mode)
(add-hook 'slime-repl-mode-hook 'enable-paredit-mode)

(add-hook 'slime-mode-hook 'set-up-slime-ac)

(add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'lisp-mode-hook 'enable-paredit-mode)

(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'rainbow-delimiters-mode)

(eval-after-load "auto-complete" '(add-to-list 'ac-modes
											   'slime-repl-mode
											   'cider-mode))

(eval-after-load 'paredit
  '(progn
     (define-key paredit-mode-map (kbd "M-{") 'paredit-wrap-curly)
	 (define-key paredit-mode-map (kbd "M-[") 'paredit-wrap-square)))

(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'cider-mode-hook 'enable-paredit-mode)
(add-hook 'cider-mode-hook 'rainbow-delimiters-mode)
(add-hook 'cider-mode-hook 'ac-flyspell-workaround)
(add-hook 'cider-mode-hook 'ac-cider-setup)

(add-hook 'cider-repl-mode-hook 'ac-cider-setup)
(add-hook 'cider-repl-mode-hook 'enable-paredit-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)

(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'my-paredit-nonlisp)
(add-hook 'js2-mode-hook 'flycheck-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)
(add-hook 'js2-mode-hook
		  (lambda ()
			(define-key js2-mode-map (kbd "C-c C-l") 'send-to-node-repl)
			(define-key js2-mode-map (kbd "C-c C-r") 'flycheck-clear)))
(skewer-setup)

(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'eldoc-mode)

(add-hook 'c-mode-hook 'semantic-mode)
(add-hook 'c-mode-hook 'semantic-idle-summary-mode)
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c-mode-hook 'flycheck-mode)
(add-hook 'c-mode-hook
		  (lambda ()
			(define-key c-mode-map [f5] 'compile)
			(define-key c-mode-map (kbd "C-c C-k") 'compile)
			(define-key c-mode-map (kbd "M-o") 'fa-show)))
(fa-config-default)

(add-hook 'c++-mode-hook 'helm-gtags-mode)

(add-hook 'asm-mode-hook 'helm-gtags-mode)

(add-hook 'rust-mode-hook
		  (lambda ()
			(define-key rust-mode-map (kbd "C-c C-k") 'rust-save-compile-and-run)))
(add-hook 'rust-mode-hook 'flycheck-mode)

(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(add-hook 'haskell-mode-hook
		  (lambda ()
			(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-load-file)
			(define-key haskell-mode-map (kbd "C-c C-n") 'haskell-navigate-imports)
			(define-key haskell-mode-map (kbd "C-c C-o") 'haskell-compile)))
(add-hook 'haskell-mode-hook #'hindent-mode)

;;; file association
(dolist (x '(("\\.ps1$" . powershell-mode)
			 ("\\.asp$" . asp-mode)
			 ("\\.js$" . js2-mode)
			 ("\\.html$" . web-mode)
			 ("\\.s65" . 6502-mode)))
  (add-to-list 'auto-mode-alist x))

;;; erc
(setq erc-autojoin-channels-alist '(("freenode.net" "#emacs"))
	  erc-current-nick-highlight-type 'nick
	  erc-keywords '("wizzo")
	  erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
								"324" "329" "332" "333" "353" "477")
	  erc-track-use-faces t
	  erc-timestamp-format-right "@ %H:%M")

(require 'erc-hl-nicks)
(erc-hl-nicks-mode 1)
(erc-services-mode 1)
(erc-spelling-mode 1)

(defun my-erc ()
  (interactive)
  (erc :server "irc.freenode.net" :port 6667 :nick "wizzo"))
