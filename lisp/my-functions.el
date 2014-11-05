;;;; wizzomafizzo: custom functions

(defun scratch-lisp-file ()
  "Insert a template (with DEFPACKAGE and IN-PACKAGE forms) into
  the current buffer."
  (interactive)
  (goto-char 0)
  (let* ((file (file-name-nondirectory (buffer-file-name)))
         (package (file-name-sans-extension file)))
    (insert ";;;; " file "\n")
    (insert "\n(defpackage :" package "\n  (:use :cl))\n\n")
    (insert "(in-package :" package ")\n\n")))

;;; try disable modal dialogs everywhere
;;; to workaround crashing on mac
(defadvice yes-or-no-p (around prevent-dialog activate)
  "Prevent yes-or-no-p from activating a dialog"
  (let ((use-dialog-box nil))
    ad-do-it))
(defadvice y-or-n-p (around prevent-dialog-yorn activate)
  "Prevent y-or-n-p from activating a dialog"
  (let ((use-dialog-box nil))
    ad-do-it))

(defun rotate-windows ()
  "Rotate your windows"
  (interactive)
  (cond ((not (> (count-windows)1))
         (message "You can't rotate a single window!"))
        (t
         (setq i 1)
         (setq numWindows (count-windows))
         (while  (< i numWindows)
           (let* (
                  (w1 (elt (window-list) i))
                  (w2 (elt (window-list) (+ (% i numWindows) 1)))

                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))

                  (s1 (window-start w1))
                  (s2 (window-start w2))
                  )
             (set-window-buffer w1  b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1)
             (setq i (1+ i)))))))

(defun handle-fish-path ()
  (when (file-name-nondirectory (getenv "SHELL")) "fish"
		(setq path-separator " ")
		(exec-path-from-shell-initialize)
		(setq path-separator ":")))

(defun backup-config ()
  "Backup emacs configuration to home drive"
  (interactive)
  (message "Backing up configuration...")
  (copy-file "~/.emacs.d/init.el" "~/backup" t)
  (copy-directory "~/.emacs.d/lisp" "~/backup")
  (message "Complete!"))

(defun move-line-down ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines 1))
    (forward-line)
    (move-to-column col)))

(defun move-line-up ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines -1))
    (move-to-column col)))

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

(defun unpop-to-mark-command ()
  "Unpop off mark ring. Does nothing if mark ring is empty."
  (interactive)
  (when mark-ring
	(let ((pos (marker-position (car (last mark-ring)))))
	  (if (not (= (point) pos))
		  (goto-char pos)
		(setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
		(set-marker (mark-marker) pos)
		(setq mark-ring (nbutlast mark-ring))
		(goto-char (marker-position (car (last mark-ring))))))))

(provide 'my-functions)
