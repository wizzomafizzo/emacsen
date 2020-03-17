(defun comment-current-line ()
  (interactive)
  (comment-or-uncomment-region (line-beginning-position)
							   (line-end-position)))

(defun whack-whitespace ()
  (interactive nil)
  (when (re-search-forward "[ \t\n]+" nil t)
    (replace-match "" nil nil)))

(defun create-shell ()
  (interactive)
  (let ((shell-name (read-string "shell name: " nil)))
    (shell (concat "*" shell-name "*"))))

(defun dos2unix ()
  (interactive)
  (set-buffer-file-coding-system 'unix 't))

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

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(provide 'utils)
