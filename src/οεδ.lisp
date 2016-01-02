
;;;; οεδ

(defvar οεδ-mode-map (make-sparse-keymap))

(define-derived-mode οεδ-mode special-mode "οεδ-mode"
    "A todo manager for emacs"
    (define-key οεδ-mode-map "n" 'οεδ-next-line)
    (define-key οεδ-mode-map "p" 'οεδ-prev-line)
    (define-key οεδ-mode-map (kbd "C-x C-s") 'οεδ-save-list))

(defface current-item-face
    '((((class color) (background dark))
       :background "grey20")
      (((class color) (background light))
       :background "grey95"))
    "Face for the current todo item."
    :group 'basic-faces)

(defun οεδ ()
    (interactive)

    (οεδ-mode)

    (setq-local *todo-list* (read-buffer-into-task-list))
    (setq-local *current-item-overlay* (make-overlay 0 0))

    (overlay-put *current-item-overlay* 'face 'current-item-face)

    (add-hook 'write-contents-hooks 'οεδ-save-list)

    (redraw-todo-list))

(defun oed ()
    (interactive)
    (οεδ))

(defun todo-title (x)
    (plist-get x 'title))
(defun todo-complete-p (x)
    (plist-get x 'complete))

(defun is-yes (x)
    (cond ((eq x 'yes) t)
          ((eq x 't) t)
          (t nil)))

(defun redraw-todo-list ()
    (let ((inhibit-read-only t))
        (erase-buffer)
        (dolist (item *todo-list*)
            (if (is-yes (todo-complete-p item))
                    (insert "☑")
                (insert "☐"))
            (insert " ")
            (insert (format "%s" (todo-title item)))
            (newline)))

    (goto-line-char (point-min)))

(defun goto-line-char (line-pos)
    (goto-char line-pos)
    (goto-char (line-beginning-position))
    (move-overlay *current-item-overlay* (line-beginning-position) (1+ (line-end-position))))

(defun οεδ-next-line ()
    (interactive)
    (goto-line-char (+ (line-end-position) 1)))

(defun οεδ-prev-line ()
    (interactive)
    (goto-line-char (- (line-beginning-position) 1)))

(defun read-buffer-into-task-list ()
    (let ((old-point (point)) (forms nil))
        (goto-char (point-min))
        (condition-case err
                (loop do (setf forms (cons (read (current-buffer)) forms)))
            (end-of-file
             (goto-char old-point)
             (nreverse forms)))))

(defvar already-saving nil)

(defun οεδ-save-list ()
    (interactive)
    (unless already-saving
        (if (buffer-modified-p)
            (save-excursion
                (let ((tmp-buf (generate-new-buffer " οεδ"))
                      (buf-name (buffer-name))
                      (file-name (buffer-file-name))
                      (todo *todo-list*))
                    (set-buffer tmp-buf)
                    (set-visited-file-name file-name t)
                    (dolist (item todo)
                        (prin1 item tmp-buf)
                        (newline))
                    (save-buffer)
                    (set-buffer buf-name)
                    (kill-buffer tmp-buf)))
            (message "(No changes need to be saved)"))
        (set-buffer-modified-p nil))
    t)
