
;;;; οεδ

(defvar οεδ-mode-map (make-sparse-keymap))

(define-derived-mode οεδ-mode special-mode "οεδ-mode"
    "A todo manager for emacs"
    (define-key οεδ-mode-map "n" 'οεδ-next-line)
    (define-key οεδ-mode-map "p" 'οεδ-prev-line)
    (define-key οεδ-mode-map (kbd "space") 'οεδ-mark-task-complete)
    (define-key οεδ-mode-map (kbd "C-x C-s") 'οεδ-save-list))

(defface current-item-face
    '((((class color) (background dark))
       :background "grey20")
      (((class color) (background light))
       :background "grey95"))
    "Face for the current todo item."
    :group 'οεδ-faces)

(defface completed-item-face
    '((((class color) (background dark))
       :foreground "grey60" :italic t)
      (((class color) (background dark))
       :foreground "grey40" :italic t)
      (t :italic t))
    "Face for the current todo item."
    :group 'οεδ-faces)

(defun οεδ ()
    (interactive)

    (οεδ-mode)

    (let ((lists (read-buffer-into-task-list)))
        (setq-local *todo-list* (car lists))
        (setq-local *done-list* (cadr lists)))

    (setq-local *current-item-overlay* (make-overlay 0 0))
    (setq-local *undone-items-overlay* (make-overlay 0 0))
    (setq-local *done-items-overlay* (make-overlay 0 0))
    
    (overlay-put *current-item-overlay* 'face 'current-item-face)
    (overlay-put *done-items-overlay* 'face 'completed-item-face)

    (add-hook 'write-contents-hooks 'οεδ-save-list)

    (redraw-todo-list)
    (set-buffer-modified-p nil))

(defun oed ()
    (interactive)
    (οεδ))

(defun is-yes (x)
    (cond ((eq x 'yes) t)
          ((eq x 't) t)
          (t nil)))

;; todo entry accessors
(defun todo-title (x)
    (plist-get x :title))
(defun todo-complete-p (x)
    (is-yes (plist-get x :complete)))
(defun todo-subtasks (x)
    (plist-get x :subtasks))

(defvar drawtask-indent-level 0)
(defvar drawtask-indent-amount 2)

(defun draw-task (task)
    (insert (todo-title task))
    (newline)
    (when (todo-subtasks task)
        (let ((drawtask-indent-level (+ drawtask-indent-amount drawtask-indent-level)))
            (draw-subtasks (todo-subtasks task)))))

(defun draw-subtasks (subtasks)
    (let ((indent-string (make-string drawtask-indent-level ?\s)))
        (dolist (task subtasks)
            (insert indent-string)
            (draw-task task))))

(defun redraw-todo-list ()
    (let ((inhibit-read-only t) (end-todo-section))
        (erase-buffer)
        (dolist (item *todo-list*)
            (draw-task item))
        (move-overlay *undone-items-overlay* 1 (point))
        (setf end-todo-section (point))
        (dolist (item *done-list*)
            (draw-task item))
        (move-overlay *done-items-overlay* end-todo-section (point)))
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
    (let ((old-point (point))
          (done-list nil)
          (todo-list nil))
        (save-excursion
            (goto-char (point-min))
            (condition-case err
                    (loop do (let ((item (read (current-buffer))))
                                 (if (todo-complete-p item)
                                         (setf done-list (cons item done-list))
                                     (setf todo-list (cons item todo-list)))))
                (end-of-file
                 (list (nreverse todo-list)
                       (nreverse done-list)))))))

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
