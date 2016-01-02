
;;;; οεδ

(defvar οεδ-mode-map (make-sparse-keymap))

(define-derived-mode οεδ-mode special-mode "οεδ-mode"
    "A todo manager for emacs"
    (define-key οεδ-mode-map "n" 'οεδ-next-line)
    (define-key οεδ-mode-map "p" 'οεδ-prev-line)
    (define-key οεδ-mode-map (kbd "space") 'οεδ-mark-task-complete)
    (define-key οεδ-mode-map (kbd "C-x C-s") 'οεδ-save-buffer)
    (define-key οεδ-mode-map "c" 'οεδ-new-todo))

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
    (setq-local *undone-items-overlay* (make-overlay 0 0 (current-buffer) nil t))
    (setq-local *done-items-overlay* (make-overlay 0 0 (current-buffer) nil t))
    
    (overlay-put *current-item-overlay* 'face 'current-item-face)
    (overlay-put *done-items-overlay* 'face 'completed-item-face)

    (add-hook 'write-contents-hooks 'οεδ-save-buffer)

    (redraw-todo-list)
    (set-buffer-modified-p nil))

(defun oed ()
    (interactive)
    (οεδ))

(defun is-yes (x)
    (cond ((eq x 'yes) t)
          ((eq x 'y) t)
          ((eq x 't) t)
          (t nil)))

;; todo entry accessors
(defun todo-title (x)
    (plist-get x :title))
(defun todo-complete-p (x)
    (is-yes (plist-get x :complete)))
(defun todo-subtasks (x)
    (plist-get x :subtasks))

(defun beginning-of-undone-tasks ()
    (overlay-start *undone-items-overlay*))
(defun beginning-of-done-tasks ()
    (overlay-start *done-items-overlay*))

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
    (goto-line-at-char (beginning-of-undone-tasks)))

(defun goto-line-at-char (line-pos)
    (goto-char line-pos)
    (goto-char (line-beginning-position))
    (move-overlay *current-item-overlay* (line-beginning-position) (1+ (line-end-position))))

(defun οεδ-next-line ()
    (interactive)
    (goto-line-at-char (+ (line-end-position) 1)))

(defun οεδ-prev-line ()
    (interactive)
    (goto-line-at-char (- (line-beginning-position) 1)))

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

;; Takes a prompt, an error prompt, and a function that returns
;; non-nil when it successfully parses an input or nil if not.
(defun try-get-input (prompt error-msg parse-function)
    (let ((input nil))
        (while (not input)
            (setf input (funcall parse-function (read-from-minibuffer prompt)))
            (when (not input)
                (minibuffer-message error-msg)
                (sit-for 3)))))

(defun οεδ-new-todo ()
    (interactive)
    (let* ((title (read-from-minibuffer "Title: "))
           (new-todo `(:title ,title :create-date ,(current-time-string) :complete no)))
        (setf *todo-list* (cons new-todo *todo-list*))
        (goto-char (beginning-of-undone-tasks))
        (let ((inhibit-read-only t))
            (draw-task new-todo))
        ;; If this is our only todo-task, we need to adjust the
        ;; overlays that cover the done and not done areas, so we just
        ;; redraw the whole thing.
        (when (eq (cdr *todo-list*) nil)
            (save-excursion
                (goto-char (point-min))
                (move-overlay *undone-items-overlay* 1 (line-end-position))
                (move-overlay *done-items-overlay* (+ 1 (line-end-position)) (overlay-end *done-items-overlay*)))))
    (goto-line-at-char (beginning-of-undone-tasks)))

(defvar already-saving nil)

(defun οεδ-refresh ()
    (sth))

(defun οεδ-save-buffer ()
    (interactive)
    (unless already-saving
        (if (buffer-modified-p)
            (save-excursion
                (let ((tmp-buf (generate-new-buffer " οεδ"))
                      (buf-name (buffer-name))
                      (file-name (buffer-file-name))
                      (todo *todo-list*)
                      (done *done-list*))
                    (set-buffer tmp-buf)
                    (set-visited-file-name file-name t)
                    (dolist (item todo)
                        (prin1 item tmp-buf)
                        (newline))
                    (dolist (item done)
                        (prin1 item tmp-buf)
                        (newline))
                    (save-buffer)
                    (set-buffer buf-name)
                    (kill-buffer tmp-buf)
                    (set-visited-file-modtime)))
            (message "(No changes need to be saved)"))
        (set-buffer-modified-p nil))
    t)
