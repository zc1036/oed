
;;;; οεδ

;(require 'doubly-linked-list)

(defvar οεδ-mode-map (make-sparse-keymap))

(define-derived-mode οεδ-mode special-mode "οεδ-mode"
    "A todo manager for emacs"
    (define-key οεδ-mode-map "n" 'οεδ-next-line)
    (define-key οεδ-mode-map "p" 'οεδ-prev-line)
    (define-key οεδ-mode-map (kbd "SPC") 'οεδ-mark-todo-done)
    (define-key οεδ-mode-map (kbd "d") 'οεδ-mark-todo-for-delete)
    (define-key οεδ-mode-map (kbd "u") 'οεδ-unmark-todo-for-delete)
    (define-key οεδ-mode-map (kbd "x") 'οεδ-prompt-execute-delete)
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

;; todo entry accessors
(defun todo-title (x)
    (plist-get x :title))
(defun todo-complete-p (x)
    (is-yes (plist-get x :complete)))
(defun todo-subtodos (x)
    (plist-get x :subtodos))
(defun todo-disabled-p (x)
    (plist-get x :disabled))

(defun todo-read-callback (todo)
    (plist-put todo :subtodos (make-dll-from-proper-list-callback (plist-get todo :subtodos) 'todo-read-callback)))

(defun οεδ ()
    (interactive)

    (οεδ-mode)

    (let ((lists (read-buffer-into-todo-list)))
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

(defun beginning-of-undone-todos ()
    (overlay-start *undone-items-overlay*))
(defun beginning-of-done-todos ()
    (overlay-start *done-items-overlay*))

(defvar drawtodo-indent-level 0)
(defvar drawtodo-indent-amount 2)

(defun draw-todo (todo-node)
    (let ((todo (dll-node-data todo-node)))
        (when (not (todo-disabled-p todo))
            (let ((line-start-pos (line-beginning-position))
                  (indent-string (make-string drawtodo-indent-level ?\s)))
                (insert indent-string)
                (insert (todo-title todo))
                (put-text-property line-start-pos (point) 'οεδ-todo-node-prop todo-node)
                (newline)
                (when (todo-subtodos todo)
                    (let ((drawtodo-indent-level (+ drawtodo-indent-amount drawtodo-indent-level)))
                        (draw-subtodos (todo-subtodos todo))))))))

(defun draw-subtodos (subtodos)
    (dll-mapcdr subtodos 'draw-todo))

(defun redraw-todo-list ()
    (let ((inhibit-read-only t) (end-todo-section))
        (erase-buffer)
        (dll-mapcdr *todo-list* 'draw-todo)
        (move-overlay *undone-items-overlay* 1 (point))
        (setf end-todo-section (point))
        (dll-mapcdr *done-list* 'draw-todo)
        (move-overlay *done-items-overlay* end-todo-section (point)))
    (goto-line-at-char (beginning-of-undone-todos)))

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

(defun read-buffer-into-todo-list ()
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
                 (list (make-dll-from-proper-list-callback (nreverse todo-list) 'todo-read-callback)
                       (make-dll-from-proper-list-callback (nreverse done-list) 'todo-read-callback)))))))

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
           (new-todo (list :title title :create-date (current-time-string) :complete 'no)))
        (dll-insert-after *todo-list* new-todo)
        (goto-char (beginning-of-undone-todos))
        (let ((inhibit-read-only t))
            (draw-todo new-todo))
        ;; If this is our only todo, we need to adjust the
        ;; overlays that cover the done and not done areas, so we just
        ;; redraw the whole thing.
        (when (eq (cdr *todo-list*) nil)
            (goto-char (point-min))
            (move-overlay *undone-items-overlay* 1 (line-end-position))
            (move-overlay *done-items-overlay* (+ 1 (line-end-position)) (overlay-end *done-items-overlay*))))
    (goto-line-at-char (beginning-of-undone-todos)))

(defun οεδ-delete-todo ()
    (interactive)
    (let ((todo-node-at-point (get-char-property (point) 'οεδ-todo-node-prop)))
        (when todo-node-at-point
            (plist-put (dll-node-data todo-node-at-point) :disabled t)
            (dll-remove-node todo-node-at-point))))

(defun οεδ-mark-todo-done ()
    (interactive)
    (let* ((todo-node-at-point (get-char-property (point) 'οεδ-todo-node-prop))
           (todo-at-point (and todo-node-at-point (dll-node-data todo-node-at-point)))
           (inhibit-read-only t))
        (when (and todo-at-point (not (is-yes (plist-get todo-at-point :complete))))
            (save-excursion
                (plist-put todo-at-point :complete 'yes)
                (dll-remove-node todo-node-at-point)
                (dll-insert-node-before *done-list* todo-node-at-point)
                (kill-region (line-beginning-position) (+ 1 (line-end-position)))
                (goto-char (beginning-of-done-todos))
                (yank))
            (goto-line-at-char (point))))) ;; just refresh the point

(defvar already-saving nil)

(defun οεδ-refresh ()
    (sth))

(defun write-todo (todo)
    (unless (todo-disabled-p todo)
        (prin1 todo (current-buffer))
        (newline)))

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
                    (dll-mapcar 'write-todo todo)
                    (dll-mapcar 'write-todo done)
                    (save-buffer)
                    (set-buffer buf-name)
                    (kill-buffer tmp-buf)
                    (set-visited-file-modtime)))
            (message "(No changes need to be saved)"))
        (set-buffer-modified-p nil))
    t)
