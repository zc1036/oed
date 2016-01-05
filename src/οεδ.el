
;;;; οεδ

;(require 'doubly-linked-list)

(defvar οεδ-mode-map (make-sparse-keymap))

(define-derived-mode οεδ-mode special-mode "οεδ-mode"
    "A todo manager for emacs"
    (define-key οεδ-mode-map "n" 'οεδ-next-line)
    (define-key οεδ-mode-map "p" 'οεδ-prev-line)
    (define-key οεδ-mode-map (kbd "u") 'οεδ-mark-todo-undone)
    (define-key οεδ-mode-map (kbd "SPC") 'οεδ-mark-todo-done)
    (define-key οεδ-mode-map (kbd "d") 'οεδ-mark-todo-for-delete)
    (define-key οεδ-mode-map (kbd "x") 'οεδ-prompt-execute-delete)
    (define-key οεδ-mode-map (kbd "C-x C-s") 'οεδ-save-buffer)
    (define-key οεδ-mode-map "c" 'οεδ-new-todo))

(defface current-item-face
    '((((class color) (background dark))
       :background "grey20")
      (((class color) (background light))
       :background "grey95"))
    "Face for the current todo item's title."
    :group 'οεδ-faces)

(defface current-item-bg-face
    '((((class color) (background dark))
       :background "grey30")
      (((class color) (background light))
       :background "grey85"))
    "Face for the current todo item's background."
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
(defun set-todo-complete (x complete-p)
    (if complete-p
            (plist-put x :complete 'yes)
        (plist-put x :complete 'no)))
(defun todo-subtodos (x)
    (plist-get x :subtodos))
(defun set-todo-subtodos (x subs)
    (plist-put x :subtodos subs))
(defun todo-disabled-p (x)
    (plist-get x :disabled))
(defun todo-notes (x)
    (plist-get x :notes))

(defun οεδ ()
    (interactive)

    (οεδ-mode)

    (let ((lists (read-buffer-into-todo-list)))
        (setq-local *todo-list* (car lists))
        (setq-local *done-list* (cadr lists)))

    (setq-local *current-item-title-overlay* (make-overlay 0 0))
    (setq-local *current-item-bg-overlay* (make-overlay 0 0))
    (setq-local *undone-items-overlay* (make-overlay 0 0 (current-buffer) nil t))
    (setq-local *done-items-overlay* (make-overlay 0 0 (current-buffer) nil t))
    
    (overlay-put *current-item-title-overlay* 'face 'current-item-face)
    (overlay-put *current-item-bg-overlay* 'face 'current-item-bg-face)
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
                (when (todo-notes todo)
                    (newline)
                    (let ((notes-begin (point)))
                        (insert (todo-notes todo))
                        (fill-region notes-begin (point))
                        (indent-region notes-begin (point) (1+ drawtodo-indent-level))
                        ;;(put-text-property notes-begin (point) 'field todo-node)
                        ))
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
    (beginning-of-line)
    (move-overlay *current-item-bg-overlay* (point) (next-todo-position))
    (move-overlay *current-item-title-overlay* (point) (1+ (line-end-position)))
    )

(defun οεδ-next-line ()
    (interactive)
    (let ((next-item-pos (next-single-property-change (point) 'οεδ-todo-node-prop)))
        (when next-item-pos
            (goto-line-at-char next-item-pos))))

(defun οεδ-prev-line ()
    (interactive)
    (let ((prev-item-pos (previous-single-property-change (line-beginning-position) 'οεδ-todo-node-prop)))
        (if prev-item-pos
                (goto-line-at-char prev-item-pos)
            (goto-line-at-char (beginning-of-undone-todos)))))

(defun prev-todo-position ()
    (save-excursion
        (οεδ-prev-line)
        (point)))

(defun next-todo-position ()
    (save-excursion
        (οεδ-next-line)
        (point)))

(defun refresh-eval-buffer ()
    (interactive)
    (find-alternate-file (buffer-file-name))
    (eval-buffer))

(defun todo-read-callback (todo)
    (when (todo-subtodos todo)
        (set-todo-subtodos todo (make-dll-from-proper-list-callback (todo-subtodos todo) 'todo-read-callback)))
    todo)

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

(defun fix-overlays-after-adding-first-undone-todo ()
    (save-excursion
        (goto-char (point-min))
        (move-overlay *undone-items-overlay* 1 (line-end-position))
        (move-overlay *done-items-overlay* (+ 1 (line-end-position)) (overlay-end *done-items-overlay*))))

(defun οεδ-new-todo ()
    (interactive)
    (let* ((title (read-from-minibuffer "Title: "))
           (new-todo (list :title title :create-date (current-time-string) :complete 'no)))
        (dll-add-node-to-list-before *todo-list* *todo-list* (make-dll-node new-todo))
        (goto-char (beginning-of-undone-todos))
        (let ((inhibit-read-only t))
            (draw-todo *todo-list*))
        ;; If this is our only todo, we need to adjust the
        ;; overlays that cover the done and not done areas, so we just
        ;; redraw the whole thing.
        (when (and (null (dll-node-prev *todo-list*)) (null (dll-node-next *todo-list*)))
            (fix-overlays-after-adding-first-undone-todo)))
    (goto-line-at-char (beginning-of-undone-todos)))

(defun οεδ-mark-todo-done ()
    (interactive)
    (let* ((todo-node-at-point (get-char-property (point) 'οεδ-todo-node-prop))
           (todo-at-point (and todo-node-at-point (dll-node-data todo-node-at-point)))
           (inhibit-read-only t))
        (when (and todo-at-point (not (todo-complete-p todo-at-point)))
            (save-excursion
                (set-todo-complete todo-at-point t)
                (dll-remove-node-from-list *todo-list* todo-node-at-point)
                (dll-add-node-to-list-before *done-list* *done-list* todo-node-at-point)
                (kill-region (line-beginning-position) (next-todo-position))
                (goto-char (beginning-of-done-todos))
                (yank))
            (goto-line-at-char (point))))) ;; just refresh the point

(defun οεδ-mark-todo-undone ()
    (interactive)
    (let* ((todo-node-at-point (get-char-property (point) 'οεδ-todo-node-prop))
           (todo-at-point (and todo-node-at-point (dll-node-data todo-node-at-point)))
           (inhibit-read-only t))
        (when (and todo-at-point (todo-complete-p todo-at-point))
            (save-excursion
                (set-todo-complete todo-at-point nil)
                (dll-remove-node-from-list *done-list* todo-node-at-point)
                (dll-add-node-to-list-before *todo-list* *todo-list* todo-node-at-point)
                (kill-region (line-beginning-position) (+ 1 (line-end-position)))
                (goto-char (beginning-of-undone-todos))
                (yank))
            (when (and (null (dll-node-prev *todo-list*)) (null (dll-node-next *todo-list*)))
                (fix-overlays-after-adding-first-undone-todo))
            (goto-line-at-char (point)))))

(defvar write-todo-disable-newline nil)

(defun write-todo (todo)
    (unless (todo-disabled-p todo)
        (princ "(")

        (while todo
            (if (and (eq (car todo) :subtodos) (not (null (cadr todo))))
                    (progn
                        (prin1 :subtodos)
                        (princ " ")
                        (let ((write-todo-disable-newline t))
                            (dll-mapcar (cadr todo) 'write-todo)))
                (prin1 (car todo))
                (princ " ")
                (prin1 (cadr todo))
                (princ " "))
            (setf todo (cddr todo)))

        (princ ")")
        (unless write-todo-disable-newline
            (newline))))

(defvar already-saving nil)

(defun οεδ-save-buffer ()
    (interactive)
    (unless already-saving
        (if (buffer-modified-p)
            (save-excursion
                (let ((tmp-buf (generate-new-buffer " οεδ"))
                      (buf-name (buffer-name))
                      (file-name (buffer-file-name))
                      (todo *todo-list*)
                      (done *done-list*)
                      (already-saving t))
                    (set-buffer tmp-buf)
                    (set-visited-file-name file-name t)
                    (let ((standard-output tmp-buf))
                        (dll-mapcar todo 'write-todo)
                        (dll-mapcar done 'write-todo))
                    (save-buffer)
                    (set-buffer buf-name)
                    (kill-buffer tmp-buf)
                    (set-visited-file-modtime)))
            (message "(No changes need to be saved)"))
        (set-buffer-modified-p nil))
    t)
