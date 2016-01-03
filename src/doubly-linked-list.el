
(provide 'doubly-linked-list)

(defun make-dll-node (&optional data prev-node next-node)
    (vector data prev-node next-node))

(defmacro dll-node-next (node)
    `(elt ,node 2))
(defmacro dll-node-prev (node)
    `(elt ,node 1))
(defmacro dll-node-data (node)
    `(elt ,node 0))

(defun dll-null-p (node)
    (null node))

(defun dll-clear-next-prev (node)
    (setf (dll-node-next node) nil)
    (setf (dll-node-prev node) nil))

(defmacro insert-node-template (name acc-fn1 acc-fn2)
    `(defun ,name (node new-node)
         (setf (,acc-fn1 new-node) (,acc-fn1 node))
         (when (,acc-fn1 new-node)
             (setf (,acc-fn2 (,acc-fn1 new-node)) new-node))
         (setf (,acc-fn2 new-node) node)
         (setf (,acc-fn1 node) new-node)
         new-node))

(insert-node-template dll-insert-node-after dll-node-next dll-node-prev)
(insert-node-template dll-insert-node-before dll-node-prev dll-node-next)

(defun dll-insert-after (node new-data)
    (dll-insert-node-after node (make-dll-node new-data)))

(defun dll-insert-before (node new-data)
    (dll-insert-node-before node (make-dll-node new-data)))

(defun dll-remove-node (node)
    (when (dll-node-prev node)
        (setf (dll-node-next (dll-node-prev node)) (dll-node-next node)))
    (when (dll-node-next node)
        (setf (dll-node-prev (dll-node-next node)) (dll-node-prev node)))
    (setf (dll-node-next node) nil)
    (setf (dll-node-prev node) nil))

(defmacro dll-remove-node-from-list (head node)
    (let ((head-sym (make-symbol "head-sym"))
          (node-sym (make-symbol "node-sym")))
        `(let ((,head-sym ,head)
               (,node-sym ,node))
             (when (eq ,head-sym ,node-sym)
                 (setf ,head (dll-node-next ,head-sym)))
             (dll-remove-node ,node-sym))))

(defmacro dll-add-node-to-list-before (head node new-node)
    (let ((head-sym (make-symbol "head-sym"))
          (node-sym (make-symbol "node-sym"))
          (new-node-sym (make-symbol "new-node-sym")))
        `(let ((,head-sym ,head)
               (,node-sym ,node)
               (,new-node-sym ,new-node))
             (cond
              ((null ,head-sym)
               (dll-clear-next-prev ,new-node-sym)
               (setf ,head ,new-node-sym))
              ((eq ,head-sym ,node-sym)
               (dll-insert-node-before ,head-sym ,new-node-sym)
               (setf ,head ,new-node-sym))
              (t (dll-insert-node-before ,node-sym ,new-node-sym)))
             ,new-node-sym)))

(defun make-dll-from-proper-list-callback (list callback)
    (let ((head nil) (tail nil))
        (while list
            (if head
                    (setf tail (dll-insert-node-after tail (make-dll-node)))
                (setf head (make-dll-node))
                (setf tail head))
            (setf (dll-node-data tail) (funcall callback (car list)))
            (setf list (cdr list)))
        head))

(defun make-dll-from-proper-list (list)
    (make-dll-from-proper-list list 'identity))

(defmacro do-dll (var-and-list &rest body)
    (let ((var-sym (make-symbol "var-sym"))
          (list-sym (make-symbol "list-sym")))
        `(let ((,list-sym ,(cadr var-and-list)))
             (while ,list-sym
                 (let ((,(car var-and-list) (dll-node-data ,list-sym)))
                     ,@body)
                 (setf ,list-sym (dll-node-next ,list-sym))))))

(defun dll-length (list)
    (let ((len 0))
        (while list
            (incf len)
            (setf list (dll-node-next list)))
        len))

(defun dll-mapcdr (list fun)
    (while list
        (funcall fun list)
        (setf list (dll-node-next list))))

(defun dll-mapcar (list fun)
    (while list
        (funcall fun (dll-node-data list))
        (setf list (dll-node-next list))))
