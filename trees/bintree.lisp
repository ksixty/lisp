(defstruct binary-tree
   value (left nil) (right nil))

(defparameter *test-btree*
  (make-binary-tree :value 5
                    :left  (make-binary-tree :value 3
                                             :left  (make-binary-tree :value 1)
                                             :right (make-binary-tree :value 4))
                    :right (make-binary-tree :value 9
                                             :left  (make-binary-tree :value 6))))

(defparameter *test-btree-fake*
  (make-binary-tree :value 5
                    :left  (make-binary-tree :value 6
                                             :left  (make-binary-tree :value 1)
                                             :right (make-binary-tree :value 4))
                    :right (make-binary-tree :value 9
                                             :left  (make-binary-tree :value 6))))


(defparameter *test-btree-2* (binary-tree-left *test-btree*))

(defun binary-tree-leaf-p (tree)
  (not (or (binary-tree-left tree)
           (binary-tree-right tree))))

(defun ordered-binary-node-p (node)
  (declare (type binary-tree))
  (with-slots (value left right) node
    (let ((left-value  (binary-tree-value left))
          (right-value (binary-tree-value right)))
      (and (<= left-value value)
           (> right-value value)))))

(defun ordered-binary-tree-p (tree)
  "Check whether a binary tree is a BST."
  (if (binary-tree-leaf-p tree) T
      (with-slots (value left right) tree
        (and (ordered-binary-node-p tree)
             (ordered-binary-tree-p left)
             (ordered-binary-tree-p right)))))

(defun binary-tree-insert (tree new-value)
  "Returns a new tree containing NEW-VALUE in the right place."
  (let ((new-tree (copy-binary-tree tree))
        (new-value-node (make-binary-tree :value new-value)))
    (format t "~a~%" new-value-node)
    (%binary-tree-inserter new-tree new-value-node)
    new-tree))

(defun %binary-tree-inserter (tree node)
  "Helper function for inserting NODE in TREE."
  (declare (type binary-tree binary-tree))
  (with-slots (value left right) tree
    (with-slots ((new-value value)) node
      (cond ((<= new-value value) (if (null left) (setf left node)
                                      (%binary-tree-inserter left node)))
            ((> new-value value) (if (null right) (setf right node)
                                     (%binary-tree-inserter right node)))))))

;; examples
;;
;; (ordered-binary-node-p *test-btree-2*) ; => T
;; (binary-tree-insert *test-btree* 123) ; => #S(...)
