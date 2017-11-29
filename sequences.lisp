(in-package :gmath)

(defun sum-first-n-elements (sequence n)
  (loop for i from 1 to n
       summing (funcall sequence i)))

(defun find-nth-element-of-recursive (recursive-sequence n)
  (loop repeat n
     for i = (funcall recursive-sequence) then (funcall recursive-sequence i)
     finally (return i)))

(defun first-n-elements-of-recursive (recursive-sequence n)
  (loop repeat n
     for i = (funcall recursive-sequence) then (funcall recursive-sequence i)
     collecting i))

(defun sum-first-n-elements-of-recursive (recursive-sequence n)
  (loop repeat n
     for i = (funcall recursive-sequence) then (funcall recursive-sequence i)
     summing i))

(defun sum-recursive-from-a-to-b (recursive-sequence a b)
  (when (> b a)
    (let ((initial (find-nth-element-of-recursive recursive-sequence a)))
      (loop repeat (1+ (- b a))
         for i = initial then (funcall recursive-sequence i)
         summing i))))

(defmacro recursive-arithmetic-sequence-from-0th (zeroth difference)
  (let ((initial-name (gensym))
        (diff-name (gensym))
        (previous (gensym)))
    `(let ((,initial-name ,zeroth)
           (,diff-name ,difference))
       (lambda (&optional (,previous ,initial-name))
         (+ ,previous ,diff-name)))))

(defmacro recursive-arithmetic-sequence (initial difference)
  (let ((initial-name (gensym))
        (diff-name (gensym)))
    `(let* ((,diff-name ,difference)
            (,initial-name (- ,initial ,diff-name)))
       (recursive-arithmetic-sequence-from-0th ,initial-name ,diff-name))))

(defmacro recursive-geometric-sequence-from-0th (zeroth ratio)
  (let ((initial-name (gensym))
        (ratio-name (gensym))
        (previous (gensym)))
    `(let ((,initial-name ,zeroth)
           (,ratio-name ,ratio))
       (lambda (&optional (,previous ,initial-name))
         (* ,previous ,ratio-name)))))

(defmacro recursive-geometric-sequence (initial ratio)
  (let ((initial-name (gensym))
        (ratio-name (gensym)))
    `(let* ((,ratio-name ,ratio)
            (,initial-name (/ ,initial ,ratio-name)))
       (recursive-geometric-sequence-from-0th ,initial-name ,ratio-name))))

(defmacro geometric-sequence (initial ratio)
  (let ((initial-name (gensym))
        (ratio-name (gensym))
        (n (gensym)))
    `(let* ((,ratio-name ,ratio)
            (,initial-name (/ ,initial ,ratio-name)))
       (lambda (,n)
         (* ,initial-name (expt ,ratio-name (1- ,n)))))))

(defmacro arithmetic-sequence (initial difference)
  (let ((initial-name (gensym))
        (difference-name (gensym))
        (n (gensym)))
    `(let* ((,difference-name ,difference)
            (,initial-name (- ,initial ,difference-name)))
       (lambda (,n)
         (+ ,initial-name (* ,difference-name (1- ,n)))))))
            
       
