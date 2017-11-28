(in-package :gmath)

;; (defmacro find-nth-element (sequence n)
;;   (let ((seq-name (gensym)))
;;     `(let ((,seq-name ,sequence))
;;        (loop repeat (1- ,n)
;;           for i = (funcall ,seq-name) then (funcall ,seq-name i)
;;           finally (return i)))))

(defun find-nth-element (sequence n)
  (loop repeat n
     for i = (funcall sequence) then (funcall sequence i)
     finally (return i)))

(defun first-n-elements (sequence n)
  (loop repeat n
     for i = (funcall sequence) then (funcall sequence i)
     collect i into values
     finally (return values)))

(defmacro arithmetic-sequence-from-0th (zeroth difference)
  (let ((initial-name (gensym))
        (diff-name (gensym))
        (previous (gensym)))
    `(let ((,initial-name ,zeroth)
           (,diff-name ,difference))
       (lambda (&optional (,previous ,initial-name))
         (+ ,previous ,diff-name)))))

(defmacro arithmetic-sequence (initial difference)
  (let ((initial-name (gensym))
        (diff-name (gensym)))
    `(let* ((,diff-name ,difference)
            (,initial-name (- ,initial ,diff-name)))
       (arithmetic-sequence-from-0th ,initial-name ,diff-name))))

(defmacro geometric-sequence-from-0th (zeroth ratio)
  (let ((initial-name (gensym))
        (ratio-name (gensym))
        (previous (gensym)))
    `(let ((,initial-name ,zeroth)
           (,ratio-name ,ratio))
       (lambda (&optional (,previous ,initial-name))
         (* ,previous ,ratio-name)))))

(defmacro geometric-sequence (initial ratio)
  (let ((initial-name (gensym))
        (ratio-name (gensym)))
    `(let* ((,ratio-name ,ratio)
            (,initial-name (/ ,initial ,ratio-name)))
       (geometric-sequence-from-0th ,initial-name ,ratio-name))))
       
