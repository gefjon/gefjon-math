(in-package :gmath)

(defun factorial (n &optional (acc 1))
  (declare (integer n acc))
  (if (<= n 1) acc
      (factorial (- n 1) (* acc n))))

(defun permute (n k)
  (declare (integer n k))
  (if (> k n) 0
      (/ (factorial n)
         (factorial (- n k)))))

(defun combine (n k)
  (declare (integer n k))
  (if (> k n) 0
      (/ (factorial n)
         (* (factorial k) (factorial (- n k))))))
