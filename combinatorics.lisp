(in-package :gmath)

(defun factorial (n)
  (let ((shift 0))
    (labels
        ((iter (n m)
           (cond ((and (evenp n) (> m 1))
                  (incf shift (ash n -1))
                  (iter (ash n -1) (ash m -1)))
                 ((<= n m) n)
                 (t (* (iter n (ash m 1))
                       (iter (- n m) (ash m 1)))))))
      (ash (iter n 1) shift))))

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
