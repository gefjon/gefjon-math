(in-package :gmath)

(defun factorial (n)
  "Compute n! using the algorith described at https://people.eecs.berkeley.edu/~fateman/papers/factorial.pdf"
  (check-type n integer)
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

(defun permute-unchecked (n k)
  "The algorithm behind gmath:permute"
  (declare (integer n k))
  (if (> k n) 0
      (loop for i downfrom n to (+ (- n k) 1)
         for x = n then (* x i)
         finally (return x))))

(defun combine-unchecked (n k)
  "The algorithm behind gmath:combine"
  (declare (integer n k))
  (if (> k n) 0
      (let ((k (min k (- n k))))
            (/ (permute-unchecked n k)
              (factorial k)))))

(defun permute (n k)
  "Compute nPk with a pretty bad O(n) algorithm"
  (check-type n integer)
  (check-type k integer)
  (permute-unchecked n k))

(defun combine (n k)
  "Compute nCk with a pretty bad O(n) algorithm"
  (check-type n integer)
  (check-type k integer)
  (combine-unchecked n k))
