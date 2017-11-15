(in-package :gmath)

(defun find-limit (n)
  (declare (integer n))
  (+ 1 (ceiling (sqrt n))))

(defun factor-p (n factor)
  (declare (integer n factor))
  (= (rem n factor) 0))

(defun no-more-factors (n factors)
  (declare (integer n)
           (list factors))
  (if (= n 1)
      factors
      (cons n factors)))

(defun more-factors (n &key
                         (limit (find-limit n))
                         (test-factor 2)
                         (factors '()))
  (declare (integer n limit test-factor)
           (list factors))
  (if (factor-p n test-factor)
      (let ((new-n (/ n test-factor)))
        (prime-factors-iter new-n
                            :factors (cons test-factor factors)))
      (prime-factors-iter n
                          :limit limit
                          :test-factor (+ test-factor 1)
                          :factors factors)))

(defun prime-factors-iter (n &key
                               (limit (find-limit n))
                               (test-factor 2)
                               (factors '()))
    (declare (integer n limit test-factor)
           (list factors))
    (if (>= test-factor limit)
        (no-more-factors n factors)
        (more-factors n
                      :limit limit
                      :test-factor test-factor
                      :factors factors)))

(defun prime-factors (n)
  (declare (integer n))
  (prime-factors-iter n))
