(in-package :gmath)

(defun find-limit (n)
  (+ 1 (ceiling (sqrt n))))

(defun factor? (n factor)
  (= (rem n factor) 0))

(defun no-more-factors (n factors)
  (if (= n 1)
      factors
      (cons n factors)))

(defun more-factors (n &key
                         (limit (find-limit n))
                         (test-factor 2)
                         (factors '()))
  (if (factor? n test-factor)
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
    (if (>= test-factor limit)
        (no-more-factors n factors)
        (more-factors n
                      :limit limit
                      :test-factor test-factor
                      :factors factors)))

(defun prime-factors (n)
  (prime-factors-iter n))
