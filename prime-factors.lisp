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

(defmacro check-limit-and-check-factor (n
                                        test-factor
                                        limit
                                        factors
                                        factor-adjust)
  (let ((limit-name (gensym)))
    `(let ((,limit-name ,limit))
       (if (>= ,test-factor ,limit-name)
           (no-more-factors ,n ,factors)
           (if (factor-p ,n ,test-factor)
               (first-factor (/ ,n ,test-factor)
                             :factors (cons ,test-factor ,factors))
               (more-factors ,n
                             :limit ,limit-name
                             :test-factor (+ ,test-factor ,factor-adjust)
                             :factors ,factors))))))

(defun first-factor (n &key
                         (factors '()))
  (declare (integer n)
           (list factors))
  (check-limit-and-check-factor n
                                2
                                (find-limit n)
                                factors
                                1))

(defun more-factors (n &key
                         (limit (find-limit n))
                         (test-factor 3)
                         (factors '()))
  (declare (integer n limit test-factor)
           (list factors))
  (check-limit-and-check-factor n
                                test-factor
                                limit
                                factors
                                2))

(defun prime-factors (n)
  (declare (integer n))
  (first-factor n))
