(in-package :gmath)

(defun find-limit (n)
  (+ 1 (ceiling (sqrt n))))
(defun factor? (n factor)
  (= (rem n factor) 0))
(defun no-more-factors (n factors)
  (if (= n 1)
      factors
      (cons n factors)))
(defun more-factors (n limit test-factor factors)
  (if (factor? n test-factor)
      (let ((new-n (/ n test-factor)))
        (prime-factors-iter
         new-n
         (find-limit new-n)
         2
         (cons test-factor factors)))
      (prime-factors-iter n limit (+ test-factor 1) factors)))
(defun prime-factors-iter (n limit test-factor factors)
  (if (>= test-factor limit)
      (no-more-factors n factors)
      (more-factors n limit test-factor factors)))
(defun prime-factors (n)
  (prime-factors-iter n (find-limit n) 2 '()))
