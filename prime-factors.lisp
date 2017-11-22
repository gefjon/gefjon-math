(in-package :gmath)

(defun find-limit (n)
  "Returns the largest possible prime factor of n other than n"
  (declare (integer n))
  ;; A composite number cannot have any prime factors larger than
  ;; its square root, so we only search up to the first integer
  ;; greater than that square root
  (+ 1 (ceiling (sqrt n))))

(defun factor-p (n factor)
  "t if factor|n, nil otherwise"
  (declare (integer n factor))
  (= (rem n factor) 0))

(defun no-more-factors (n factors)
  "Check if n is prime and return the list of factors from gmath:prime-factors"
  (declare (integer n)
           (list factors))
  ;; If passed some n > 1, n is a prime factor of whatever number we
  ;; were factoring in the first place
  (if (= n 1)
      factors
      (cons n factors)))

(defmacro check-limit-and-check-factor (n
                                        test-factor
                                        limit
                                        factors
                                        factor-adjust)
  "Check if test-factor is too large to be a factor of n, and otherwise if it is a factor. If it is not, try again with (+ test-factor factor-adjust)"
  (let ((limit-name (gensym)))                   ; limit is the only var that
                                                 ; gets passed as an evaluated
                                                 ; form which may have side
                                                 ; effects or be expensive
    `(let ((,limit-name ,limit))
       (if (>= ,test-factor ,limit-name)          ; if we've passed the limit,
           (no-more-factors ,n ,factors)         ; return
           (if (factor-p ,n ,test-factor)
               (factor-start-iter (/ ,n ,test-factor) ; first-factor checks 2
                             (cons ,test-factor ,factors))
               (factor-continue-iter ,n          ; whereas more-factors proceeds
                             ,limit-name         ; across the odd numbers
                             (+ ,test-factor ,factor-adjust)
                             ,factors))))))

(defun factor-start-iter (n factors)
  "Called during gmath:prime-factors to check if 2 is a factor"
  (declare (integer n)
           (list factors))
  ;; This function exists because 2 is the only even prime. We want to test
  ;; 2 as a factor, but we also want to count only across the odd numbers
  ;; because otherwise we're just doing twice the work for nothing
  (check-limit-and-check-factor n
                                2
                                (find-limit n)
                                factors
                                1))

(defun factor-continue-iter (n
                     limit
                     test-factor
                     factors)
  "Called during gmath:prime-factors to check odd factors > 2"
  (declare (integer n limit test-factor)
           (list factors))
  (check-limit-and-check-factor n
                                test-factor
                                limit
                                factors
                                2))

(defun prime-factors (n)
  "Return a list of the prime factors of n"
  (check-type n integer)
  (factor-start-iter n '()))
