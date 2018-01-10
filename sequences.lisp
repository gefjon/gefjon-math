(in-package :gmath)

(defun arithmetic-sequence (first difference)
  (declare (type real first difference)
           (optimizable-series-function))
  (scan-range :from first :by difference))

(defun geometric-sequence (first ratio)
  (declare (type real first ratio)
            (optimizable-series-function))
  (scan-fn 'real
           #'(lambda () first)
           #'(lambda (previous)
               (* previous ratio))))
            
       
