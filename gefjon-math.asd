;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defpackage gmath
  (:use :cl :asdf)
  (:export "PRIME-FACTORS"))

(in-package :gmath)

(defsystem gefjon-math
  :name "gefjon-math"
  :version "0.0.0"
  :serial t
  :components ((:file "prime-factors")))
