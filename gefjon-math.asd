;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defpackage gmath
  (:use :cl :asdf)
  (:export :prime-factors
           :factorial
           :permute
           :combine
           :find-nth-element
           :first-n-elements
           :arithmetic-sequence
           :geometric-sequence))

(in-package :gmath)

(defsystem gefjon-math
  :name "gefjon-math"
  :version "0.0.0"
  :author "Arthur Goldman"
  :license "MIT"
  :description "Math utilities"
  :long-description "Some mathematical utility functions I have written for use in my classes."
  :serial t
  :components ((:file "prime-factors")
               (:file "combinatorics")
               (:file "sequences")))
