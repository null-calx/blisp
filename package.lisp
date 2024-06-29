(in-package :cl-user)

(defpackage :parser
  (:use :cl)
  (:export
   :define-binary-class
   :define-tagged-binary-class
   :define-binary-type
   :define-binary-enum
   :define-binary-literal
   :read-value
   :write-value
   :*in-progress-objects*
   :current-binary-object
   :parent-of-type))

(defpackage :readelf
  (:use :cl :parser))
