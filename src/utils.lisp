(defpackage #:coalton-streams/util
  (:use #:coalton #:coalton-prelude)
  (:import-from #:coalton-library/system #:LispCondition)
  (:export
   #:lisp-result
   #:define-instances))
(in-package #:coalton-streams/util)
(named-readtables:in-readtable coalton:coalton)

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (cl:defmacro lisp-result (type capture cl:&body body)
    "Lisp FFI call that catches errors and returns a Result."
    `(lisp (Result LispCondition ,type) ,capture
       (cl:handler-case (Ok (cl:progn ,@body)) (cl:error (c) (Err c)))))

  (cl:defmacro define-instances ((class cl:&rest types) cl:&body methods)
    "Define instances in batches."
    `(progn ,@(cl:mapcar
               (cl:lambda (type)
                 `(define-instance (,class ,@type) ,@methods))
               types))))
