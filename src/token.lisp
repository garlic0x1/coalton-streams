(defpackage #:coalton-streams/token
  (:use #:coalton #:coalton-prelude)
  (:import-from #:coalton-library/system #:LispCondition)
  (:local-nicknames
   (#:char #:coalton-library/char)
   (#:list #:coalton-library/list))
  (:export
   #:Newline
   #:newline?
   #:Whitespace
   #:whitespace?))
(in-package #:coalton-streams/token)
(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (define whitespaces
    (lisp (List UFix) ()
      (cl:mapcar #'cl:char-code '(#\Space #\Tab #\Linefeed #\Return #\Page))))

  (define-class (Newline :elt)
    (newline? (:elt -> Boolean)))

  (define-instance (Newline Char)
    (define (newline? obj) (== obj #\Newline)))

  (define-instance (Newline U8)
    (define (newline? obj) (== obj 10)))

  (define-class (Whitespace :elt)
    (whitespace? (:elt -> Boolean)))

  (define-instance (Whitespace Char)
    (define (whitespace? obj) (list:member (char:char-code obj) whitespaces)))

  (define-instance (Whitespace U8)
    (define (whitespace? obj) (list:member (into obj) whitespaces))))
