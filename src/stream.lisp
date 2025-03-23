(defpackage #:coalton-streams/stream
  (:use #:coalton #:coalton-prelude)
  (:import-from #:coalton-library/system #:LispCondition)
  (:local-nicknames
   (#:vec #:coalton-library/vector))
  (:local-nicknames
   (#:util #:coalton-streams/util)
   (#:token #:coalton-streams/token))
  (:export
   #:InputStream #:OutputStream #:IOStream
   #:Readable #:Writable #:Closable
   #:read #:read-sequence #:unread #:peek
   #:write #:write-sequence
   #:close
   #:ReaderErr #:EOF
   #:ReaderPredicate #:Inclusive #:Exclusive
   #:drop-to #:read-to
   #:read-word #:read-line #:read-all))
(in-package #:coalton-streams/stream)
(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (repr :native (cl:or flex:flexi-input-stream flex::vector-input-stream))
  (define-type (InputStream :elt)
    "A stream that can be read from.")

  (repr :native (cl:or flex:flexi-output-stream flex::vector-output-stream))
  (define-type (OutputStream :elt)
    "A stream that can be written to.")

  (repr :native flex:flexi-io-stream)
  (define-type (IOStream :elt)
    "A stream that can be read from or written to.")

  (define-class (Closable :stream :elt)
    "A stream that can be closed."
    (close          (:stream :elt -> (Result LispCondition Unit))))

  (define-class (Readable :stream :elt)
    "An input or IO stream."
    (read           (:stream :elt -> (Result LispCondition :elt)))
    (read-sequence  (:stream :elt -> UFix -> (Result LispCondition (Vector :elt))))
    (unread         (:stream :elt -> :elt -> :stream :elt))
    (peek           (:stream :elt -> (Result LispCondition :elt))))

  (define-class (Writable :stream :elt)
    "An output or IO stream."
    (write          (:stream :elt -> :elt -> (Result LispCondition Unit)))
    (write-sequence (:stream :elt -> Vector :elt -> (Result LispCondition Unit)))))

(coalton-toplevel
  (util:define-instances (Closable (InputStream Char) (OutputStream Char) (IOStream Char)
                              (InputStream U8) (OutputStream U8) (IOStream U8))
    (define (close stream)
      (util:lisp-result Unit (stream)
        (cl:close stream))))

  (util:define-instances (Readable (InputStream Char) (IOStream Char))
    (define (read stream)
      (util:lisp-result Char (stream)
        (cl:read-char stream)))
    (define (read-sequence stream n)
      (util:lisp-result (Vector Char) (stream n)
        (cl:let ((vec (cl:make-array n :adjustable cl:t)))
          (cl:read-sequence vec stream)
          vec)))
    (define (unread stream elt)
      (lisp :a (stream elt)
        (cl:unread-char elt stream)
        stream))
    (define (peek stream)
      (util:lisp-result Char (stream)
        (cl:peek-char nil stream))))
  (util:define-instances (Writable (OutputStream Char) (IOStream Char))
    (define (write stream elt)
      (util:lisp-result Unit (stream elt)
        (cl:write-char elt stream)))
    (define (write-sequence stream vec)
      (util:lisp-result Unit (stream vec)
        (cl:write-sequence vec stream))))

  (util:define-instances (Readable (InputStream U8) (IOStream U8))
    (define (read stream)
      (util:lisp-result U8 (stream)
        (cl:read-byte stream)))
    (define (read-sequence stream n)
      (util:lisp-result (Vector U8) (stream n)
        (cl:let ((vec (cl:make-array n :adjustable cl:t)))
          (cl:read-sequence vec stream)
          vec)))
    (define (unread stream elt)
      (lisp :a (stream elt)
        (flex:unread-byte elt stream)
        stream))
    (define (peek stream)
      (util:lisp-result U8 (stream)
        (flex:peek-byte stream))))
  (util:define-instances (Writable (OutputStream U8) (IOStream U8))
    (define (write stream elt)
      (util:lisp-result Unit (stream elt)
        (cl:write-byte elt stream)))
    (define (write-sequence stream vec)
      (util:lisp-result Unit (stream vec)
        (cl:write-sequence vec stream)))))

(coalton-toplevel
  (define-type (ReaderErr :elt)
    (EOF (Vector :elt)))

  (define-type (ReaderPredicate :elt)
    (Inclusive (:elt -> Boolean))
    (Exclusive (:elt -> Boolean)))

  (declare read-to ((Readable :stream :elt) => :stream :elt -> ReaderPredicate :elt -> (Result (ReaderErr :elt) (Vector :elt))))
  (define (read-to stream pred)
    (let vec = (vec:make))
    (while-let (Ok elt) = (read stream)
      (match pred
        ((Inclusive f)
         (progn (vec:push! elt vec) Unit)
         (when (f elt) (return (Ok vec))))
        ((Exclusive f)
         (if (f elt)
             (progn (unread stream elt) (return (Ok vec)))
             (progn (vec:push! elt vec) Unit)))))
    (Err (EOF vec)))

  (declare drop-to ((Readable :stream :elt) => :stream :elt -> ReaderPredicate :elt -> (Result (ReaderErr :elt) (:stream :elt))))
  (define (drop-to stream pred)
    (while-let (Ok elt) = (read stream)
      (match pred
        ((Inclusive f)
         (when (f elt)
           (return (Ok stream))))
        ((Exclusive f)
         (when (f elt)
           (unread stream elt)
           (return (Ok stream))))))
    (Err (EOF (vec:make))))

  (declare read-word ((Readable :stream :elt) (token:Whitespace :elt) => :stream :elt -> (Result (ReaderErr :elt) (Vector :elt))))
  (define (read-word stream)
    (drop-to stream (Exclusive (fn (elt) (not (token:whitespace? elt)))))
    (read-to stream (Exclusive token:whitespace?)))

  (declare read-line ((Readable :stream :elt) (token:Newline :elt) => :stream :elt -> (Result (ReaderErr :elt) (Vector :elt))))
  (define (read-line stream)
    (drop-to stream (Exclusive (fn (elt) (not (token:newline? elt)))))
    (read-to stream (Exclusive token:newline?)))

  (declare read-all ((Readable :stream :elt) => :stream :elt -> (Vector :elt)))
  (define (read-all stream)
    (match (read-to stream (Inclusive (fn (_) False)))
      ((Ok result) result)
      ((Err (EOF result)) result))))
