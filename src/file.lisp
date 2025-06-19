(defpackage #:coalton-streams/file
  (:use #:coalton #:coalton-prelude)
  (:import-from #:coalton-library/system #:LispCondition)
  (:local-nicknames
   (#:types #:coalton-library/types)
   (#:seq #:coalton-library/seq)
   (#:vec #:coalton-library/vector))
  (:local-nicknames
   (#:util #:coalton-streams/util)
   (#:stream #:coalton-streams/stream))
  (:shadow :append)
  (:export
   #:IfExists #:EError #:Overwrite #:Append #:Supersede #:Rename
   #:IfDoesNotExist #:DNEError #:Create
   #:open-input-file* #:open-output-file* #:open-io-file*
   #:open-input-file #:open-output-file #:open-io-file
   #:with-input-file* #:with-output-file* #:with-io-file*
   #:with-input-file #:with-output-file #:with-io-file
   #:touch
   #:read-file-string #:read-file-lines))
(in-package #:coalton-streams/file)
(named-readtables:in-readtable coalton:coalton)

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (cl:defmacro %open-file (path if-exists if-does-not-exist cl:&rest opts)
    `(progn
       (let if-exists = (symbolize ,if-exists))
       (let if-does-not-exist = (symbolize ,if-does-not-exist))
       (let prox = types:Proxy)
       (let type = (types:runtime-repr (types:proxy-inner (types:proxy-inner prox))))
       (types:as-proxy-of
        (util:lisp-result :result (,path if-exists if-does-not-exist type)
          (flex:make-flexi-stream 
           (cl:open ,path
                    :element-type '(cl:unsigned-byte 8)
                    :if-exists if-exists
                    :if-does-not-exist if-does-not-exist
                    ,@opts)
           :element-type type))
        prox)))

  (cl:defmacro %with-file (path fn if-exists if-does-not-exist cl:&rest opts)
    `(progn
       (let if-exists = (symbolize ,if-exists))
       (let if-does-not-exist = (symbolize ,if-does-not-exist))
       (let prox = types:Proxy)
       (let _ = (types:as-proxy-of ,fn prox))
       (let ((declare proxy-fst (types:Proxy (:a -> :b) -> types:Proxy :a))
             (proxy-fst (fn (_) types:Proxy))
             (type (types:runtime-repr (types:proxy-inner (proxy-fst prox)))))
         (util:lisp-result :result (,path ,fn if-exists if-does-not-exist type)
           (cl:with-open-file (stream ,path
                                      :element-type '(cl:unsigned-byte 8)
                                      :if-exists if-exists
                                      :if-does-not-exist if-does-not-exist
                                      ,@opts)
             (call-coalton-function ,fn (flex:make-flexi-stream stream :element-type type))))))))

(coalton-toplevel
  (repr :native cl:symbol)
  (define-type Symbol)

  (define-class (Symbolic :t)
    (symbolize (:t -> Symbol)))

  (repr :enum)
  (define-type IfExists
    EError
    Overwrite
    Append
    Supersede
    Rename)

  (define-instance (Symbolic IfExists)
    (define (symbolize obj)
      (match obj
        ((EError)    (lisp Symbol () ':error))
        ((Overwrite) (lisp Symbol () ':overwrite))
        ((Append)    (lisp Symbol () ':append))
        ((Supersede) (lisp Symbol () ':supersede))
        ((Rename)    (lisp Symbol () ':rename)))))

  (repr :enum)
  (define-type IfDoesNotExist
    DNEError
    Create)

  (define-instance (Symbolic IfDoesNotExist)
    (define (symbolize obj)
      (match obj
        ((DNEError) (lisp Symbol () ':error))
        ((Create)   (lisp Symbol () ':create))))))

(coalton-toplevel
  (declare open-input-file* ((types:RuntimeRepr :elt) => String -> IfDoesNotExist -> (Result LispCondition (stream:InputStream :elt))))
  (define (open-input-file* path if-does-not-exist)
    (%open-file path Append if-does-not-exist :direction ':input))

  (declare open-input-file ((types:RuntimeRepr :elt) => String -> (Result LispCondition (stream:InputStream :elt))))
  (define (open-input-file path)
    (%open-file path Append DNEError :direction ':input))

  (declare open-output-file* ((types:RuntimeRepr :elt) => String -> IfExists -> IfDoesNotExist -> (Result LispCondition (stream:OutputStream :elt))))
  (define (open-output-file* path if-exists if-does-not-exist)
    (%open-file path if-exists if-does-not-exist :direction ':input))

  (declare open-output-file ((types:RuntimeRepr :elt) => String -> (Result LispCondition (stream:OutputStream :elt))))
  (define (open-output-file path)
    (%open-file path Append DNEError :direction ':output))

  (declare open-io-file* ((types:RuntimeRepr :elt) => String -> IfExists -> IfDoesNotExist -> (Result LispCondition (stream:IOStream :elt))))
  (define (open-io-file* path if-exists if-does-not-exist)
    (%open-file path if-exists if-does-not-exist :direction ':io))

  (declare open-io-file ((types:RuntimeRepr :elt) => String -> (Result LispCondition (stream:IOStream :elt))))
  (define (open-io-file path)
    (%open-file path Append DNEError :direction ':io)))

(coalton-toplevel
  (declare with-input-file* ((types:RuntimeRepr :elt) => String -> IfDoesNotExist -> (stream:InputStream :elt -> :result) -> (Result LispCondition :result)))
  (define (with-input-file* path if-does-not-exist fn)
    "Call `fn' with file at `path' open to read."
    (%with-file path fn Append if-does-not-exist :direction ':input))

  (declare with-input-file ((types:RuntimeRepr :elt) => String -> (stream:InputStream :elt -> :result) -> (Result LispCondition :result)))
  (define (with-input-file path fn)
    "Call `fn' with file at `path' open to read.
Results in an error if the file does not exist."
    (%with-file path fn Append DNEError :direction ':input))

  (declare with-output-file* ((types:RuntimeRepr :elt) => String -> IfExists -> IfDoesNotExist -> (stream:OutputStream :elt -> :result) -> (Result LispCondition :result)))
  (define (with-output-file* path if-exists if-does-not-exist fn)
    "Call `fn' with file at `path' open to write."
    (%with-file path fn if-exists if-does-not-exist :direction ':output))

  (declare with-output-file ((types:RuntimeRepr :elt) => String -> (stream:OutputStream :elt -> :result) -> (Result LispCondition :result)))
  (define (with-output-file path fn)
    "Call `fn' with file at `path' open to write.
Creates the file if it does not exist.
Appends to the file if it exists."
    (%with-file path fn Append Create :direction ':output))

  (declare with-io-file* ((types:RuntimeRepr :elt) => String -> IfExists -> IfDoesNotExist -> (stream:IOStream :elt -> :result) -> (Result LispCondition :result)))
  (define (with-io-file* path if-exists if-does-not-exist fn)
    "Call `fn' with file at `path' open to read and write."
    (%with-file path fn if-exists if-does-not-exist :direction ':io))

  (declare with-io-file ((types:RuntimeRepr :elt) => String -> (stream:IOStream :elt -> :result) -> (Result LispCondition :result)))
  (define (with-io-file path fn)
    "Call `fn' with file at `path' open to read and write.
Creates the file if it does not exist.
Appends to the file if it exists."
    (%with-file path fn Append Create :direction ':io)))

(coalton-toplevel
  (declare touch (String -> (Result LispCondition Unit)))
  (define (touch path)
    (match (%open-file path Append Create :direction ':output)
      ((Ok stream)
       (stream:close (the (stream:OutputStream U8) stream))
       (Ok Unit))
      ((Err condition)
       (Err condition))))

  (declare read-file-string (String -> (Result LispCondition String)))
  (define (read-file-string path)
    (with-input-file path
      (fn (stream)
        (into (the (Vector Char) (stream:read-all stream))))))

  (declare read-file-lines (String -> (Result LispCondition (seq:Seq String))))
  (define (read-file-lines path)
    (with-input-file path
      (fn (stream) (the (stream:InputStream Char) stream)
        (rec f ((lines (seq:new)))
          (match (stream:read-line stream)
            ((Err (stream:EOF partial))
             (seq:push lines (into partial)))
            ((Ok line)
             (f (seq:push lines (into line))))))))))
