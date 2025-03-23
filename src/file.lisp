(defpackage #:coalton-streams/file
  (:use #:coalton #:coalton-prelude)
  (:import-from #:coalton-library/system #:LispCondition)
  (:local-nicknames
   (#:types #:coalton-library/types)
   (#:vec #:coalton-library/vector))
  (:local-nicknames
   (#:util #:coalton-streams/util)
   (#:stream #:coalton-streams/stream)
   (#:token #:coalton-streams/token))
  (:export
   #:with-input-file
   #:with-output-file))
(in-package #:coalton-streams/file)
(named-readtables:in-readtable coalton:coalton)

(cl:defmacro %with-file (path fn cl:&rest opts)
  `(progn
     (let prox = types:Proxy)
     (let _ = (types:as-proxy-of ,fn prox))
     (let ((declare proxy-fst (types:Proxy (:a -> :b) -> types:Proxy :a))
           (proxy-fst (fn (_) types:Proxy))
           (type (types:runtime-repr (types:proxy-inner (proxy-fst prox)))))
       (util:lisp-result :result (,path ,fn type)
         (cl:with-open-file (stream ,path :element-type '(cl:unsigned-byte 8) ,@opts)
           (call-coalton-function ,fn (flex:make-flexi-stream stream :element-type type)))))))

(coalton-toplevel
  (declare with-input-file ((types:RuntimeRepr :elt) => String -> (stream:InputStream :elt -> :result) -> (Result LispCondition :result)))
  (define (with-input-file path fn)
    "Call `fn' with file at `path' open to read."
    (%with-file path fn :direction :input))

  (declare with-output-file ((types:RuntimeRepr :elt) => String -> (stream:OutputStream :elt -> :result) -> (Result LispCondition :result)))
  (define (with-output-file path fn)
    "Call `fn' with file at `path' open to write."
    (%with-file path fn :direction :output :if-exists :append))

  (declare with-io-file ((types:RuntimeRepr :elt) => String -> (stream:IOStream :elt -> :result) -> (Result LispCondition :result)))
  (define (with-io-file path fn)
    "Call `fn' with file at `path' open to read and write."
    (%with-file path fn :direction :io :if-exists :append)))

(coalton-toplevel
  (declare read-file-string (String -> (Result LispCondition String)))
  (define (read-file-string path)
    (with-input-file path
      (fn (stream)
        (into (the (Vector Char) (stream:read-all stream))))))

  (declare read-file-lines (String -> (Result LispCondition (Vector String))))
  (define (read-file-lines path)
    (with-input-file path
      (fn (stream)
        (rec f ((lines (vec:new)))
          (let ((line (into (the (Vector Char) (stream:read-line stream)))))
            (if (mempty? line)
                lines
                (f (progn (vec:push! line lines) lines)))))))))
