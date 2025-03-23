(in-package #:coalton-streams/test)

(coalton-toplevel
  (declare make-test-stream (String -> (stream:InputStream :elt)))
  (define (make-test-stream str)
    (lisp (stream:InputStream :elt) (str)
      (flex:make-in-memory-input-stream
       (flex:string-to-octets str)))))

(define-test test-read-u8-stream ()
  (let ((declare s (stream:InputStream U8))
        (s (make-test-stream "Hello")))
    (is (== 72 (unwrap (stream:read s))))
    (is (== (vec:make 101 108) (unwrap (stream:read-sequence s 2))))
    (is (== 108 (unwrap (stream:peek s))))
    (is (== (vec:make 108 111) (unwrap (stream:read-sequence s 2))))
    (is (result:err? (stream:read s)))))
