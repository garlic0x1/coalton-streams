(in-package #:coalton-streams/test)

(coalton-toplevel
  (declare make-test-stream (String -> (streams:InputStream :elt)))
  (define (make-test-stream str)
    (lisp (streams:InputStream :elt) (str)
      (flex:make-in-memory-input-stream
       (flex:string-to-octets str)))))

(define-test test-read-u8-stream ()
  (let ((declare s (streams:InputStream U8))
        (s (make-test-stream "Hello")))
    (is (== 72 (unwrap (streams:read s))))
    (is (== (vec:make 101 108) (unwrap (streams:read-sequence s 2))))
    (is (== 108 (unwrap (streams:peek s))))
    (is (== (vec:make 108 111) (unwrap (streams:read-sequence s 2))))
    (is (result:err? (streams:read s)))))
