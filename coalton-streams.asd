(asdf:defsystem #:coalton-streams
  :author "garlic0x1"
  :license "MIT"
  :depends-on (#:coalton #:flexi-streams)
  :components ((:module "src"
                :components ((:file "utils")
                             (:file "token")
                             (:file "stream")
                             (:file "file"))))
  :in-order-to ((test-op (test-op #:coalton-streams/test))))

(asdf:defsystem #:coalton-streams/test
  :depends-on (#:coalton-streams #:coalton/testing)
  :components ((:module "test"
                :serial t
                :components ((:file "package")
                             (:file "read"))))
  :perform (asdf:test-op
            (o s)
            (uiop:symbol-call '#:coalton-streams/test '#:run-tests)))
