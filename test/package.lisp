(defpackage #:coalton-streams/test
  (:use #:coalton #:coalton-prelude #:coalton-testing)
  (:local-nicknames
   (#:vec #:coalton-library/vector)
   (#:types #:coalton-library/types)
   (#:result #:coalton-library/result))
  (:local-nicknames
   (#:stream #:coalton-streams/stream)
   (#:file #:coalton-streams/file)))
(in-package #:coalton-streams/test)

(fiasco:define-test-package #:coalton-streams/fiasco-test-package)
(coalton-fiasco-init #:coalton-streams/fiasco-test-package)

(cl:defun run-tests (cl:&optional interactive)
  (fiasco:run-package-tests
   :packages '(#:coalton-streams/fiasco-test-package)
   :interactive interactive))
