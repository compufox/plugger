;;;; plugger-test.asd

(asdf:defsystem #:plugger-test
  :description "Describe plugger-test here"
  :author "ava fox"
  :license  "BSD 3-Clause"
  :version "0.0.1"
  :serial t
  :depends-on (#:plugger #:ningle #:prove #:clack)
  :components
  ((:module "t"
    :components
    ((:file "main"))))
  :perform (test-op (op c) (symbol-call :prove :run-test-system c)))
