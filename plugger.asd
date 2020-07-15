;;;; plugger.asd

(asdf:defsystem #:plugger
  :description "Describe plugger here"
  :author "ava fox"
  :license  "BSD 3-Clause"
  :version "0.0.1"
  :serial t
  :depends-on (#:dexador #:json-mop #:str #:cl-ppcre)
  :components ((:file "package")
               (:file "plugger")))
