;;;; package.lisp

(defpackage #:plugger
  (:use #:cl)
  (:export :*api-root*
	   :defjsonclass
	   :defplug))
