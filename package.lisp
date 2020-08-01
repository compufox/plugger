;;;; package.lisp

(defpackage #:plugger
  (:use #:cl)
  (:import-from :cl-change-case
		:param-case
		:snake-case
		:camel-case)
  (:export :*api-root*
	   :defjsonclass
	   :defplugs
	   :defplug))
