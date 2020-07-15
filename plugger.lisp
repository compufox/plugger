;;;; plugger.lisp

(in-package #:plugger)

(defvar *api-root* "/api/v1/")


(defmacro defplug (domain path &key return-type (methods (list :get)) trailing-argument)
  `(progn
     ,@(loop with converted-path = (str:replace-all "/" "-" path)
	     for method in methods
	     for func = (intern (string method) :dexador)
	     collect 
	     `(defun ,(intern (string-upcase
			       (str:concat (string method) "-"
					   converted-path)))
		  ,(append nil
		    (when trailing-argument
		      '(argument))
		    (when (or (eql method :post)
			      (eql method :patch))
		      '(data))
		    '(&key (root *api-root*) headers basic-auth))
		(json-mop:json-to-clos ,(append `(funcall #',func)
						`((str:concat ,domain root ,path
							      ,(when (and trailing-argument
									   (eql method :get))
								  '(format nil "/~A" argument))))
						'(:headers headers :basic-auth basic-auth)
						(unless (or (eql method :delete)
							    (eql method :get))
						  '(:content data)))
				       ',return-type)))))
