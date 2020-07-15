;;;; plugger.lisp

(in-package #:plugger)

(defvar *api-root* "/api/v1/")

(defun replace-char (o n s)
  (let ((place (search o s :test #'string=)))
    (if place
	(replace-char o n
		      (uiop:strcat (subseq s 0 place)
				   n
				   (subseq s (1+ place))))
	s)))

(defmacro defplug (domain path &key return-type (methods (list :get)) trailing-argument)
  `(progn
     ,@(loop with converted-path = (replace-char "/" "-" path)
	     for method in methods
	     for func = (intern (string method) :dexador)
	     collect 
	     `(defun ,(intern (string-upcase
			       (uiop:strcat (string method) "-"
					    converted-path)))
		  ,(append nil
		    (when trailing-argument
		      '(argument))
		    (when (or (eql method :post)
			      (eql method :patch))
		      '(data))
		    '(&key (root *api-root*) headers basic-auth))
		(json-mop:json-to-clos ,(append `(funcall #',func)
						`((uiop:strcat ,domain root ,path
							       ,(when (and trailing-argument
									   (eql method :get))
								  '(format nil "/~A" argument))))
						'(:headers headers :basic-auth basic-auth)
						(unless (or (eql method :delete)
							    (eql method :get))
						  '(:content data)))
				       ',return-type)))))
