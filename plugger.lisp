;;;; plugger.lisp

(in-package #:plugger)

(defvar *api-root* "/api/v1/")

(defun symbol-to-camel-case (symbol)
  "converts SYMBOL to a string in camel case"
  (declare (ftype (function (symbol) string) symbol-to-camel-case))
  (let ((string-list (str:split #\- (string symbol))))
    (format nil "~(~a~)~{~@(~a~)~}"
	    (first string-list)
	    (rest string-list))))

(defun symbol-to-snake-case (symbol)
  (declare (ftype (function (symbol) string) symbol-to-snake-case))
  (let ((string-list (str:split #\- (string symbol))))
    (format nil "~{~(~a~)~^_~}"
	    string-list)))

(defun match-to-symbol (match)
  (intern (string-upcase
	   (str:replace-all "_" "-" (ppcre:regex-replace-all "(/|{|})" match "")))))

;; TODO rework this to accept slots with nothing but slot name
;;  and turns it into a full slot definition
(defmacro defjsonclass (name supertypes &rest options)
  `(defclass ,name ,supertypes
     ,(loop for slot in (car options)
	    if (or (member :json-type slot)
		   (member :json-key slot))
	      collect slot
	    else
	      collect
	    `(,@(remove :underscore slot)
	      :initarg ,(car slot)
	      :json-type :any
	      :json-key ,(if (member :underscore slot)
			     (symbol-to-snake-case (car slot))
			     (symbol-to-camel-case (car slot)))))
     (:metaclass json-mop:json-serializable-class)
     ,@(cdr options)))
	     
(defmacro defplug (domain path &key return-type (methods (list :get)))
  `(progn
     ,@(loop with converted-path = (str:replace-all "/" "-"
						    (ppcre:regex-replace-all "/{\\w+}/"
									     path
									     "-"))
	     with arguments = (mapcar #'match-to-symbol
				      (ppcre:all-matches-as-strings "/{\\w+}/"
								    path))
	     for method in methods
	     for func = (intern (string method) :dexador)
	     collect 
	     `(defun ,(intern (string-upcase
			       (str:concat (string method) "-"
					   converted-path)))
		  (,@arguments 
		   ,@(when (or (eql method :post)
			       (eql method :patch))
		       '(data))
		   &key (root *api-root*) headers basic-auth)
		(json-mop:json-to-clos
		 (funcall #',func
			  (str:concat ,domain root
				      (format nil ,(ppcre:regex-replace-all "{\\w+}"
									    path
									    "~A")
					      ,@arguments))
			  :headers headers :basic-auth basic-auth
			  ,@(unless (or (eql method :delete)
					(eql method :get))
			      '(:content data)))
		 ',return-type)))))
