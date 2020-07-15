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

(defmacro defjsonclass (name supertypes slots &rest options)
  `(defclass ,name ,supertypes
     ,(loop for slot in slots
	    if (listp slot)
	      collect
	    `(,(car slot)
	      ,@(unless (member :initarg slot)
		  (list :initarg (intern (string (car slot)) :keyword)))
	      ,@(unless (member :json-type slot)
		  (list :json-type :any))
	      ,@(unless (member :json-key slot)
		  (list :json-key (if (member :underscore slot)
				      (symbol-to-snake-case (car slot))
				      (symbol-to-camel-case (car slot)))))
	      ,@(remove :underscore (cdr slot)))
	    else
	      collect
	    `(,slot :initarg ,(intern (string slot) :keyword)
		    :json-type :any
		    :json-key ,(symbol-to-camel-case slot)))
     ,@options
     (:metaclass json-mop:json-serializable-class)))

(defmacro defplug (domain path &key return-type (methods (list :get)))
  (let ((converted-path (str:replace-all "/" "-"
					 (ppcre:regex-replace-all "/{\\w+}/?"
								  path
								  "-"))))
  `(progn
     ,@(loop with arguments = (mapcar #'match-to-symbol
				      (ppcre:all-matches-as-strings "/{\\w+}/?"
								    path))
	     for method in methods
	     for func = (intern (string method) :dexador)
	     collect 
	     `(defun ,(intern (string-upcase
			       (str:concat (string method) "-"
					   (if (str:ends-with-p "-" converted-path)
					       (subseq converted-path 0 (1- (length converted-path)))
					       converted-path))))
		  (,@arguments 
		   ,@(when (or (eql method :post)
			       (eql method :patch))
		       '(data))
		   &key (root *api-root*) headers basic-auth)
		(let ((response (funcall #',func
					 (str:concat ,domain root
						     (format nil ,(ppcre:regex-replace-all "{\\w+}?"
											   path
											   "~A")
							     ,@arguments))
					 :headers headers :basic-auth basic-auth
					 ,@(unless (or (eql method :delete)
						       (eql method :get))
					     '(:content data)))))
		  (,@(if (listp return-type)
			 '(map 'list)
			 '(funcall))
		   #'(lambda (r)
		       (json-mop:json-to-clos r ',(if (listp return-type)
						      (car (last return-type))
						      return-type)))
		   (yason:parse response))))))))
