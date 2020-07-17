;;;; plugger.lisp

(in-package #:plugger)

(defvar *api-root* "/api/v1/"
  "the root of the API path, after the main domain name")

(defun symbol-to-camel-case (symbol)
  "converts SYMBOL to a string in camelCase"
  (declare (ftype (function (symbol) string) symbol-to-camel-case))
  (let ((string-list (str:split #\- (string symbol))))
    (format nil "~(~a~)~{~@(~a~)~}"
	    (first string-list)
	    (rest string-list))))

(defun symbol-to-snake-case (symbol)
  "converts SYMBOL to a string in snake_case"
  (declare (ftype (function (symbol) string) symbol-to-snake-case))
  (let ((string-list (str:split #\- (string symbol))))
    (format nil "~{~(~a~)~^_~}"
	    string-list)))

(defun match-to-symbol (match)
  "converts a string, MATCH, to a symbol"
  (declare (ftype (function (string) symbol) match-to-symbol))
  (intern (string-upcase
	   (str:replace-all "_" "-" (ppcre:regex-replace-all "(/|{|})" match "")))))

(defmacro defjsonclass (name supertypes slots &rest options)
  "wrapper around defclass to quickly define json objects, automatically setting metaclass to json-serializable-class

if a slot is just a symbol INITARG, ACCESSOR, JSON-TYPE, and JSON-KEY is automatically supplied
providing any of those options will override the automatically generated default

JSON-KEY defaults to a camelCase string representation of the slot name
JSON-TYPE defaults to :any"
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
	      ,@(unless (member :accessor slot)
		  (list :accessor (car slot)))
	      ,@(remove :underscore (cdr slot)))
	    else
	      collect
	    `(,slot :initarg ,(intern (string slot) :keyword)
		    :json-type :any
		    :json-key ,(symbol-to-camel-case slot)
		    :accessor ,slot))
     ,@options
     (:metaclass json-mop:json-serializable-class)))

(defmacro defplugs (domain &rest plugs)
  "run defplug using a common domain

each PLUG in PLUGS is a list matching the signature for defplug minus DOMAIN"
  `(progn
     ,@(loop for plug in plugs
	     collect
	     `(defplug ,domain ,@plug))))

(defmacro defplug (domain path return-type &key (methods (list :get)))
  "define a plug for PATH on DOMAIN. RETURN-TYPE is a class name defined by DEFJSONCLASS
METHODS is a list representing any HTTP methods the plug should expand
PATH can contain variables which, when expanded into a function, will be arguments to the function"
  (let ((converted-path (str:replace-all "/" "-"
					 (ppcre:regex-replace-all "/{\\w+}/?"
								  path
								  "-")))
	(arguments (mapcar #'match-to-symbol
			   (ppcre:all-matches-as-strings "/{\\w+}/?"
							 path))))
    `(progn
       ,@(loop for method in methods
	       for func = (intern (string method) :dexador)
	       collect 
	       `(defun ,(intern (string-upcase
				 (str:concat (string method) "-"
					     (ppcre:regex-replace "-$" converted-path ""))))
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
