;;;; plugger.lisp

(in-package #:plugger)

(defvar +variable-regex+ "{.+?}")

(defvar *api-root* "/api/v1/"
  "the root of the API path, after the main domain name")

(defun spec-to-function-name (spec)
  "converts url SPEC into a suitable name for a function

removes trailing -
removes all /
removes all =
converts ? to -by-
converts & to and-
converts any \{variables\} to -"
  (declare (ftype (function (string) string) spec-to-function-name))
  (str:replace-all "=" ""
    (str:replace-all "?" "-by-"
      (str:replace-all "&" "and-"
        (str:replace-all "/" ""
	  (ppcre:regex-replace "-$"
	    (ppcre:regex-replace-all +variable-regex+
				     spec
				     "-")
	    ""))))))

(defun match-to-symbol (match)
  "converts a string, MATCH, to a symbol"
  (declare (ftype (function (string) symbol) match-to-symbol))
  (intern (string-upcase
	   (str:replace-all "_" "-" (ppcre:regex-replace-all "(/|{|})" match "")))))

(defmacro defjsonclass (name superclasses slots &rest options)
  "wrapper around defclass to quickly define json objects, automatically setting metaclass to json-serializable-class

if a slot is just a symbol INITARG, ACCESSOR, JSON-TYPE, and JSON-KEY is automatically supplied
providing any of those options will override the automatically generated default

if :EXPORT-ACCESSORS is provided in OPTIONS all slot readers/writers/accessors will be exported from the package

JSON-KEY defaults to a camelCase string representation of the slot name
JSON-TYPE defaults to :any"
  `(prog1
       (defclass ,name ,superclasses
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
					  (snake-case (string (car slot)))
					  (camel-case (string (car slot))))))
		  ,@(unless (or (member :accessor slot)
				(member :reader slot)
				(member :writer slot))
		      (list :accessor (car slot)))
		  ,@(remove :underscore (cdr slot)))
		else
		  collect
		`(,slot :initarg ,(intern (string slot) :keyword)
			:json-type :any
			:json-key ,(camel-case (string slot))
			:accessor ,slot))
	 ,@(remove :export-accessors options :key #'car)
	 (:metaclass json-mop:json-serializable-class))
     ,(when (member :export-accessors options :key #'car)
	`(export
	  ',(loop for slot in slots
		  if (listp slot)
		    collect (or (getf (cdr slot) :accessor)
				(getf (cdr slot) :reader)
				(getf (cdr slot) :writer)
				(car slot))
		  else
		    collect slot)))))
  
(defmacro defplugs (domain &rest plugs)
  "run defplug using a common domain

each PLUG in PLUGS is a list matching the signature for defplug minus DOMAIN"
  `(progn
     ,@(loop for plug in plugs
	     collect
	     `(defplug ,domain ,@plug))))

(defmacro defplug (domain path return-type &key (methods '(:get)) fn-name)
  "define a plug for PATH on DOMAIN. RETURN-TYPE is a class name defined by DEFJSONCLASS
METHODS is a list representing any HTTP methods the plug should expand
PATH can contain variables which, when expanded into a function, will be arguments to the function

if FN-NAME is provided that name is used instead of a function name being generated from the PATH"
  (let ((arguments (mapcar #'match-to-symbol
			   (ppcre:all-matches-as-strings +variable-regex+
							 path))))
    `(progn
       ,@(loop for method in methods
	       for func = (intern (string method) :dexador)
	       collect 
	       `(defun ,(or fn-name
			    (intern (string-upcase
				     (concatenate 'string
						  (string method) "-"
						  (param-case (spec-to-function-name path))))))
		    (,@arguments 
		     ,@(when (or (eql method :post)
				 (eql method :patch))
			 '(data))
		     &key (root *api-root*) headers basic-auth)
		  (let ((yason:*parse-json-arrays-as-vectors* t)
			(response (funcall #',func
					   (str:concat ,domain root
						       (format nil ,(ppcre:regex-replace-all +variable-regex+
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
