;;;; main.lisp

(defpackage :plugger-test
  (:use :cl :prove))
(in-package :plugger-test)

(defvar *app* (make-instance 'ningle:app))
(defvar *json-format*
  "{\"id\":~a, \"name\":\"~a\", \"value\":~a}")

;; sets up our routes
(setf (ningle:route *app* "/api/v1/endpoint")
      #'(lambda (parms)
	  (declare (ignore parms))
	  (format nil *json-format*
		  1 "Test McName" 15))

      (ningle:route *app* "/api/v1/endpoint" :method :POST)
      #'(lambda (parms)
	  (format t "~a" parms)
	  (let ((id (cdr (assoc "id" parms :test #'string=)))
		(name (cdr (assoc "name" parms :test #'string=)))
		(value (cdr (assoc "value" parms :test #'string=))))
	    (format nil *json-format*
		    id name value))))

;; create a json class 
(plugger:defjsonclass test-object ()
  (id name value))

;; make a plug that covers GET and POST HTTP methods
(plugger:defplug "http://localhost:5000" "endpoint" test-object :methods (:get :post))

(plan 6)

;; starts our simple webapp
(let ((handler (clack:clackup *app*)))
  (let ((response (get-endpoint)))
    (is (id response) 1)
    (is (name response) "Test McName")
    (is (value response) 15))
  
  (let* ((post-data (make-instance 'test-object
				   :id 2
				   :name "Jimmi Hendrix"
				   :value 30))
	 (encoded (with-output-to-string (output)
		    (yason:encode post-data output)))
	 (response (post-endpoint encoded)))
    (is (id response) 2)
    (is (name response) "Jimmi Hendrix")
    (is (value response) 30))
  (clack:stop handler))

(finalize)
