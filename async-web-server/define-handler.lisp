(in-package #:house)

;;;;;;;;;; Parameter type parsing.
;;;;; Basics
(defparameter *http-type-priority* (make-hash-table)
  "Priority table for all parameter types. 
Types will be parsed from highest to lowest priority;
parameters with a lower priority can refer to parameters of a higher priority.")

(defgeneric type-expression (parameter type &optional restrictions)
  (:documentation
   "A type-expression will tell the server how to convert a parameter from a string to a particular, necessary type."))
(defgeneric lookup-assertion (parameter type &optional restrictions)
  (:documentation
   "A lookup assertion is run on a parameter immediately after conversion. Use it to restrict the space of a particular parameter."))
(defmethod type-expression (parameter type &optional restrictions) nil)
(defmethod lookup-assertion (parameter type &optional restrictions) nil)

;;;;; Definition macro
(defmacro define-http-type ((type &key (priority 0)) &key type-expression lookup-assertion)
  (assert (numberp priority) nil "`priority` should be a number. The highest will be converted first")
  (with-gensyms (tp)
    `(let ((,tp ,type))
       (setf (gethash ,tp *http-type-priority*) ,priority)
       ,@(when type-expression
	       `((defmethod type-expression (parameter (type (eql ,tp)) &optional restrictions)
		   (declare (ignorable restrictions))
		   ,type-expression)))
       ,@(when lookup-assertion
	       `((defmethod lookup-assertion (parameter (type (eql ,tp)) &optional restrictions)
		   (declare (ignorable restrictions))
		   ,lookup-assertion))))))

;;;;; Common HTTP types
(define-http-type (:string)
    :lookup-assertion (match restrictions
			((list :min min)
			 `(>= (length ,parameter) ,min))
			((list :max max)
			 `(>= ,max (length ,parameter)))
			((list :min min :max max)
			 `(>= ,max (length ,parameter) ,min))
			(_ nil)))

(define-http-type (:integer)
    :type-expression `(parse-integer ,parameter :junk-allowed t)
    :lookup-assertion (match restrictions
			((list :min min)
			 `(>= ,parameter ,min))
			((list :max max)
			 `(>= ,max ,parameter))
			((list :min min :max max)
			 `(>= ,max ,parameter ,min))
			(_ nil)))

(define-http-type (:json)
    :type-expression `(json:decode-json-from-string ,parameter))

(define-http-type (:keyword)
    :type-expression `(->keyword ,parameter))

(define-http-type (:list-of-keyword)
    :type-expression `(loop for elem in (json:decode-json-from-string ,parameter)
			 if (stringp elem) collect (->keyword elem)
			 else do (error (make-instance 'http-assertion-error :assertion `(stringp ,elem)))))

(define-http-type (:list-of-integer)
    :type-expression `(json:decode-json-from-string ,parameter)
    :lookup-assertion `(every #'numberp ,parameter))

;;;;;;;;;; Constructing argument lookups
(defun args-by-type-priority (args &optional (priority-table *http-type-priority*))
  (let ((cpy (copy-list args)))
    (sort cpy #'<= 
	  :key (lambda (arg)
		 (if (listp arg)
		     (gethash (second arg) priority-table)
		     0)))))

(defun arg-exp (arg-sym)
  `(aif (cdr (assoc ,(->keyword arg-sym) parameters))
	(uri-decode it)
	(error (make-instance 'http-assertion-error :assertion ',arg-sym))))

(defun arguments (args body)
  (loop with res = body
     for arg in (args-by-type-priority args)
     do (match arg
	  ((guard arg-sym (symbolp arg-sym))
	   (setf res `(let ((,arg-sym ,(arg-exp arg-sym)))
			,res)))
	  ((list* arg-sym type restrictions)
	   (setf res
		 `(let ((,arg-sym ,(or (type-expression (arg-exp arg-sym) type restrictions) (arg-exp arg-sym))))
		    ,@(awhen (lookup-assertion arg-sym type restrictions) `((assert-http ,it)))
		    ,res))))
     finally (return res)))

;;;;;;;;;; Defining Handlers
(defparameter *handlers* (make-hash-table :test 'equal))
(defmacro make-closing-handler ((&key (content-type "text/html")) (&rest args) &body body)
  `(lambda (sock parameters)
     (declare (ignorable parameters))
     ,(arguments args
		 `(let ((res (make-instance 
			      'response 
			      :content-type ,content-type 
			      :body (progn ,@body))))
		    (write! res sock)
		    (socket-close sock)))))

(defmacro make-stream-handler ((&rest args) &body body)
  `(lambda (sock parameters)
     (declare (ignorable parameters))
     ,(arguments args
		 `(let ((res (progn ,@body)))
		    (write! (make-instance 'response
					   :keep-alive? t :content-type "text/event-stream")
			    sock)
		    (write! (make-instance 'sse :data (or res "Listening...")) sock)
		    (force-output (socket-stream sock))))))

(defmacro bind-handler (name handler)
  (assert (symbolp name) nil "`name` must be a symbol")
  (let ((uri (if (eq name 'root) "/" (format nil "/~(~a~)" name))))
    `(progn
       (when (gethash ,uri *handlers*)
	 (warn ,(format nil "Redefining handler '~a'" uri)))
       (setf (gethash ,uri *handlers*) ,handler))))

(defmacro define-closing-handler ((name &key (content-type "text/html")) (&rest args) &body body)
  `(bind-handler ,name (make-closing-handler (:content-type ,content-type) ,args ,@body)))

(defmacro define-stream-handler ((name) (&rest args) &body body)
  `(bind-handler ,name (make-stream-handler ,args ,@body)))
