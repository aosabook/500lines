(in-package #:house)

;;;;;;;;;; Parameter type parsing.
;;;;; Basics
(defgeneric type-expression (parameter type)
  (:documentation
   "A type-expression will tell the server how to convert a parameter from a string to a particular, necessary type."))
(defgeneric type-assertion (parameter type)
  (:documentation
   "A lookup assertion is run on a parameter immediately after conversion. Use it to restrict the space of a particular parameter."))
(defmethod type-expression (parameter type) nil)
(defmethod type-assertion (parameter type) nil)

;;;;; Definition macro
(defmacro define-http-type ((type) &key type-expression type-assertion)
  (with-gensyms (tp)
    `(let ((,tp ,type))
       ,@(when type-expression
	       `((defmethod type-expression (parameter (type (eql ,tp))) ,type-expression)))
       ,@(when type-assertion
	       `((defmethod type-assertion (parameter (type (eql ,tp))) ,type-assertion))))))

;;;;; Common HTTP types
(define-http-type (:string))

(define-http-type (:integer)
    :type-expression `(parse-integer ,parameter :junk-allowed t)
    :type-assertion `(numberp ,parameter))

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
    :type-assertion `(every #'numberp ,parameter))

;;;;;;;;;; Constructing argument lookups
(defun arg-exp (arg-sym)
  `(aif (cdr (assoc ,(->keyword arg-sym) parameters))
	(uri-decode it)
	(error (make-instance 'http-assertion-error :assertion ',arg-sym))))

(defun arguments (args body)
  (loop with res = body
     for arg in args
     do (match arg
	  ((guard arg-sym (symbolp arg-sym))
	   (setf res `(let ((,arg-sym ,(arg-exp arg-sym))) ,res)))
	  ((list* arg-sym type restrictions)
	   (setf res
		 `(let ((,arg-sym ,(or (type-expression (arg-exp arg-sym) type) (arg-exp arg-sym))))
		    ,@(awhen (type-assertion arg-sym type) `((assert-http ,it)))
		    ,@(loop for r in restrictions collect `(assert-http ,r))
		    ,res))))
     finally (return res)))

;;;;;;;;;; Defining Handlers
(defparameter *handlers* (make-hash-table :test 'equal))
(defmacro make-closing-handler ((&key (content-type "text/html")) (&rest args) &body body)
  `(lambda (socket parameters)
     (declare (ignorable parameters))
     ,(arguments args
		 `(let ((res (make-instance 
			      'response 
			      :content-type ,content-type 
			      :body (progn ,@body))))
		    (write! res socket)
		    (socket-close socket)))))

(defmacro make-stream-handler ((&rest args) &body body)
  `(lambda (socket parameters)
     (declare (ignorable parameters))
     ,(arguments args
		 `(let ((res (progn ,@body)))
		    (write! (make-instance 'response
					   :keep-alive? t :content-type "text/event-stream")
			    socket)
		    (write! (make-instance 'sse :data (or res "Listening...")) socket)
		    (force-output (socket-stream socket))))))

(defmacro bind-handler (name handler)
  (assert (symbolp name) nil "`name` must be a symbol")
  (let ((uri (if (eq name 'root) "/" (format nil "/~(~a~)" name))))
    `(progn
       (when (gethash ,uri *handlers*)
	 (warn ,(format nil "Redefining handler '~a'" uri)))
       (setf (gethash ,uri *handlers*) ,handler))))

(defmacro define-handler ((name &key (close-socket? t) (content-type "text/html")) (&rest args) &body body)
  (if close-socket?
      `(bind-handler ,name (make-closing-handler (:content-type ,content-type) ,args ,@body))
      `(bind-handler ,name (make-stream-handler ,args ,@body))))
