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
	       `((defmethod type-expression (parameter (type (eql ,type)) &optional restrictions)
		   (declare (ignorable restrictions))
		   ,type-expression)))
       ,@(when lookup-assertion
	       `((defmethod lookup-assertion (parameter (type (eql ,type)) &optional restrictions)
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
			 collect (->keyword elem)))

(define-http-type (:list-of-integer)
    :type-expression `(loop for elem in (json:decode-json-from-string ,parameter)
			 collect (parse-integer elem :junk-allowed t)))

;;;;;;;;;; Constructing argument lookups
(defun args-by-type-priority (args &optional (priority-table *http-type-priority*))
  (let ((cpy (copy-list args)))
    (sort cpy #'<= :key (lambda (arg) (gethash (second arg) priority-table)))))

(defun arguments (args body)
  (loop with res = body
     for arg in (args-by-type-priority args)
     do (match arg
	  ((list* arg-sym type restrictions)
	   (let ((arg-exp `(uri-decode (cdr (assoc ,(->keyword arg-sym) parameters)))))
	     (setf res
		   `(let ((,arg-sym ,(or (type-expression arg-exp type restrictions) arg-exp)))
		      ,@(awhen (lookup-assertion arg-sym type restrictions) `((assert-http ,it)))
		      ,res))))
	  ((guard arg-sym (symbolp arg-sym))
	   (setf res `(let ((,arg-sym (cdr (assoc ,(->keyword arg-sym) parameters)))) 
			,res))))
     finally (return res)))

;;;;;;;;;; Defining Handlers
(defparameter *handlers* (make-hash-table :test 'equal))
(defmacro make-closing-handler ((&key (content-type "text/html")) (&rest args) &body body)
  (with-gensyms (cookie?)
    `(lambda (sock ,cookie? session parameters)
       (declare (ignorable session parameters))
       ,(arguments args
		   `(let ((res (make-instance 
				'response 
				:content-type ,content-type 
				:cookie (unless ,cookie? (token session))
				:body (progn ,@body))))
		      (write! res sock)
		      (socket-close sock))))))

(defmacro make-stream-handler ((&rest args) &body body)
  (with-gensyms (cookie?)
    `(lambda (sock ,cookie? session parameters)
       (declare (ignorable session))
       ,(arguments args
		   `(let ((res (progn ,@body)))
		      (write! (make-instance 'response
					     :keep-alive? t :content-type "text/event-stream" 
					     :cookie (unless ,cookie? (token session))) sock)
		      (write! (make-instance 'sse :data (or res "Listening...")) sock)
		      (force-output (socket-stream sock)))))))

(defmacro bind-handler (name handler)
  (assert (symbolp name) nil "`name` must be a symbol")
  (let ((uri (if (eq name 'root) "/" (format nil "/~(~a~)" name))))
    `(progn
       (when (gethash ,uri *handlers*)
	 (warn ,(format nil "Redefining handler '~a'" uri)))
       (setf (gethash ,uri *handlers*) ,handler))))

(defmacro define-closing-handler ((name &key (content-type "text/html")) (&rest args) &body body)
  `(bind-handler ,name (make-closing-handler (:content-type ,content-type) ,args ,@body)))

(defmacro define-json-handler ((name) (&rest args) &body body)
  `(bind-handler ,name (make-closing-handler (:content-type "application/json") ,args (json:encode-json-to-string (progn ,@body)))))

(defmacro define-stream-handler ((name) (&rest args) &body body)
  `(bind-handler ,name (make-stream-handler ,args ,@body)))

;;;;; Special case handlers
;;; Don't use these in production. There are better ways.
(defmethod define-file-handler ((path pathname) &key stem-from)
  (cond ((cl-fad:directory-exists-p path)
	 (cl-fad:walk-directory 
	  path 
	  (lambda (fname)
	    (define-file-handler fname :stem-from (or stem-from (format nil "~a" path))))))
	((cl-fad:file-exists-p path)
	 (setf (gethash (path->uri path :stem-from stem-from) *handlers*)
	       (let ((mime (path->mimetype path)))
		 (lambda (sock cookie? session parameters)
		   (declare (ignore cookie? session parameters))
		   (with-open-file (s path :direction :input :element-type 'octet)
		     (let ((buf (make-array (file-length s) :element-type 'octet)))
		       (read-sequence buf s)
		       (write! (make-instance 'response :content-type mime :body buf) sock))
		     (socket-close sock))))))
	(t
	 (warn "Tried serving nonexistent file '~a'" path)))
  nil)

(defmethod define-file-handler ((path string) &key stem-from)
  (define-file-handler (pathname path) :stem-from stem-from))

(defmacro define-redirect-handler ((name &key permanent?) target)
  (with-gensyms (cookie?)
    `(bind-handler 
      ,name
      (lambda (sock ,cookie? session parameters)
	(declare (ignorable sock ,cookie? session parameters))
	(write! (make-instance 
		 'response :response-code ,(if permanent? "301 Moved Permanently" "307 Temporary Redirect")
		 :location ,target :content-type "text/plain"
		 :body "Resource moved...")
		sock)
	(socket-close sock)))))
