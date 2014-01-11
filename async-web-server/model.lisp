(in-package :house)

(define-condition http-assertion-error (error)
  ((assertion :initarg :assertion :initform nil :reader assertion))
  (:report (lambda (condition stream)
	     (format stream "Failed assertions '~s'"
		     (assertion condition)))))

(defmacro assert-http (assertion)
  `(unless ,assertion
     (error (make-instance 'http-assertion-error :assertion ',assertion))))

(defclass buffer ()
  ((contents :accessor contents :initform nil)
   (bi-stream :reader bi-stream :initarg :bi-stream)
   (found-crlf? :accessor found-crlf? :initform nil)
   (content-size :accessor content-size :initform 0)
   (started :reader started :initform (get-universal-time))))

(defclass session ()
  ((started :reader started :initform (get-universal-time))
   (last-poked :accessor last-poked :initform (get-universal-time))
   (token :reader token :initarg :token)
   (session-values :reader session-values :initform (make-hash-table :test 'equal))))

(defclass request ()
  ((resource :accessor resource :initarg :resource)
   (headers :accessor headers :initarg :headers :initform nil)
   (token :accessor token :initarg :token :initform nil)
   (session-token :accessor session-token :initarg :session-token :initform nil)
   (parameters :accessor parameters :initarg :parameters :initform nil)))

(defclass response ()
  ((content-type :accessor content-type :initform "text/html" :initarg :content-type)
   (charset :accessor charset :initform "utf-8")
   (response-code :accessor response-code :initform "200 OK" :initarg :response-code)
   (cookie :accessor cookie :initform nil :initarg :cookie)
   (location :accessor location :initform nil :initarg :location)
   (cache-control :accessor cache-control :initform nil)
   (keep-alive? :accessor keep-alive? :initform nil :initarg :keep-alive?)
   (expires :accessor expires :initform nil)
   (body :accessor body :initform nil :initarg :body)))

(defclass sse ()
  ((id :reader id :initarg :id :initform nil)
   (event :reader event :initarg :event :initform nil)
   (retry :reader retry :initarg :retry :initform nil)
   (data :reader data :initarg :data)))

;;;;;;;;;; HTTP basic responses
(defparameter +404+
  (make-instance 'response :response-code "404 Not Found"
		 :content-type "text/plain" :body "Resource not found..."))

(defparameter +400+
  (make-instance 'response :response-code "400 Bad Request"
		 :content-type "text/plain" :body "Malformed, or slow HTTP request..."))

(defparameter +413+
  (make-instance 'response :response-code "413 Request Entity Too Large"
		 :content-type "text/plain" :body "Your request is too long..."))

(defparameter +500+
  (make-instance 'response :response-code "500 Internal Server Error"
		 :content-type "text/plain" :body "Something went wrong on our end..."))