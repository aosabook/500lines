(in-package :house)

(setf *print-failures* t
      *print-summary* t
      *print-errors* t)

(define-test parameter-parsing 
  (assert-equal '((:a . "1")) (parse-params "a=1"))
  (assert-equal '((:a . "1") (:b . "2")) (parse-params "a=1&b=2"))
  (assert-equal '((:longer . "parameter names") (:look . "something like this"))
		(parse-params "longer=parameter names&look=something like this"))
  ;; Nope, no automatic argument decoding (the default define-*-handler does decoding internally though)
  (assert-equal '((:longer . "parameter%20names") (:look . "something%20like%20this"))
		(parse-params "longer=parameter%20names&look=something%20like%20this")))

(define-test uri-decoding
  (assert-equal "test test" (uri-decode "test test"))
  (assert-equal "test test" (uri-decode "test+test")) ;; we expect encodeURIComponent on the content
  (assert-equal "test test" (uri-decode "test%20test"))
  (assert-equal ",./<>?:\";'[]{}~!@#$%^&*()_+-=` "
		(uri-decode "%2C.%2F%3C%3E%3F%3A%22%3B'%5B%5D%7B%7D~!%40%23%24%25%5E%26*()_%2B-%3D%60%20")))

(define-test request-parsing
  ;; Fail on older HTTP versions
  (assert-error 'http-assertion-error 
		(parse "GET /index.html HTTP/0.9
Host: www.example.com

"))
  (assert-error 'http-assertion-error
		(parse "GET /index.html HTTP/1.0
Host: www.example.com

"))

  ;; vanilla GET request
  (let ((req (parse "GET /index.html HTTP/1.1
Host: www.example.com

")))
    (assert-equal "/index.html" (resource req))
    (assert-equal '((:host . "www.example.com")) (headers req)))

  ;; GET with parameters
  (let ((req (parse "GET /index.html?test=1 HTTP/1.1
Host: www.example.com

")))
    (assert-equal "/index.html" (resource req))
    (assert-equal '((:host . "www.example.com")) (headers req))
    (assert-equal '(:test . "1") (assoc :test (parameters req))))

  ;; POST with body
  (let ((req (parse "POST /index.html HTTP/1.1
Host: www.example.com

test=1
")))
    (assert-equal "/index.html" (resource req))
    (assert-equal '((:host . "www.example.com")) (headers req))
    (assert-equal '(:test . "1") (assoc :test (parameters req))))

  ;; POST with parameters and body
  (let ((req (parse "POST /index.html?get-test=get HTTP/1.1
Host: www.example.com

post-test=post
")))
    (assert-equal "/index.html" (resource req))
    (assert-equal '((:host . "www.example.com")) (headers req))
    (assert-equal '(:get-test . "get") (assoc :get-test (parameters req)))
    (assert-equal '(:post-test . "post") (assoc :post-test (parameters req))))

  (let ((req (parse "POST /index.html?test=get HTTP/1.1
Host: www.example.com

test=post
")))
    (assert-equal "/index.html" (resource req))
    (assert-equal '((:host . "www.example.com")) (headers req))
    (assert-equal '((:test . "get") (:test . "post")) (parameters req))
    (assert-equal '(:test . "get") (assoc :test (parameters req)))))

(defmethod read-all ((stream stream))
  (coerce 
   (loop for char = (read-char-no-hang stream nil :eof)
      until (or (null char) (eq char :eof)) collect char into msg
      finally (return (values msg char)))
   'string))

(defmethod write! ((strings list) (stream stream))
  (mapc (lambda (seq)
	  (write-sequence seq stream)
	  (crlf stream))
	strings)
  (crlf stream)
  (force-output stream)
  (values))

(define-test running-server!
  (let* ((port 4321)
	 (server (bt:make-thread (lambda () (start port)))))
    (sleep 0.5)
    (define-closing-handler (test :content-type "text/plain") ()
      "Hello!")
    (define-closing-handler (arg-test :content-type "text/plain") ((num :integer) (key :keyword) (num-list :list-of-integer))
      (format nil "~{~s~^ ~}" (list num key num-list)))
    (define-closing-handler (arg-test-two :content-type "text/plain") ((a :string) b (key-list :list-of-keyword) (json :json))
      (format nil "~{~s~^ ~}" (list a b key-list json)))
    (unwind-protect
	 (labels ((parse-res (res)
		    (destructuring-bind (hdr bdy) (cl-ppcre:split "\\r\\n\\r\\n" res)
		      (list (cl-ppcre:split "\\r\\n" hdr) 
			    (cl-ppcre:regex-replace "\\r\\n" bdy ""))))
		  (req (&rest lines)
		    (with-client-socket (socket stream "localhost" port)
		      (write! lines stream)
		      (when (wait-for-input socket :timeout 2 :ready-only t)
			(parse-res (read-all stream))))))
	   (destructuring-bind (headers body) (req "GET /test HTTP/1.1")
	     (assert-equal "HTTP/1.1 200 OK" (first headers))
	     (assert-equal "Hello!" body))
	   (destructuring-bind (headers body) (req "POST /test HTTP/1.1")
	     (assert-equal "HTTP/1.1 200 OK" (first headers))
	     (assert-equal "Hello!" body))
	   (destructuring-bind (headers body) (req "POST /arg-test HTTP/1.1" "" "num=1&key=test&num-list=%5B1%2C2%2C3%2C4%2C5%5D")
	     (assert-equal "HTTP/1.1 200 OK" (first headers))
	     (assert-equal "1 :TEST (1 2 3 4 5)" body))
	   (destructuring-bind (headers body) (req "POST /arg-test-two HTTP/1.1")
	     (assert-equal "HTTP/1.1 400 Bad Request" (first headers))
	     (assert-equal "Failed HTTP assertion: HOUSE::A
Possibly invalid arguments." body))
	   (destructuring-bind (headers body) (req "POST /arg-test-two HTTP/1.1" "" "a=test&b=blah&key-list=%5B%22one%22%2C%22two%22%2C%22three%22%5D&json=%5B%22one%22%2C%22two%22%2C%22three%22%5D")
	     (assert-equal "HTTP/1.1 200 OK" (first headers))
	     (assert-equal "\"test\" \"blah\" (:ONE :TWO :THREE) (\"one\" \"two\" \"three\")" body)))
      (ignore-errors 
	(loop for h in (list "/test" "/arg-test" "/arg-test-two")
	   do (remhash h *handlers*))
	(bt:destroy-thread server)))))
