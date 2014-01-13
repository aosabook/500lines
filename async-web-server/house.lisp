;; house.lisp
(in-package :house)

;;;;;;;;;; System tables
(defparameter *channels* (make-hash-table))

;;;;;;;;;; Function definitions
;;; The basic structure of the server is
; buffering-listen -> parse -> handle -> channel

;;;;; Buffer/listen-related
(defmethod start ((port integer))
  (let ((server (socket-listen usocket:*wildcard-host* port :reuse-address t))
	(conns (make-hash-table))
        (buffers (make-hash-table)))
    (unwind-protect
	 (loop (loop for ready in (wait-for-input (cons server (alexandria:hash-table-keys conns)) :ready-only t)
		  do (if (typep ready 'stream-server-usocket)
			 (setf (gethash (socket-accept ready :element-type 'octet) conns) :on)
			 (let ((buf (gethash ready buffers (make-instance 'buffer :bi-stream (flex-stream ready)))))
			   (when (eq :eof (buffer! buf))
			     (remhash ready conns)
			     (remhash ready buffers))
			   (let ((complete? (complete? buf))
				 (big? (too-big? buf))
				 (old? (too-old? buf)))
			     (when (or complete? big? old?)
			       (remhash ready conns)
			       (remhash ready buffers)
			       (cond (big? 
				      (error! +413+ ready))
				     (old? 
				      (error! +400+ ready))
				     (t (handler-case
					    (handle-request ready (parse buf))
					  ((not simple-error) ()
					    (error! +400+ ready)))))))))))
      (loop for c being the hash-keys of conns
	 do (loop while (socket-close c)))
      (loop while (socket-close server)))))

(defmethod complete? ((buffer buffer)) (found-crlf? buffer))

(defmethod too-big? ((buffer buffer))
  (> (content-size buffer) +max-request-size+))

(defmethod too-old? ((buffer buffer))
  (> (- (get-universal-time) (started buffer)) +max-request-size+))

(defmethod buffer! ((buffer buffer))
  (unwind-protect
       (let ((stream (bi-stream buffer))
	     (partial-crlf (list #\return #\newline #\return)))
	 (loop for char = (read-char-no-hang stream nil :eof)
	    do (when (and (eql #\newline char)
			  (starts-with-subseq partial-crlf (contents buffer)))
		 (setf (found-crlf? buffer) t))
	    until (or (null char) (eql :eof char))
	    do (push char (contents buffer)) do (incf (content-size buffer))
	    finally (return char)))
    :eof))

;;;;; Parse-related
(defmethod parse-params ((params null)) nil)
(defmethod parse-params ((params string))
  (loop for pair in (split "&" params)
     for (name val) = (split "=" pair)
     collect (cons (->keyword name) (or val ""))))

(defmethod parse ((str string))
  (let ((lines (split "\\r?\\n" str)))
    (destructuring-bind (req-type path http-version) (split " " (pop lines))
      (declare (ignore req-type))
      (assert-http (string= http-version "HTTP/1.1"))
      (let* ((path-pieces (split "\\?" path))
	     (resource (first path-pieces))
	     (parameters (second path-pieces))
	     (req (make-instance 'request :resource resource :parameters parameters)))
	(loop for header = (pop lines) for (name value) = (split ": " header)
	   until (null name)
	   for n = (->keyword name)
	   do (push (cons n value) (headers req)))
	(setf (parameters req)
	      (append (parse-params (parameters req))
		      (parse-params (pop lines))))
	req))))

(defmethod parse ((buf buffer))
  (parse (coerce (reverse (contents buf)) 'string)))

;;;;; Handling requests
(defmethod handle-request ((sock usocket) (req request))
  (aif (gethash (resource req) *handlers*)
       (funcall it sock (parameters req))
       (error! +404+ sock)))

(defun crlf (&optional (stream *standard-output*))
  (write-char #\return stream)
  (write-char #\linefeed stream)
  (values))

(defmethod write! ((res response) (sock usocket))
  (let ((stream (flex-stream sock)))
    (flet ((write-ln (&rest sequences)
	     (mapc (lambda (seq) (write-sequence seq stream)) sequences)
	     (crlf stream)))
      (write-ln "HTTP/1.1 " (response-code res))  
      (write-ln "Content-Type: " (content-type res) "; charset=" (charset res))
      (write-ln "Cache-Control: no-cache, no-store, must-revalidate")
      (awhen (cookie res)
	(write-ln "Set-Cookie: " it))
      (awhen (location res)
	(write-ln "Location: " it))
      (when (keep-alive? res) 
	(write-ln "Connection: keep-alive")
	(write-ln "Expires: Thu, 01 Jan 1970 00:00:01 GMT"))
      (awhen (body res)
	(write-ln "Content-Length: " (write-to-string (length it)))
	(crlf stream)
	(write-ln it))
      (values))))

(defmethod write! ((res sse) (sock usocket))
  (let ((stream (flex-stream sock)))
    (format stream "~@[id: ~a~%~]~@[event: ~a~%~]~@[retry: ~a~%~]data: ~a~%~%"
	    (id res) (event res) (retry res) (data res))))

(defmethod error! ((err response) (sock usocket))
  (ignore-errors 
    (write! err sock)
    (socket-close sock)))

;;;;; Channel-related
(defmethod subscribe! ((channel symbol) (sock usocket))
  (push sock (gethash channel *channels*))
  nil)

(defmethod publish! ((channel symbol) (message string))
  (awhen (gethash channel *channels*)
    (setf (gethash channel *channels*)
	  (loop with msg = (make-instance 'sse :data message)
	     for sock in it
	     when (ignore-errors 
		    (write! msg sock)
		    (force-output (socket-stream sock))
		    sock)
	     collect it))))
