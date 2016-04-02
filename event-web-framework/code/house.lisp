;; house.lisp
(in-package :house)

;;;;;;;;;; System tables
(defparameter *channels* (make-hash-table))

;;;;;;;;;; Function definitions
;;;;; Buffer/listen-related
(defmethod start ((port integer))
  (let ((server (socket-listen usocket:*wildcard-host* port :reuse-address t :element-type 'octet))
	(conns (make-hash-table)))
    (unwind-protect
	 (loop (loop for ready in (wait-for-input (cons server (alexandria:hash-table-keys conns)) :ready-only t)
		  do (process-ready ready conns)))
      (loop for c being the hash-keys of conns
	 do (loop while (socket-close c)))
      (loop while (socket-close server)))))

(defmethod process-ready ((ready stream-server-usocket) (conns hash-table))
  (setf (gethash (socket-accept ready :element-type 'octet) conns) nil))

(defmethod process-ready ((ready stream-usocket) (conns hash-table))
  (let ((buf (or (gethash ready conns)
		 (setf (gethash ready conns) (make-instance 'buffer :bi-stream (flex-stream ready))))))
    (if (eq :eof (buffer! buf))
	(ignore-errors
	  (remhash ready conns)
	  (socket-close ready))
	(let ((too-big? (> (total-buffered buf) +max-request-size+))
	      (too-old? (> (- (get-universal-time) (started buf)) +max-request-age+))
	      (too-needy? (> (tries buf) +max-buffer-tries+)))
	  (cond (too-big?
		 (error! +413+ ready)
		 (remhash ready conns))
		((or too-old? too-needy?)
		 (error! +400+ ready)
		 (remhash ready conns))
		((and (request buf) (zerop (expecting buf)))
		 (remhash ready conns)
		 (when (contents buf)
		   (setf (parameters (request buf))
			 (nconc (parse buf) (parameters (request buf)))))
		 (handler-case
		     (handle-request ready (request buf))
		   (http-assertion-error () (error! +400+ ready))
		   ((and (not warning)
		     (not simple-error)) (e)
		     (error! +500+ ready e))))
		(t
		 (setf (contents buf) nil)))))))

(defmethod buffer! ((buffer buffer))
  (handler-case
      (let ((stream (bi-stream buffer)))
	(incf (tries buffer))
	(loop for char = (read-char-no-hang stream) until (null char)
	   do (push char (contents buffer))
	   do (incf (total-buffered buffer))
	   when (request buffer) do (decf (expecting buffer))
	   when (starts-with-subseq
		 '(#\linefeed #\return #\linefeed #\return)
		 (contents buffer))
	   do (multiple-value-bind (parsed expecting) (parse buffer)
		(setf (request buffer) parsed
		      (expecting buffer) expecting)
		(return char))
	   when (> (total-buffered buffer) +max-request-size+) return char
	   finally (return char)))
    (error () :eof)))

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
	   until (null name) do (push (cons (->keyword name) value) (headers req)))
	(setf (parameters req)
	      (append (parse-params (parameters req))
		      (parse-params (pop lines))))
	req))))

(defmethod parse ((buf buffer))
  (let ((str (coerce (reverse (contents buf)) 'string)))
    (if (request buf)
	(parse-params str)
	(parse str))))

;;;;; Handling requests
(defmethod handle-request ((socket usocket) (req request))
  (aif (lookup (resource req) *handlers*)
       (funcall it socket (parameters req))
       (error! +404+ socket)))

(defun crlf (&optional (stream *standard-output*))
  (write-char #\return stream)
  (write-char #\linefeed stream)
  (values))

(defmethod write! ((res response) (socket usocket))
  (handler-case
      (with-timeout (.2)
	(let ((stream (flex-stream socket)))
	  (flet ((write-ln (&rest sequences)
		   (mapc (lambda (seq) (write-sequence seq stream)) sequences)
		   (crlf stream)))
	    (write-ln "HTTP/1.1 " (response-code res))
	    (write-ln
	     "Content-Type: " (content-type res) "; charset=" (charset res))
	    (write-ln "Cache-Control: no-cache, no-store, must-revalidate")
	    (when (keep-alive? res)
	      (write-ln "Connection: keep-alive")
	      (write-ln "Expires: Thu, 01 Jan 1970 00:00:01 GMT"))
	    (awhen (body res)
	      (write-ln "Content-Length: " (write-to-string (length it)))
	      (crlf stream)
	      (write-ln it))
	    (values))))
    (trivial-timeout:timeout-error ()
      (values))))

(defmethod write! ((res sse) (socket usocket))
  (let ((stream (flex-stream socket)))
    (handler-case
	(with-timeout (.2)
	  (format
	   stream "~@[id: ~a~%~]~@[event: ~a~%~]~@[retry: ~a~%~]data: ~a~%~%"
	   (id res) (event res) (retry res) (data res)))
      (trivial-timeout:timeout-error ()
	(values)))))

(defmethod error! ((err response) (socket usocket) &optional instance)
  (declare (ignorable instance))
  (ignore-errors
    (write! err socket)
    (socket-close socket)))

;;;;; Channel-related
(defmethod subscribe! ((channel symbol) (socket usocket))
  (push socket (lookup channel *channels*))
  nil)

(defmethod publish! ((channel symbol) (message string))
  (awhen (lookup channel *channels*)
    (setf (lookup channel *channels*)
	  (loop with msg = (make-instance 'sse :data message)
	     for socket in it
	     when (ignore-errors
		    (write! msg socket)
		    (force-output (socket-stream socket))
		    socket)
	     collect it))))
