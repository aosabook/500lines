(in-package :house)

(defun debug! (&optional (stream *standard-output*))
  (flet ((dbg (label &rest msg) (format stream ">>>> ~a~%~{~s~%----------~%~}~%" label msg)))
    (defmethod process-ready :before ((sock stream-usocket) conns buffers)
	       (dbg "Preparing to buffer..." sock
		    "CONNECTIONS: " (alexandria:hash-table-keys conns)
		    "BUFFERS: " (alexandria:hash-table-keys buffers)))
    (defmethod handle-request :before (sock req) 
	       (dbg "Handling request..." sock req (resource req) (headers req) (parameters req)))
    (defmethod handle-request :after (sock req) 
	       (dbg "Completed request..."))
    (defmethod buffer! :before (buf)
	       (dbg "Buffering..." buf (tries buf)))
    (defmethod buffer! :after (buf)
	       (when (> (tries buf) +max-buffer-tries+)
		 (dbg "Needy buffer..." buf (tries buf) (coerce (reverse (contents buf)) 'string))))
    (defmethod write! :before ((res response) sock) 
	       (dbg "Writing response..."))
    (defmethod error! :before (res sock &optional instance) 
	       (dbg "Sending error response..."
		    instance sock res (response-code res)))
    (defmethod subscribe! :before (chan sock) 
	       (dbg "New subscriber" chan))
    (defmethod publish! :before (chan msg) 
	       (dbg "Publishing to channel" chan msg))
    nil))

(defmethod ->keyword ((thing symbol))
  (intern (symbol-name thing) :keyword))

(defmethod ->keyword ((thing string))
  (intern (string-upcase thing) :keyword))

(defmethod lookup (key (hash hash-table))
  (gethash key hash))

(defmethod lookup (key (session session))
  (gethash key (session-values session)))

(defgeneric (setf lookup) (new-value key session)
  (:documentation "Setter for lookup methods"))

(defmethod (setf lookup) (new-value key (session session))
  (setf (gethash key (session-values session)) new-value))

(defmethod (setf lookup) (new-value key (hash hash-table))
  (setf (gethash key hash) new-value))

(defmethod flex-stream ((sock usocket))
  (flex:make-flexi-stream (socket-stream sock) :external-format :utf-8))

(defmethod uri-decode ((thing null)) nil)

(defmethod uri-decode ((string string))
  (coerce 
   (loop with len = (length string) and i = 0
      until (>= i len)
      for char = (aref string i) for inc-by = 1
      collect  (cond ((eql #\+ char)
		      #\space)
		     ((eql #\% char)
		      (setf inc-by 3)
		      (code-char (parse-integer (subseq string (+ i 1) (+ i 3)) :radix 16)))
		     (t
		      char))
      do (incf i inc-by))
   'string))
