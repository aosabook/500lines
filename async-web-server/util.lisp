(in-package :house)

(defun debug! (&optional (stream *standard-output*))
  (flet ((dbg (label &rest msg) (format stream ">>>> ~a~%~{~s~%----------~%~}~%" label msg)))
    (defmethod process-ready :before ((socket stream-usocket) conns buffers)
	       (dbg "Preparing to buffer..." socket
		    "CONNECTIONS: " (alexandria:hash-table-keys conns)
		    "BUFFERS: " (alexandria:hash-table-keys buffers)))
    (defmethod handle-request :before (socket req) 
	       (dbg "Handling request..." socket req (resource req) (headers req) (parameters req)))
    (defmethod handle-request :after (socket req) 
	       (dbg "Completed request..."))
    (defmethod buffer! :before (buf)
	       (dbg "Buffering..." buf (tries buf)))
    (defmethod buffer! :after (buf)
	       (when (> (tries buf) +max-buffer-tries+)
		 (dbg "Needy buffer..." buf (tries buf) (coerce (reverse (contents buf)) 'string))))
    (defmethod write! :before ((res response) socket) 
	       (dbg "Writing response..."))
    (defmethod error! :before (res socket &optional instance) 
	       (dbg "Sending error response..."
		    instance socket res (response-code res)))
    (defmethod subscribe! :before (chan socket) 
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

(defmethod flex-stream ((socket usocket))
  (flex:make-flexi-stream (socket-stream socket) :external-format :utf-8))

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
