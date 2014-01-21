(in-package :house)

(defun debug! (&optional (stream *standard-output*))
  (flet ((dbg (msg) (format stream "~a~%" msg)))
    (defmethod handle-request :before (sock req)
	       (dbg "Handling request..."))
    (defmethod handle-request :after (sock req)
	       (dbg "Completed request..."))
    (defmethod buffer! :before (buf sock)
	       (dbg "Buffering from socket..."))
    (defmethod write! :before ((res response) sock)
	       (dbg "Writing response..."))
    (defmethod error! :before (res sock)
	       (dbg "Sending error response..."))
    (defmethod subscribe! :before (chan sock)
	       (dbg (format nil "Subscribing to ~s..." chan)))
    (defmethod publish! :before (chan msg) 
	       (dbg (format nil "Sending to ~s..." chan)))))

(defmethod ->keyword ((thing symbol))
  (intern (symbol-name thing) :keyword))

(defmethod ->keyword ((thing string))
  (intern (string-upcase thing) :keyword))

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
		      ;;; TODO error trap here
		      (code-char (parse-integer (subseq string (+ i 1) (+ i 3)) :radix 16)))
		     (t
		      char))
      do (incf i inc-by))
   'string))
