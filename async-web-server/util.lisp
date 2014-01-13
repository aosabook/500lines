(in-package :house)

(defun debug! (&optional (stream *standard-output*))
  (flet ((dbg (msg) (format stream "~a~%" msg)))
    (defmethod handle-request :before (sock req) (dbg "Handling request..."))
    (defmethod handle-request :after (sock req) (dbg "Completed request..."))
    (defmethod buffer! :before (buf) (dbg "Buffering from socket..."))
    (defmethod write! :before ((res response) sock) (dbg "Writing response..."))
    (defmethod error! :before (res sock) (dbg "Sending error response..."))
    (defmethod subscribe! :before (chan sock) (dbg (format nil "Subscribing to ~s..." chan)))
    (defmethod publish! :before (chan msg) (dbg (format nil "Sending to ~s..." chan)))))

(defmethod ->keyword ((thing symbol))
  (intern (symbol-name thing) :keyword))

(defmethod ->keyword ((thing string))
  (intern (string-upcase thing) :keyword))

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
		      ;;; TODO error trap here
		      (code-char (parse-integer (subseq string (+ i 1) (+ i 3)) :radix 16)))
		     (t
		      char))
      do (incf i inc-by))
   'string))

(defmethod path->uri ((path pathname) &key stem-from)
  (format
   nil "~a" 
   (make-pathname 
    :directory
    (cons :absolute
	  (if stem-from
	      (member stem-from (cdr (pathname-directory path)) :test #'string=)
	      (cdr (pathname-directory path))))
    :name (pathname-name path)
    :type (pathname-type path))))

(defparameter *mimetype-table*
  '(("atom" . "application/atom+xml")
    ("bmp" . "image/bmp")
    ("cmc" . "application/vnd.cosmocaller")
    ("css" . "text/css")
    ("gif" . "image/gif")
    ("htm" . "text/html")
    ("html" . "text/html")
    ("ico" . "image/x-icon")
    ("jpe" . "image/jpeg")
    ("jpeg" . "image/jpeg")
    ("jpg" . "image/jpeg")
    ("js" . "application/javascript")
    ("json" . "application/json")
    ("mid" . "audio/midi")
    ("midi" . "audio/midi")
    ("mov" . "video/quicktime")
    ("mp3" . "audio/mpeg")
    ("mp4" . "video/mp4")
    ("mpe" . "video/mpeg")
    ("mpeg" . "video/mpeg")
    ("mpg" . "video/mpeg")
    ("oga" . "audio/ogg")
    ("ogg" . "audio/ogg")
    ("ogv" . "video/ogg")
    ("ogx" . "application/ogg")
    ("png" . "image/png")
    ("tif" . "image/tiff")
    ("tiff" . "image/tiff")
    ("wav" . "audio/x-wav")
    ("xhtml" . "application/xhtml+xml")
    ("xml" . "application/xml")))

(defmethod path->mimetype ((path pathname))
  (aif (cdr (assoc (pathname-type path) *mimetype-table* :test #'string=))
       it
       "text/plain"))
