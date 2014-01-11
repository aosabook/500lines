(in-package :house)

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