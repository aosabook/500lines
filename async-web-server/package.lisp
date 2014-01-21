(defpackage :house  
  (:use :cl #:optima #:cl-ppcre #:usocket)
  (:import-from #:alexandria :starts-with-subseq :with-gensyms)
  (:import-from #:anaphora :aif :awhen :aand :it)
  (:export 
   :define-closing-handler
   :define-json-handler
   :define-stream-handler 

   :define-http-type :parameter :restrictions

   :assert-http
   :root :sock :parameters
   :subscribe! :publish!
   :start))

(in-package :house)

(defparameter +max-request-size+ 50000)
(defparameter +max-request-age+ 30)
