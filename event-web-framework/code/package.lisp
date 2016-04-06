(defpackage :house
  (:use :cl #:optima #:cl-ppcre #:usocket #:lisp-unit)
  (:import-from #:alexandria :starts-with-subseq :with-gensyms)
  (:import-from #:flexi-streams :octet)
  (:import-from #:anaphora :aif :awhen :aand :it)
  (:import-from #:trivial-timeout :with-timeout)
  (:export
   :define-closing-handler
   :define-json-handler
   :define-stream-handler
   :define-redirect-handler
   :define-file-handler

   :define-http-type :parameter :restrictions

   :assert-http
   :root :socket :session :parameters
   :new-session! :new-session-hook! :clear-session-hooks! :get-session! :lookup
   :subscribe! :publish!
   :start))

(in-package :house)

(defparameter +max-request-size+ 50000)
(defparameter +max-request-age+ 30)
(defparameter +max-buffer-tries+ 10)
