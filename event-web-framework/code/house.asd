;;;; house.asd

(asdf:defsystem #:house
    :serial t
    :description "Custom asynchronous HTTP server for the Deal project."
  :author "Inaimathi <leo.zovic@gmail.com>"
  :license "AGPL3"
  :depends-on (#:alexandria #:anaphora #:cl-base64 #:cl-ppcre #:cl-json #:bordeaux-threads #:cl-fad #:usocket #:cl-ppcre #:optima #:flexi-streams #:lisp-unit #:trivial-timeout)
  :components ((:file "package")
	       (:file "model")
	       (:file "util")
	       (:file "define-handler")
	       (:file "house")
	       (:file "unit-tests")))
