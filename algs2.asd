;;;; algs2.asd

(asdf:defsystem #:algs2
  :description "Describe algs2 here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:cl-ppcre
	       #:alexandria
               #:minheap)
  :serial t
  :components ((:file "package")
	       ;(:file :pathname "/home/bear/lisp/clocc/src/ext/union-find/union-find.lisp")
	       ;(:file "/home/bear/lisp/clocc/src/ext/union-find/union-find.lisp")
               (:file "algs2")))

