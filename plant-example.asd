;;;; plant-example.asd

(asdf:defsystem #:plant-example
  :serial t
  :description "An example of a project that uses plant"
  :author "photex@lofidelitygames.com"
  :license "MIT"
  :depends-on (#:cl-graph
               #:sb-cga
               #:alexandria)
  :components ((:file "package")
               (:file "plant-example")))

