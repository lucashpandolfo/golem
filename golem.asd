#|
  This file is a part of golem project.
|#

#|
  Database models for Common Lisp
|#

(in-package :cl-user)
(defpackage golem-asd
  (:use :cl :asdf))
(in-package :golem-asd)

(defsystem golem
  :version "0.1"
  :author ""
  :license ""
  :depends-on (:sxql
               :dbi
               :trivial-types)
  :components ((:module "src"
                :components
                ((:file "golem" :depends-on ("model" "db" "query"))
                 (:file "model" :depends-on ("db"))
                 (:file "query" :depends-on ("model" "db"))
                 (:file "db"))))
  :description "Database models for Common Lisp"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op golem-test))))
