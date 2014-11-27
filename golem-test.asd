#|
  This file is a part of golem project.
|#

(in-package :cl-user)
(defpackage golem-test-asd
  (:use :cl :asdf))
(in-package :golem-test-asd)

(defsystem golem-test
  :author ""
  :license ""
  :depends-on (:golem
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "golem"))))

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
