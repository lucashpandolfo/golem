(in-package :cl-user)
(defpackage golem-test
  (:use :cl
        :golem.model
        :golem.query
        :golem.db
        :prove))
(in-package :golem-test)

;; NOTE: To run this test file, execute `(asdf:test-system :golem)' in your Lisp.

(plan nil)

;;; Define some models

(defmodel person ()
  (first-name :type (:char 30) :null-allowed nil)
  (last-name  :type (:char 30)))

(defmodel musician ()
  (first-name :type (:char 50))
  (last-name  :type (:char 50))
  (instrument :type (:char 100)))

(defmodel album ()
  (artist       :foreign-key musician)
  (name         :type (:char 100))
  (num-stars    :type integer))

(defmodel topping ()
  (name  :type (:char 20))
  (price :type float))

(defmodel pizza ()
  (name     :type (:char 20))
  (toppings :many-to-many topping))

(connect-toplevel  :sqlite3 :database-name ":memory:")

(is (initialize-models 'person 'musician 'album 'topping 'pizza) '((T) (T) (T) (T) (T T)) :test #'equalp "Initialize models")

(ok (get-model 'pizza)  "Retrieve a defined model")

(ok (get-model 'album)  "Retrieve another defined model")

(ok (get-model 'person) "Retrieve yet another defined model")

(is (model-name (get-model 'musician)) 'musician "Correctly retrieve model name" :test #'eq)

(ok (create 'person :first-name "John" :last-name "Doe") "Create an object")

(is-error (create 'person :last-name "Doe") 'error "Create an object without defining a required field should fail")

(is-error (create 'person :first-name "John" :last-name "Doe" :some-random-slot "Nope") 'error "Create an object with wrong slots should fail")

(is-error (create 'random-udefined-model) 'error "Create an object of an undefined model should fail")

;;; Simple objects
(let ((one-person (create 'person :first-name "John" :last-name "Doe"))
      (another-person (create 'person :first-name "Natalia" :last-name "Natalia")))
  
  (ok (save one-person) "Save an object")
  
  (is-type (get-property one-person :id) 'number "Auto assign primary key")
  
  (save another-person)

  (isnt (get-property one-person :id) (get-property another-person :id) "Auto asigned primary keys should be different for different objects")

  (setf (get-property one-person :first-name) "Jonh")
  
  (is (get-property one-person :first-name) "Jonh" :test #'string= "Change slot value")

  (let ((previous-id (get-property one-person :id)))
    (ok (save one-person) "Update an object")
    (is (get-property one-person :id) previous-id "Primary key should not change after an update")
    )
  
  (is-error (get-property one-person :undefined-random-slot-name) 'error "Attempts to use undefined slots should fail")
  )

;;; Objects with foreign keys
(let ((musician (create 'musician :first-name "Miku" :last-name "Hatsune" :instrument "Voice"))
      (album    (create 'album    :name "First Album" :num-stars 5)))
  
  (is-error (setf (get-property album :artist) "Some garbage") 'error "Foreign keys should be only of the defined type")

  (ok (setf (get-property album :artist) musician) "Correct foreign key assignment")
  
  (ok (save album) "Correctly save an object with foreign keys")
  
  (is (get-property album :artist) musician :test #'eq "Foreign key assignment should remain the same after save")
  
  (is-type (get-property musician :id) 'number "Foreign object should be saved along the main object")
  
  )

(disconnect-toplevel)

(finalize)
