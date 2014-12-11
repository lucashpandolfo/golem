(in-package :cl-user)
(defpackage golem-test
  (:use :cl
        :golem.model
        :golem.query
        :golem.db
        :prove))
(in-package :golem-test)

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

(let* ((cheese    (create 'topping :name "Chesse" :price 1))
       (onion     (create 'topping :name "Onion"  :price 2))
       (bacon     (create 'topping :name "Bacon"  :price 1.3))
       (pepperoni (create 'topping :name "Pepperoni" :price 2.5))
       (pizza     (create 'pizza   :name "Plain old pizza"))
       (pizza-2   (create 'pizza   :name "Medium pizza"))
       (pizza-3   (create 'pizza   :name "Pizza Deluxe" ))
      )
  
  (is-error (setf (get-property pizza :toppings) '("Garbage" 2 "More garbage")) 'error "Many to many should allow only objects of the correct type")
  
  (ok (setf (get-property pizza :toppings) (list cheese)) "Assign many members of the correct type to the m2m field")
  
  (ok (save pizza) "Save an object with a many to many field")
  
  (is-type (get-property pizza :id) 'number "Primary key should be assigned after saving the object")
  
  (is-type (get-property cheese :id) 'number "Primary key should be assigned to the m2m objects too")
  
  (is (get-property pizza :toppings) (list cheese) :test #'equal "Many to many objects should not change after save")
  
  (setf (get-property pizza-2 :toppings) (list cheese onion))
  
  (ok (save pizza-2) "Save an object with a many to many field with two objects")
  
  (setf (get-property pizza-3 :toppings) (list cheese onion bacon pepperoni))
  
  (ok (save pizza-3) "Save an object with a many to many field with more than two objects")
  
  (let ((cheese-id    (get-property cheese    :id))
        (onion-id     (get-property onion     :id))
        (bacon-id     (get-property bacon     :id))
        (pepperoni-id (get-property pepperoni :id))
        (pizza-id     (get-property pizza     :id))
        (pizza-2-id   (get-property pizza-2   :id))
        (pizza-3-id   (get-property pizza-3   :id)))
    
    (ok (not (or (= pizza-id pizza-2-id) (= pizza-id pizza-3-id) (= pizza-2-id pizza-3-id)))
        "Saved objects should have different primary keys")
    
    (is (length (remove-duplicates (list cheese-id onion-id bacon-id pepperoni-id))) 4
        "Indirectly saved objects should have different primary keys")
    
    
    )
  )

(disconnect-toplevel)

(finalize)
