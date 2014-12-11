(defpackage mypackage
  (:use :cl :golem))
(in-package :mypackage)

;;; Define a simple model

(defmodel person ()
  (first-name :type (:char 30))
  (last-name  :type (:char 30)))


;;; Connect the database

(connect-toplevel  :sqlite3 :database-name #P"/tmp/mydb.db")

;;; Initialize the model
(initialize-models 'person)


(let ((myself (create 'person :first-name "Lucas" :last-name "Pandlfo")))

  (save myself) ;=> Object saved to the database
  
  (get-property myself :last-name)
  (setf (get-property myself :last-name) "Pandolfo")
  
  (save myself) ;=> Object updated in the database
  )

;;; Create some objects
(loop :for (name last-name) :in '(("John" "Doe") ("Natalia" "Natalia") ("Fulano" "De Tal"))
   :do (save (create 'person :first-name name :last-name last-name)))

(fetch-one (filter (all 'person) '(:= :first-name "Lucas"))) ;=> fetch object from te database

(fetch (filter (all 'person) '(:< :last-name "O"))) ;=> fetch objects from te database

;;; Define some more models
(defmodel musician ()
  (first-name :type (:char 50))
  (last-name  :type (:char 50))
  (instrument :type (:char 100)))

(defmodel album ()
  (artist       :foreign-key musician)
  (name         :type        (:char 100))
  (num-stars    :type        integer))

(initialize-models 'musician 'album)

(let ((one-musician (create 'musician :first-name "Miku" :last-name "Hatsune")))
  (let ((one-album (create 'album :name "First album ever" :num-stars 5)))
    
    (setf (get-property one-album :artist) one-musician)
    
    (save one-album) ;=> Both objects saved to the database
    ))

(defparameter *main-musician* (fetch-one (filter (all 'musician) '(:= :last-name "Hatsune"))))
(setf (get-property *main-musician* :instrument) "Voice")

(save *main-musician*) ;=> Update object

(loop :for i :from 1 :to 10 :do
   (save (create 'album 
		 :artist *main-musician* 
		 :name   (format nil "Album ~2,'0d" i) 
		 :num-stars (random 5))))

;;; Some fetchs
(fetch (all 'album) :limit 3 :offset 4) 
(fetch (filter (all 'album) '(:>= :num-stars 4)))


;;; Yet more models
(defmodel topping ()
  (name  :type (:char 20))
  (price :type float))

(defmodel pizza ()
  (name     :type (:char 20))
  (toppings :many-to-many topping))

(initialize-models 'pizza 'topping)

(let ((cheese    (create 'topping :name "Chesse" :price 1))
      (onion     (create 'topping :name "Onion"  :price 2))
      (bacon     (create 'topping :name "Bacon"  :price 1.3))
      (pepperoni (create 'topping :name "Pepperoni" :price 2.5))
      (pizza-1   (create 'pizza   :name "Plain old pizza"))
      (pizza-2   (create 'pizza   :name "Medium pizza"))
      (pizza-3   (create 'pizza   :name "Pizza Deluxe" )))
  
  (setf (get-property pizza-1 :toppings) (list cheese)) 
  (setf (get-property pizza-2 :toppings) (list cheese onion))
  (setf (get-property pizza-3 :toppings) (list cheese onion bacon pepperoni))

  (save pizza-1)
  (save pizza-2)
  (save pizza-3))

(fetch-one (filter (all 'pizza) '(:= :name "Medium pizza")))
