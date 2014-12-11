# Golem

Database models for Common Lisp

## Warning

Experimental pre alpha. Use at your own risk.

## Usage


```common-lisp
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
```

## Models with foreign keys
```common-lisp
(defmodel musician ()
  (first-name :type (:char 50))
  (last-name  :type (:char 50))
  (instrument :type (:char 100)))

(defmodel album ()
  (artist       :foreign-key musician)
  (name         :type        (:char 100))
  (num-stars    :type        integer))
```

After a `save` to an `album` object, the object assigned to the
`artist` field is also saved/updated. After a `fetch` the object is
restored.

## Many to many relationships

Works the same as with foreign keys, but a list is assigned instead of
a single element.

```common-lisp
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
```

## Working with objects

Objects are created using `create`, then the slots can be accessed
using `get-property`. The database is not touched unless a `save` is
issued.

As for now, each model has an auto defined primary key named `:id`,
this id can be assigned by the user or left blank. After a `save` the
key value is updated from the database (if blank, otherwise the
database is updated).

## To come later.....

* Custom primary keys
* Types validation
* Migrations
* Default values
* Reconsider filter api
* Order by
* Transactions
* Deletions
* Decent manual
* More

## See Also

* [SxQL](https://github.com/fukamachi/sxql)
* [CL-DBI](https://github.com/fukamachi/cl-dbi)
* [datafly](https://github.com/fukamachi/datafly)
* [crane](https://github.com/eudoxia0/crane)

## Author

Lucas H. Pandolfo
