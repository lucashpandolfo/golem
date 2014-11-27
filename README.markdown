# Golem

Database models for Common Lisp

## Warning

Experimental pre alpha. Use at your own risk.

## Usage

```common-lisp
(defpackage mypackage
  (:use :cl :golem))
(in-package :mypackage)

(defmodel musician ()
  (first-name :type (:char 50))
  (last-name  :type (:char 50))
  (instrument :type (:char 100)))

(defmodel album ()
  (artist       :foreign-key musician)
  (name         :type        (:char 100))
  (num-stars    :type        integer))

(connect-toplevel :sqlite3 :database-name #P"/tmp/mydb.db")
(initialize-models 'musician 'album)

(let ((one-musician (create 'musician :first-name "John" :last-name "Doe")))
  (save one-musician))
;=> #S(GOLEM.QUERY::ROW
;      :MODEL MUSICIAN
;      :COLUMNS (:ID 1 :FIRST-NAME "John" :LAST-NAME "Doe"))

(defparameter *other-musician* (create 'musician :first-name "Miku" :last-name "Hatsune"))
(get-property *other-musician* :id) ;=> NIL
(save *other-musician*)
(get-property *other-musician* :id) ;=> 2
(get-property *other-musician* :first-name) ;=> "Miku"


(fetch (all 'musician))
;=> (#S(GOLEM.QUERY::ROW
;       :MODEL MUSICIAN
;       :COLUMNS (:ID 1 :FIRST-NAME "John" :LAST-NAME "Doe" :INSTRUMENT :NIL))
;    #S(GOLEM.QUERY::ROW
;       :MODEL MUSICIAN
;       :COLUMNS (:ID 2 :FIRST-NAME "Miku" :LAST-NAME "Hatsune" :INSTRUMENT
;                 :NIL)))

(defparameter *main-musician* (fetch-one (filter (all 'musician) '(:= :last-name "Hatsune"))))
(setf (get-property *main-musician* :instrument) "Voice")
(save *main-musician*)

(loop :for i :from 1 :to 10 :do
   (save (create 'album 
		 :artist *main-musician* 
		 :name (format nil "Album ~2,'0d" i) 
		 :num-stars (random 5))))

(fetch (all 'album) :limit 3 :offset 4)
;=>
;(#S(GOLEM.QUERY::ROW
;    :MODEL ALBUM
;    :COLUMNS (:ID 5 :ARTIST
;              #S(GOLEM.QUERY::ROW
;                 :MODEL MUSICIAN
;                 :COLUMNS (:ID 2 :FIRST-NAME "Miku" :LAST-NAME "Hatsune"
;                           :INSTRUMENT "Voice"))
;              :NAME "Album 05" :NUM-STARS 4))
; #S(GOLEM.QUERY::ROW
;    :MODEL ALBUM
;    :COLUMNS (:ID 6 :ARTIST
;              #S(GOLEM.QUERY::ROW
;                 :MODEL MUSICIAN
;                 :COLUMNS (:ID 2 :FIRST-NAME "Miku" :LAST-NAME "Hatsune"
;                           :INSTRUMENT "Voice"))
;              :NAME "Album 06" :NUM-STARS 1))
; #S(GOLEM.QUERY::ROW
;    :MODEL ALBUM
;    :COLUMNS (:ID 7 :ARTIST
;              #S(GOLEM.QUERY::ROW
;                 :MODEL MUSICIAN
;                 :COLUMNS (:ID 2 :FIRST-NAME "Miku" :LAST-NAME "Hatsune"
;                           :INSTRUMENT "Voice"))
;              :NAME "Album 07" :NUM-STARS 2))) 


(fetch (filter (all 'album) '(:>= :num-stars 4)))
;=>
;(#S(GOLEM.QUERY::ROW
;    :MODEL ALBUM
;    :COLUMNS (:ID 3 :ARTIST
;              #S(GOLEM.QUERY::ROW
;                 :MODEL MUSICIAN
;                 :COLUMNS (:ID 2 :FIRST-NAME "Miku" :LAST-NAME "Hatsune"
;                           :INSTRUMENT "Voice"))
;              :NAME "Album 03" :NUM-STARS 4))
; #S(GOLEM.QUERY::ROW
;    :MODEL ALBUM
;    :COLUMNS (:ID 5 :ARTIST
;              #S(GOLEM.QUERY::ROW
;                 :MODEL MUSICIAN
;                 :COLUMNS (:ID 2 :FIRST-NAME "Miku" :LAST-NAME "Hatsune"
;                           :INSTRUMENT "Voice"))
;              :NAME "Album 05" :NUM-STARS 4)))

```

## To come later.....

* Many to many relationships
* Custom primary keys
* Types validation
* Migrations
* Default values
* Reconsider filter api
* Order by
* Tests
* Transactions
* Deletions
* More

## See Also

* [SxQL](https://github.com/fukamachi/sxql)
* [CL-DBI](https://github.com/fukamachi/cl-dbi)
* [datafly](https://github.com/fukamachi/datafly)
* [crane](https://github.com/eudoxia0/crane)

## Author

Lucas H. Pandolfo
