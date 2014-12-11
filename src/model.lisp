(in-package :cl-user)
(defpackage golem.model
  (:use :cl :golem.db)
  (:export :defmodel
           :get-model
           :initialize-models
           :model-name
           :model-table-name-as-keyword
           :model-column-name-as-keyword
           :model-m2m-table-name-as-keyword
	   :model-defined-slots
           :model-primary-key
           :model-foreign-keys
	   :model-m2m-keys))

(in-package :golem.model)

;;; Model definition
(defstruct model
  "A database Model."
  (name    nil :type symbol)
  (columns nil :type list))

(defstruct model-column
  "A column of the database model."
  (name          nil :type symbol)
  (type          nil)
  (primary-key-p nil :type boolean)
  (foreign-key   nil :type symbol)
  (many-to-many  nil :type symbol)
  (null-allowed  nil :type boolean)
  (default-value nil))

(defvar *defined-models* (make-hash-table)
  "All currently defined models are stored here.")

;;; Model convenience functions
(defun escape-string (string)
  "Replaces some symbols in a string (-.\/) with '_'"
  (ppcre:regex-replace-all "[-.\\/\|]" string "_"))

(defun model-table-name-as-keyword (symbol)
  "Converts a symbol to a table keyword name. The
   final keyword consist of <package-name>_<symbol-name>"
  (intern 
   (string-upcase
    (escape-string
     (concatenate 'string
		  (package-name (symbol-package symbol))
		  "_"
		  (symbol-name symbol))))
   :keyword))

(defun model-column-name-as-keyword (symbol)
  "Converts a symbol to a column keyword name."
  (intern 
   (string-upcase 
    ;(escape-string 
     (symbol-name symbol)
     ;)
   )
   :keyword))

(defun model-m2m-table-name-as-keyword (model field-name-symbol)
  "Returns the m2m table name for a field of a model as a keyword. Yhe
   final keyword consist of <package-name>_<model-name>_<field-name>."
  (intern
   (string-upcase
    (escape-string
     (concatenate 'string 
		  (package-name (symbol-package (model-name model)))
		  "_"
		  (symbol-name (model-name model))
		  "_" 
		  (symbol-name field-name-symbol))))
   :keyword))


(defun valid-type-p (type)
  t
  )

(defparameter *default-primary-key* 
  (make-model-column :name 'id :type 'integer :primary-key-p t :null-allowed nil)
  "Default primary key if not defined by user.")

(defun model-has-primary-key (model)
  "Check if model contains a primary key."
  (member t (mapcar #'model-column-primary-key-p (model-columns model))))

(defun model-primary-key (model)
  "Returns the model primary key name as a keyword."
  (if (model-has-primary-key model)      
      (loop :for column :in (model-columns model)
         :if (model-column-primary-key-p column) :return (model-column-name-as-keyword (model-column-name column)))
      (model-column-name-as-keyword (model-column-name *default-primary-key*))))

(defun model-foreign-keys (model)
  "Returns the model columns which are foreign keys as (fname-keyword . fmodel-name-symbol) pairs."
  (loop :for column :in (model-columns model)
     :if (model-column-foreign-key column)
     :collect (cons (model-column-name-as-keyword (model-column-name column))
		    (model-column-foreign-key column))))

(defun model-m2m-keys (model)
  "Returns the model columns which are many to many keys
   as (m2m-name-keyword . m2m-model-name-symbol) pairs."
  (loop :for column :in (model-columns model)
     :if (model-column-many-to-many column)
     :collect (cons (model-column-name-as-keyword (model-column-name column))
		    (model-column-many-to-many column))))

(defun model-defined-slots (model)
  (let ((slots
	 (loop :for column :in (model-columns model)
	    :collect (model-column-name-as-keyword (model-column-name column)))))
    (if (model-has-primary-key model)
	slots
	(push (model-column-name-as-keyword (model-column-name *default-primary-key*)) slots))))

(defun at-most-one-of (&rest params)
  "Returns boolean indicating if zero or one of the parameters is not
   null."
  (>=  (count-if #'null params)
       (1- (length params))))

;;; Model parsing and validation
(defun parse-model-slot (slot)
  "Validates a model slot and creates a model-column. Barfs if not valid."
  (let* ((name         (first slot))
         (slot         (rest slot))
         (type         (getf slot :type))
         (primary-key  (getf slot :primary-key))
         (foreign-key  (getf slot :foreign-key))
         (many-to-many (getf slot :many-to-many)))

    (assert (or type primary-key foreign-key many-to-many) ()
            "A type should be provided for slot ~a." name)
    (assert (at-most-one-of primary-key foreign-key many-to-many) ()
            "Only one of :primary-key, :foreign-key or :many-to-many can be specified for slot ~a." name)
    (cond
      (type (assert (valid-type-p type) () "The type ~a is not valid for the slot ~a." type name)))

    (make-model-column :name name
		       :type type
		       :primary-key-p primary-key
		       :foreign-key  foreign-key
		       :many-to-many many-to-many
		       :null-allowed t
		       :default-value nil)))

(defun add-model (model-symbol-name model)
  "Add a model to the model databse."
  (when (gethash model-symbol-name *defined-models*)
    (warn "Redifining model ~a:~a: Migrations not implemented." 
	  (package-name (symbol-package model-symbol-name)) 
	  model-symbol-name))
  (setf (gethash model-symbol-name *defined-models*) model)
  model-symbol-name)

(defun get-model (model-symbol-name)
  "Returns the model asociated with a name."
  (let ((model (gethash model-symbol-name *defined-models*)))
    (unless model
      (error "Not such model: ~a." model-symbol-name))
    model))

(defmacro defmodel (name (&rest params) &rest slots)
  "Defines a model. Name will be used to form the table name. Params
  are not used yet (but will be used in the future ... maybe).

  The name of the table will be derived from the fully qualified symbol
  name.

  Each slot has the form (name &allow-other-keys). As for now valid keys are:
  - type: A valid SxQL type
  - primary-key:  boolean
  - null-allowed: boolean (TODO)
  - foreign-key:  model name (symbol) (TODO)
  - many-to-many: model name (symbol) (TODO)

  If a primary key is not defined, a default one is created (named id)."
  (declare (ignorable params))
  `(add-model ',name 
	      (make-model :name    ',name
			  :columns (loop :for slot :in ',slots 
				      :collect
				      (parse-model-slot slot)))))

;;; Model to SxQL
(defun prepare-column-definition (column)
  ""
  (list (model-column-name-as-keyword (model-column-name column))
        :type        (or (model-column-type column) 'integer)
        :not-null    (not (model-column-null-allowed column))
        :primary-key (model-column-primary-key-p column)))

(defun make-create-table-statement (table-name model-columns)
  (let ((columns (mapcar #'prepare-column-definition model-columns)))
    (sxql:make-statement :create-table
			 table-name
			 columns)))

(defun create-table-for-model (model)
  "Create one or more SxQL create-table statements for the model. Adds
   a primary key if one is not defined. Creates auxiliary m2m tables
   statements if neccessary. Returns a list of 'create table'
   statements (main model table and many-to-many auxiliary tables if
   needed)."
  (declare (optimize (debug 3)))
  (alexandria:flatten
   (list

    ;; Create main table
    (let ((model-columns (model-columns model)))
      (unless (model-has-primary-key model)
	(push *default-primary-key* model-columns))
      (make-create-table-statement (model-table-name-as-keyword (model-name model))
				   model-columns))

    ;; Create auxiliary m2m tables
    (let ((m2m (model-m2m-keys model)))
      (when m2m
	(loop :for (m2m-field-name . m2m-model-name) :in m2m
	   :collect
	   (make-create-table-statement 
	    (model-m2m-table-name-as-keyword model m2m-field-name)
	    (list (make-model-column :name (model-name model) :type 'integer)
		  (make-model-column :name m2m-model-name     :type 'integer)))))))))


;;; Model initialization (DB creation)
(defun initialize-model (model)
  (let ((statements (create-table-for-model (get-model model))))
    (loop :for statement :in statements :collect
       (handler-case
	   (prog1 t (execute-with-connection statement))
	 (error () nil)))))

(defun initialize-models (&rest models)
  (map 'list #'initialize-model models))

