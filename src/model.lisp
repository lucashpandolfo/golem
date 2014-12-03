(in-package :cl-user)
(defpackage golem.model
  (:use :cl :golem.db)
  (:export :defmodel
           :get-model
           :initialize-models
           :model-name
           :model-primary-key
           :model-foreign-keys
	   :model-m2m-keys))

(in-package :golem.model)

;;; Model definition
(defstruct model
  "A database Model."
  (name    nil :type string)
  (columns nil :type list))

(defstruct model-column
  "A column of the database model."
  (name          nil :type string)
  (type          nil)
  (primary-key-p nil :type boolean)
  (foreign-key   nil :type symbol)
  (many-to-many  nil :type symbol)
  (null-allowed  nil :type boolean)
  (default-value nil))

(defvar *defined-models* (make-hash-table)
  "All currently defined models are stored here.")

;;; Model convenience functions
(defun valid-type-p (type)
  t
  )

(defun normalize-name (name)
  "Converts a symbol or string name to a downcased string."
  (etypecase name
    (null    nil)
    (string  (string-downcase name))
    (symbol  (normalize-name (symbol-name name)))))

(defun denormalize-name (name)
  "Transform a string name to a keyword."
  (etypecase name
    (string (intern (string-upcase name) :keyword))
    (t name)))

(defparameter *default-primary-key* 
  (make-model-column :name (normalize-name  "id") :type 'integer :primary-key-p t :null-allowed nil)
  "Default primary key if not defined by user.")

(defun model-has-primary-key (model)
  "Check if model contains a primary key."
  (member t (mapcar #'model-column-primary-key-p (model-columns model))))

(defun model-primary-key (model)
  "Returns the model primary key name as a keyword."
  (denormalize-name
   (if (model-has-primary-key model)
       (loop :for column :in (model-columns model)
          :if (model-column-primary-key-p column) :return (model-column-name column))
      (model-column-name *default-primary-key*))))

(defun model-foreign-keys (model)
  "Returns the model columns which are foreign keys as (fname-symbol . fmodel-name) pairs."
  (loop :for column :in (model-columns model)
     :if (model-column-foreign-key column) 
     :collect (cons (denormalize-name (model-column-name column)) 
		    (model-column-foreign-key column))))

(defun model-m2m-keys (model)
  "Returns the model columns which are many to many keys
   as (m2m-name-symbol . m2m-model-name) pairs."
  (loop :for column :in (model-columns model)
     :if (model-column-many-to-many column)
     :collect (cons (denormalize-name (model-column-name column))
		    (model-column-many-to-many column))))

(defun model-m2m-table-name (model field-symbol-name)
  "Returns the m2m table name for a field of a model as a string."
  (concatenate 'string (model-name model) "-" (normalize-name field-symbol-name)))

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

    (make-model-column :name (normalize-name name)
		       :type type
		       :primary-key-p primary-key
		       :foreign-key  foreign-key
		       :many-to-many many-to-many
		       :null-allowed t
		       :default-value nil)))

(defun add-model (model-name model)
  "Add a model to the model databse."
  (when (gethash model-name *defined-models*)
    (warn "Redifining model ~a:~a: Migrations not implemented." 
	  (package-name (symbol-package model-name)) 
	  model-name))
  (setf (gethash model-name *defined-models*) model)
  model-name)

(defun get-model (model-name)
  "Returns the model asociated with a name."
  (let ((model (gethash model-name *defined-models*)))
    (unless model
      (error "Not such model: ~a." model-name))
    model))

(defmacro defmodel (name (&rest params) &rest slots)
  "Defines a model. Name will be translated to the table name. Params
  are not used yet (but will be used in the future ... maybe).

  Each slot has the form (name &allow-other-keys). As for now valid keys are:
  - type: A valid SxQL type
  - primary-key:  boolean
  - null-allowed: boolean (TODO)
  - foreign-key:  model name (symbol) (TODO)
  - many-to-many: model name (symbol) (TODO)

  If a primary key is not defined, a default one is created (named id)."
  (declare (ignorable params))
  `(add-model ',name 
	      (make-model :name (normalize-name ',name)
			  :columns (loop :for slot :in ',slots 
					:collect
				      (parse-model-slot slot)))))

;;; Model to SxQL
(defun prepare-column-definition (column)
  (list (convert-column-name (model-column-name column))
        :type        (or (model-column-type column) 'integer)
        :not-null    (not (model-column-null-allowed column))
        :primary-key (model-column-primary-key-p column)
   ))

(defun create-table-for-model (model)
  "Create one or more SxQL create-table statements for the model. Adds
   a primary key if one is not defined. Creates auxiliary m2m tables
   statements if neccessary. Returns a list of statements."
  (declare (optimize (debug 3)))
  (alexandria:flatten
   (list 
    (let ((columns (mapcar #'prepare-column-definition (model-columns model))))
      (unless (model-has-primary-key model)
	(push (prepare-column-definition *default-primary-key*) columns))
      (sxql:make-statement :create-table
			   (convert-column-name (model-name model))
			   columns))

    ;; Create auxiliary m2m tables
    (let ((m2m (model-m2m-keys model)))
      (when m2m
	(loop :for (m2m-field-name . m2m-model-name) :in m2m
	   :collect
	   (let ((aux-table-name (model-m2m-table-name model m2m-field-name))
		 (columns (mapcar #'prepare-column-definition 
				  (list (make-model-column :name (normalize-name (model-name model)) :type 'integer)
					(make-model-column :name (normalize-name m2m-model-name) :type 'integer)))))
	     (sxql:make-statement :create-table
				  (convert-column-name aux-table-name)
				  columns))))))))

;;; Model initialization (DB creation)
(defun initialize-model (model)
  (let ((statements (create-table-for-model (get-model model))))
    (loop :for statement :in statements :collect
       (handler-case
	   (prog1 t (execute-with-connection statement))
	 (error () nil)))))

(defun initialize-models (&rest models)
  (map 'list #'initialize-model models))

