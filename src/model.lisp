(in-package :cl-user)
(defpackage golem.model
  (:use :cl :golem.db)
  (:export :defmodel
           :get-model
           :initialize-models
           :model-name
           :model-primary-key
           :model-foreign-keys))

(in-package :golem.model)

;;; Model definition
(defstruct model 
  (name    nil :type string)
  (columns nil :type list))

(defstruct model-column
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
  (etypecase name
    (null    nil)
    (string  (string-downcase name))
    (symbol  (normalize-name (symbol-name name)))))

(defun denormalize-name (name)
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
  "Returns the model columns which are foreign keys as (fname . fmodel) pairs."
  (loop :for column :in (model-columns model)
     :if (model-column-foreign-key column) :collect (cons (denormalize-name (model-column-name column)) (model-column-foreign-key column))))

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
         (many-to-many (getf slot :many-to-many))
         )

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

(defun add-model (name model)
  (when (gethash name *defined-models*)
    (warn "Redifining model ~a:~a: Migrations not implemented." 
	  (package-name (symbol-package name)) 
	  name))
  (setf (gethash name *defined-models*) model)
  name)

(defun get-model (name)
  "Returns the model asociated with a name."
  (let ((model (gethash name *defined-models*)))
    (unless model
      (error "Not such model: ~a." name))
    model))

(defmacro defmodel (name (&rest params) &rest slots)
  "Defines a model. Name will be translated to the table name. Params
  are not used yet (but will be used in the future ... maybe).

  Each slot has the form (name &allow-other-keys). As for now valid keys are:
  - type: A valid SxQL type
  - primary-key:  boolean
  - null-allowed: boolean (TODO)
  - many-to-many: another model name (TODO)"
  (declare (ignorable params))
  `(add-model ',name 
	      (make-model :name (normalize-name ',name)
			  :columns (loop :for slot :in ',slots 
					:collect
				      (parse-model-slot slot))
			  )))

;;; Model to SxQL
(defun prepare-column-definition (column)
  (list (convert-column-name (model-column-name column))
        :type        (or (model-column-type column) 'integer)
        :not-null    (not (model-column-null-allowed column))
        :primary-key (model-column-primary-key-p column)
   ))

(defun create-table-for-model (model)
  "Create an SxQL create-table statement for the model. Adds a primary
   key if one is not defined."
  (let ((columns (mapcar #'prepare-column-definition (model-columns model))))
    (unless (model-has-primary-key model)
      (push (prepare-column-definition *default-primary-key*) columns))
    (sxql:make-statement :create-table
                         (convert-column-name (model-name model))
                         columns)))

;;; Model initialization (DB creation)
(defun initialize-model (model)
  (handler-case
      (prog1 t (execute-with-connection (create-table-for-model (get-model model))))
    (error () nil)))

(defun initialize-models (&rest models)
  (map 'list #'initialize-model models))

