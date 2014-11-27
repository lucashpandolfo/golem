(in-package :cl-user)
(defpackage golem.query
  (:use :cl 
        :golem.model
        :golem.db)
  (:export :create
           :get-property
           :save
           :all
           :filter
           :exclude
           :fetch
           :fetch-one
           ))

(in-package :golem.query)

;;; Object representation
(defstruct row
  (model   nil :type symbol)
  (columns nil :type list))

;;; Object manipulation
(defun create (model &rest params)
  "Create an object with a given model.

   TODO: Param type/name check for the model.
   TODO: Fill default values?"
  (check-type model symbol)
  ;Verify if model exist
  (get-model model)
  (assert (evenp (length params)) () "Even number of parameters expected while creating a ~a." model)
  (make-row :model model :columns params))

(defun get-property (row property-name)
  "Get a property from the object."
  (getf (row-columns row) property-name))

(defun (setf get-property) (value row property-name)
  "Set a property in the object."
  (setf (getf (row-columns row) property-name) value)
  row)

(defun save (row)
  "Save an object in the database. If the primary key is set for the
  object an update is executed. If the update is unsuccessfull an
  insert is executed. If the insert also fails an error is signaled.

  If the primary key is not set an insert is executed and the primary
  key is set in the object by requesting the 'last-row-id' from the
  database.

  See https://docs.djangoproject.com/en/dev/ref/models/instances/#how-django-knows-to-update-vs-insert

  TODO: 
  - Non standard primary key
  - validate types/models
  - validate field names
  - many-to-many"
  (declare (optimize (debug 3)))
  (let* ((model (get-model (row-model row)))
         (table (convert-column-name (convert-column-name (model-name model))))
         (set-values   (copy-seq (row-columns row)))
	 (foreign-keys (model-foreign-keys model))
         (primary-key (model-primary-key model))
         (primary-key-value (get-property row primary-key))
	 (try-insert t)
	 )
    ;; First save foreign objects recursively
    (loop :for (fkey-name . fkey-model) :in foreign-keys :do
       (let ((fobject (get-property row fkey-name)))
	 (when fobject
	   (let* ((saved (save fobject))
		  (fkey-value (get-property saved (model-primary-key (get-model fkey-model)))))
	     (setf (getf set-values fkey-name) fkey-value)))))
    
    (when primary-key-value
      ;; Try update
      (let* ((where-update (sxql:make-op := primary-key primary-key-value))
	     (updated (execute-with-connection
                       (sxql:make-statement :update table (apply #'sxql:set= set-values)
					   (sxql:make-clause :where where-update)))))
	;; Update successfull, do not insert
	(when (> updated 0) (setf try-insert nil))))
    
    (when try-insert
      ;;Try inserting
      (let ((inserted (execute-with-connection
		       (sxql:make-statement :insert-into table (apply #'sxql:set= set-values)))))
	(unless (> inserted 0)
	  (error "Could not insert or update the object in the database."))
	
	(when (not primary-key-value)
	  ;; Update primary key
	  (setf (get-property row primary-key) (last-row-id))))))
  row)

;;; Querys and filters
(defstruct query-set
  (model    nil :type symbol)
  (filters (list)))

(defun all (model)
  "Create a query set matching all objects in the database for the
   give model."
  (make-query-set :model model))

(defun filter (query-set expression)
  "Filter a query set to match a given set. Expression has the form of
   an sxql :where clause. For example (:> :age 20). Multiple filters
   can be chained. Creates a new query-set instead of modifying the
   existing one.

   TODO: Clause validation (types mostly).
   TODO: Compound clauses (:or (:> :age 20) (:< :age 10))"
  (make-query-set :model (query-set-model query-set)
                  :filters (list* expression (query-set-filters query-set))))

(defun exclude (query-set expression)
  (error "Not implemented.")
  )

(defun fetch (query-set &optional &key (limit nil) (offset 0))
  "Fetch the database entries matching the query-set filters (or all
   objects if no filters). Only 'limit' results are fetched, starting
   from 'offset'.

   TODO: 'order by'?.
   TODO: fetch many to many."
  (let ((model (get-model (query-set-model query-set)))
        (filters (reverse (query-set-filters query-set))))
    (let ((expression (when filters 
			(apply #'sxql:make-op 
			       :and
			       (mapcar (lambda (f) (apply  #'sxql:make-op f)) filters))))
          (limit (when (and (numberp limit) (numberp offset)) (sxql:limit offset limit))))
      (let* ((clauses (list :select (sxql:fields :*) 
                            (sxql:from (convert-column-name (model-name model)))
                            (if expression (sxql:make-clause :where expression) nil)
                            limit
                            ))
             (result
              (fetch-with-connection
               (apply #'sxql:make-statement (remove-if #'null clauses)))))
        (let ((objects (loop :for r :in result
			  :collect
			  (make-row :model (query-set-model query-set)
				    :columns
				    ;; Convert the from :|symbol| to :symbol.  :/ 
				    (mapcar (lambda (e)
					      (typecase e
						(symbol (intern (string-upcase (symbol-name e)) :keyword))
						(t e))) r)))))
          (let ((foreign-keys (model-foreign-keys model)))
            (loop :for object :in objects :do
               (loop :for (fkey-name . fkey-model) :in foreign-keys :do
                  (let* ((foreign-model (get-model fkey-model))
                         (foreign-value (get-property object fkey-name)))
		    (when foreign-value
		      (setf (get-property object fkey-name)
			    (fetch-one 
			     (filter (all fkey-model) 
				     (list := (model-primary-key foreign-model) foreign-value)))))))))
          objects)))))

(defun fetch-one (query-set)
  "Fetch only the first entry matching the query-set."
  (first (fetch query-set :limit 1)))
