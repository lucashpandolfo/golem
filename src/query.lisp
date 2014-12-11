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
(defun  validate-property-name (model property-name)
  (unless (member property-name (model-defined-slots model))
    (error "Invalid property (~S) for the model ~S." property-name (row-model row))))

(defun create (model &rest params)
  "Create an object with a given model.

   TODO: Param type/name check for the model.
   TODO: Fill default values?"
  (check-type model symbol)
  ;Verify if model exist
  
  (let ((model  (get-model model)))
    (assert (evenp (length params)) () "Even number of parameters expected while creating a ~a." model)
    (loop :for p :in params 
       :for n :from 0
       :do (when (evenp n) (validate-property-name model p))))
  
  (make-row :model model :columns params))

(defun get-property (row property-name)
  "Get a property from the object."
  (validate-property-name (get-model (row-model row)) property-name)
  (getf (row-columns row) property-name))

(defun (setf get-property) (value row property-name)
  "Set a property in the object."
  (validate-property-name (get-model (row-model row)) property-name)
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
  - validate field names"
  (declare (optimize (debug 3)))
  (let* ((model             (get-model (row-model row)))
         (main-model-name   (model-name model))
         (table-name        (model-table-name-as-keyword main-model-name))
         (set-values        (copy-seq (row-columns row)))
	 (foreign-keys      (model-foreign-keys model))
	 (m2m-keys          (model-m2m-keys model))
         (primary-key-name  (model-primary-key model))
         (primary-key-value (get-property row primary-key-name))
	 (try-insert t))
    ;; First save foreign objects recursively
    (loop :for (fkey-name . fkey-model) :in foreign-keys :do
       ;; Save each foreign object
       (let ((fobject (get-property row fkey-name)))
	 (when fobject
	   (let* ((saved (save fobject))
		  (fkey-value (get-property saved (model-primary-key (get-model fkey-model)))))
	     ;; Replace the foreign objects with the corresponding foreign key
	     (setf (getf set-values fkey-name) fkey-value)))))

    ;; Then save the main object
    ;; Remove m2m objects from  the data to be saved
    (loop :for (m2m-key-name . m2m-key-model) :in m2m-keys :do
       (remf set-values m2m-key-name))

    (when primary-key-value
      ;; Try updating if primary key is set
      (let* ((where-update (sxql:make-op := primary-key-name primary-key-value))
             (updated (execute-with-connection
                       (sxql:make-statement :update table-name (apply #'sxql:set= set-values)
                                            (sxql:make-clause :where where-update)))))
        ;; Update successfull, do not insert
        (when (> updated 0) (setf try-insert nil))))
    
    (when try-insert
      ;;Try inserting
      (let ((inserted (execute-with-connection
                       (sxql:make-statement :insert-into table-name (apply #'sxql:set= set-values)))))
        (unless (> inserted 0)
          (error "Could not insert or update the object in the database."))
        
        (when (not primary-key-value)
          ;; Update primary key
          (let ((new-value (last-row-id)))
            (setf primary-key-value                   new-value
                  (get-property row primary-key-name) new-value )))))
    
    ;; Object correctly inserted/updated. At this point the primary
    ;; key should be valid, so m2m objects can be saved.
    (loop :for (m2m-key-name . m2m-model) :in m2m-keys :do
       (let ((m2m-objects (get-property row m2m-key-name)))
         ;; Save objects and get the saved pk's
         (when m2m-objects
           (let* ((saved      (mapcar #'save m2m-objects))
                  (m2m-pk     (model-primary-key (get-model m2m-model)))
                  (m2m-table  (model-m2m-table-name-as-keyword model m2m-key-name))
                  (m2m-values (mapcar (lambda (s) (get-property s m2m-pk)) saved)))
             ;; Delete previous values
             (execute-with-connection 
              (sxql:make-statement :delete-from m2m-table 
                                   (sxql:make-clause :where 
                                                     (list := (model-column-name-as-keyword main-model-name) primary-key-value))))

             ;; Insert new values
             (loop :for object-key :in m2m-values :do
                (execute-with-connection 
                 (sxql:make-statement 
                  :insert-into m2m-table 
                  (sxql:set= (model-column-name-as-keyword main-model-name) primary-key-value
                             m2m-model object-key))))))))
    )
  row)

;;; Querys and filters
(defstruct query-set
  (model    nil :type symbol)
  (filters (list)))

(defun all (model)
  "Create a query set matching all objects in the database for the
   given model."
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

(defun recreate-object-from-plist (model plist)
  "Reconstruct an object from a plist. First converts all keyword
   symbols to be compatible with the rest of the system."
  (let ((converted-plist
	 ;; Convert the from :|symbol| to :symbol.  :/ 
	 (loop :for element :in plist
	    :for n :from 0
	    :if (evenp n) :collect (intern (string-upcase (symbol-name element)) :keyword)
	    :else :collect element)))
    (apply #'create (model-name model) converted-plist)))

(defun fetch (query-set &optional &key (limit nil) (offset 0))
  "Fetch the database entries matching the query-set filters (or all
   objects if no filters). Only 'limit' results are fetched, starting
   from 'offset'.

   TODO: 'order by'?.
   TODO: fetch many to many."
  (let* ((model      (get-model (query-set-model query-set)))
	 (model-name (model-name model))
	 (filters    (reverse   (query-set-filters query-set)))
	 (model-pk   (model-primary-key model))
	 )
    (let ((expression (when filters 
			(apply #'sxql:make-op 
			       :and
			       (mapcar (lambda (f) (apply  #'sxql:make-op f)) filters))))
          (limit (when (and (numberp limit) (numberp offset)) (sxql:limit offset limit))))
      (let* ((clauses (list :select (sxql:fields :*) 
                            (sxql:from (model-table-name-as-keyword model-name))
                            (if expression (sxql:make-clause :where expression) nil)
                            limit))
             (result
              (fetch-with-connection
               (apply #'sxql:make-statement (remove-if #'null clauses)))))
        (let ((retrieved-objects 
	       (mapcar (lambda (o) 
			 (recreate-object-from-plist model o))
		       result)))
          
	  ;; Fetch foreign keys
          (let ((foreign-keys (model-foreign-keys model)))
            (loop :for object :in retrieved-objects :do
               (loop :for (fkey-name . fkey-model) :in foreign-keys :do
                  (let* ((foreign-model (get-model fkey-model))
                         (foreign-value (get-property object fkey-name)))
		    (when foreign-value
		      (setf (get-property object fkey-name)
			    (fetch-one 
			     (filter (all fkey-model)
				     (list := (model-primary-key foreign-model) foreign-value)))))))))
          
	  ;; Fetch m2m objects
          (let ((m2m-keys (model-m2m-keys model)))
            (loop :for object :in retrieved-objects :do
               (loop :for (m2m-key-name . m2m-model) :in m2m-keys :do
                  (let* ((m2m-pk     (model-primary-key (get-model m2m-model)))
                         (m2m-table  (model-m2m-table-name-as-keyword model m2m-key-name))
			 (pk-value   (get-property object model-pk))
			 (m2m-foreign-name (model-column-name-as-keyword m2m-model)))
                    (let ((foreign-keys
			   (mapcar #'second
				   (fetch-with-connection
				    (sxql:make-statement :select 
							 (sxql:fields m2m-foreign-name)
							 (sxql:from m2m-table)
							 (sxql:make-clause :where (list := (model-column-name-as-keyword model-name) pk-value)))))))
		      (setf (get-property object m2m-key-name)
			    (loop :for fkey :in foreign-keys
			       :collect
			       (fetch-one (filter (all m2m-model) (list := m2m-pk fkey))))))))))
          
          retrieved-objects)))))

(defun fetch-one (query-set)
  "Fetch only the first entry matching the query-set."
  (first (fetch query-set :limit 1)))
