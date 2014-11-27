(in-package :cl-user)

(defpackage golem
  (:use :cl 
        :golem.model
        :golem.query
        :golem.db)
  (:export :defmodel
	   :initialize-models
           
           :create
           :get-property
           :save
           :all
           :filter
           :exclude
           :fetch
           :fetch-one
           
           :connect-toplevel
           :disconnect-toplevel))

(in-package :golem)
