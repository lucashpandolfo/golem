(in-package :cl-user)
(defpackage golem.db
  (:use :cl)
  (:import-from :dbi
                :prepare
                :fetch-all
                :fetch
                :ping
                :connect
                :disconnect
                :connection-driver-type)
  (:export :connect-toplevel
           :disconnect-toplevel
	   :fetch-with-connection
           :execute-with-connection
	   :last-row-id
           ))

(in-package :golem.db)

;;; MOSTLY FROM DATAFLY - https://github.com/fukamachi/datafly
(defvar *trace-sql* nil)

(defparameter *sql-logger-pattern*
  "[%D{%H:%M:%S}] <DB> %m%n")

(defvar *sql-logger*
  (let ((logger (log:category '(golem))))
    (log:config logger :own :trace)
    (log4cl:add-appender logger
                         (make-instance 'log4cl:this-console-appender
                                        :layout (make-instance 'log4cl:pattern-layout
                                                               :conversion-pattern *sql-logger-pattern*)))
    logger))

(defvar *connection* nil)

(defvar *connections* (make-hash-table :test 'equal))

(defun database-type ()
  (connection-driver-type *connection*))

(defun connect-cached (&rest connect-args)
  ;; Connection cache was disabled because it was not working with
  ;; sqlite (the testing db)
  (apply #'dbi:connect connect-args))

(defun connect-toplevel (&rest connect-args)
  (when *connection*
    (error "Connection is already established in toplevel."))
  (setf *connection*
        (apply #'connect-cached connect-args)))

(defun disconnect-toplevel ()
  (when *connection*
    (dbi:disconnect *connection*)
    (setf *connection* nil)))

(defun connection-quote-character (conn)
  (case (connection-driver-type conn)
    (:mysql #\`)
    (:postgres #\")
    (:sqlite3 #\")
    (T nil)))

(defun fetch-with-connection (statement)
  "Executes a statement and fetches the resulting rows."
  (multiple-value-bind (sql params)
      (let ((sxql:*quote-character* (or sxql:*quote-character*
                                        (connection-quote-character *connection*))))
        (typecase statement
          (string (values statement nil))
          (otherwise (sxql:yield statement))))
    (let* ((prepared (dbi:prepare *connection* sql))
           (results (dbi:fetch-all (apply #'dbi:execute prepared params))))
      (when *trace-sql*
	(log:trace :logger *sql-logger*
		   "~A (~{~S~^, ~}) [~D row~:P]" sql params (length results)))
      results)))

(defun execute-with-connection (statement)
  "Executes a statement and returns the number of affected rows."
  (multiple-value-bind (sql params)
      (let ((sxql:*quote-character* (or sxql:*quote-character*
                                        (connection-quote-character *connection*))))
        (typecase statement
          (string (values statement nil))
          (otherwise (sxql:yield statement))))
    (let* ((prepared (dbi:prepare *connection* sql))
	   (results (apply #'dbi:execute prepared params)))
      (when *trace-sql*
	(log:trace :logger *sql-logger*
		   "~A (~{~S~^, ~})" sql params))))
  (dbi:row-count *connection*))

(defun last-row-id ()
  "Returns the last inserted PK."
  (ecase (connection-driver-type *connection*)
    (:sqlite3  (second (dbi:fetch (dbi:prepare *connection* "SELECT LAST_INSERT_ROWID();"))))
    (:mysql    (second (dbi:fetch (dbi:prepare *connection* "SELECT LAST_INSERT_ID();")))) 
    (:postgres (second (dbi:fetch (dbi:prepare *connection* "SELECT LASTVAL();"))))))
