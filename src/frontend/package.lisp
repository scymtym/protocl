;;;; package.lisp --- Package definition for the frontend module.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:protocl.frontend
  (:use
   #:cl
   #:alexandria
   #:let-plus

   #:protocl.parser)

  ;; Conditions
  (:export
   #:import-error
   #:import-error-import

   #:cannot-resolve-import
   #:import-error-locations

   #:ambiguous-import
   #:import-error-candidates)

  ;; Variables
  (:export
   #:*import-search-path*)

  ;;
  (:export
   #:load/text)

  (:documentation
   "This package contains the functions for producing abstract
representations form textual protocol buffer descriptions (the
frontend of the protocl system)."))
