;;;; conditions.lisp --- Conditions used in the frontend module.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :protocl.frontend)


;;; Import-related errors.
;;

(define-condition import-error (error)
  ((import :initarg  :import
	   :reader   import-error-import
	   :documentation
	   "The offending import."))
  (:report
   (lambda (condition stream)
     (format stream "~@<Import ~S failed.~@:>"
	     (import-error-import condition))))
  (:documentation
   "This error and subclasses are signaled when an import statement is
incorrect or cannot be processed."))

(define-condition cannot-resolve-import (import-error)
  ((locations :initarg  :locations
	      :type     list
	      :reader   import-error-locations
	      :initform nil
	      :documentation
	      "Locations that have been consulted when trying to
resolve the import."))
  (:report
   (lambda (condition stream)
     (format stream "~@<The resource designated by the import ~S could ~
not be resolved. ~:[No locations have been tried. Check ~S or the ~
installed dependency handler~;~:*These locations have been tried: ~
~{~S~^, ~}~].~@:>"
	     (import-error-import    condition)
	     (import-error-locations condition)
	     '*import-search-path*)))
  (:documentation
   "This error is signaled if the resource designated by an import
statement cannot be found or loaded."))

(defun cannot-resolve-import (import &optional locations)
  "Convenience function for signaling `cannot-resolve-import' errors."
  (error 'cannot-resolve-import
	 :import    import
	 :locations locations))

(define-condition ambiguous-import (import-error)
  ((candidates :initarg  :candidates
	       :type     list
	       :reader   import-error-candidates
	       :initform nil
	       :documentation
	       "The set of candidates causing the ambiguity."))
  (:report
   (lambda (condition stream)
     (format stream "~@<Ambiguous import ~S. Candidates are ~{~S~^, ~
~}.~@:>"
	     (import-error-import     condition)
	     (import-error-candidates condition))))
  (:documentation
   "This error is signaled if there are multiple candidates for an
import and no resolution strategy has been specified."))

(defun ambiguous-import (import &optional candidates)
  "Convenience function for signaling `ambiguous-import' errors."
  (error 'ambiguous-import
	 :import     import
	 :candidates candidates))
