;;; mixins.lisp --- Mixin classes used in the parser module.
;;
;; Copyright (C) 2012, 2013 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This Program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This Program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses>.

(cl:in-package :protocl.parser)


;;; Class `comment-attaching-mixin'
;;

(declaim (special *processing-comment?*))

(defvar *processing-comment?* nil
  "Dynamically bound to a boolean indicating whether a comment is
being processed.")

(defclass comment-attaching-mixin ()
  ((most-recent-comments :initarg  :most-recent-comments
			 :type     hash-table
			 :accessor %most-recent-comments
			 :initform (make-hash-table :test #'eq)
			 :documentation
			 "Associates most recently parsed comment
elements to their respective parent elements for later association to
the appropriate child elements.")
   (assoc                :initarg  :assoc
			 :type     hash-table
			 :accessor %assoc
			 :initform (make-hash-table :test #'eq)
			 :documentation
			 "Associates comment elements to the elements
to which the comments refer."))
  (:documentation
   "This mixin adds to builder classes the ability to associate
comment elements to the elements to which they refer."))

(defmethod most-recent-comment ((builder comment-attaching-mixin)
				(for     t))
  (values (gethash for (%most-recent-comments builder))))

(defmethod (setf most-recent-comment) ((new-value t)
				       (builder   comment-attaching-mixin)
				       (for       t))
  (appendf (gethash for (%most-recent-comments builder)) (list new-value)))

(defmethod (setf most-recent-comment) ((new-value (eql nil))
				       (builder   comment-attaching-mixin)
				       (for       t))
  (remhash for (%most-recent-comments builder)))

(defmethod comment ((builder comment-attaching-mixin)
		    (for     t))
  (values (gethash for (%assoc builder))))

(defmethod (setf comment) ((new-value t)
			   (builder   comment-attaching-mixin)
			   (for       t))
  (setf (gethash for (%assoc builder)) new-value))

(defmethod comment? ((builder   comment-attaching-mixin)
		     (thing     t))
  nil)

(defmethod comment? ((builder   comment-attaching-mixin)
		     (thing     string))
  thing)

(defmethod add-child :around ((builder comment-attaching-mixin)
			      (parent  t)
			      (child   t))

  (cond
    ;; When processing a comment, just call the next method.
    (*processing-comment?*
     (call-next-method))

    ;; When CHILD is a comment, store it for association with the
    ;; following element.
    ((comment? builder child)
     (setf (most-recent-comment builder parent) child)
     parent)

    ;; If CHILD is not a comment and comments are queued, concatenate
    ;; them and attach the resulting string to CHILD.
    (t
     (let ((*processing-comment?* t))
       (when-let ((comment (most-recent-comment builder parent)))
	 ;; Note that `comment?' returns a string representation of
	 ;; comment nodes.
	 (setf (comment builder child)
	       (string-trim
		'(#\Space #\Tab #\Newline)
		(format nil "~{~A~%~}"
			(mapcar (curry #'comment? builder) comment)))
	       (most-recent-comment builder parent)
	       nil))
       (call-next-method)))))
