;;; mixins.lisp --- Mixin classes used in the parser module.
;;
;; Copyright (C) 2012 Jan Moringen
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

(cl:in-package :protobuf.parser)


;;; Class `comment-attaching-mixin'
;;

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
  (setf (gethash for (%most-recent-comments builder)) new-value))

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

(defmethod add-child :after ((builder comment-attaching-mixin)
			     (parent  t)
			     (child   t))
  (if (eq (first child) :comment)
      (setf (most-recent-comment builder parent) child)
      (when-let ((comment (most-recent-comment builder parent)))
	(setf (comment builder child)              comment
	      (most-recent-comment builder parent) nil))))
