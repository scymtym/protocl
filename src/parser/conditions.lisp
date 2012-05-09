;;; conditions.lisp --- Conditions used in the parser module.
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

(cl:in-package :protocl.parser)

(define-condition proto-parse-error (error)
  ((line              :initarg  :line
		      :type     positive-integer
		      :accessor proto-parse-error-line
		      :documentation
		      "The line in which the parse error occurred.")
   (column            :initarg  :column
		      :type     non-negative-integer
		      :accessor proto-parse-error-column
		      :documentation
		      "The column in which the parse error occurred.")
   (offset            :initarg  :offset
		      :type     non-negative-integer
		      :accessor proto-parse-error-offset
		      :documentation
		      "The offset in characters at which the parse
error occurred.")
   (causing-condition :initarg  :causing-condition
		      :accessor proto-parse-error-causing-condition
		      :documentation
		      "The condition that represents the actual parse
error."))
  (:report
   (lambda (condition stream)
     (format stream "~@<Parse error at line ~D, column ~D (offset ~D): ~
~A.~@:>"
	     (proto-parse-error-line              condition)
	     (proto-parse-error-column            condition)
	     (proto-parse-error-offset            condition)
	     (proto-parse-error-causing-condition condition))))
  (:documentation
   "This error is signaled when a parse error occurs. The position of
the parse error as well as the causing condition are recorded in the
instance."))
