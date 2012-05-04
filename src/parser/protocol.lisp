;;; protocol.lisp --- Protocol functions of the parser module.
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


;;; Parser protocol
;;

(defgeneric parse (source builder)
  (:documentation
   "Parse content of SOURCE as textual protocol buffer description and
build a representation using BUILDER and the builder protocol
functions (`make-message', `make-field', `add-child', ...). Return
whatever BUILDER constructed.

Note: Currently, the grammar accepts only a (quite usable) subset of
the textual protocol buffer descriptor syntax. Major restrictions are:
* Comments may only appear at certain positions:
  * Between top-level elements (include before the first and after the
    last)
  * Between message elements
  * Between enum elements
  On the positive side, this allows attaching comments to specific
  objects (see `comment' function')
* The following constructs are not supported:
  * service keyword
  * rpc keyword
  * extend keyword
  * group keyword"))


;;; Builder protocol
;;

(defgeneric make-comment (builder content)
  (:documentation
   "Make and return a representation of the comment CONTENT using
BUILDER."))

(defgeneric make-syntax (builder value)
  (:documentation
   "Make and return a representation of the syntax element VALUE using
BUILDER."))

(defgeneric make-import (builder name)
  (:documentation
   "Make and return a representation of the import statement for NAME
using BUILDER."))

(defgeneric make-option (builder name value)
  (:documentation
   "Make and return a representation of the option consisting of NAME
and VALUE using BUILDER."))

(defgeneric make-enum-value (builder name value)
  (:documentation
   "Make and return a representation of the enum value named NAME with
numeric value VALUE using BUILDER."))

(defgeneric make-enum (builder name)
  (:documentation
   "Make and return an empty enum named NAME using BUILDER."))

(defgeneric make-field (builder name number type label)
  (:documentation
   "Make and return a representation of the field named NAME with
field number NUMBER, data type TYPE and label LABEL using BUILDER."))

(defgeneric make-message (builder name)
  (:documentation
   "Make and return an empty (no field, nested messages or nested
enums) message named NAME using BUILDER."))

(defgeneric make-package1 (builder name)
  (:documentation
   "Make and return a package named NAME using BUILDER."))

(defgeneric make-file (builder)
  (:documentation
   "Make and return an empty file container using BUILDER."))

(defgeneric add-child (builder parent child)
  (:documentation
   "Add CHILD to PARENT using BUILDER and return the modified
 PARENT."))

(defmethod make-syntax ((builder t) (value t))
  "Default behavior is to ignore syntax declarations."
  (values))

(defmethod make-comment ((builder t) (content t))
  "Default behavior is to ignore comments."
  (values))


;;; Comment attaching protocol
;;

(defgeneric most-recent-comment (builder for)
  (:documentation
   "Return the most recent comment encountered by BUILDER for element
FOR. Return nil if there is no such comment."))

(defgeneric (setf most-recent-comment) (new-value builder for)
  (:documentation
   "Set the most recent comment encountered by BUILDER for element FOR
to NEW-VALUE."))

(defgeneric comment (builder for)
  (:documentation
   "Return the comment associated to element FOR by BUILDER. Return
nil if there is no such comment."))

(defgeneric (setf comment) (new-value builder for)
  (:documentation
   "Set the comment associated to element FOR by BUILDER to
NEW-VALUE."))
