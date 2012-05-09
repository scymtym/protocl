;;; package.lisp --- Package definition for the parser module.
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

(cl:defpackage :protocl.parser
  (:use
   :cl
   :alexandria)

  (:import-from :esrap
   :defrule
   :! :?)

  ;; Conditions
  (:export
   :proto-parse-error
   :proto-parse-error-line
   :proto-parse-error-column
   :proto-parse-error-offset
   :proto-parse-error-causing-condition)

  ;; Variables
  (:export
   :*path*)

  ;; Builder protocol
  (:export
   :make-comment

   :make-syntax
   :make-import

   :make-option

   :make-enum-value
   :make-enum

   :make-field
   :make-message

   :make-package1

   :make-file

   :add-child)

  ;; Comment attaching protocol
  (:export
   :most-recent-comment
   :comment)

  ;; Parser protocol
  (:export
   :parse)

  ;; Mixin classes
  (:export
   :comment-attaching-mixin)

  (:documentation
   "This package contains parser infrastructure for the textual
protocol buffer descriptor syntax.

The primary entry-point is the `parse' function. A builder protocol,
consisting of `make-*' and `add-child' functions can be implemented to
receive parsing results.

The following things are not supported:
* service keyword
* rpc keyword
* extend keyword
* group keyword

For an informal discussion of the protocol buffer descriptor syntax see
http://groups.google.com/group/protobuf/browse_thread/thread/1cccfc624cd612da"))
