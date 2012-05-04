;;; package.lisp --- Package definition for the protobuf system.
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

(cl:defpackage :protobuf
  (:use
   :cl
   :alexandria
   :nibbles)

  ;; Wire types
  (:export
   :field-number

   :wire-type/code
   :wire-type/name

   :wire-type-code->name
   :wire-type-name->code)

  ;; Start codes
  (:export
   :start-code/code/cons
   :start-code/name/cons
   :start-code/number)

  (:export
   :+primitive-proto-types+)

  (:documentation
   "Main package for the protobuf system."))
