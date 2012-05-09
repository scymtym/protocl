;;; package.lisp --- Package definition for the frontend module.
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

(cl:defpackage :protocl.frontend
  (:use
   :cl
   :alexandria
   :let-plus

   :protocl.parser)

  ;; Conditions
  (:export
   :import-error
   :import-error-import

   :cannot-resolve-import
   :import-error-locations

   :ambiguous-import
   :import-error-candidates)

  ;; Variables
  (:export
   :*import-search-path*)

  ;;
  (:export
   :load/text)

  (:documentation
   "This package contains the functions for producing abstract
representations form textual protocol buffer descriptions (the
frontend of the protocl system)."))
