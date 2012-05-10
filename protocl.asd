;;; protocl.asd --- Google protocol buffer compiler.
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

(cl:defpackage :protocl-system
  (:use
   :cl
   :asdf)

  (:export
   :version/list
   :version/string))

(cl:in-package :protocl-system)


;;; Version stuff
;;

(defconstant +version-major+ 2
  "Major component of version number.")

(defconstant +version-minor+ 1
  "Minor component of version number.")

(defconstant +version-revision+ 0
  "Revision component of version number.")

(defun version/list ()
  "Return a version of the form (MAJOR MINOR REVISION)."
  (list +version-major+ +version-minor+ +version-revision+))

(defun version/string ()
  "Return a version string of the form \"MAJOR.MINOR.REVISION\"."
  (format nil "~{~A.~A.~A~}" (version/list)))


;;; System definitions
;;

(defsystem :protocl-parser
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LLGPLv3; see COPYING file for details."
  :description "Parser infrastructure for textual protocol buffer format."
  :depends-on  (:alexandria
		:let-plus

		:esrap)
  :components  ((:module     "parser"
		 :pathname   "src/parser"
		 :serial     t
		 :components ((:file       "package")
			      (:file       "protocol")
			      (:file       "grammar")
			      (:file       "parser")
			      (:file       "mixins")))))

(defsystem :protocl
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LLGPLv3; see COPYING file for details."
  :description "Protocol buffer compiler infrastructure."
  :depends-on  (:alexandria
		:let-plus

		:nibbles

		:protocl-parser)
  :components  ((:module     "src"
		 :serial     t
		 :components ((:file       "package")
			      (:file       "types")))

		(:module     "coding"
		 :pathname   "src/coding"
		 :depends-on ("src")
		 :serial     t
		 :components ((:file       "package")
			      (:file       "coders")))

		(:module     "model"
		 :pathname   "src/model"
		 :depends-on ("src")
		 :components ((:file       "package")))

		(:module     "frontend"
		 :pathname   "src/frontend"
		 :depends-on ("src" "model")
		 :serial     t
		 :components ((:file       "package")
			      (:file       "types")
			      (:file       "conditions")
			      (:file       "variables")
			      (:file       "protocol")
			      (:file       "builder")))

		(:module     "backend"
		 :pathname   "src/backend"
		 :depends-on ("src" "coding" "model")
		 :serial     t
		 :components ((:file       "package")
			      (:file       "protocol")))))
