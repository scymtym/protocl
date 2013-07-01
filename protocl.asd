;;;; protocl.asd --- Google protocol buffer compiler.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:protocl-system
  (:use
   #:cl
   #:asdf)

  (:export
   #:version/list
   #:version/string))

(cl:in-package #:protocl-system)

;;; Version stuff

(defparameter +version-major+ 2
  "Major component of version number.")

(defparameter +version-minor+ 1
  "Minor component of version number.")

(defparameter +version-revision+ 0
  "Revision component of version number.")

(defun version/list ()
  "Return a version of the form (MAJOR MINOR REVISION)."
  (list +version-major+ +version-minor+ +version-revision+))

(defun version/string ()
  "Return a version string of the form \"MAJOR.MINOR.REVISION\"."
  (format nil "~{~A.~A.~A~}" (version/list)))

;;; System definitions

(defsystem :protocl-parser
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LLGPLv3; see COPYING file for details."
  :description "Parser infrastructure for textual protocol buffer format."
  :depends-on  (:alexandria
                :split-sequence
                :let-plus

                (:version :esrap "0.9"))
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
