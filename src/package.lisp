;;;; package.lisp --- Package definition for the protocl system.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:protocl
  (:use
   #:cl
   #:alexandria)

  ;; Wire types
  (:export
   #:field-number

   #:wire-type/code
   #:wire-type/name

   #:wire-type-code->name
   #:wire-type-name->code)

  ;; Start codes
  (:export
   #:start-code/code/cons
   #:start-code/name/cons
   #:start-code/number)

  (:export
   #:+primitive-proto-types+)

  (:documentation
   "Main package for the protocl system."))
