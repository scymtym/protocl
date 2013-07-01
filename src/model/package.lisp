;;;; package.lisp --- Package definition for the descriptor module.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:protocl.model
  (:use
   #:cl
   #:alexandria

   #:protocl)

  ;; named protocol
  (:export
   #:name)

  ;; composite protocol
  (:export
   #:children)

  ;; field
  (:export
   #:field
   #:number
   #:type)

  ;; classes
  (:export
   #:package1
   #:message)

  (:documentation
   "This package contains classes and functions that act as abstract
representations of protocol buffer data types. "))
