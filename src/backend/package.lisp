;;;; package.lisp --- Package definition for the backend module.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage :protocl.backend
  (:use
   :cl
   :alexandria

   :protocl)

  ;; Emission protocol
  (:export
   :emit)

  (:documentation
   "This package contains the code generation machinery of the
protocl system."))
