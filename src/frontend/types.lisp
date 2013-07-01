;;;; types.lisp --- Types used in the frontend module.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:protocl.frontend)

(deftype if-ambiguous-policy ()
  '(member :first :error))
