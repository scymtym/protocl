;;;; parser.lisp --- Implementation of the parser protocol.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :protocl.parser)

(defmethod parse ((source string) (builder t))
  (let ((*builder* builder))
    (values (esrap:parse 'proto source))))
