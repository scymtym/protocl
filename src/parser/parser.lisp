;;;; parser.lisp --- Implementation of the parser protocol.
;;;;
;;;; Copyright (C) 2012-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:protocl.parser)

(defmethod parse ((source string) (builder t))
  (let ((*builder* builder))
    (values-list (esrap:parse 'proto source))))
