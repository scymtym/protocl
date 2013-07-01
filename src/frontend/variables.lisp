;;;; variables.lisp --- Variables used in the protocol buffer compiler frontend
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :protocl.frontend)

(declaim (special *import-search-path*))

(defvar *import-search-path* nil
  "A list of directory pathnames that should be consulted when
resolving \"import\" directives.")
