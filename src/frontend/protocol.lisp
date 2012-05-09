;;; protocol.lisp ---
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

(cl:in-package :protocl.frontend)

(defgeneric load/text (source
		       &key
		       pathname
		       dependency-handler)
  (:documentation
   "Parse content of SOURCE as textual protocol buffer description.
Return a `file-set-desc' instance that contains the complete
description in its child nodes.

PATHNAME TODO

DEPENDENCY-HANDLER has to be a function of single argument that
accepts a source and loads the textual protocol buffer descriptor
designated by the source. If this fails for some reason, the supplied
function should signal an error of a subtype of `import-error' such as
`cannot-resolve-import'."))


;;; Default implementation
;;

(defmethod load/text ((source stream)
		      &key
		      (pathname           "<stream>")
		      (dependency-handler #'load-from-path))
  (let* ((file (make-file-desc pathname (parse source)))
	 (set  (make-instance
		'file-set-desc
		:file (make-array 1
				  :initial-element file
				  :fill-pointer    1))))
    ;; Load all dependencies specified via "import" statements in
    ;; SOURCE.
    (map nil (compose dependency-handler #'parse-namestring)
	 (pb::file-desc-dependency file))
    ;; After dependencies have been resolved, try to register names
    ;; and relations.
    (pbb:emit set :relations)
    ;; Return the parsed and augmented descriptors.
    set))

(defmethod load/text ((source string)
		      &key
		      (pathname           "<string>")
		      (dependency-handler #'load-from-path))
  (with-input-from-string (stream source)
    (load/text stream
	       :pathname           pathname
	       :dependency-handler dependency-handler)))

(defmethod load/text ((source pathname)
		      &key
		      (pathname           (format nil "~A.~A"
						  (pathname-name source)
						  (pathname-type source)))
		      (dependency-handler #'load-from-path))
  (with-input-from-file (stream source)
    (load/text stream
	       :pathname           pathname
	       :dependency-handler dependency-handler)))

(defmethod load/text ((source sequence)
		      &rest args
		      &key &allow-other-keys)
  "This method read descriptions from all files in the list SOURCE and
collects the resulting `file-desc' instance in one `file-set-desc'."
  (let+ (((result &rest rest)
	  (map 'list (apply #'rcurry #'load/text args) source))
	 ((&flet merge-one (desc)
	    (vector-push-extend
	     (aref (pb::file-set-desc-file desc) 0)
	     (pb::file-set-desc-file result)))))
    (map nil #'merge-one rest)
    result))


;;; Default dependency resolution strategy
;;

(defun load-from-path (path
		       &key
		       (if-ambiguous :first))
  "Try to load the textual protocol buffer descriptor designated by
PATH. If PATH is an absolute pathname, it is used directly. Otherwise,
it is merged with the elements of `*proto-load-path*' to form absolute
pathnames of candidate files. An error of type `cannot-resolve-import'
is signaled if PATH cannot be resolved to an existing file.

IF-AMBIGUOUS can be used to control the behavior in case of multiple
candidate files. The value :FIRST (the default) causes the candidate
file that was produced by the leftmost item of `*proto-load-path*' to
be selected. The value :ERROR causes an error of type
`ambiguous-import' to be signaled."
  (check-type path         pathname)
  (check-type if-ambiguous if-ambiguous-policy)

  (let* (;; If PATH is absolute, itself is the only
	 ;; location. Otherwise, elements of *proto-load-path* are
	 ;; used.
	 (locations  (if (eq (first (pathname-directory path)) :absolute)
			 (list path)
			 (map 'list (curry #'merge-pathnames path)
			      *proto-load-path*)))
	 ;; Restrict locations to existing files and process candidate
	 ;; set.
	 (candidates (remove-if-not #'probe-file locations)))
    (load/text (cond
		 ((emptyp candidates)
		  (cannot-resolve-import path locations))
		 ((not (length= 1 candidates))
		  (ecase if-ambiguous
		    (:first (first candidates))
		    (:error (ambiguous-import path candidates))))
		 (t
		  (first candidates))))))
