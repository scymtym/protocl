;;; grammar.lisp --- Protocol buffer descriptor grammar.
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

(cl:in-package :protobuf.parser)

;;; TODO(jmoringe, 2012-04-16): duplicated in src/types.lisp

(deftype field-number ()
  'positive-integer)

(define-constant +primitive-types+
    '(:bool

      :fixed32 :sfixed32
      :fixed64 :sfixed64

      :float   :double

      :int32   :uint32   :sint32
      :int64   :uint64   :sint64

      :string  :bytes)
  :test #'equal)


;;; Special variables
;;

(declaim (special *builder* *package1* *path* *names* *options* *fields*))

(defvar *builder* nil
  "Bound to a builder object while parsing results are being
constructed.")

(defvar *package1* nil
  "Bound to the current package while parsing results are being
constructed. Initially bound to the default package.")

(defvar *path* nil
  "Bound to a list of names which describes the nesting of definitions
while parsing results are being constructed.")

(defvar *names* nil
  "Bound to a list of lists of names while parsing results are being
constructed. Used to detect name clashes.")

(defvar *options* nil
  "Bound to a list of option names for a specific context while
parsing results are being constructed. Used to detect option name
clashes.")

(defvar *fields* nil
  "Bound to a list of lists of field information entries of the form 

  (NUMBER . NAME)

while parsing results are being constructed. Used to detect
conflicting field declarations.")


;;; Utilities
;;

(defun check-name (name &optional (up 0))
  (when (member name (nth up *names*) :test #'string=)
    (error "~@<Duplicate name ~S.~@:>"
	   name))
  (push name (car (nthcdr up *names*)))
  (push name *path*)
  name)

(defun check-option (name)
  (when (member name *options* :test #'string=)
    (error "~@<Duplicate option ~S.~@:>"
	   name))
  (push name *options*))

(defun check-field (number name)
  ;; Check name and number.
  (check-name name)
  (unless (typep number 'field-number)
    (error "~@<Number ~A of field ~S is not a positive integer.~@:>"
	   number name))

  (when-let ((entry (find number *fields* :test #'= :key #'car)))
    (error "~@<Duplicate field number ~D for field ~S; previously used for field ~S.~@:>"
	   number name (cdr entry)))
  (when-let ((entry (find name *fields* :test #'string= :key #'cdr)))
    (error "~@<Duplicate field name ~S.~@:>"
	   name))
  (push (cons number name) *fields*))

(defmacro defrule/ws (name expression &body options)
  "Like `esrap:defule' but define an additional rule named NAME/WS
which requires EXPRESSION to be followed by whitespace."
  (let ((name/ws  (format-symbol *package* "~A/WS" name))
	(name/?ws (format-symbol *package* "~A/?WS" name)))
    `(progn
       (defrule ,name
	   ,expression
	 ,@options)
       (defrule ,name/ws
	   (and ,name whitespace)
	 (:function first))
       (defrule ,name/?ws
	   (and ,name (? whitespace))
	 (:function first)))))


;;; Single-character and whitespace rules
;;

(macrolet
    ((define-not (character)
       (let ((name (format-symbol *package* "NOT-~@:(~A~)"
				  (substitute #\- #\_ (char-name character)))))
	 `(defun ,name (character)
	    (not (char= character ,character))))))
  (define-not #\Newline)
  (define-not #\*)
  (define-not #\"))

(macrolet ((define-ws (character)
	     (let ((name (format-symbol *package* "~C/WS" character)))
	       `(defrule ,name
		    (and ,character (? whitespace))
		  (:constant nil)))))
  (define-ws #\=)
  (define-ws #\,)
  (define-ws #\[)
  (define-ws #\])
  (define-ws #\{)
  (define-ws #\}))

(defrule whitespace
    (+ (or #\Tab #\Space #\Newline))
  (:constant nil))


;;; Comment-related rules
;;

(defrule same-line
    (* (not-newline character))
  (:text t))

(defrule comment/rest-of-line
    (and (and #\/ #\/) same-line (? #\Newline))
  (:destructure (begin content end)
    (declare (ignore begin end))
    (make-comment *builder* content)))

(defrule comment-content/delimited
    (* (or (and #\* (! #\/))
	   (not-asterisk character)))
  (:text t))

(defrule comment/delimited
    (and (and #\/ #\*) comment-content/delimited (and #\* #\/))
  (:destructure (open content close)
    (declare (ignore open close))
    (make-comment *builder* content)))


;;; Literals and names
;;

(defrule/ws number
    (+ (digit-char-p character))
  (:lambda (digits)
    (parse-integer (esrap:text digits))))

(defrule/ws string
    (and #\"
	 (* (or (and #\\ #\")
		(not-quotation-mark character)))
	 #\")
  (:destructure (open characters close)
    (declare (ignore open close))
    (esrap:text (substitute #\" '("\\" "\"") characters
			    :test #'equal))))

(defrule/ws identifier
    (and (* (or (alpha-char-p character) #\_))
	 (* (or (alphanumericp character) #\_)))
  (:text t))

(defrule dotted-identifier
    (and identifier (* (and #\. identifier)))
  (:destructure (first rest)
    (cons first (mapcar #'second rest))))

(defrule/ws identifier/checked
    identifier
  (:lambda (name)
    (check-name name 1)))


;;; Types
;;

(macrolet
    ((frob ()
       `(defrule type/primitive
	    (or ,@(mapcar (compose #'string-downcase #'string)
			  +primitive-types+))
	  (:lambda (name)
	    (make-keyword (string-upcase name))))))
  (frob))

(defrule type/reference
    (and (? #\.) dotted-identifier)
  (:destructure (dot name)
    (cons (if dot :absolute :relative) name)))

(defrule/ws type1
    (or type/primitive type/reference))


;;; Option-stuff rules
;;

(defrule syntax
    (and (and "syntax" (? whitespace))
	 =/ws #\" (* (alphanumericp character)) #\")
  (:destructure (keyword equals open value close)
    (declare (ignore keyword equals open close))
    (make-syntax *builder* (esrap:text value))))

(defrule import
    (and (and "import" whitespace) string #\;)
  (:destructure (keyword value semicolon)
    (declare (ignore keyword semicolon))
    (make-import *builder* value)))

(defrule option-body
    (and identifier/?ws =/ws (or string number identifier))
  (:destructure (name equals value)
    (declare (ignore equals))
    (check-option name)
    (make-option *builder* name value)))

(defrule option
    (and (and "option" whitespace) option-body)
  (:function second))


;;; Messages
;;

(defrule/ws label
    (or "required" "optional" "repeated")
  (:lambda (label)
    (make-keyword (string-upcase label))))

(defrule field-options
    (and [/ws option-body (* (and \,/ws option-body)) ]/ws)
  (:destructure (open first rest close)
    (declare (ignore open close))
    (cons first (mapcar #'second rest))))

(defrule field
    (and label/ws type1/ws identifier/?ws =/ws number/?ws (? field-options) #\;)
  (:destructure (label type name equals number options semicolon)
    (declare (ignore equals semicolon))
    (check-field number name)
    (reduce (curry #'add-child *builder*) options
	    :initial-value (make-field *builder* name number type label)))
  (:around ()
    (let ((*path*    *path*)
	  (*options* nil))
      (esrap:call-transform))))

(defrule message-element
    (or comment/rest-of-line comment/delimited
	(or message enum field option)
	whitespace #\;))

(defrule message
    (and (and "message" whitespace) identifier/checked/?ws
	 {/ws (* message-element) }/ws)
  (:destructure (keyword name open content close)
    (declare (ignore keyword open close))
    #+no (check-name name 1)
    (reduce (curry #'add-child *builder*)
	    ;; remove ";" and ignored comments
	    (remove-if (disjoin #'stringp #'null) content)
	    :initial-value (make-message *builder* name)))
  (:around ()
    (let ((*fields* nil)
	  (*path*   *path*)
	  (*names*  (cons (list) *names*)))
      (esrap:call-transform))))


;;; Enum
;;

(defrule enum-value
    (and identifier/?ws =/ws number/?ws #\;)
  (:destructure (name equals value semicolon)
    (declare (ignore equals semicolon))
    (check-name name)
    (make-enum-value *builder* name value))
  (:around ()
    (let ((*path* *path*))
      (esrap:call-transform))))

(defrule enum-element
    (or comment/rest-of-line comment/delimited
	enum-value
	whitespace #\;))

(defrule enum
    (and (and "enum" whitespace) identifier/checked/?ws
	 {/ws (* enum-element) }/ws)
  (:destructure (keyword name open content close)
    (declare (ignore keyword open close))
    #+no (check-name name 1)
    (reduce (curry #'add-child *builder*)
	    ;; remove ";" and ignored comments
	    (remove-if (disjoin #'stringp #'null) content)
	    :initial-value (make-enum *builder* name)))
  (:around ()
    (let ((*path*  *path*)
	  (*names* (cons (list) *names*)))
      (esrap:call-transform))))


;;; Package
;;

(defrule package
    (and (and "package" whitespace) dotted-identifier #\;)
  (:destructure (keyword name semicolon)
    (declare (ignore keyword semicolon))
    (let ((qname (cons :absolute name)))
     (setf *path*     (list qname)
	   *package1* (make-package1 *builder* qname)))))


;;; Root
;;

(defrule packaged-element
    (or syntax import option message enum)
  (:lambda (element)
    (setf *package1* (add-child *builder* *package1* element))
    nil))

(defrule proto
    (* (or comment/rest-of-line comment/delimited
	   package
	   packaged-element
	   whitespace #\;))
  ;; Root production; parses top-level comments, package directives
  ;; and top-level definitions. Package directives change the value of
  ;; `*package1*' and affect subsequent top-level definitions.
  (:lambda (content)
    (reduce (curry #'add-child *builder*)
	    ;; remove everything but packages
	    (remove-if (disjoin #'stringp #'null) content)
	    :initial-value (make-file *builder*)))
  (:around ()
    (let* ((default-package (make-package1 *builder* '(:absolute "default")))
	   (*package1*      default-package)
	   (*path*          (list (list :absolute)))
	   (*names*         (list (list)))
	   (*options*       (list)))
      (add-child *builder* (esrap:call-transform) default-package))))
