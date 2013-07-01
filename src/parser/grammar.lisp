;;;; grammar.lisp --- Protocol buffer descriptor grammar.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:protocl.parser)

;; TODO(jmoringe, 2012-04-16): duplicated in src/types.lisp

(defconstant +most-positive-field-number+
  #x1fffffff)

(deftype field-number ()
  `(integer 0 ,+most-positive-field-number+))

(defconstant +most-negative-enum-value+ #x-80000000)

(defconstant +most-positive-enum-value+ #x7fffffff)

(deftype enum-value ()
  `(integer ,+most-negative-enum-value+ ,+most-positive-enum-value+))

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
    (error "~@<Number ~A of field ~S is not a positive integer in the range [~D, ~D].~@:>"
           number name 0 +most-positive-field-number+))

  (when-let ((entry (find number *fields* :test #'= :key #'car)))
    (error "~@<Duplicate field number ~D for field ~S; previously used for field ~S.~@:>"
           number name (cdr entry)))
  (when-let ((entry (find name *fields* :test #'string= :key #'cdr)))
    (error "~@<Duplicate field name ~S.~@:>"
           name))

  (push (cons number name) *fields*))

(defmacro defrule/ws (name-and-options
                      expression &body options)
  "Like `esrap:defule' but define additional rules named NAME/WS and
NAME/?WS which respectively require/ allow EXPRESSION to be followed
by whitespace.

NAME-AND-OPTIONS can either just be a rule name or list of the form

  (NAME &key WS? ?WS? DEFINER)

where WS? and ?WS? control which of the NAME/WS and NAME/?WS rules
should be generated. Default is generating both.

DEFINER is the name of the macro used to define \"main\"
rule. Defaults to `esrap:defrule'.
"
  (let+ (((name
           &key
           (definer         'defrule))
          (ensure-list name-and-options))
         (name/ws  (format-symbol *package* "~A/WS" name))
         (name/?ws (format-symbol *package* "~A/?WS" name)))
    `(progn
       (,definer ,name
                 ,expression
                 ,@options)
       (defrule ,name/ws
           (and ,name whitespace)
         (:function first))
       (defrule ,name/?ws
           (and ,name (? whitespace))
         (:function first)))))

(defmacro defrule/locations (symbol expression &body options)
  "Like `esrap:defrule' but add code which captures the source
location and transfers it to conditions signaled from the rule."
  (labels
      ((ensure-bounds-args (option &optional transform)
         (let+ (((keyword args &body body) option)
                ((&values body declarations) (parse-body body))
                ((&optional &bounds-supplied? (start-var (gensym)) (end-var (gensym)))
                 (if-let ((position (position 'esrap:&bounds args)))
                   (cons t (subseq args (1+ position)))
                   (list nil))))
           (values
            `(,keyword (,@args ,@(unless &bounds-supplied?
                                   `(esrap:&bounds ,start-var ,end-var)))
                       ,@declarations
                       ,@(if transform
                             (funcall transform body start-var end-var)
                             body))
            start-var end-var)))
       (replace-builder-functions (body start-var end-var)
         `((flet ,(mapcar
                   (lambda (name)
                     `(,name (&rest args)
                        (apply #',name (append
                                        args
                                        (list :bounds (cons ,start-var ,end-var))))))
                   '(make-comment make-syntax make-import make-option
                     make-enum-value make-enum make-field make-message
                     make-package1 make-file))
             ,@body)))
       (process-around (option)
         (ensure-bounds-args option #'replace-builder-functions))
       (process-lambda (option)
         (ensure-bounds-args option #'replace-builder-functions))
       (process-options (options)
         (mapcar (lambda (option)
                   (case (first option)
                     (:around
                      (process-around option))
                     ((:lambda :destructure)
                      (process-lambda option))
                     (t
                      option)))
                 (adjoin `(:around () (esrap:call-transform)) options
                         :key  #'first
                         :test #'eq))))
    `(defrule ,symbol ,expression
       ,@(process-options options))))

;;; Single-character and whitespace rules

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

(defrule semicolon
    #\;
  (:constant nil))

;;; Comment-related rules

(defrule same-line
    (* (not #\Newline))
  (:text t))

(defrule/locations comment/rest-of-line
    (and (and #\/ #\/) same-line (? #\Newline))
  (:destructure (open content close)
    (declare (ignore open close))
    (make-comment *builder* (string-trim '(#\/ #\Tab #\Space) content))))

(defrule comment-content/delimited
    (* (or (not #\*) (and #\* (! #\/)))))

(defrule comment-content/delimited/cleaned
    (and (? #\*) comment-content/delimited)
  (:destructure (star content)
    (declare (ignore star))
    (%cleanup-comment (esrap:text content))))

(defrule/locations comment/delimited
    (and (and #\/ #\*) comment-content/delimited/cleaned (and #\* #\/))
  (:destructure (open content close)
    (declare (ignore open close))
    (make-comment *builder* content)))

(defrule comment
    (or comment/rest-of-line comment/delimited))

;;; Literals and names

(defrule/ws number
    (+ (digit-char-p character))
  (:lambda (digits)
    (parse-integer (esrap:text digits))))

(defrule/ws string
    (and #\" (* (or (and #\\ #\") (not #\"))) #\")
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

(defrule/ws (identifier/checked
             :definer defrule/locations)
    identifier
  (:lambda (name)
    (check-name name 1)))

;;; Types

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

(defrule/locations syntax
    (and (and "syntax" (? whitespace))
         =/ws #\" (* (alphanumericp character)) #\")
  (:destructure (keyword equals open value close)
    (declare (ignore keyword equals open close))
    (make-syntax *builder* (esrap:text value))))

(defrule/locations import
    (and (and "import" whitespace) string semicolon)
  (:destructure (keyword value semicolon)
    (declare (ignore keyword semicolon))
    (make-import *builder* value)))

(defrule/locations option-body
    (and identifier/?ws =/ws (or string number identifier))
  (:destructure (name equals value)
    (declare (ignore equals))
    (check-option name)
    (make-option *builder* name value)))

(defrule option
    (and (and "option" whitespace) option-body)
  (:function second))

;;; Messages

(defrule/ws label
    (or "required" "optional" "repeated")
  (:lambda (label)
    (make-keyword (string-upcase label))))

(defrule field-options
    (and [/ws option-body (* (and \,/ws option-body)) ]/ws)
  (:destructure (open first rest close)
    (declare (ignore open close))
    (cons first (mapcar #'second rest))))

(defrule/locations field
    (and label/ws type1/ws identifier/?ws =/ws number/?ws (? field-options) semicolon)
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
    (or comment
        (or message enum field option)
        whitespace semicolon))

(defrule/locations message
    (and (and "message" whitespace) identifier/checked/?ws
         {/ws (* message-element) #\})
  (:destructure (keyword name open content close)
    (declare (ignore keyword open close))
    (reduce (curry #'add-child *builder*)
            (remove nil content)
            :initial-value (make-message *builder* name)))
  (:around ()
    (let ((*fields* nil)
          (*path*   *path*)
          (*names*  (cons (list) *names*)))
      (esrap:call-transform))))

;;; Enum

(defrule/locations enum-value
    (and identifier/?ws =/ws number/?ws semicolon)
  (:destructure (name equals value semicolon)
    (declare (ignore equals semicolon))
    (check-name name)
    (unless (typep value 'enum-value)
      (error "~@<Enum value ~D (named ~S) is not an integer in the range [~D, ~D].~@:>"
             value name
             +most-negative-enum-value+ +most-positive-enum-value+))
    (make-enum-value *builder* name value))
  (:around ()
    (let ((*path* *path*))
      (esrap:call-transform))))

(defrule enum-element
    (or comment
        enum-value
        whitespace semicolon))

(defrule/locations enum
    (and (and "enum" whitespace) identifier/checked/?ws
         {/ws (* enum-element) #\})
  (:destructure (keyword name open content close)
    (declare (ignore keyword open close))
    (reduce (curry #'add-child *builder*)
            (remove nil content)
            :initial-value (make-enum *builder* name)))
  (:around ()
    (let ((*path*  *path*)
          (*names* (cons (list) *names*)))
      (esrap:call-transform))))

;;; Package

(defrule/locations package
    (and (and "package" whitespace) dotted-identifier semicolon)
  (:destructure (keyword name semicolon)
    (declare (ignore keyword semicolon))
    (let ((qname (cons :absolute name)))
      (setf *path*     (list qname)
            *package1* (make-package1 *builder* qname)))))

;;; Root

(defrule packaged-element
    (or comment
        syntax option
        message enum)
  (:lambda (element)
    (setf *package1* (add-child *builder* *package1* element))
    nil))

(defrule/locations proto
    (* (or package
           import
           packaged-element
           whitespace semicolon))
  ;; Root production; parses top-level comments, package directives
  ;; and top-level definitions. Package directives change the value of
  ;; `*package1*' and affect subsequent top-level definitions.
  (:lambda (content)
    (reduce (curry #'add-child *builder*)
            (remove nil content)
            :initial-value (make-file *builder*)))
  (:around ()
    (let* ((default-package (make-package1 *builder* '(:absolute "default")))
           (*package1*      default-package)
           (*path*          (list (list :absolute)))
           (*names*         (list (list)))
           (*options*       (list)))
      (add-child *builder* (esrap:call-transform) default-package))))

;;; Utility functions

(defun %cleanup-comment (content)
  (let+ (((&flet starts-with-or-harmless (character)
            #'(lambda (line)
                (or (emptyp line) (starts-with character line)))))
         ((&flet trim-first (line)
            (if (emptyp line)
                line
                (subseq line 1))))
         ((&labels trim (lines)
            "Strip common leading whitespace from LINES."
            (cond
              ((every #'emptyp lines) ; terminate recursion
               nil)
              ((or (every (starts-with-or-harmless #\Space) lines)
                   (every (starts-with-or-harmless #\Tab)   lines)
                   (every (starts-with-or-harmless #\*)     lines))
               (trim (mapcar #'trim-first lines)))
              (t
               (mapcar (curry #'string-right-trim '(#\Space #\Tab)) lines)))))
         (lines (trim (split-sequence #\Newline content))))
    (string-trim '(#\Newline) (format nil "~{~A~^~%~}" lines))))
