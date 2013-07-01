;;;; types.lisp --- Types used in the protocl system.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :protocl)


;;; Wire-types
;;

(defconstant +most-positive-field-number+
  #x1fffffff)

(deftype field-number ()
  `(integer 0 ,+most-positive-field-number+))

(deftype wire-type/code ()
  '(member 0 1 2 5))

(deftype wire-type/name ()
  '(member :varint :fixed64 :fixed32 :size-delimited))

(declaim (ftype (function (wire-type/code) wire-type/name)
		wire-type-code->name)
	 (ftype (function (wire-type/name) wire-type/code)
		wire-type-name->code)
	 (inline wire-type-code->name wire-type-name->code))

(defun wire-type-code->name (code)
  (ecase code
    (0 :varint)
    (1 :fixed64)
    (5 :fixed32)
    (2 :size-delimited)))

(defun wire-type-name->code (name)
  (ecase name
    (:varint         0)
    (:fixed64        1)
    (:fixed32        5)
    (:size-delimited 2)))


;;; Start-codes
;;

(deftype start-code/code/cons ()
  '(cons field-number wire-type/code))

(deftype start-code/name/cons ()
  '(cons field-number wire-type/name))

(deftype start-code/number ()
  'non-negative-fixnum)


;;; Symbol-designated proto types
;;

(defconstant +most-negative-enum-value+ #x-80000000)

(defconstant +most-positive-enum-value+ #x7fffffff)

(deftype enum-value ()
  `(integer ,+most-negative-enum-value+ ,+most-positive-enum-value+))

(define-constant +primitive-proto-types+
    '(:bool

      :fixed32 :sfixed32
      :fixed64 :sfixed64

      :float   :double

      :int32   :uint32   :sint32
      :int64   :uint64   :sint64

      :string  :bytes)
  :test #'equal)

(deftype primitive-type ()
  `(member ,@+primitive-proto-types+))

(deftype fixed32-type ()
  '(member :float :fixed32 :sfixed32))

(deftype fixed64-type ()
  '(member :double :fixed64 :sfixed64))

(deftype fixed-type ()
  '(or fixed32 fixed64))

(deftype varint-type ()
  '(member :bool
           :int32 :sint32 :uint32
           :int64 :sint64 :uint64
           :enum))

(deftype svarint-type ()
  '(member :sint32 :sint64))

(deftype uvarint-type ()
  '(and varint-type (not svarint-type)))

(deftype integer-type ()
  '(or varint-enum fixed-type))

(deftype length-delim-type ()
  '(and (not fixed64-type) (not fixed32-type) (not varint-type)))

(deftype message-type ()
  '(cons (eql :message) (cons symbol null)))

(deftype enum-type ()
  '(cons (eql :enum) (cons symbol null)))

(deftype proto-type ()
  `(or message-type
       enum-type
       (member ,@+primitive-proto-types+)))

;; TODO remove
(defun varint-enum-p (type)
  (typep type '(or varint-type enum-type)))
;;; TODO(jmoringe): decide how enums should be represented



;;; Type queries
;;

(defun fixed-size (type)
  (etypecase type
    (fixed32-type 4)
    (fixed64-type 8)))


;;; Type conversions
;;

(defun proto-type->wire-type (type &optional repeated? packed?)
  (check-type type proto-type "a protocol buffer type designator")

  (cond
    ((and repeated? packed?)         :size-delimited)
    ((varint-enum-p type)            :varint)
    ((typep type 'fixed64-type)      :fixed64)
    ((typep type 'fixed32-type)      :fixed32)
    ((typep type 'length-delim-type) :size-delimited)
    (t                               :size-delimited)))

(defun scalar-proto-type->lisp-type (type)
  (check-type type proto-type "a protocol buffer type designator")

  (case type
    ((:int32  :sfixed32 :sint32) '(cl:signed-byte 32))
    ((:uint32 :fixed32)          '(cl:unsigned-byte 32))
    ((:int64  :sfixed64 :sint64) '(cl:signed-byte 64))
    ((:uint64 :fixed64)          '(cl:unsigned-byte 64))
    (:bool                       'cl:boolean)
    (:double                     'cl:double-float)
    (:float                      'cl:single-float)
    (:string                     'cl:string)
    (:bytes                      '(cl:simple-array (cl:unsigned-byte 8) *))
    ((enum-type-p type)          type)   ;; enum
    (t                           type))) ;; message

(defun proto-type->lisp-type (type &optional repeated? optional?)
  (flet ((maybe-repeated (base-type)
	   (if repeated? `(array ,base-type (*)) base-type))
	 (maybe-optional (base-type)
	   (if optional? `(or null ,base-type) base-type)))
    (maybe-optional
     (maybe-repeated
      (scalar-proto-type->lisp-type type)))))
