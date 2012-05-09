;;; types.lisp --- Types used in the protocl system.
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

(cl:in-package :protocl)


;;; Wire-types
;;

(deftype field-number ()
  'non-negative-fixnum)

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

(defun varint-enum-p (type)
  (or (varint-p type)
      (enum-type-p type)))
;;; TODO(jmoringe): decide how enums should be represented

(deftype svarint ()
  '(member type :sint32 :sint64))

(deftype uvarint ()
  '(and varint (not svarint)))

(deftype integer-type ()
  '(or varint-enum fixed))

(deftype length-delim ()
  '(and (not fixed64) (not fixed32) (not varint)))

(deftype message-type ()
  '(cons :message (cons symbol null)))

(deftype enum-type ()
  '(cons :enum (cons symbol null)))

(deftype proto-type ()
  `(or messate-type
       enum-type
       (member ,@+primitive-proto-types+)))


;;; Type queries
;;

(defun fixed-size (type)
  (etypecase type
    (fixed32 4)
    (fixed64 8)))


;;; Type conversions
;;

(defun proto-type->wire-type (type &optional repeated? packed?)
  (check-type type proto-type "a protocol buffer type designator")

  (cond
    ((and repeated? packed?) 2)
    ((varint-enum-p type)    0)
    ((fixed64-p type)        1)
    ((fixed32-p type)        5)
    ((length-delim-p type)   2)
    (t                       2)))

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
	   (if repeated? `(array ,base-type *) base-type))
	 (maybe-optional (base-type)
	   (if optional? `(or null ,base-type) base-type)))
    (maybe-optional
     (maybe-repeated
      (scalar-proto-type->lisp-type type)))))
