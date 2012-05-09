;;; coders.lisp ---
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

(cl:in-package :protocl.coding)

(eval-when (:compile-toplevel :load-toplevel)
  (defvar *default-optimization-settings* '((speed 0) (debug 3) (safety 3) (compilation-speed 0))
    "TODO(jmoringe): document"))

(defmacro define-wrapper ((name type
			   &key
			   (optimize *default-optimization-settings*)))
  "TODO(jmoringe): document"
  (let+ (((&flet make-name (prefix operation)
	    (format-symbol *package* "~@[~A~]~A~{-~A~}"
			   prefix operation (ensure-list name))))
	 ((%size-of-name %encode-name %decode-name)
	  (map 'list (curry #'make-name "%") '(:size-of :encode :decode)))
	 ((size-of-name encode-name decode-name)
	  (map 'list (curry #'make-name nil) '(:size-of :encode :decode))))
    `(progn
       (declaim (ftype (function (,type) non-negative-fixnum) ,size-of-name))

       (defun ,size-of-name (value)
	 ,@(when optimize
	     `((declare (optimize ,@optimize))))
	 (,%size-of-name value))

       (declaim (ftype (function (,type
				  &optional
				  octet-vector
				  non-negative-fixnum)
				 (values non-negative-fixnum octet-vector))
		       ,encode-name))

       (defun ,encode-name (value
			    &optional
			    (buffer (make-octet-vector (,size-of-name value)))
			    (start  0))
	 ,@(when optimize
	     `((declare (optimize ,@optimize))))
	 (values (,%encode-name value buffer start) buffer))

       (declaim (ftype (function (octet-vector
				  &optional
				  non-negative-fixnum)
				 (values ,type non-negative-fixnum))
		       ,decode-name))

       (defun ,decode-name (buffer
			    &optional
			    (start 0))
	 ,@(when optimize
	     `((declare (optimize ,@optimize))))
	 (,%decode-name buffer start)))))

(defmacro define-coder ((name type
			 &key
			 (optimize        *default-optimization-settings*)
			 (define-wrapper? t))
			&key
			(size   (required-argument :size))
			(encode (required-argument :encode))
			(decode (required-argument :decode)))
  "TODO(jmoringe): document"
  (let+ (((&flet make-name (operation)
	    (format-symbol *package* "%~A~{-~A~}"
			   operation (ensure-list name))))
	 ((size-of-name encode-name decode-name)
	  (map 'list #'make-name '(:size-of :encode :decode))))
    `(progn
       (declaim (ftype (function (,type) non-negative-fixnum) ,size-of-name)
		(inline ,size-of-name))

       (defun ,size-of-name (value)
	 (declare (ignorable value))
	 ,@(when optimize
	   `((declare (optimize ,@optimize))))
	 ,size)

       (declaim (ftype (function (,type octet-vector non-negative-fixnum)
				 non-negative-fixnum)
		       ,encode-name)
		(inline ,encode-name))

       (defun ,encode-name (value buffer start)
	 ,@(when optimize
	   `((declare (optimize ,@optimize))))
	 ,encode)

       (declaim (ftype (function (octet-vector non-negative-fixnum)
				 (values ,type non-negative-fixnum))
		       ,decode-name)
		(inline ,decode-name))

       (defun ,decode-name (buffer start)
	 ,@(when optimize
	   `((declare (optimize ,@optimize))))
	 ,decode)

       ,@(when define-wrapper?
	   `((define-wrapper (,name ,type)))))))

(define-coder (bool t)
  :size
  1
  :encode
  (progn
    (setf (aref buffer start) (if value 1 0))
    1)
  :decode
  (values (case (aref buffer start)
            (0 nil)
            (1 t)
            (t (error "Invalid boolean value")))
          1))

(define-coder (uvarint (unsigned-byte 64))
  :size
  (max 1 (ceiling (integer-length value) 7))
  :encode
  (loop
     for v      = value then (ash v -7)
     for v-next = (ash v -7)
     for j from 0 below most-positive-fixnum
     for i      = (+ start j)
     until (or (and (zerop v) (> j 0)))
     do (progn
          (setf (aref buffer i)
                (logior (ldb (byte 7 0) v)
                        (if (zerop v-next) 0 (ash 1 7)))))
     finally (return (- i start)))
  :decode
  (loop
     for offset from start  ; position in buffer
     for k      from 0 by 7 ; position in integer
     for octet  =    (aref buffer offset)
     for piece  =    (ldb (byte 7 0) octet)
     for accum  =    piece then (dpb piece (byte 7 k) accum)
     when (not (logbitp 7 octet))
     return (values accum (1+ (- offset start)))))

(declaim (ftype (function (integer) integer) zigzag unzigzag)
	 (inline zigzag unzigzag))

(defun zigzag (value)
  (let ((signum (signum value)))
    (- (* 2 (abs value))
       (* signum (ash (1- signum) -1)))))

(defun unzigzag (value)
  (let ((lowbit (ldb (byte 1 0) value)))
    (* (ash (+ value lowbit) -1)
       (- 1 (* 2 lowbit)))))

(define-coder (svarint integer)
  :size
  (%size-of-uvarint (zigzag value))
  :encode
  (%encode-uvarint (zigzag value) buffer start)
  :decode
  (multiple-value-bind (raw length) (%decode-uvarint buffer start)
    (values (unzigzag raw) length)))


;;; Width- and sign-specified types
;;

(macrolet
    ((define-varint-coder (size signed?)
       (let ((name      (format nil "VAR~:[U~;~]INT~D" signed? size))
	     (byte-type (list (if signed? 'signed-byte 'unsigned-byte) size))
	     (mask/size (1- (ash 1 (1- size))))
	     (mask/64   (1- (ash 1 64))))
	 `(define-coder (,name ,byte-type)
	    :size
	    (let ((raw ,(if signed? `(logand value ,mask/64) 'value)))
	      (%size-of-uvarint raw))
	    :decode
	    (multiple-value-bind (raw length)
		(%decode-uvarint buffer start)
	      (values
	       ,(if signed?
		    `(if (plusp (ldb (byte 1 ,(1- size)) raw))
			 (- (- ,(1+ mask/size) (logand raw ,mask/size)))
			 raw)
		    'raw)
	       length))
	    :encode
	    (let ((raw ,(if signed? `(logand value ,mask/64) 'value)))
	      (%encode-uvarint raw buffer start))))))

  (define-varint-coder 32 t)
  (define-varint-coder 32 nil)
  (define-varint-coder 64 t)
  (define-varint-coder 64 nil))


;;; String coders
;;

(define-coder (string string)
  :size
  (length (sb-ext:string-to-octets value))
  :encode
  (let ((octets (sb-ext:string-to-octets value)))
    (replace buffer octets :start1 start)
    (length octets))
  :decode
  (values (sb-ext:octets-to-string buffer :start start)
	  (- (length buffer) start)))


;;; start-code coder
;;

(declaim (ftype (function (start-code/code/cons) start-code/number)
		make-start-code)
	 (inline make-start-code))

(defun+ make-start-code ((field-number . wire-type))
  (logior (ash field-number 3) wire-type))

(define-coder (start-code/code start-code/code/cons)
  :size
  (%size-of-uvarint (make-start-code value))
  :encode
  (%encode-uvarint (make-start-code value) buffer start)
  :decode
  (let+ (((&values number-and-wire-type length)
	  (%decode-uvarint buffer start))
	 (number    (ash number-and-wire-type  -3))
	 (wire-type (ldb (byte 3 0) number-and-wire-type)))
    (unless (typep wire-type 'wire-type/code)
      (error 'invalid-wire-type
	     :offset     start
	     :designator wire-type))
    (values (cons number wire-type) length)))

(define-coder (start-code/name start-code/name/cons)
  :size
  (let+ (((field-number . name) value))
    (%size-of-start-code/code (cons field-number (wire-type-name->code name))))
  :encode
  (let+ (((field-number . name) value))
    (%encode-start-code/code (cons field-number (wire-type-name->code name))
			     buffer start))
  :decode
  (let+ (((&values (field-number . code) length)
	  (%decode-start-code/code buffer start)))
    (values (cons field-number (wire-type-code->name code))
	    length)))
