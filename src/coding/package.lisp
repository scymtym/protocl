;;; package.lisp --- Package definition for the coding module.
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

(cl:defpackage :protobuf.coding
  (:nicknames
   :pb.c)

  (:use
   :cl
   :alexandria
   :let-plus
   :nibbles

   :protobuf)

  (:export
   :%size-of-bool            :%encode-bool            :%decode-bool

   :%size-of-varint32        :%encode-varint32        :%decode-varint32
   :%size-of-varuint32       :%encode-varuint32       :%decode-varuint32
   :%size-of-varint64        :%encode-varint64        :%decode-varint64
   :%size-of-varuint64       :%encode-varuint64       :%decode-varuint64
   :%size-of-svarint         :%encode-svarint         :%decode-svarint
   :%size-of-uvarint         :%encode-uvarint         :%decode-uvarint

   :%size-of-string          :%encode-string          :%decode-string

   :%size-of-start-code/code :%encode-start-code/code :%decode-start-code/code
   :%size-of-start-code/name :%encode-start-code/name :%decode-start-code/name)

  (:documentation
   "This package contains function for decoding and encoding protocol
buffer types from/to `nibbles:octet-vector' buffers."))
