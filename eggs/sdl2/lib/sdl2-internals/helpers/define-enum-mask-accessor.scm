;;
;; chicken-sdl2: CHICKEN Scheme bindings to Simple DirectMedia Layer 2
;;
;; Copyright © 2013, 2015-2016  John Croisant.
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;; - Redistributions of source code must retain the above copyright
;;   notice, this list of conditions and the following disclaimer.
;;
;; - Redistributions in binary form must reproduce the above copyright
;;   notice, this list of conditions and the following disclaimer in
;;   the documentation and/or other materials provided with the
;;   distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
;; FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
;; COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
;; INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
;; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
;; OF THE POSSIBILITY OF SUCH DAMAGE.


;;; Macro: define-enum-mask-accessor
;;;
;;; Defines a getter and (optionally) setter that automatically
;;; convert enums from integer to list of symbols when getting, and
;;; from list of symbols to integer when setting. This macro is
;;; usually used for struct fields that hold an enum bitfield.
;;;
;;; This macro works by wrapping an existing "raw" getter and setter
;;; that work with integer values. The integer values are converted
;;; to/from lists of symbols using enum mask packer/unpacker
;;; procedures.
;;;
;;; Usage (getter and setter):
;;;
;;;   (define-enum-mask-accessor
;;;     getter: (GETTER
;;;              raw:    GETTER-RAW
;;;              unpack: UNPACKER
;;;              exact:  DEFAULT-EXACT?)
;;;     setter: (SETTER
;;;              raw:    SETTER-RAW
;;;              pack:   PACKER))
;;;
;;; Usage (getter only):
;;;
;;;   (define-enum-mask-accessor
;;;     getter: (GETTER
;;;              raw:    GETTER-RAW
;;;              unpack: UNPACKER
;;;              exact:  DEFAULT-EXACT?))
;;;
;;; GETTER is the name for a getter procedure that returns the field
;;; value as a list of symbols. The getter procedure accepts one
;;; required argument, an object that will be passed to GETTER-RAW,
;;; and one optional argument, which controls whether bitmasks must
;;; match exactly (see define-enum-mask-unpacker). The getter
;;; procedure will be defined by this macro. If a setter is also
;;; specified, the getter procedure will be enhanced to work with
;;; "generalized set!" (SRFI 17).
;;;
;;; GETTER-RAW is the name of an existing struct field getter that
;;; returns an integer value.
;;;
;;; UNPACKER is the name of an existing procedure that unpacks an
;;; integer value to a list of symbols. It is used to convert the
;;; value returned by GETTER-RAW. Usually UNPACKER is a procedure
;;; defined with define-enum-mask-unpacker.
;;;
;;; DEFAULT-EXACT? must be #t or #f, and determines the default value
;;; for the optional argument to the getter procedure, which controls
;;; whether bitmasks much match exactly or not.
;;;
;;; SETTER is the name for a setter procedure that accepts an object
;;; and a list of enum symbols. The setter procedure will also accept
;;; an integer, which is assumed to be an already-packed bitfield
;;; value. The setter will throw an error if given a list containing
;;; an unrecognized symbol, or invalid type. The setter procedure will
;;; be defined by this macro.
;;;
;;; SETTER-RAW is the name of an existing procedure which sets a
;;; struct field to an integer value.
;;;
;;; PACKER is the name of an existing procedure that packs a list of
;;; enum symbols to an integer value. It is used to convert a list of
;;; symbols before passing it to SETTER-RAW. Usually PACKER is a
;;; procedure defined with define-enum-mask-packer.
;;;
;;; Example:
;;;
;;;   (define-enum-mask-accessor
;;;     getter: (keysym-mod
;;;              raw:    keysym-mod-raw
;;;              unpack: unpack-keymods
;;;              exact:  #f)
;;;     setter: (keysym-mod-set!
;;;              raw:    keysym-mod-raw-set!
;;;              pack:   pack-keymods))
;;;
(define-syntax define-enum-mask-accessor
  (syntax-rules (getter: raw: unpack: exact: setter: pack:)
    ;; Getter and setter
    ((define-enum-mask-accessor
       getter: (getter-name
                raw:    getter-raw
                unpack: unpacker
                exact:  default-exact?)
       setter: (setter-name
                raw:    setter-raw
                pack:   packer))
     (begin
       (define (setter-name record new)
         (setter-raw
          record
          (cond ((integer? new) new)
                ((list? new)
                 (packer
                  new
                  (lambda (x)
                    (error 'setter-name "unrecognized enum value" x))))
                (else
                 (error 'setter-name
                        "invalid type (expected list of symbols, or integer)"
                        new)))))
       (define (getter-name record #!optional (exact? default-exact?))
         (unpacker (getter-raw record) exact?))
       (set! (setter getter-name) setter-name)))

    ;; Only getter
    ((define-enum-mask-accessor
       getter: (getter-name
                raw:    getter-raw
                unpack: unpacker
                exact:  default-exact?))
     (define (getter-name record #!optional (exact? default-exact?))
       (unpacker (getter-raw record) exact?)))))
