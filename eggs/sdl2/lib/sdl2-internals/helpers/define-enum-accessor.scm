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


;;; Macro: define-enum-accessor
;;;
;;; Defines a getter and (optionally) setter that automatically
;;; convert enums from integer to symbol when getting, and from symbol
;;; to integer when setting. This macro is usually used for struct
;;; fields that hold an enum. (See define-enum-mask-accessor, below,
;;; for an accessor that works with enums that are bitmasks.)
;;;
;;; This macro works by wrapping an existing "raw" getter and setter
;;; that work with integer values. The integer values are converted
;;; to/from symbols using enum converter procedures.
;;;
;;; Usage (getter and setter):
;;;
;;;   (define-enum-accessor
;;;     getter: (GETTER
;;;              raw:   GETTER-RAW
;;;              conv:  VALUE->SYMBOL)
;;;     setter: (SETTER
;;;              raw:   SETTER-RAW
;;;              conv:  SYMBOL->VALUE))
;;;
;;; Usage (getter only):
;;;
;;;   (define-enum-accessor
;;;     getter: (GETTER
;;;              raw:  GETTER-RAW
;;;              conv: VALUE->SYMBOL))
;;;
;;; GETTER is the name for a getter procedure that returns an enum as
;;; a symbol. The getter procedure accept one argument, an object that
;;; will be passed to GETTER-RAW. If GETTER-RAW returns an
;;; unrecognized integer value, GETTER will return that integer value
;;; as-is. The getter procedure will be defined by this macro. If a
;;; setter is also specified, the getter procedure will be enhanced to
;;; work with "generalized set!" (SRFI 17).
;;;
;;; GETTER-RAW is the name of an existing struct field getter that
;;; returns an integer enum value.
;;;
;;; VALUE->SYMBOL is the name of an existing procedure that converts
;;; an enum integer value to a symbol. It is used to convert the value
;;; returned by GETTER-RAW. Usually VALUE->SYMBOL is a procedure
;;; defined with the define-enum-mappings macro.
;;;
;;; SETTER is the name for a setter procedure that accepts an object
;;; and an enum symbol. The setter procedure will also accept an
;;; integer, which is assumed to be an already-converted enum value.
;;; The setter will throw an error if given an unrecognized symbol or
;;; invalid type. The setter procedure will be defined by this macro.
;;;
;;; SETTER-RAW is the name of an existing procedure which sets a
;;; struct field to an integer enum value.
;;;
;;; SYMBOL->VALUE is the name of an existing procedure that converts
;;; an enum symbol to an integer value. It is used to convert a symbol
;;; before passing it to SETTER-RAW. Usually SYMBOL->VALUE is a
;;; procedure defined with the define-enum-mappings macro.
;;;
;;; Example:
;;;
;;;   (define-enum-accessor
;;;     getter: (keysym-scancode
;;;              raw:   keysym-scancode-raw
;;;              conv:  scancode->symbol)
;;;     setter: (keysym-scancode-set!
;;;              raw:   keysym-scancode-raw-set!
;;;              conv:  symbol->scancode))
;;;
(define-syntax define-enum-accessor
  (syntax-rules (getter: raw: conv: setter: error:)
    ;; Getter and setter
    ((define-enum-accessor
       getter: (getter-name
                raw:   getter-raw
                conv:  value->symbol)
       setter: (setter-name
                raw:   setter-raw
                conv:  symbol->value))
     (begin
       (define (setter-name record new)
         (setter-raw
          record
          (cond ((integer? new)
                 new)
                ((symbol? new)
                 (symbol->value
                  new
                  (lambda (x)
                    (error 'setter "unrecognized enum symbol" x))))
                (#t
                 (error 'setter
                        "invalid type (expected symbol or integer)"
                        new)))))
       (define (getter-name record)
         (value->symbol
          (getter-raw record)
          identity))
       (set! (setter getter-name) setter-name)))

    ;; Only getter
    ((define-enum-accessor
       getter: (getter-name
                raw:  getter-raw
                conv: value->symbol))
     (define (getter-name record)
       (value->symbol
        (getter-raw record)
        identity)))))
