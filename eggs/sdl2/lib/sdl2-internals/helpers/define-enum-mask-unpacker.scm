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


;;; Macro: define-enum-mask-unpacker
;;;
;;; Defines a procedure that will "unpack" an integer bitfield into a
;;; list of enum symbols. This is intended to supplement
;;; define-enum-mappings in cases where the enums are bitmasks, e.g.
;;; flags and keymods.
;;;
;;; The unpacker procedure works by comparing the bitfield to a list
;;; of pre-specified bitmasks (using bitwise-and with each bitmask
;;; individually), then converting each matching bitmask into a symbol
;;; (using the specified VALUE->SYMBOL conversion procedure).
;;;
;;; The unpacker procedure will accept one required argument, an
;;; integer bitfield, and one optional argument, a boolean that
;;; determines whether bitmasks much match exactly (default #f). If
;;; #t, each bitmask matches if *every* true bit in the bitmask is
;;; true in the bitfield. If #f, each bitmask matches if *any* true
;;; bit in the bitmask is true in the bitfield.
;;;
;;; Usage:
;;;
;;;   (define-enum-mask-unpacker UNPACKER
;;;     VALUE->SYMBOL
;;;     BITMASKS-EXPR)
;;;
;;; UNPACKER is the name for the unpacker procedure, which will be
;;; defined by this macro.
;;;
;;; VALUE->SYMBOL is the name of an existing procedure for converting
;;; an integer value (bitmask) into a symbol. Usually VALUE->SYMBOL is
;;; defined using the define-enum-mappings macro.
;;;
;;; BITMASKS-EXPR is an expression that evaluates to a list of all the
;;; integer bitmasks that should be recognized. The expression will be
;;; evaluated once, at the time the unpacker procedure is defined, and
;;; the value re-used every time the unpacker procedure is called.
;;; Usually this is a simple (list ...) form explicitly listing all
;;; the constants (if there are not too many). Another approach is to
;;; do (hash-table-keys %value->symbol-table), using a conversion
;;; table from define-enum-mappings, although this makes the order of
;;; symbols in the unpacker's result unpredictable (and thus maybe not
;;; as easy for a human to read at a glance).
;;;
;;; Example:
;;;
;;;   ;; Assuming init-flag->symbol and SDL_INIT_* constants have been
;;;   ;; defined as in the example for define-enum-mappings, above.
;;;   (define-enum-mask-unpacker unpack-init-flags
;;;     init-flag->symbol
;;;     (list SDL_INIT_TIMER
;;;           SDL_INIT_AUDIO
;;;           SDL_INIT_VIDEO))
;;;
;;;   ;; Alternative BITMASKS-EXPR, assuming %init-flag->symbol-table
;;;   ;; is the conversion table defined with define-enum-mappings.
;;;   (define-enum-mask-unpacker unpack-init-flags
;;;     init-flag->symbol
;;;     (hash-table-keys %init-flag->symbol-table))
;;;
(define-syntax define-enum-mask-unpacker
  (syntax-rules ()
    ((define-enum-mask-unpacker unpacker
       value->symbol
       bitmasks-expr)
     (define (unpacker bitfield #!optional (exact? #f))
       (define bitmasks bitmasks-expr)
       (map value->symbol
            (%separate-bitfield
             exact? bitfield bitmasks))))))



;;; Compares a bitfield (an integer) to a list of bitmasks (also
;;; integers), and returns a list of the matching bitmasks. This is
;;; used to "separate" a bitfield into its constituent bitmasks.
;;;
;;; exact? affects the matching behavior. If #t, each bitmask matches
;;; if *every* true bit in the bitmask is true in the bitfield. If #f,
;;; each bitmask matches if *any* true bit in the bitmask is true in
;;; the bitfield.
;;;
;;; Example:
;;;
;;;   ;; Exact match. The mask #b111 does NOT match #b011.
;;;   (%separate-bitfield
;;;    #t
;;;    #b011
;;;    (list #b001 #b010 #b100 #b111))
;;;   ;; => (list #b001 #b010)
;;;
;;;   ;; Not exact match. The mask #b111 matches #b011.
;;;   (%separate-bitfield
;;;    #f
;;;    #b011
;;;    (list #b001 #b010 #b100 #b111))
;;;   ;; => (list #b001 #b010 #b111)
;;;
(define (%separate-bitfield exact? bitfield bitmasks)
  (filter (if exact?
              (lambda (bitmask)
                (= bitmask (bitwise-and bitfield bitmask)))
              (lambda (bitmask)
                (not (zero? (bitwise-and bitfield bitmask)))))
          bitmasks))
