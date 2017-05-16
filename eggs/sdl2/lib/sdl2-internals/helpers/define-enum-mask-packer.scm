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


;;; Macro: define-enum-mask-packer
;;;
;;; Defines a procedure that will "pack" a list of enum symbols into
;;; an integer bitfield. This is intended to supplement
;;; define-enum-mappings in cases where the enums are bitmasks, e.g.
;;; flags and keymods.
;;;
;;; The packer procedure works by converting each symbol in the list
;;; into its integer value (using the specified SYMBOL->VALUE
;;; conversion procedure), then combining them using bitwise-ior.
;;;
;;; The packer procedure accepts an optional argument, a procedure
;;; which will be called if a symbol cannot be converted to an
;;; integer. In such a case, the procedure will be called with one
;;; argument, the symbol that could not be converted. You may use this
;;; callback procedure to throw an error or provide a default value
;;; for the symbol (e.g. 0 to ignore unrecognized symbols).
;;;
;;; The packer procedure will alternatively accept an integer instead
;;; of a list of symbols. This is assumed to be an already-packed
;;; bitfield, so it is immediately retured, unchanged. This is to
;;; allow users to manually pack their own bitfields if desired (for
;;; advanced usage or optimization).
;;;
;;; Usage:
;;;
;;;   (define-enum-mask-packer PACKER
;;;     SYMBOL->VALUE)
;;;
;;; PACKER is the name for the packer procedure, which will be defined
;;; by this macro.
;;;
;;; SYMBOL->VALUE is the name of an existing procedure for converting
;;; a symbol into an integer value. Usually SYMBOL->VALUE is defined
;;; using the define-enum-mappings macro.
;;;
;;; Example:
;;;
;;;   ;; Assuming symbol->init-flag has been defined as in the example
;;;   ;; for define-enum-mappings, above.
;;;   (define-enum-mask-packer pack-init-flags
;;;     symbol->init-flag)
;;;
(define-syntax define-enum-mask-packer
  (syntax-rules ()
    ((define-enum-mask-packer packer
       symbol->value)
     (define (packer syms #!optional not-found-callback)
       (cond
        ((integer? syms) syms)
        (else (apply bitwise-ior
                (map (lambda (sym)
                       (symbol->value sym not-found-callback))
                     syms))))))))
