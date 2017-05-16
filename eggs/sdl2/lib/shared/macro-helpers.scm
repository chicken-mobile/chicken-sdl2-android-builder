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


;;; Macro-time if expression. Expands to one expression or the other,
;;; depending on whether the first argument is #f at macro expansion
;;; time. This is a building block macro used for conditional
;;; expansion in other macros.
;;;
;;;   (macro-if a
;;;     (b)
;;;     (c))
;;;   ;; Expands to (b)
;;;
;;;   (macro-if #f
;;;     (b)
;;;     (c))
;;;   ;; Expands to (c)
;;;
(define-syntax macro-if
  (syntax-rules ()
    ;; First argument is #f, so expand to false-expr.
    ((macro-if #f true-expr false-expr)
     false-expr)
    ;; First argument is anything else, so expand to true-expr.
    ((macro-if x true-expr false-expr)
     true-expr)))


;;; Macro-time when expression. If the first argument is not #f (at
;;; macro expansion time), expands to a (begin ...) expression with
;;; the remaining expressions. If the first argument is #f, expands to
;;; (void). This is a building block macro used for conditional
;;; expansion in other macros.
;;;
;;;   (macro-when a
;;;     (b) (c) (d))
;;;   ;; Expands to (begin (b) (c) (d))
;;;
;;;   (macro-when #f
;;;     (b) (c) (d))
;;;   ;; Expands to (void)
;;;
(define-syntax macro-when
  (syntax-rules ()
    ((macro-when x expr ...)
     (macro-if x (begin expr ...) (void)))))
