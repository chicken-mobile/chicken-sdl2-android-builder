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


;;; Convenient way of constructing a composite condition.
(define (make-condition condition-lists)
  (apply make-composite-condition
    (map (lambda (condition-list)
           (apply make-property-condition condition-list))
         condition-lists)))


;;; Macro: assert-bounds
;;;
;;;   (assert-bounds ?VAL ?LOWER ?UPPER ?MSG ?NAME)
;;;
;;; Assert that a numeric value is within the given lower and upper
;;; bounds (inclusive). If the value is within the bounds, the
;;; assertion evaluates to the value. If the value is outside of the
;;; bounds, an exception of kind (exn bounds) will be signalled.
;;;
;;; ?VAL is an expression that evaluates to the numeric value being
;;; considered.
;;;
;;; ?LOWER is an expression that evaluates to the lower bound
;;; (inclusive), i.e. the minimum allowed value.
;;;
;;; ?UPPER is an expression that evaluates to the upper bound
;;; (inclusive), i.e. the maximum allowed value.
;;;
;;; ?MSG is an expression that evaluates to an error message string
;;; giving some context to the person who sees the error. Usually of
;;; the form "___ out of bounds", e.g. "x coordinate out of bounds".
;;;
;;; ?NAME is an expression that evaluates to a quoted symbol
;;; indicating the name of the procedure (or macro) that performed the
;;; assertion. It is shown when the error is printed, to help debug.
;;;
;;; Example:
;;;
;;;   #;1> (assert-bounds 10 0 10 "foo out of bounds" 'some-proc)
;;;   ;; No error, because 10 is within the bounds 0 to 10.
;;;
;;;   #;2> (assert-bounds 42 0 10 "foo out of bounds" 'some-proc)
;;;
;;;   Error: (some-proc) foo out of bounds: 42
;;;   Lower bound: 0
;;;   Upper bound: 10
;;;
;;;         Call history:
;;;         ...
;;;
(define-syntax assert-bounds
  (syntax-rules ()
    ((assert-bounds ?val ?lower ?upper ?msg ?name)
     (let ((val ?val)
           (lower ?lower)
           (upper ?upper)
           (msg ?msg)
           (name ?name))
       (if (<= lower val upper)
           val
           (let ((full-msg (sprintf "~A: ~S~NLower bound: ~S~NUpper bound: ~S"
                                    msg val lower upper)))
             (abort (make-condition
                     `((exn message ,full-msg
                            location ,name
                            call-chain ,(get-call-chain 1))
                       (bounds))))))))))
