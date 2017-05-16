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


(export int-guard
        Uint-guard  Uint8-guard  Uint16-guard  Uint32-guard  Uint64-guard
        Sint-guard  Sint8-guard  Sint16-guard  Sint32-guard  Sint64-guard)

(define (int-guard description #!key min max)
  (cond
   ((and min max)
    (lambda (value)
      (assert (and (integer? value) (<= min value max))
              (format "~A must be an integer in range [~S, ~S], but got: ~S"
                      description min max value))
      (inexact->exact value)))
   (min
    (lambda (value)
      (assert (and (integer? value) (<= min value))
              (format "~A must be an integer >= ~S, but got: ~S"
                      description min value))
      (inexact->exact value)))
   (max
    (lambda (value)
      (assert (and (integer? value) (<= value max))
              (format "~A must be an integer <= ~S, but got: ~S"
                      description max value))
      (inexact->exact value)))
   (else
    (lambda (value)
      (assert (integer? value)
              (format "~A must be an integer, but got: ~S"
                      description value))
      (inexact->exact value)))))


(define (Uint-guard description bits)
  (int-guard description
             min: 0
             max: (- (expt 2 bits) 1)))

(define (Uint8-guard  description) (Uint-guard description 8))
(define (Uint16-guard description) (Uint-guard description 16))
(define (Uint32-guard description) (Uint-guard description 32))
(define (Uint64-guard description) (Uint-guard description 64))


(define (Sint-guard description bits)
  (int-guard description
             min: (- (expt 2 (- bits 1)))
             max: (- (expt 2 (- bits 1)) 1)))

(define (Sint8-guard  description) (Sint-guard description 8))
(define (Sint16-guard description) (Sint-guard description 16))
(define (Sint32-guard description) (Sint-guard description 32))
(define (Sint64-guard description) (Sint-guard description 64))
