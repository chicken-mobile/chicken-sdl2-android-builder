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


;;; Like foreign-value, except the body string is constructed
;;; dynamically (at macro expansion time) using sprintf.
;;;
;;; Usage:
;;;
;;;   (foreign-value-with-dynamic-body
;;;    (BODY-FORMATSTRING FORMAT-ARG ...)
;;;    FOREIGN-TYPE)
;;;
;;; This macro is used within other macros to generate foreign values
;;; based on a template. For example, a version number, type, or field
;;; name within the body can be filled in based on the arguments given
;;; to the high-level macro.
;;;
;;; Because the body string is constructed at macro expansion time,
;;; each FORMAT-ARG must be a literal value like a symbol, string, or
;;; number.
;;;
(define-syntax foreign-value-with-dynamic-body
  (ir-macro-transformer
   (lambda (form inject compare?)
     (let ((body (list-ref form 1))
           (type (list-ref form 2)))
       `(foreign-value
         ,(apply sprintf (map strip-syntax body))
         ,type)))))
