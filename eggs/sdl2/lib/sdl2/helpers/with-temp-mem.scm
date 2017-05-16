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


;;; Temporarily allocates some memory, executes the body, frees the
;;; memory that was allocated, then returns the value(s) of the final
;;; expression of the body. Supports multiple return values.
;;;
;;; IMPORTANT: For performance reasons, this macro is not designed to
;;; automatically free the memory if an exception is signalled from
;;; the body. If you intend to signal an exception from the body (e.g.
;;; because an SDL function failed), you should manually free the
;;; memory before signalling the exception.
;;;
;;; This is useful for wrapping C functions that use "output
;;; parameters", i.e. the function accepts some pointers, and modifies
;;; the values of the pointers as a way of returning multiple results.
;;;
;;; Example:
;;;
;;;   (with-temp-mem ((w-out (%allocate-Sint32))
;;;                   (h-out (%allocate-Sint32)))
;;;     (SDL_GetWindowSize window w-out h-out)
;;;     (values (pointer-s32-ref w-out)
;;;             (pointer-s32-ref h-out)))
;;;
(define-syntax with-temp-mem
  (syntax-rules ()
    ((with-temp-mem ((temp-var alloc-expr) ...)
       body ...)
     (let ((temp-var alloc-expr) ...)
       (receive result-values (begin body ...)
         (free temp-var)
         ...
         (apply values result-values))))))
