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


(export sdl-failure)


;;; Returns an exception condition of kind (exn sdl2) indicating that
;;; an SDL function failed. Use abort to signal the exception.
;;;
;;; fn-name should be a string giving the name of the SDL function
;;; that failed. ret-code should be the integer error code that was
;;; returned by the failing function (usually -1), or #f if the
;;; function failed without an integer return code (e.g. it returned a
;;; NULL pointer instead).
;;;
;;; The error message is constructed automatically based on the
;;; function name, return code, and the error string from get-error.
;;;
;;; Examples:
;;;
;;;   ;; With return code
;;;   (let ((ret-code (SDL_Init ...)))
;;;     (unless (zero? ret-code)
;;;       (abort (sdl-failure "SDL_Init" ret-code))))
;;;
;;;   ;; Without return code
;;;   (let ((surface (SDL_CreateRGBSurface ...)))
;;;     (if (and (surface? surface) (not (struct-null? surface)))
;;;         surface
;;;         (abort (sdl-failure "SDL_CreateRGBSurface" #f))))
;;;
(: sdl-failure (string (or fixnum boolean) -> condition))
(define (sdl-failure fn-name ret-code)
  (let ((sdl-err-msg (SDL_GetError)))
    (make-composite-condition
     (make-property-condition
      'exn 'message
      (sprintf "~A failed~A.~A"
               fn-name
               (if ret-code
                   (sprintf " (return code ~S)" ret-code)
                   "")
               (if (zero? (string-length sdl-err-msg))
                   ""
                   (sprintf "~NDetails: ~A" sdl-err-msg))))
     (make-property-condition
      'sdl2
      'function fn-name
      'return-code ret-code
      'sdl-error sdl-err-msg))))
