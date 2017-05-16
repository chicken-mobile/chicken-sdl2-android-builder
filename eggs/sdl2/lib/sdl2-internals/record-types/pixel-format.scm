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


(export pixel-format?
        wrap-pixel-format
        unwrap-pixel-format
        %pixel-format-pointer
        %pixel-format-pointer-set!

        free-pixel-format!)


(define-struct-record-type
  sdl2:pixel-format "SDL_PixelFormat"
  pred:    pixel-format?
  wrap:    wrap-pixel-format
  unwrap:  unwrap-pixel-format
  (pointer %pixel-format-pointer
           %pixel-format-pointer-set!))


(define (free-pixel-format! pixel-format)
  (define foreign-freer
    ;; Cannot use SDL_PixelFormat* foreign type because it has
    ;; not been defined yet.
    (foreign-lambda void "SDL_FreeFormat"
                    (c-pointer "SDL_PixelFormat")))
  (assert (pixel-format? pixel-format))
  (unless (struct-null? pixel-format)
    (foreign-freer (%pixel-format-pointer pixel-format))
    (%nullify-struct! pixel-format))
  pixel-format)


(define-struct-record-printer sdl2:pixel-format
  %pixel-format-pointer
  show-address: #f
  (#f pixel-format-format)
  (palette pixel-format-palette))
