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


(export color?
        wrap-color
        unwrap-color
        %color-pointer
        %color-pointer-set!
        free-color!
        alloc-color*
        alloc-color

        colour?
        free-colour!
        alloc-colour*
        alloc-colour)

(define-struct-record-type
  sdl2:color "SDL_Color"
  pred:    color?
  wrap:    wrap-color
  unwrap:  unwrap-color
  (pointer %color-pointer
           %color-pointer-set!))

(define-struct-memory-helpers
  "SDL_Color"
  using: (wrap-color
          color?
          %color-pointer
          %color-pointer-set!)
  define: (free-color!
           alloc-color*
           #f)) ;; alloc-color defined below

(define (alloc-color)
  (wrap-color
   (make-locative
    (make-blob (foreign-type-size "SDL_Color")))))


(define-struct-record-printer sdl2:color
  %color-pointer
  show-address: #f
  (#f color->list))


(define colour?       color?)
(define free-colour!  free-color!)
(define alloc-colour* alloc-color*)
(define alloc-colour  alloc-color)
