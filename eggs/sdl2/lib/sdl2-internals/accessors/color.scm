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


(export color-r  color-r-set!
        color-g  color-g-set!
        color-b  color-b-set!
        color-a  color-a-set!

        colour-r  colour-r-set!
        colour-g  colour-g-set!
        colour-b  colour-b-set!
        colour-a  colour-a-set!)


(define %color-r-guard (Uint8-guard "sdl2:color field r"))
(define %color-g-guard (Uint8-guard "sdl2:color field g"))
(define %color-b-guard (Uint8-guard "sdl2:color field b"))
(define %color-a-guard (Uint8-guard "sdl2:color field a"))

(define-struct-field-accessors
  SDL_Color*
  color?
  ("r"
   type:   Uint8
   getter: color-r
   setter: color-r-set!
   guard:  %color-r-guard)
  ("g"
   type:   Uint8
   getter: color-g
   setter: color-g-set!
   guard:  %color-g-guard)
  ("b"
   type:   Uint8
   getter: color-b
   setter: color-b-set!
   guard:  %color-b-guard)
  ("a"
   type:   Uint8
   getter: color-a
   setter: color-a-set!
   guard:  %color-a-guard))


(define colour-r      color-r)
(define colour-g      color-g)
(define colour-b      color-b)
(define colour-a      color-a)
(define colour-r-set! color-r-set!)
(define colour-g-set! color-g-set!)
(define colour-b-set! color-b-set!)
(define colour-a-set! color-a-set!)
