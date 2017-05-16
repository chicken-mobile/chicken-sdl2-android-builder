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


(export rect-x  rect-x-set!
        rect-y  rect-y-set!
        rect-w  rect-w-set!
        rect-h  rect-h-set!)


(define %rect-x-guard (Sint32-guard "sdl2:rect field x"))
(define %rect-y-guard (Sint32-guard "sdl2:rect field y"))
(define %rect-w-guard (Sint32-guard "sdl2:rect field w"))
(define %rect-h-guard (Sint32-guard "sdl2:rect field h"))

(define-struct-field-accessors
  SDL_Rect*
  rect?
  ("x"
   type:   Sint32
   getter: rect-x
   setter: rect-x-set!
   guard:  %rect-x-guard)
  ("y"
   type:   Sint32
   getter: rect-y
   setter: rect-y-set!
   guard:  %rect-y-guard)
  ("w"
   type:   Sint32
   getter: rect-w
   setter: rect-w-set!
   guard:  %rect-w-guard)
  ("h"
   type:   Sint32
   getter: rect-h
   setter: rect-h-set!
   guard:  %rect-h-guard))
