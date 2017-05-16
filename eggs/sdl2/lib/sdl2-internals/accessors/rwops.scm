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


(export %rwops-size
        %rwops-seek
        %rwops-read
        %rwops-write
        %rwops-close
        rwops-type-raw
        rwops-type)


(define-struct-field-accessors
  SDL_RWops*
  rwops?
  ("size"
   type:   (function Sint64 (SDL_RWops*))
   getter: %rwops-size)
  ("seek"
   type:   (function Sint64 (SDL_RWops* Sint64 Sint32))
   getter: %rwops-seek)
  ("read"
   type:   (function size_t (SDL_RWops* c-pointer size_t size_t))
   getter: %rwops-read)
  ("write"
   type:   (function size_t (SDL_RWops* c-pointer size_t size_t))
   getter: %rwops-write)
  ("close"
   type:   (function Sint32 (SDL_RWops*))
   getter: %rwops-close)
  ("type"
   type:   Uint32
   getter: rwops-type-raw))


(define-enum-accessor
  getter: (rwops-type
           raw:   rwops-type-raw
           conv:  rwops-type-enum->symbol))
