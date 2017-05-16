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


(export system-cursor->symbol
        symbol->system-cursor)

(define-enum-mappings
  type: SDL_SystemCursor
  value->symbol: system-cursor->symbol
  symbol->value: symbol->system-cursor

  ((SDL_SYSTEM_CURSOR_ARROW      arrow)
   (SDL_SYSTEM_CURSOR_IBEAM      ibeam)
   (SDL_SYSTEM_CURSOR_WAIT       wait)
   (SDL_SYSTEM_CURSOR_CROSSHAIR  crosshair)
   (SDL_SYSTEM_CURSOR_WAITARROW  wait-arrow)
   (SDL_SYSTEM_CURSOR_SIZENWSE   size-nwse)
   (SDL_SYSTEM_CURSOR_SIZENESW   size-nesw)
   (SDL_SYSTEM_CURSOR_SIZEWE     size-we)
   (SDL_SYSTEM_CURSOR_SIZENS     size-ns)
   (SDL_SYSTEM_CURSOR_SIZEALL    size-all)
   (SDL_SYSTEM_CURSOR_NO         no)
   (SDL_SYSTEM_CURSOR_HAND       hand)))
