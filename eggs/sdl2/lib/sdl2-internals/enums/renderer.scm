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


(export renderer-flag->symbol
        symbol->renderer-flag
        pack-renderer-flags
        unpack-renderer-flags

        renderer-flip->symbol
        symbol->renderer-flip
        pack-renderer-flip
        unpack-renderer-flip)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; RENDERER FLAGS

(define-enum-mappings
  type: SDL_RendererFlags
  value->symbol: renderer-flag->symbol
  symbol->value: symbol->renderer-flag
  ((SDL_RENDERER_SOFTWARE       software)
   (SDL_RENDERER_ACCELERATED    accelerated)
   (SDL_RENDERER_PRESENTVSYNC   present-vsync)
   (SDL_RENDERER_TARGETTEXTURE  target-texture)))

(define-enum-mask-packer pack-renderer-flags
  symbol->renderer-flag)

(define-enum-mask-unpacker unpack-renderer-flags
  renderer-flag->symbol
  (list SDL_RENDERER_SOFTWARE
        SDL_RENDERER_ACCELERATED
        SDL_RENDERER_PRESENTVSYNC
        SDL_RENDERER_TARGETTEXTURE))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; RENDERER FLIP

(define-enum-mappings
  type: SDL_RendererFlip
  value->symbol: renderer-flip->symbol
  symbol->value: symbol->renderer-flip
  ((SDL_FLIP_NONE       none)
   (SDL_FLIP_HORIZONTAL horizontal)
   (SDL_FLIP_VERTICAL   vertical)))

(define-enum-mask-packer pack-renderer-flip
  symbol->renderer-flip)

(define-enum-mask-unpacker unpack-renderer-flip
  renderer-flip->symbol
  (list SDL_FLIP_HORIZONTAL
        SDL_FLIP_VERTICAL))
