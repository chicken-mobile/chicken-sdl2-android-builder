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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MOUSE BUTTON

(export mouse-button->symbol
        symbol->mouse-button)

(define-enum-mappings
  type: SDL_MouseButton
  value->symbol: mouse-button->symbol
  symbol->value: symbol->mouse-button

  ((SDL_BUTTON_LEFT    left)
   (SDL_BUTTON_MIDDLE  middle)
   (SDL_BUTTON_RIGHT   right)
   (SDL_BUTTON_X1      x1)
   (SDL_BUTTON_X2      x2)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MOUSE BUTTON MASK

(export mouse-button-mask->symbol
        symbol->mouse-button-mask
        pack-mouse-button-masks
        unpack-mouse-button-masks)

(define-enum-mappings
  type: SDL_MouseButtonMask
  value->symbol: mouse-button-mask->symbol
  symbol->value: symbol->mouse-button-mask

  ((SDL_BUTTON_LMASK   left)
   (SDL_BUTTON_MMASK   middle)
   (SDL_BUTTON_RMASK   right)
   (SDL_BUTTON_X1MASK  x1)
   (SDL_BUTTON_X2MASK  x2)))

(define-enum-mask-packer pack-mouse-button-masks
  symbol->mouse-button-mask)

(define-enum-mask-unpacker unpack-mouse-button-masks
  mouse-button-mask->symbol
  (list SDL_BUTTON_LMASK
        SDL_BUTTON_MMASK
        SDL_BUTTON_RMASK
        SDL_BUTTON_X1MASK
        SDL_BUTTON_X2MASK))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MOUSE WHEEL DIRECTION (SDL 2.0.4+)

(cond-expand
  (libSDL-2.0.4+
   (export mouse-wheel-direction->symbol
           symbol->mouse-wheel-direction)
   (define-enum-mappings
     type: SDL_MouseWheelDirection
     value->symbol: mouse-wheel-direction->symbol
     symbol->value: symbol->mouse-wheel-direction
     ((SDL_MOUSEWHEEL_NORMAL   normal)
      (SDL_MOUSEWHEEL_FLIPPED  flipped))))
  (else))
