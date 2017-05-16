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
;;; GENERAL

(define-foreign-constants int
  SDL_ENABLE
  SDL_DISABLE
  SDL_QUERY
  SDL_IGNORE

  SDL_BYTEORDER
  SDL_BIG_ENDIAN
  SDL_LIL_ENDIAN)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; INIT / SUBSYSTEMS

(export init-flag->symbol
        symbol->init-flag
        pack-init-flags
        unpack-init-flags)

(define-enum-mappings
  type: Uint32
  value->symbol: init-flag->symbol
  symbol->value: symbol->init-flag

  ((SDL_INIT_TIMER           timer)
   (SDL_INIT_AUDIO           audio)
   (SDL_INIT_VIDEO           video)
   (SDL_INIT_JOYSTICK        joystick)
   (SDL_INIT_HAPTIC          haptic)
   (SDL_INIT_GAMECONTROLLER  game-controller)
   (SDL_INIT_EVENTS          events)
   (SDL_INIT_EVERYTHING      everything)))

(define-enum-mask-packer pack-init-flags
  symbol->init-flag)

(define-enum-mask-unpacker unpack-init-flags
  init-flag->symbol
  (list SDL_INIT_TIMER
        SDL_INIT_AUDIO
        SDL_INIT_VIDEO
        SDL_INIT_JOYSTICK
        SDL_INIT_HAPTIC
        SDL_INIT_GAMECONTROLLER
        SDL_INIT_EVENTS
        SDL_INIT_EVERYTHING))
