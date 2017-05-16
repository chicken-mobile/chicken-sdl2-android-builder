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


(export joystick-hat-position->symbol
        symbol->joystick-hat-position)

#+libSDL-2.0.4+
(export joystick-power-level->symbol
        symbol->joystick-power-level)


(define-enum-mappings
  type: SDL_JoystickHatPosition
  value->symbol: joystick-hat-position->symbol
  symbol->value: symbol->joystick-hat-position

  ((SDL_HAT_CENTERED   centered)
   (SDL_HAT_UP         up)
   (SDL_HAT_RIGHT      right)
   (SDL_HAT_DOWN       down)
   (SDL_HAT_LEFT       left)
   (SDL_HAT_RIGHTUP    right-up)
   (SDL_HAT_RIGHTDOWN  right-down)
   (SDL_HAT_LEFTUP     left-up)
   (SDL_HAT_LEFTDOWN   left-down)))


#+libSDL-2.0.4+
(define-enum-mappings
  type: SDL_JoystickPowerLevel
  value->symbol: joystick-power-level->symbol
  symbol->value: symbol->joystick-power-level

  ((SDL_JOYSTICK_POWER_UNKNOWN  unknown)
   (SDL_JOYSTICK_POWER_EMPTY    empty)
   (SDL_JOYSTICK_POWER_LOW      low)
   (SDL_JOYSTICK_POWER_MEDIUM   medium)
   (SDL_JOYSTICK_POWER_FULL     full)
   (SDL_JOYSTICK_POWER_WIRED    wired)
   (SDL_JOYSTICK_POWER_MAX      max)))
