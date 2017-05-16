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


(export joy-hat-event?
        joy-hat-event-which
        joy-hat-event-which-set!
        joy-hat-event-hat
        joy-hat-event-hat-set!
        joy-hat-event-value-raw
        joy-hat-event-value-raw-set!
        joy-hat-event-value
        joy-hat-event-value-set!)


(define-event-type "SDL_JoyHatEvent"
  types: (SDL_JOYHATMOTION)
  pred:  joy-hat-event?
  print: ((which joy-hat-event-which)
          (hat joy-hat-event-hat)
          (value joy-hat-event-value))
  ("jhat.which"
   type:   SDL_JoystickID
   getter: joy-hat-event-which
   setter: joy-hat-event-which-set!)
  ("jhat.hat"
   type:   Uint8
   getter: joy-hat-event-hat
   setter: joy-hat-event-hat-set!
   guard:  (Uint8-guard "sdl2:joy-hat-event field hat"))
  ("jhat.value"
   type:   Uint8
   getter: joy-hat-event-value-raw
   setter: joy-hat-event-value-raw-set!
   guard:  (Uint8-guard "sdl2:joy-hat-event field value")))


(define-enum-accessor
  getter: (joy-hat-event-value
           raw:  joy-hat-event-value-raw
           conv: joystick-hat-position->symbol)
  setter: (joy-hat-event-value-set!
           raw:  joy-hat-event-value-raw-set!
           conv: symbol->joystick-hat-position))
