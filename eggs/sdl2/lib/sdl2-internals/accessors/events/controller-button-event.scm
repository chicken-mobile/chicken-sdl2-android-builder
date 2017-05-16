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


(export controller-button-event?
        controller-button-event-which
        controller-button-event-which-set!
        controller-button-event-button
        controller-button-event-button-set!
        controller-button-event-state
        controller-button-event-state-set!)


(define-event-type "SDL_ControllerButtonEvent"
  types: (SDL_CONTROLLERBUTTONDOWN
          SDL_CONTROLLERBUTTONUP)
  pred:  controller-button-event?
  print: ((which controller-button-event-which)
          (button controller-button-event-button))
  ("cbutton.which"
   type:   SDL_JoystickID
   getter: controller-button-event-which
   setter: controller-button-event-which-set!)
  ("cbutton.button"
   type:   Uint8
   getter: controller-button-event-button
   setter: controller-button-event-button-set!
   guard:  (Uint8-guard "sdl2:controller-button-event field button"))
  ("cbutton.state"
   type:   bool
   getter: controller-button-event-state
   setter: controller-button-event-state-set!))
