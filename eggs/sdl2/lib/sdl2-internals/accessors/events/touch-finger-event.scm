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


(export touch-finger-event?
        touch-finger-event-touch-id
        touch-finger-event-touch-id-set!
        touch-finger-event-finger-id
        touch-finger-event-finger-id-set!
        touch-finger-event-x
        touch-finger-event-x-set!
        touch-finger-event-y
        touch-finger-event-y-set!
        touch-finger-event-dx
        touch-finger-event-dx-set!
        touch-finger-event-dy
        touch-finger-event-dy-set!
        touch-finger-event-pressure
        touch-finger-event-pressure-set!)


(define-event-type "SDL_TouchFingerEvent"
  types: (SDL_FINGERDOWN
          SDL_FINGERUP
          SDL_FINGERMOTION)
  pred:  touch-finger-event?
  print: ((touch-id touch-finger-event-touch-id)
          (finger-id touch-finger-event-finger-id)
          (x touch-finger-event-x)
          (y touch-finger-event-y)
          (dx touch-finger-event-dx)
          (dy touch-finger-event-dy)
          (pressure touch-finger-event-pressure))
  ("tfinger.touchId"
   type:   SDL_TouchID
   getter: touch-finger-event-touch-id
   setter: touch-finger-event-touch-id-set!)
  ("tfinger.fingerId"
   type:   SDL_FingerID
   getter: touch-finger-event-finger-id
   setter: touch-finger-event-finger-id-set!)
  ("tfinger.x"
   type:   float
   getter: touch-finger-event-x
   setter: touch-finger-event-x-set!)
  ("tfinger.y"
   type:   float
   getter: touch-finger-event-y
   setter: touch-finger-event-y-set!)
  ("tfinger.dx"
   type:   float
   getter: touch-finger-event-dx
   setter: touch-finger-event-dx-set!)
  ("tfinger.dy"
   type:   float
   getter: touch-finger-event-dy
   setter: touch-finger-event-dy-set!)
  ("tfinger.pressure"
   type:   float
   getter: touch-finger-event-pressure
   setter: touch-finger-event-pressure-set!))
