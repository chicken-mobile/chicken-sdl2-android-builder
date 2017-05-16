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


(export dollar-gesture-event?
        dollar-gesture-event-touch-id
        dollar-gesture-event-touch-id-set!
        dollar-gesture-event-gesture-id
        dollar-gesture-event-gesture-id-set!
        dollar-gesture-event-num-fingers
        dollar-gesture-event-num-fingers-set!
        dollar-gesture-event-error
        dollar-gesture-event-error-set!
        dollar-gesture-event-x
        dollar-gesture-event-x-set!
        dollar-gesture-event-y
        dollar-gesture-event-y-set!)


(define-event-type "SDL_DollarGestureEvent"
  types: (SDL_DOLLARGESTURE
          SDL_DOLLARRECORD)
  pred:  dollar-gesture-event?
  print: ((touch-id dollar-gesture-event-touch-id)
          (gesture-id dollar-gesture-event-gesture-id)
          (num-fingers dollar-gesture-event-num-fingers)
          (error dollar-gesture-event-error)
          (x dollar-gesture-event-x)
          (y dollar-gesture-event-y))
  ("dgesture.touchId"
   type:   SDL_TouchID
   getter: dollar-gesture-event-touch-id
   setter: dollar-gesture-event-touch-id-set!)
  ("dgesture.gestureId"
   type:   SDL_GestureID
   getter: dollar-gesture-event-gesture-id
   setter: dollar-gesture-event-gesture-id-set!)
  ("dgesture.numFingers"
   type:   Uint32
   getter: dollar-gesture-event-num-fingers
   setter: dollar-gesture-event-num-fingers-set!
   guard:  (Uint32-guard "sdl2:dollar-gesture-event field numFingers"))
  ("dgesture.error"
   type:   float
   getter: dollar-gesture-event-error
   setter: dollar-gesture-event-error-set!)
  ("dgesture.x"
   type:   float
   getter: dollar-gesture-event-x
   setter: dollar-gesture-event-x-set!)
  ("dgesture.y"
   type:   float
   getter: dollar-gesture-event-y
   setter: dollar-gesture-event-y-set!))
