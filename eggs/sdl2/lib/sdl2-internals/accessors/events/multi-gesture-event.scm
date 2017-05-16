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


(export multi-gesture-event?
        multi-gesture-event-touch-id
        multi-gesture-event-touch-id-set!
        multi-gesture-event-dtheta
        multi-gesture-event-dtheta-set!
        multi-gesture-event-ddist
        multi-gesture-event-ddist-set!
        multi-gesture-event-x
        multi-gesture-event-x-set!
        multi-gesture-event-y
        multi-gesture-event-y-set!
        multi-gesture-event-num-fingers
        multi-gesture-event-num-fingers-set!)


(define-event-type "SDL_MultiGestureEvent"
  types: (SDL_MULTIGESTURE)
  pred:  multi-gesture-event?
  print: ((touch-id multi-gesture-event-touch-id)
          (dtheta multi-gesture-event-dtheta)
          (ddist multi-gesture-event-ddist)
          (x multi-gesture-event-x)
          (y multi-gesture-event-y)
          (num-fingers multi-gesture-event-num-fingers))
  ("mgesture.touchId"
   type:   SDL_TouchID
   getter: multi-gesture-event-touch-id
   setter: multi-gesture-event-touch-id-set!)
  ("mgesture.dTheta"
   type:   float
   getter: multi-gesture-event-dtheta
   setter: multi-gesture-event-dtheta-set!)
  ("mgesture.dDist"
   type:   float
   getter: multi-gesture-event-ddist
   setter: multi-gesture-event-ddist-set!)
  ("mgesture.x"
   type:   float
   getter: multi-gesture-event-x
   setter: multi-gesture-event-x-set!)
  ("mgesture.y"
   type:   float
   getter: multi-gesture-event-y
   setter: multi-gesture-event-y-set!)
  ("mgesture.numFingers"
   type:   Uint16
   getter: multi-gesture-event-num-fingers
   setter: multi-gesture-event-num-fingers-set!
   guard:  (Uint16-guard "sdl2:multi-gesture-event field numFingers")))
