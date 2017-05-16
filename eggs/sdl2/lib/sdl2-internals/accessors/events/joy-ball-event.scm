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


(export joy-ball-event?
        joy-ball-event-which
        joy-ball-event-which-set!
        joy-ball-event-ball
        joy-ball-event-ball-set!
        joy-ball-event-xrel
        joy-ball-event-xrel-set!
        joy-ball-event-yrel
        joy-ball-event-yrel-set!)


(define-event-type "SDL_JoyBallEvent"
  types: (SDL_JOYBALLMOTION)
  pred:  joy-ball-event?
  print: ((which joy-ball-event-which)
          (ball joy-ball-event-ball)
          (xrel joy-ball-event-xrel)
          (yrel joy-ball-event-yrel))
  ("jball.which"
   type:   SDL_JoystickID
   getter: joy-ball-event-which
   setter: joy-ball-event-which-set!)
  ("jball.ball"
   type:   Uint8
   getter: joy-ball-event-ball
   setter: joy-ball-event-ball-set!
   guard:  (Uint8-guard "sdl2:joy-ball-event field ball"))
  ("jball.xrel"
   type:   Sint16
   getter: joy-ball-event-xrel
   setter: joy-ball-event-xrel-set!
   guard:  (Sint16-guard "sdl2:joy-ball-event field xrel"))
  ("jball.yrel"
   type:   Sint16
   getter: joy-ball-event-yrel
   setter: joy-ball-event-yrel-set!
   guard:  (Sint16-guard "sdl2:joy-ball-event field yrel")))
