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


(export mouse-motion-event?
        mouse-motion-event-window-id
        mouse-motion-event-window-id-set!
        mouse-motion-event-which
        mouse-motion-event-which-set!
        mouse-motion-event-state-raw
        mouse-motion-event-state-raw-set!
        mouse-motion-event-state
        mouse-motion-event-state-set!
        mouse-motion-event-x
        mouse-motion-event-x-set!
        mouse-motion-event-y
        mouse-motion-event-y-set!
        mouse-motion-event-xrel
        mouse-motion-event-xrel-set!
        mouse-motion-event-yrel
        mouse-motion-event-yrel-set!)


(define-event-type "SDL_MouseMotionEvent"
  types: (SDL_MOUSEMOTION)
  pred:  mouse-motion-event?
  print: ((state mouse-motion-event-state)
          (x mouse-motion-event-x)
          (y mouse-motion-event-y)
          (xrel mouse-motion-event-xrel)
          (yrel mouse-motion-event-yrel))
  ("motion.windowID"
   type:   Uint32
   getter: mouse-motion-event-window-id
   setter: mouse-motion-event-window-id-set!
   guard:  (Uint32-guard "sdl2:mouse-motion-event field windowID"))
  ("motion.which"
   type:   Uint32
   getter: mouse-motion-event-which
   setter: mouse-motion-event-which-set!
   guard:  (Uint32-guard "sdl2:mouse-motion-event field which"))
  ("motion.state"
   type:   Uint32
   getter: mouse-motion-event-state-raw
   setter: mouse-motion-event-state-raw-set!
   guard:  (Uint32-guard "sdl2:mouse-motion-event field state"))
  ("motion.x"
   type:   Sint32
   getter: mouse-motion-event-x
   setter: mouse-motion-event-x-set!
   guard:  (Sint32-guard "sdl2:mouse-motion-event field x"))
  ("motion.y"
   type:   Sint32
   getter: mouse-motion-event-y
   setter: mouse-motion-event-y-set!
   guard:  (Sint32-guard "sdl2:mouse-motion-event field y"))
  ("motion.xrel"
   type:   Sint32
   getter: mouse-motion-event-xrel
   setter: mouse-motion-event-xrel-set!
   guard:  (Sint32-guard "sdl2:mouse-motion-event field xrel"))
  ("motion.yrel"
   type:   Sint32
   getter: mouse-motion-event-yrel
   setter: mouse-motion-event-yrel-set!
   guard:  (Sint32-guard "sdl2:mouse-motion-event field yrel")))


(define-enum-mask-accessor
  getter: (mouse-motion-event-state
           raw:    mouse-motion-event-state-raw
           unpack: unpack-mouse-button-masks
           exact:  #f)
  setter: (mouse-motion-event-state-set!
           raw:    mouse-motion-event-state-raw-set!
           pack:   pack-mouse-button-masks))
