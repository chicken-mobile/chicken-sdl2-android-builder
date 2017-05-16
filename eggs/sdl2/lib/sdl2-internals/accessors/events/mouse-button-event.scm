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


(export mouse-button-event?
        mouse-button-event-window-id
        mouse-button-event-window-id-set!
        mouse-button-event-which
        mouse-button-event-which-set!
        mouse-button-event-button-raw
        mouse-button-event-button-raw-set!
        mouse-button-event-button
        mouse-button-event-button-set!
        mouse-button-event-state
        mouse-button-event-state-set!
        mouse-button-event-x
        mouse-button-event-x-set!
        mouse-button-event-y
        mouse-button-event-y-set!)


(define-event-type "SDL_MouseButtonEvent"
  types: (SDL_MOUSEBUTTONDOWN
          SDL_MOUSEBUTTONUP)
  pred:  mouse-button-event?
  print: ((button mouse-button-event-button)
          (x mouse-button-event-x)
          (y mouse-button-event-y))
  ("button.windowID"
   type:   Uint32
   getter: mouse-button-event-window-id
   setter: mouse-button-event-window-id-set!
   guard:  (Uint32-guard "sdl2:mouse-button-event field windowID"))
  ("button.which"
   type:   Uint32
   getter: mouse-button-event-which
   setter: mouse-button-event-which-set!
   guard:  (Uint32-guard "sdl2:mouse-button-event field which"))
  ("button.button"
   type:   Uint8
   getter: mouse-button-event-button-raw
   setter: mouse-button-event-button-raw-set!
   guard:  (Uint8-guard "sdl2:mouse-button-event field button"))
  ("button.state"
   type:   bool
   getter: mouse-button-event-state
   setter: mouse-button-event-state-set!)
  ("button.x"
   type:   Sint32
   getter: mouse-button-event-x
   setter: mouse-button-event-x-set!
   guard:  (Sint32-guard "sdl2:mouse-button-event field x"))
  ("button.y"
   type:   Sint32
   getter: mouse-button-event-y
   setter: mouse-button-event-y-set!
   guard:  (Sint32-guard "sdl2:mouse-button-event field y")))


(define-enum-accessor
  getter: (mouse-button-event-button
           raw:  mouse-button-event-button-raw
           conv: mouse-button->symbol)
  setter: (mouse-button-event-button-set!
           raw:  mouse-button-event-button-raw-set!
           conv: symbol->mouse-button))
