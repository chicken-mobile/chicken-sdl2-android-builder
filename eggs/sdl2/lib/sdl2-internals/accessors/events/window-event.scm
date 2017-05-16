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


(export window-event?
        window-event-window-id
        window-event-window-id-set!
        window-event-event-raw
        window-event-event-raw-set!
        window-event-event
        window-event-event-set!
        window-event-data1
        window-event-data1-set!
        window-event-data2
        window-event-data2-set!)


(define-event-type "SDL_WindowEvent"
  types: (SDL_WINDOWEVENT)
  pred:  window-event?
  print: ((window-id window-event-window-id)
          (event window-event-event)
          (data1 window-event-data1)
          (data2 window-event-data2))
  ("window.windowID"
   type:   Uint32
   getter: window-event-window-id
   setter: window-event-window-id-set!
   guard:  (Uint32-guard "sdl2:window-event field windowID"))
  ("window.event"
   type:   Uint8
   getter: window-event-event-raw
   setter: window-event-event-raw-set!
   guard:  (Uint8-guard "sdl2:window-event field event"))
  ("window.data1"
   type:   Sint32
   getter: window-event-data1
   setter: window-event-data1-set!
   guard:  (Sint32-guard "sdl2:window-event field data1"))
  ("window.data2"
   type:   Sint32
   getter: window-event-data2
   setter: window-event-data2-set!
   guard:  (Sint32-guard "sdl2:window-event field data2")))


(define-enum-accessor
  getter: (window-event-event
           raw:  window-event-event-raw
           conv: window-event-id->symbol)
  setter: (window-event-event-set!
           raw:  window-event-event-raw-set!
           conv: symbol->window-event-id))
