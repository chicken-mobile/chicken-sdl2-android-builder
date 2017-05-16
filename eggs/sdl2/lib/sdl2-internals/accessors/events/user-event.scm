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


(export user-event?
        user-event-window-id
        user-event-window-id-set!
        user-event-code
        user-event-code-set!
        user-event-data1-raw
        user-event-data1-raw-set!
        user-event-data2-raw
        user-event-data2-raw-set!)


(define-event-type "SDL_UserEvent"
  types: (SDL_USEREVENT)
  pred:  user-event?
  print: ((code user-event-code))
  ("user.windowID"
   type:   Uint32
   getter: user-event-window-id
   setter: user-event-window-id-set!
   guard:  (Uint32-guard "sdl2:user-event field windowID"))
  ("user.code"
   type:   Sint32
   getter: user-event-code
   setter: user-event-code-set!
   guard:  (Sint32-guard "sdl2:user-event field code"))
  ("user.data1"
   type:   c-pointer
   getter: user-event-data1-raw
   setter: user-event-data1-raw-set!)
  ("user.data2"
   type:   c-pointer
   getter: user-event-data2-raw
   setter: user-event-data2-raw-set!))


(define (user-event? event)
  ;; Any event type of SDL_USEREVENT or greater is a user event.
  ;; User event types are registered with SDL_RegisterEvents.
  (and (event? event)
       (>= (event-type-raw event)
           SDL_USEREVENT)))
