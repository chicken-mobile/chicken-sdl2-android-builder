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


(export mouse-wheel-event?
        mouse-wheel-event-window-id
        mouse-wheel-event-window-id-set!
        mouse-wheel-event-which
        mouse-wheel-event-which-set!
        mouse-wheel-event-x
        mouse-wheel-event-x-set!
        mouse-wheel-event-y
        mouse-wheel-event-y-set!
        mouse-wheel-event-direction
        mouse-wheel-event-direction-set!)


(define-event-type "SDL_MouseWheelEvent"
  types: (SDL_MOUSEWHEEL)
  pred:  mouse-wheel-event?
  print: ((x mouse-wheel-event-x)
          (y mouse-wheel-event-y))
  ("wheel.windowID"
   type:   Uint32
   getter: mouse-wheel-event-window-id
   setter: mouse-wheel-event-window-id-set!
   guard:  (Uint32-guard "sdl2:mouse-wheel-event field windowID"))
  ("wheel.which"
   type:   Uint32
   getter: mouse-wheel-event-which
   setter: mouse-wheel-event-which-set!
   guard:  (Uint32-guard "sdl2:mouse-wheel-event field which"))
  ("wheel.x"
   type:   Sint32
   getter: mouse-wheel-event-x
   setter: mouse-wheel-event-x-set!
   guard:  (Sint32-guard "sdl2:mouse-wheel-event field x"))
  ("wheel.y"
   type:   Sint32
   getter: mouse-wheel-event-y
   setter: mouse-wheel-event-y-set!
   guard:  (Sint32-guard "sdl2:mouse-wheel-event field y"))
  ;; See below.
  ;; ("wheel.direction"
  ;;  type:   Uint32
  ;;  getter: mouse-wheel-event-direction
  ;;  setter: mouse-wheel-event-direction-set!
  ;;  guard:  (Uint32-guard "sdl2:mouse-wheel-event field direction"))
  )


;;; SDL_MouseWheelEvent's "direction" field requires SDL >= 2.0.4.
#+libSDL-2.0.4+
(define-struct-field-accessor
  SDL_Event*
  mouse-wheel-event?
  "wheel.direction"
  type: Uint32
  getter: %mouse-wheel-event-direction
  setter: %mouse-wheel-event-direction-set!
  guard: (Uint32-guard "sdl2:mouse-wheel-event field direction"))


(: mouse-wheel-event-direction
   (sdl2:event* -> symbol))
(define (mouse-wheel-event-direction ev)
  (cond-expand
    (libSDL-2.0.4+
     (mouse-wheel-direction->symbol
      (%mouse-wheel-event-direction ev)))
    (else
     (error 'mouse-wheel-event-direction
            "requires SDL 2.0.4 or higher."))))

(: mouse-wheel-event-direction-set!
   (sdl2:event* (or fixnum symbol) -> void))
(define (mouse-wheel-event-direction-set! ev dir)
  (cond-expand
    (libSDL-2.0.4+
     (define (not-found x)
       (error 'mouse-wheel-event-direction-set!
              "Invalid mouse wheel direction" x))
     (%mouse-wheel-event-direction-set!
      ev (if (integer? dir)
             dir
             (symbol->mouse-wheel-direction dir not-found))))
    (else
     (error 'mouse-wheel-event-direction-set!
            "requires SDL 2.0.4 or higher."))))

(set! (setter mouse-wheel-event-direction)
  mouse-wheel-event-direction-set!)
