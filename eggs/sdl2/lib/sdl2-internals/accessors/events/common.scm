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


;;; All SDL events have a type and timestamp. These functions can be
;;; used on any kind of SDL event.

(export make-event
        make-event*

        event-type-raw
        event-type-raw-set!
        event-type
        event-type-set!

        event-timestamp
        event-timestamp-set!)


(define (make-event #!optional (type 'first))
  (let ((event (alloc-event)))
    (event-type-set! event type)
    event))

(define (make-event* #!optional (type 'first))
  (let ((event (alloc-event*)))
    (event-type-set! event type)
    event))


(define-struct-field-accessors
  SDL_Event*
  event?
  ("type"
   type:   SDL_EventType
   getter: event-type-raw
   setter: event-type-raw-set!)
  ("common.timestamp"
   type:   Uint32
   getter: event-timestamp
   setter: event-timestamp-set!
   guard:  (Uint32-guard "sdl2:event field timestamp")))


(define-enum-accessor
  getter: (event-type
           raw:  event-type-raw
           conv: event-type->symbol)
  setter: (event-type-set!
           raw:  event-type-raw-set!
           conv: symbol->event-type))
