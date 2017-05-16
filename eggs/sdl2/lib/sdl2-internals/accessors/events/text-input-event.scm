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


(export text-input-event?
        text-input-event-window-id
        text-input-event-window-id-set!
        text-input-event-text
        text-input-event-text-set!)


(define-event-type "SDL_TextInputEvent"
  types: (SDL_TEXTINPUT)
  pred:  text-input-event?
  print: ((text text-input-event-text))
  ("text.windowID"
   type:   Uint32
   getter: text-input-event-window-id
   setter: text-input-event-window-id-set!
   guard:  (Uint32-guard "sdl2:text-input-event field windowID"))
  ;; See below
  ;; ("text.text"
  ;;  type:   "char[SDL_TEXTINPUTEVENT_TEXT_SIZE]"
  ;;  getter: text-input-event-text
  ;;  setter: text-input-event-text-set!)
  )


(define (text-input-event-text-set! event text)
  (define foreign-setter
    (foreign-lambda*
     c-string ((SDL_Event* event) (c-string text))
     "strncpy(event->text.text, text, SDL_TEXTINPUTEVENT_TEXT_SIZE);"))
  (assert (text-input-event? event))
  (assert (<= (string-length text) SDL_TEXTINPUTEVENT_TEXT_SIZE))
  (foreign-setter event text))

(define (text-input-event-text event)
  (define foreign-getter
    (foreign-lambda*
     c-string ((SDL_Event* event))
     "C_return( &(event->text.text) );"))
  (assert (text-input-event? event))
  (foreign-getter event))

(set! (setter text-input-event-text)
      text-input-event-text-set!)
