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


(export text-editing-event?
        text-editing-event-window-id
        text-editing-event-window-id-set!
        text-editing-event-text
        text-editing-event-text-set!
        text-editing-event-start
        text-editing-event-start-set!
        text-editing-event-length
        text-editing-event-length-set!)


(define-event-type "SDL_TextEditingEvent"
  types: (SDL_TEXTEDITING)
  pred:  text-editing-event?
  print: ((start text-editing-event-start)
          (text text-editing-event-text))
  ("edit.windowID"
   type:   Uint32
   getter: text-editing-event-window-id
   setter: text-editing-event-window-id-set!
   guard:  (Uint32-guard "sdl2:text-editing-event field windowID"))
  ;; See below
  ;; ("edit.text"
  ;;  type:   "char[SDL_TEXTEDITINGEVENT_TEXT_SIZE]"
  ;;  getter: text-editing-event-text
  ;;  setter: text-editing-event-text-set!)
  ("edit.start"
   type:   Sint32
   getter: text-editing-event-start
   setter: text-editing-event-start-set!
   guard:  (Sint32-guard "sdl2:text-editing-event field start"))
  ("edit.length"
   type:   Sint32
   getter: text-editing-event-length
   setter: text-editing-event-length-set!
   guard:  (Sint32-guard "sdl2:text-editing-event field length")))


(define (text-editing-event-text-set! event text)
  (define foreign-setter
    (foreign-lambda*
     c-string ((SDL_Event* event) (c-string text))
     "strncpy(event->edit.text, text, SDL_TEXTEDITINGEVENT_TEXT_SIZE);"))
  (assert (text-editing-event? event))
  (assert (<= (string-length text) SDL_TEXTEDITINGEVENT_TEXT_SIZE))
  (foreign-setter event text))

(define (text-editing-event-text event)
  (define foreign-getter
    (foreign-lambda*
     c-string ((SDL_Event* event))
     "C_return( &(event->edit.text) );"))
  (assert (text-editing-event? event))
  (foreign-getter event))

(set! (setter text-editing-event-text)
      text-editing-event-text-set!)
