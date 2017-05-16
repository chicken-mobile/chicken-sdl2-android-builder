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


(export keyboard-event?
        keyboard-event-window-id
        keyboard-event-window-id-set!
        keyboard-event-state
        keyboard-event-state-set!
        keyboard-event-repeat
        keyboard-event-repeat-set!
        keyboard-event-keysym
        keyboard-event-keysym-set!

        ;; Shortcuts
        keyboard-event-sym-raw       keyboard-event-sym-raw-set!
        keyboard-event-sym           keyboard-event-sym-set!
        keyboard-event-scancode-raw  keyboard-event-scancode-raw-set!
        keyboard-event-scancode      keyboard-event-scancode-set!
        keyboard-event-mod-raw       keyboard-event-mod-raw-set!
        keyboard-event-mod           keyboard-event-mod-set!)


(define-event-type "SDL_KeyboardEvent"
  types: (SDL_KEYDOWN
          SDL_KEYUP)
  pred:  keyboard-event?
  print: ((sym keyboard-event-sym)
          (scancode keyboard-event-scancode)
          (mod keyboard-event-mod))
  ("key.windowID"
   type:   Uint32
   getter: keyboard-event-window-id
   setter: keyboard-event-window-id-set!
   guard:  (Uint32-guard "sdl2:keyboard-event field windowID"))
  ("key.state"
   type:   bool
   getter: keyboard-event-state
   setter: keyboard-event-state-set!)
  ("key.repeat"
   type:   Uint8
   getter: keyboard-event-repeat
   setter: keyboard-event-repeat-set!
   guard:  (Uint8-guard "sdl2:keyboard-event field repeat"))
  ;; See below
  ;; ("key.keysym"
  ;;  type:   SDL_Keysym
  ;;  getter: keyboard-event-keysym
  ;;  setter: keyboard-event-keysym-set!)
  )


;;; Since the keysym is stored in the event struct by value, not a
;;; pointer, we need to treat it specially. In particular, we don't
;;; access the event's keysym directly, but rather copy its value
;;; to/from a keysym being held separately. It would not be safe to
;;; wrap a pointer to the event's keysym, because the event could be
;;; freed/GC'd while there is still a Scheme reference to the keysym.

(define (keyboard-event-keysym-set! event keysym)
  (define foreign-setter
    ;; Copy the given keysym's value into the event's keysym.
    (foreign-lambda*
     void ((SDL_Event* event) (SDL_Keysym* keysym))
     "event->key.keysym = *keysym;"))
  (assert (keyboard-event? event))
  (foreign-setter event keysym))

(define (keyboard-event-keysym event)
  (define foreign-getter
    ;; Allocate a new keysym and then copy the event's keysym value.
    (foreign-lambda*
     void ((SDL_Event* event) (SDL_Keysym* keysym))
     "*keysym = event->key.keysym;"))
  (assert (keyboard-event? event))
  (let ((keysym (alloc-keysym)))
    (foreign-getter event keysym)
    keysym))

(set! (setter keyboard-event-keysym)
      keyboard-event-keysym-set!)



;;; Shortcuts for directly getting values out of the event keysym.
;;; These are more user-convenient (less code to write) and efficient
;;; (less overhead from allocating/copying memory).

(define-struct-field-accessors
  SDL_Event*
  keyboard-event?
  ("key.keysym.scancode"
   type:   SDL_Scancode
   getter: keyboard-event-scancode-raw
   setter: keyboard-event-scancode-raw-set!
   guard:  (Sint32-guard "sdl2:keyboard-event field scancode"))
  ("key.keysym.sym"
   type:   SDL_Keycode
   getter: keyboard-event-sym-raw
   setter: keyboard-event-sym-raw-set!
   guard:  (Sint32-guard "sdl2:keyboard-event field sym"))
  ("key.keysym.mod"
   type:   Uint16
   getter: keyboard-event-mod-raw
   setter: keyboard-event-mod-raw-set!
   guard:  (Uint16-guard "sdl2:keyboard-event field mod")))


(define-enum-accessor
  getter: (keyboard-event-scancode
           raw:   keyboard-event-scancode-raw
           conv:  scancode->symbol)
  setter: (keyboard-event-scancode-set!
           raw:   keyboard-event-scancode-raw-set!
           conv:  symbol->scancode))

(define-enum-accessor
  getter: (keyboard-event-sym
           raw:   keyboard-event-sym-raw
           conv:  keycode->symbol)
  setter: (keyboard-event-sym-set!
           raw:   keyboard-event-sym-raw-set!
           conv:  symbol->keycode))

(define-enum-mask-accessor
  getter: (keyboard-event-mod
           raw:    keyboard-event-mod-raw
           unpack: unpack-keymods
           exact:  #f)
  setter: (keyboard-event-mod-set!
           raw:    keyboard-event-mod-raw-set!
           pack:   pack-keymods))
