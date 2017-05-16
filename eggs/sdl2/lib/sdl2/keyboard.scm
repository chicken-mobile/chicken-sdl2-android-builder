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


(export get-key-from-name       get-key-from-name-raw
        get-key-from-scancode   get-key-from-scancode-raw
        get-key-name

        get-scancode-from-name  get-scancode-from-name-raw
        get-scancode-from-key   get-scancode-from-key-raw
        get-scancode-name

        get-keyboard-focus
        scancode-pressed?

        mod-state  mod-state-raw
        mod-state-set!

        text-input-rect-set!
        start-text-input!
        stop-text-input!
        text-input-active?

        screen-keyboard-support?
        screen-keyboard-shown?)



(define (get-key-from-name name-str)
  (keycode->symbol
   (get-key-from-name-raw name-str)))

(define (get-key-from-name-raw name-str)
  (SDL_GetKeyFromName name-str))

(define (get-key-from-scancode scancode)
  (keycode->symbol
   (get-key-from-scancode-raw scancode)))

(define (get-key-from-scancode-raw scancode)
  (SDL_GetKeyFromScancode (if (integer? scancode)
                              scancode
                              (symbol->scancode scancode))))

(define (get-key-name key)
  (SDL_GetKeyName (if (integer? key)
                      key
                      (symbol->keycode key))))


(define (get-scancode-from-name name-str)
  (scancode->symbol
   (get-scancode-from-name-raw name-str)))

(define (get-scancode-from-name-raw name-str)
  (SDL_GetScancodeFromName name-str))

(define (get-scancode-from-key key)
  (scancode->symbol
   (get-scancode-from-key-raw key)))

(define (get-scancode-from-key-raw key)
  (SDL_GetScancodeFromKey (if (integer? key)
                              key
                              (symbol->keycode key))))

(define (get-scancode-name scancode)
  (SDL_GetScancodeName (if (integer? scancode)
                           scancode
                           (symbol->scancode scancode))))


(define (get-keyboard-focus)
  (SDL_GetKeyboardFocus))


;;; Related to SDL_GetKeyboardState.
(define (scancode-pressed? scancode)
  (case (%query-keyboard-state
         (if (integer? scancode)
             scancode
             (symbol->scancode scancode)))
    ((1) #t)
    ((0) #f)
    (else
     (error 'scancode-pressed? "invalid scancode" scancode))))


(define (mod-state-set! mods)
  (SDL_SetModState (pack-keymods mods)))

(define (mod-state)
  (unpack-keymods
   (mod-state-raw)))

(set! (setter mod-state)
      mod-state-set!)

(define (mod-state-raw)
  (SDL_GetModState))

(set! (setter mod-state-raw)
      mod-state-set!)


(define (text-input-rect-set! rect-or-false)
  (SDL_SetTextInputRect rect-or-false))

(define (start-text-input!)
  (SDL_StartTextInput))

(define (stop-text-input!)
  (SDL_StopTextInput))

(define (text-input-active?)
  (SDL_IsTextInputActive))


(define (screen-keyboard-support?)
  (SDL_HasScreenKeyboardSupport))

(define (screen-keyboard-shown? window)
  (SDL_IsScreenKeyboardShown window))
