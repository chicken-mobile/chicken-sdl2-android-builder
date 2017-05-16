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


(export SDL_GetKeyFromName
        SDL_GetKeyFromScancode
        SDL_GetKeyName

        SDL_GetScancodeFromKey
        SDL_GetScancodeFromName
        SDL_GetScancodeName

        SDL_GetKeyboardFocus
        SDL_GetKeyboardState
        %query-keyboard-state

        SDL_GetModState
        SDL_SetModState

        SDL_SetTextInputRect
        SDL_StartTextInput
        SDL_StopTextInput
        SDL_IsTextInputActive

        SDL_HasScreenKeyboardSupport
        SDL_IsScreenKeyboardShown)


(define-function-binding SDL_GetKeyFromName
  return: (SDL_Keycode key)
  args: ((c-string name)))

(define-function-binding SDL_GetKeyFromScancode
  return: (SDL_Keycode key)
  args: ((SDL_Scancode name)))

(define-function-binding SDL_GetKeyName
  return: (c-string name)
  args: ((SDL_Keycode key)))


(define-function-binding SDL_GetScancodeFromKey
  return: (SDL_Scancode scancode)
  args: ((SDL_Keycode key)))

(define-function-binding SDL_GetScancodeFromName
  return: (SDL_Scancode scancode)
  args: ((c-string name)))

(define-function-binding SDL_GetScancodeName
  return: (c-string name)
  args: ((SDL_Scancode scancode)))


(define-function-binding SDL_GetKeyboardFocus
  return: (SDL_Window* window-with-focus))


(define-function-binding SDL_GetKeyboardState
  return: (Uint8* key-states-array)
  args: ((Sint32* numkeys-out)))

;;; Returns 1 if the given scancode is currently pressed, 0 if not
;;; pressed, or -1 if the scancode is invalid / out of bounds.
(define %query-keyboard-state
  (foreign-lambda*
   int ((SDL_Scancode scancode))
   "int numkeys;
    const Uint8* keys = SDL_GetKeyboardState(&numkeys);
    if (0 <= scancode && scancode < numkeys) {
      C_return( (int)(keys[scancode]) );
    } else {
      C_return( -1 );
    }"))


(define-function-binding SDL_GetModState
  return: (SDL_Keymod modstate))

(define-function-binding SDL_SetModState
  args: ((SDL_Keymod modstate)))


(define-function-binding SDL_SetTextInputRect
  args: ((SDL_Rect* rect-or-null)))

(define-function-binding SDL_StartTextInput)

(define-function-binding SDL_StopTextInput)

(define-function-binding SDL_IsTextInputActive
  return: (bool events-enabled?))


(define-function-binding SDL_HasScreenKeyboardSupport
  return: (bool has-support?))

(define-function-binding SDL_IsScreenKeyboardShown
  return: (bool is-shown?)
  args: ((SDL_Window* window)))
