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


(export SDL_Init
        SDL_InitSubSystem
        SDL_Quit
        SDL_QuitSubSystem
        SDL_WasInit
        SDL_SetMainReady

        SDL_ClearError
        SDL_GetError
        SDL_SetError

        SDL_GetPlatform

        SDL_DisableScreenSaver
        SDL_EnableScreenSaver
        SDL_IsScreenSaverEnabled

        SDL_HasClipboardText
        SDL_GetClipboardText
        SDL_SetClipboardText

        %current-version
        %compiled-version
        SDL_VERSION_ATLEAST)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; INITIALIZATION AND SHUTDOWN

(define-function-binding SDL_Init
  return: (Sint32 zero-if-success)
  args: ((Uint32 flags)))

(define-function-binding SDL_InitSubSystem
  return: (Sint32 zero-if-success)
  args: ((Uint32 flags)))

(define-function-binding SDL_Quit)

(define-function-binding SDL_QuitSubSystem
  args: ((Uint32 flags)))

(define-function-binding SDL_WasInit
  return: (Uint32 initialized-subsystems-mask)
  args: ((Uint32 flags)))

(define-function-binding SDL_SetMainReady)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ERROR HANDLING

(define-function-binding SDL_ClearError)

(define-function-binding SDL_GetError
  return: (c-string error-message))

(define-function-binding SDL_SetError
  args: ((c-string fmt))
  ;; SDL_SetError takes printf-style varargs, but I don't know how to
  ;; declare that, so the string by itself will have to suffice.
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PLATFORM

(define-function-binding SDL_GetPlatform
  return: (c-string platform-name))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SCREEN SAVER

(define-function-binding SDL_DisableScreenSaver)
(define-function-binding SDL_EnableScreenSaver)

(define-function-binding SDL_IsScreenSaverEnabled
  return: (bool enabled?))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CLIPBOARD TEXT

(define-function-binding SDL_HasClipboardText
  return: (bool has-text?))

(define-function-binding SDL_GetClipboardText
  ;; NOTE: c-string* (with asterisk) not c-string. The original string
  ;; will be automatically freed by CHICKEN after copying.
  return: (c-string* text))

(define-function-binding SDL_SetClipboardText
  return: (Sint32 zero-if-success)
  args: (((const c-string) text)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; VERSION

(define %current-version
  (foreign-lambda*
   void ((Uint8* majorOut) (Uint8* minorOut) (Uint8* patchOut))
   "SDL_version v;"
   "SDL_GetVersion(&v);"
   "*majorOut = v.major;"
   "*minorOut = v.minor;"
   "*patchOut = v.patch;"))

(define %compiled-version
  (foreign-lambda*
   void ((Uint8* majorOut) (Uint8* minorOut) (Uint8* patchOut))
   "SDL_version v;"
   "SDL_VERSION(&v);"
   "*majorOut = v.major;"
   "*minorOut = v.minor;"
   "*patchOut = v.patch;"))

(define-function-binding* SDL_VERSION_ATLEAST
  return: (bool at-least?)
  args: ((Sint32 x) (Sint32 y) (Sint32 z))
  body: "C_return( SDL_VERSION_ATLEAST(x, y, z) );")
