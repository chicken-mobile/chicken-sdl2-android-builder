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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; OPENGL SUPPORT

(export SDL_GL_CreateContext
        SDL_GL_DeleteContext
        SDL_GL_MakeCurrent
        SDL_GL_GetCurrentWindow
        SDL_GL_GetCurrentContext
        SDL_GL_GetAttribute
        SDL_GL_SetAttribute
        SDL_GL_SwapWindow
        SDL_GL_GetSwapInterval
        SDL_GL_SetSwapInterval
        SDL_GL_ExtensionSupported
        SDL_GL_GetProcAddress
        SDL_GL_LoadLibrary
        SDL_GL_UnloadLibrary
        SDL_GL_BindTexture
        SDL_GL_UnbindTexture)

#+libSDL-2.0.1+
(begin
  (export SDL_GL_GetDrawableSize)
  (define-function-binding SDL_GL_GetDrawableSize
    args: ((SDL_Window* window) (Sint32* w_out) (Sint32* h_out))))

#+libSDL-2.0.2+
(begin
  (export SDL_GL_ResetAttributes)
  (define-function-binding SDL_GL_ResetAttributes))


(define-function-binding SDL_GL_CreateContext
  return: (SDL_GLContext* new-context)
  args: ((SDL_Window* window)))

(define-function-binding SDL_GL_DeleteContext
  args: ((SDL_GLContext* context)))

(define-function-binding SDL_GL_MakeCurrent
  return: (Sint32 zero-if-success)
  args: ((SDL_Window* window)
         (SDL_GLContext* context)))

(define-function-binding SDL_GL_GetCurrentWindow
  return: (SDL_Window* current-window))

(define-function-binding SDL_GL_GetCurrentContext
  return: (SDL_GLContext* current-context))


(define-function-binding SDL_GL_GetAttribute
  return: (Sint32 zero-if-success)
  args: ((SDL_GLattr attr_id)
         (Sint32* value)))

(define-function-binding SDL_GL_SetAttribute
  return: (Sint32 zero-if-success)
  args: ((SDL_GLattr attr_id)
         (Sint32 value)))


(define-function-binding SDL_GL_SwapWindow
  args: ((SDL_Window* window)))

(define-function-binding SDL_GL_GetSwapInterval
  return: (Sint32 interval))

(define-function-binding SDL_GL_SetSwapInterval
  return: (Sint32 zero-if-success)
  args: ((Sint32 interval)))


(define-function-binding SDL_GL_ExtensionSupported
  return: (bool supported?)
  args: ((c-string extension)))


(define-function-binding SDL_GL_GetProcAddress
  return: (c-pointer address)
  args: ((c-string proc)))

(define-function-binding SDL_GL_LoadLibrary
  return: (Sint32 zero-if-success)
  args: ((c-string path)))

(define-function-binding SDL_GL_UnloadLibrary)


(define-function-binding SDL_GL_BindTexture
  return: (Sint32 zero-if-success)
  args: ((SDL_Texture* texture)
         (float* tex-w-out)
         (float* tex-h-out)))

(define-function-binding SDL_GL_UnbindTexture
  return: (Sint32 zero-if-success)
  args: ((SDL_Texture* texture)))
