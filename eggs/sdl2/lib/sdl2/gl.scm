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


(export gl-create-context!
        gl-delete-context!
        gl-make-current!
        gl-get-current-window
        gl-get-current-context

        gl-attribute  gl-attribute-set!
        gl-reset-attributes! ;; SDL >= 2.0.2

        gl-get-drawable-size ;; SDL >= 2.0.1

        gl-swap-window!
        gl-swap-interval  gl-swap-interval-set!

        gl-bind-texture!
        gl-unbind-texture!

        gl-extension-supported?
        ;; TODO?: gl-get-proc-address
        ;; TODO?: gl-load-library
        ;; TODO?: gl-unload-library
        )


(: gl-create-context!
   (sdl2:window* -> sdl2:gl-context))
(define (gl-create-context! window)
  (let ((context (SDL_GL_CreateContext window)))
    (if (and (gl-context? context) (not (struct-null? context)))
        context
        (abort (sdl-failure "SDL_GL_CreateContext" #f)))))


(: gl-delete-context!
   (sdl2:gl-context* -> void))
(define (gl-delete-context! context)
  (SDL_GL_DeleteContext context))


(: gl-make-current!
   (sdl2:window* sdl2:gl-context* -> void))
(define (gl-make-current! window gl-context)
  (let ((ret-code (SDL_GL_MakeCurrent window gl-context)))
    (unless (zero? ret-code)
      (abort (sdl-failure "SDL_GL_MakeCurrent" ret-code)))))


(: gl-get-current-window
   (-> sdl2:window))
(define (gl-get-current-window)
  (let ((window (SDL_GL_GetCurrentWindow)))
    (if (and (window? window) (not (struct-null? window)))
        window
        (abort (sdl-failure "SDL_GL_GetCurrentWindow" #f)))))


(: gl-get-current-context
   (-> sdl2:gl-context))
(define (gl-get-current-context)
  (let ((context (SDL_GL_GetCurrentContext)))
    (if (and (gl-context? context) (not (struct-null? context)))
        context
        (abort (sdl-failure "SDL_GL_GetCurrentContext" #f)))))


(define-inline (%gl-attr->int attr fn-name)
  (if (integer? attr)
      attr
      (symbol->gl-attr
       attr
       (lambda (x)
         (error fn-name "invalid GL attr symbol" x)))))


;;; - If attr is 'context-flags, the return value will be a list of
;;;   symbols.
;;; - If attr is 'context-profile-mask, the return value will be a
;;;   symbol ('core, 'compatibility, or 'es).
;;; - (2.0.4+) If attr is 'context-release-behavior, the return value
;;;   will be a symbol ('none or 'flush).
;;; - For other attributes, the return value will be an integer.
(define-inline (%int->gl-attr-value attr-int value-int)
  (cond ((= attr-int SDL_GL_CONTEXT_FLAGS)
         (unpack-gl-context-flags value-int))
        ((= attr-int SDL_GL_CONTEXT_PROFILE_MASK)
         (gl-profile->symbol value-int))
        ((and (feature? 'libSDL-2.0.4+)
              #+libSDL-2.0.4+
              (= attr-int SDL_GL_CONTEXT_RELEASE_BEHAVIOR))
         #+libSDL-2.0.4+
         (gl-context-release-flag->symbol value-int))
        (else
         value-int)))


;;; - If attr is 'context-flags, the value must be a list of GL
;;;   context flag enum symbols, or an integer.
;;; - If attr is 'context-profile-mask, the value must be a GL profile
;;;   enum symbol, or an integer.
;;; - For other attributes, the value must be an integer.
(define-inline (%gl-attr-value->int attr-int value)
  (cond ((integer? value)
         value)
        ((= attr-int SDL_GL_CONTEXT_FLAGS)
         (pack-gl-context-flags value))
        ((= attr-int SDL_GL_CONTEXT_PROFILE_MASK)
         (symbol->gl-profile value))
        ((and (feature? 'libSDL-2.0.4+)
              #+libSDL-2.0.4+
              (= attr-int SDL_GL_CONTEXT_RELEASE_BEHAVIOR))
         #+libSDL-2.0.4+
         (symbol->gl-context-release-flag value))
        (else
         value)))


;;; Get the value of a GL attribute. Returns the value (usually an
;;; integer) on success. Signals exception on failure.
(: gl-attribute
   ((or symbol fixnum) -> (or fixnum symbol (list-of symbol))))
(define (gl-attribute attr)
  (let ((attr-int (%gl-attr->int attr 'gl-attribute)))
    (with-temp-mem ((value-out (%allocate-Sint32)))
      (let ((ret-code (SDL_GL_GetAttribute attr-int value-out)))
        (if (zero? ret-code)
            (%int->gl-attr-value attr-int (pointer-s32-ref value-out))
            (begin
              (free value-out)
              (abort (sdl-failure "SDL_GL_GetAttribute" ret-code))))))))


;;; Set the value of a GL attribute. Signals exception on failure.
(: gl-attribute-set!
   ((or symbol fixnum) (or fixnum symbol (list-of symbol)) -> void))
(define (gl-attribute-set! attr value)
  (let* ((attr-int (%gl-attr->int attr 'gl-attribute-set!))
         (value-int (%gl-attr-value->int attr-int value))
         (ret-code (SDL_GL_SetAttribute attr-int value-int)))
    (unless (zero? ret-code)
      (abort (sdl-failure "SDL_GL_SetAttribute" ret-code)))))

(set! (setter gl-attribute)
      gl-attribute-set!)


(: gl-reset-attributes!
   (-> void))
(define-versioned (gl-reset-attributes!)
    libSDL-2.0.2+
  (SDL_GL_ResetAttributes))


(: gl-get-drawable-size
   (sdl2:window* -> fixnum fixnum))
(define-versioned (gl-get-drawable-size window)
    libSDL-2.0.1+
  (with-temp-mem ((w-out (%allocate-Uint32))
                  (h-out (%allocate-Uint32)))
    (SDL_GL_GetDrawableSize window w-out h-out)
    (values (pointer-u32-ref w-out)
            (pointer-u32-ref h-out))))


(: gl-swap-window!
   (sdl2:window* -> void))
(define (gl-swap-window! window)
  (SDL_GL_SwapWindow window))


(: gl-swap-interval
   (-> fixnum))
(define (gl-swap-interval)
  (SDL_GL_GetSwapInterval))

(: gl-swap-interval-set!
   (fixnum -> void))
(define (gl-swap-interval-set! interval)
  (let ((ret-code (SDL_GL_SetSwapInterval interval)))
    (unless (zero? ret-code)
      (abort (sdl-failure "SDL_GL_SetSwapInterval" ret-code)))))

(set! (setter gl-swap-interval)
      gl-swap-interval-set!)


(: gl-bind-texture!
   (sdl2:texture* -> float float))
(define (gl-bind-texture! texture)
  (with-temp-mem ((tex-w-out (%allocate-float))
                  (tex-h-out (%allocate-float)))
    (let ((ret-code (SDL_GL_BindTexture texture tex-w-out tex-h-out)))
      (if (zero? ret-code)
          (values (pointer-f32-ref tex-w-out)
                  (pointer-f32-ref tex-h-out))
          (begin
            (free tex-w-out)
            (free tex-h-out)
            (abort (sdl-failure "SDL_GL_BindTexture" ret-code)))))))

(: gl-unbind-texture!
   (sdl2:texture* -> void))
(define (gl-unbind-texture! texture)
  (let ((ret-code (SDL_GL_UnbindTexture texture)))
    (unless (zero? ret-code)
      (abort (sdl-failure "SDL_GL_UnbindTexture" ret-code)))))


(define (gl-extension-supported? name-string)
  (SDL_GL_ExtensionSupported name-string))

;; TODO?: gl-get-proc-address  (SDL_GL_GetProcAddress)
;; TODO?: gl-load-library  (SDL_GL_LoadLibrary)
;; TODO?: gl-unload-library  (SDL_GL_UnloadLibrary)
