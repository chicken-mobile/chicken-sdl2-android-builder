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


(export create-renderer!
        create-software-renderer!
        create-window-and-renderer!
        destroy-renderer!
        get-renderer
        get-renderer-info  get-renderer-info*
        renderer-output-size
        num-render-drivers
        render-driver-info
        render-present!

        render-clip-rect     render-clip-rect-set!
        render-clip-enabled? ;; SDL >= 2.0.4
        render-logical-size  render-logical-size-set!
        render-scale         render-scale-set!
        render-viewport      render-viewport-set!

        render-copy!
        render-copy-ex!
        render-target-supported?
        render-target
        render-target-set!
        render-read-pixels-raw)


(: create-renderer!
   (sdl2:window* #!optional fixnum (or fixnum (list-of symbol))
    -> sdl2:renderer))
(define (create-renderer! window #!optional (index -1) (flags '()))
  (let ((renderer (SDL_CreateRenderer
                   window index (pack-renderer-flags flags))))
    (if (and (renderer? renderer) (not (struct-null? renderer)))
        renderer
        (abort (sdl-failure "SDL_CreateRenderer" #f)))))


(: create-software-renderer!
   (sdl2:surface* -> sdl2:renderer))
(define (create-software-renderer! surface)
  (let ((renderer (SDL_CreateSoftwareRenderer surface)))
    (if (and (renderer? renderer) (not (struct-null? renderer)))
        renderer
        (abort (sdl-failure "SDL_CreateSoftwareRenderer" #f)))))


(: create-window-and-renderer!
   (fixnum fixnum #!optional (or fixnum (list-of symbol))
    -> sdl2:window sdl2:renderer))
(define (create-window-and-renderer! width height #!optional (window-flags '()))
  ;; This is a bit tricky because SDL_CreateWindowAndRenderer uses
  ;; pointer-pointers (SDL_Window** and SDL_Renderer**).
  (let* ((window**   (make-pointer-vector 1))
         (renderer** (make-pointer-vector 1))
         (ret-code (SDL_CreateWindowAndRenderer
                    width height (pack-window-flags window-flags)
                    window** renderer**)))
    (if (zero? ret-code)
        (values (wrap-window   (pointer-vector-ref window**   0))
                (wrap-renderer (pointer-vector-ref renderer** 0)))
        (abort (sdl-failure "SDL_CreateWindowAndRenderer" #f)))))


(: destroy-renderer!
   (sdl2:renderer* -> void))
(define (destroy-renderer! renderer)
  (SDL_DestroyRenderer renderer))


(: get-renderer
   (sdl2:window* -> sdl2:renderer))
(define (get-renderer window)
  (let ((renderer (SDL_GetRenderer window)))
    (if (and (renderer? renderer) (not (struct-null? renderer)))
        renderer
        (abort (sdl-failure "SDL_GetRenderer" #f)))))


(: get-renderer-info
   (sdl2:renderer* -> sdl2:renderer-info))
(define (get-renderer-info renderer)
  (set-finalizer! (get-renderer-info* renderer)
                  free-renderer-info!))

(: get-renderer-info*
   (sdl2:renderer* -> sdl2:renderer-info))
(define (get-renderer-info* renderer)
  (let* ((info (alloc-renderer-info))
         (ret-code (SDL_GetRendererInfo renderer info)))
    (if (zero? ret-code)
        info
        (begin
          (free-renderer-info! info)
          (abort (sdl-failure "SDL_GetRendererInfo" ret-code))))))


(: renderer-output-size
   (sdl2:renderer* -> fixnum fixnum))
(define (renderer-output-size renderer)
  (with-temp-mem ((w-out (%allocate-Sint32))
                  (h-out (%allocate-Sint32)))
    (let ((ret-code (SDL_GetRendererOutputSize renderer w-out h-out)))
      (if (zero? ret-code)
          (values (pointer-s32-ref w-out)
                  (pointer-s32-ref h-out))
          (begin
            (free w-out)
            (free h-out)
            (abort (sdl-failure "SDL_GetRendererOutputSize" ret-code)))))))


(: num-render-drivers
   (-> fixnum))
(define (num-render-drivers)
  (let ((num (SDL_GetNumRenderDrivers)))
    (if (negative? num)
        (abort (sdl-failure "SDL_GetNumRenderDrivers" num))
        num)))


(: render-driver-info
   (fixnum -> sdl2:renderer-info))
(define (render-driver-info index)
  (let* ((info (alloc-renderer-info))
         (ret-code (SDL_GetRenderDriverInfo index info)))
    (if (zero? ret-code)
        info
        (begin
          (free-renderer-info! info)
          (abort (sdl-failure "SDL_GetRendererInfo" ret-code))))))


(: render-present!
   (sdl2:renderer* -> void))
(define (render-present! renderer)
  (SDL_RenderPresent renderer))


(: render-clip-rect
   (sdl2:renderer* -> sdl2:rect))
(define (render-clip-rect renderer)
  (let ((rect-out (alloc-rect)))
    (SDL_RenderGetClipRect renderer rect-out)
    rect-out))

(: render-clip-rect-set!
   (sdl2:renderer* (or sdl2:rect* boolean) -> void))
(define (render-clip-rect-set! renderer rect)
  (let ((ret-code (SDL_RenderSetClipRect renderer rect)))
    (unless (zero? ret-code)
      (abort (sdl-failure "SDL_RenderSetClipRect" ret-code)))))

(set! (setter render-clip-rect) render-clip-rect-set!)


(: render-clip-enabled?
   (sdl2:renderer* -> boolean))
(define-versioned (render-clip-enabled? renderer)
    libSDL-2.0.4+
  (SDL_RenderIsClipEnabled renderer))


(: render-logical-size
   (sdl2:renderer* -> fixnum fixnum))
(define (render-logical-size renderer)
  (with-temp-mem ((w-out (%allocate-Sint32))
                  (h-out (%allocate-Sint32)))
    (SDL_RenderGetLogicalSize renderer w-out h-out)
    (values (pointer-s32-ref w-out)
            (pointer-s32-ref h-out))))

(: render-logical-size-set!
   (sdl2:renderer* (list fixnum fixnum) -> void))
(define (render-logical-size-set! renderer size)
  (let ((ret-code (SDL_RenderSetLogicalSize renderer (car size) (cadr size))))
    (unless (zero? ret-code)
      (abort (sdl-failure "SDL_RenderSetLogicalSize" ret-code)))))

(set! (setter render-logical-size) render-logical-size-set!)


(: render-scale
   (sdl2:renderer* -> float float))
(define (render-scale renderer)
  (with-temp-mem ((scale-x-out (%allocate-float))
                  (scale-y-out (%allocate-float)))
    (SDL_RenderGetScale renderer scale-x-out scale-y-out)
    (values (pointer-f32-ref scale-x-out)
            (pointer-f32-ref scale-y-out))))

(: render-scale-set!
   (sdl2:renderer* (list fixnum fixnum) -> void))
(define (render-scale-set! renderer scale)
  (let ((ret-code (SDL_RenderSetScale renderer (car scale) (cadr scale))))
    (unless (zero? ret-code)
      (abort (sdl-failure "SDL_RenderSetScale" ret-code)))))

(set! (setter render-scale) render-scale-set!)


(: render-viewport
   (sdl2:renderer* -> sdl2:rect))
(define (render-viewport renderer)
  (let ((rect-out (alloc-rect)))
    (SDL_RenderGetViewport renderer rect-out)
    rect-out))

(: render-viewport-set!
   (sdl2:renderer* sdl2:rect -> void))
(define (render-viewport-set! renderer rect)
  (let ((ret-code (SDL_RenderSetViewport renderer rect)))
    (unless (zero? ret-code)
      (abort (sdl-failure "SDL_RenderSetViewport" ret-code)))))

(set! (setter render-viewport) render-viewport-set!)


(: render-copy!
   (sdl2:renderer* sdl2:texture*
    #!optional
    (or sdl2:rect boolean) (or sdl2:rect boolean)
    -> void))
(define (render-copy! renderer texture #!optional
                        srcrect dstrect)
  (let ((ret-code (SDL_RenderCopy
                   renderer texture srcrect dstrect)))
    (unless (zero? ret-code)
      (abort (sdl-failure "SDL_RenderCopy" ret-code)))))

(: render-copy-ex!
   (sdl2:renderer* sdl2:texture*
    #!optional
    (or sdl2:rect* boolean) (or sdl2:rect* boolean)
    float (or sdl2:point* boolean)
    (or fixnum (list-of symbol))
    -> void))
(define (render-copy-ex! renderer texture #!optional
                           srcrect dstrect
                           (angle 0) center
                           (flip '()))
  (let* ((flip-int (if (integer? flip)
                       flip
                       (pack-renderer-flip flip)))
         (ret-code (SDL_RenderCopyEx
                    renderer texture
                    srcrect dstrect
                    angle center
                    flip-int)))
    (unless (zero? ret-code)
      (abort (sdl-failure "SDL_RenderCopyEx" ret-code)))))


(: render-target-supported?
   (sdl2:renderer* -> boolean))
(define (render-target-supported? renderer)
  (SDL_RenderTargetSupported renderer))


(: render-target
   (sdl2:renderer* -> (or sdl2:texture boolean)))
(define (render-target renderer)
  (let ((texture (SDL_GetRenderTarget renderer)))
    (if (struct-null? texture)
        #f
        texture)))

(: render-target-set!
   (sdl2:renderer* sdl2:texture* -> void))
(define (render-target-set! renderer texture)
  (let ((ret-code (SDL_SetRenderTarget renderer texture)))
    (unless (zero? ret-code)
      (abort (sdl-failure "SDL_SetRenderTarget" ret-code)))))

(set! (setter render-target) render-target-set!)


(: render-read-pixels-raw
   (sdl2:renderer* (or sdl2:rect* boolean)
   enum (or pointer locative) fixnum
   -> void))
(define (render-read-pixels-raw renderer rect format pixels-out pitch)
  (let* ((format-int (if (integer? format)
                         format
                         (symbol->pixel-format-enum format)))
         (ret-code (SDL_RenderReadPixels
                    renderer rect format-int pixels-out pitch)))
    (unless (zero? ret-code)
      (abort (sdl-failure "SDL_RenderReadPixels" ret-code)))))
