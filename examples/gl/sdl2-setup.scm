(sdl2:set-main-ready!)
(sdl2:init! '(video timer))
(on-exit sdl2:quit!)

;; ====================
(define window (sdl2:create-window!
                "SDL2 + OpenGL Example"
                'undefined 'undefined 512 512
                '(opengl)))



;;; Use OpenGL 3.3
(cond-expand
 (arm
  (sdl2:gl-attribute-set! 'context-profile-mask 'es)
  (sdl2:gl-attribute-set! 'context-major-version 3)
  (sdl2:gl-attribute-set! 'context-minor-version 0))
 (else ;; even though my machine support es 3 0, i can't seem to create float textures with it :-(
  (sdl2:gl-attribute-set! 'context-profile-mask 'core)
  (sdl2:gl-attribute-set! 'context-major-version 3)
  (sdl2:gl-attribute-set! 'context-minor-version 3)))

(define gl-context (sdl2:gl-create-context! window))

(assert (equal? window     (sdl2:gl-get-current-window)))
(assert (equal? gl-context (sdl2:gl-get-current-context)))

(gl:init)
(gl-utils:check-error)

;; (set! (sdl2:gl-swap-interval) 0) ;; turn off vsync
;; (set! (sdl2:gl-swap-interval) 1) ;; turn on vsync (default)


;; this is just very useful somtimes.
;; (set-gc-report! #t)


