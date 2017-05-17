;;; This file is the native entrypoint for the app. We first set up
;;; our environment for Android and then dispatch (or include) a main
;;; program. The main program should not need modifications from
;;; running on your desktop.

;; we need this to use SDL's main macro
(foreign-declare "#include <SDL.h>")
(foreign-declare "#include \"stdio2log.c\"")

;; ==================== bootstrapping ====================
;; hack of the year!
;;
;; you can have shared libraries in your android projects (under
;; ./libs). but only the ones that start with "lib" get included
;; during packaging. that's great.
;;
;; so we have a "make libs" target which adds the "lib" prefix to all
;; shared libraries. this hack then changes the chicken runtime to
;; look for files with the "lib" prefix if the one without it doesn't
;; work.
;;
;; include this file in your main-scheme file before any
;; (require-library) or (use) forms!
;;
;; you can (use tcp) and other units, but without this patch, (use
;; eggs) won't find the extensions.

(define ##sys#find-extension
  (let ((old-##sys#find-extension ##sys#find-extension))
    (lambda (p inc?)
      (or (old-##sys#find-extension p inc?)
          (old-##sys#find-extension (string-append "lib" p) inc?)))))


;; start a thread which will read our stdout/stderr and forward to
;; logcat. this is pretty much a must-have since GLSL compile errors
;; etc end up on stderr usually.
((foreign-lambda void start_logger))

;; ==================== your environment sould be ready here  ====================

;; replace with your favorite program here
;;(include "base.scm")
(use srfi-18)
(thread-sleep! 1)

(print "setting (repository-path) to " (repository-path "/data/data/org.libsdl.app/lib"))

(use nrepl)


;; it may be good practice to have your game start a srfi-18 thread,
;; do it's game loop there, and then return. this way, you can just do
;;
;; csi example.scm
;;
;; and the same file should run on your desktop too, and you get the
;; repl in the background. can be handy! let's follow the same
;; convention here, but using nrepl instead:

(use ports) ;; <-- workaround for nrepl bug
(use nrepl)
;; add a network REPL and loop forever
(nrepl 1234)


;; if we reach here, the app will exit
(let loop () (thread-sleep! 1) (print "OBS") (loop))
