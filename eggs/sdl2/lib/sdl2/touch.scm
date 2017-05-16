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


(export get-num-touch-devices
        get-num-touch-fingers
        get-touch-device
        get-touch-finger

        ;; TODO: record-gesture!
        ;; TODO: save-dollar-template!
        ;; TODO: save-all-dollar-templates!
        ;; TODO: load-dollar-templates
        )


(define (get-num-touch-devices)
  (SDL_GetNumTouchDevices))

(define (get-num-touch-fingers touch-id)
  (SDL_GetNumTouchFingers touch-id))

(define (get-touch-device device-id)
  (let ((id (SDL_GetTouchDevice device-id)))
    (if (positive? id)
        id
        (abort (sdl-failure "SDL_GetTouchDevice" id)))))

(define (get-touch-finger touch-id index)
  (let ((finger (SDL_GetTouchFinger touch-id index)))
    (if (and (finger? finger)
             (not (struct-null? finger)))
        finger
        (abort (sdl-failure "SDL_GetTouchFinger" #f)))))


;; TODO: record-gesture!
;; TODO: save-dollar-template!
;; TODO: save-all-dollar-templates!
;; TODO: load-dollar-templates
