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


(export num-joysticks
        joystick-open!
        joystick-close!
        joystick-from-instance-id

        joystick-update!
        joystick-event-state  joystick-event-state-set!
        joystick-attached?
        joystick-current-power-level

        joystick-num-axes
        joystick-num-balls
        joystick-num-buttons
        joystick-num-hats

        joystick-get-axis
        joystick-get-ball
        joystick-get-button
        joystick-get-hat
        joystick-get-hat-raw

        joystick-instance-id
        joystick-name
        joystick-name-for-index

        joystick-get-device-guid
        joystick-get-guid
        joystick-get-guid-from-string
        joystick-get-guid-string)



(define (num-joysticks)
  (let ((ret-code (SDL_NumJoysticks)))
    (when (negative? ret-code)
      (abort (sdl-failure "SDL_NumJoysticks" ret-code)))))


(define (joystick-open! device-index)
  (let ((joystick (SDL_JoystickOpen device-index)))
    (if (and (joystick? joystick) (not (struct-null? joystick)))
        joystick
        (abort (sdl-failure "SDL_JoystickOpen" #f)))))

(define (joystick-close! joystick)
  (SDL_JoystickClose joystick))


(define-versioned (joystick-from-instance-id joy-id)
    libSDL-2.0.4+
  (let ((joystick (SDL_JoystickFromInstanceID joy-id)))
    (if (and (joystick? joystick) (not (struct-null? joystick)))
        joystick
        (abort (sdl-failure "SDL_JoystickFromInstanceID" #f)))))


(define (joystick-update!)
  (SDL_JoystickUpdate))


(: joystick-event-state-set!
   (boolean -> boolean))
(define (joystick-event-state-set! state)
  (let* ((state-int (case state
                      ((#t) SDL_ENABLE)
                      ((#f) SDL_IGNORE)
                      (else (error 'joystick-event-state-set!
                                   "invalid state" state))))
         (result (SDL_JoystickEventState state-int)))
    (if (negative? result)
        (abort (sdl-failure "SDL_JoystickEventState" result))
        (= result SDL_ENABLE))))

(: joystick-event-state
   (-> boolean))
(define (joystick-event-state)
  (let ((result (SDL_JoystickEventState SDL_QUERY)))
    (if (negative? result)
        (abort (sdl-failure "SDL_JoystickEventState" result))
        (= result SDL_ENABLE))))

(set! (setter joystick-event-state)
      joystick-event-state-set!)


(define (joystick-attached? joystick)
  (SDL_JoystickGetAttached joystick))


(: joystick-current-power-level
   (sdl2:joystick* -> symbol))
(define-versioned (joystick-current-power-level joystick)
    libSDL-2.0.4+
  (joystick-power-level->symbol
   (SDL_JoystickCurrentPowerLevel joystick)))


(define (joystick-num-axes joystick)
  (let ((ret-code (SDL_JoystickNumAxes joystick)))
    (when (negative? ret-code)
      (abort (sdl-failure "SDL_JoystickNumAxes" ret-code)))))

(define (joystick-num-balls joystick)
  (let ((ret-code (SDL_JoystickNumBalls joystick)))
    (when (negative? ret-code)
      (abort (sdl-failure "SDL_JoystickNumBalls" ret-code)))))

(define (joystick-num-buttons joystick)
  (let ((ret-code (SDL_JoystickNumButtons joystick)))
    (when (negative? ret-code)
      (abort (sdl-failure "SDL_JoystickNumButtons" ret-code)))))

(define (joystick-num-hats joystick)
  (let ((ret-code (SDL_JoystickNumHats joystick)))
    (when (negative? ret-code)
      (abort (sdl-failure "SDL_JoystickNumHats" ret-code)))))


(define (joystick-get-axis joystick axis)
  (assert-bounds axis 0 (sub1 (joystick-num-axes joystick))
                 "axis number out of bounds" 'joystick-get-axis)
  (SDL_JoystickGetAxis joystick axis))

(define (joystick-get-ball joystick ball)
  (assert-bounds ball 0 (sub1 (joystick-num-balls joystick))
                 "ball number out of bounds" 'joystick-get-ball)
  (with-temp-mem ((dx-out (%allocate-Sint32))
                  (dy-out (%allocate-Sint32)))
    (let ((ret-code (SDL_JoystickGetBall
                     joystick ball dx-out dy-out)))
      (if (zero? ret-code)
          (values (pointer-s32-ref dx-out)
                  (pointer-s32-ref dy-out))
          (begin
            (free dx-out)
            (free dy-out)
            (abort (sdl-failure "SDL_JoystickGetBall" ret-code)))))))

(define (joystick-get-button joystick button)
  (assert-bounds button 0 (sub1 (joystick-num-buttons joystick))
                 "button number out of bounds" 'joystick-get-button)
  (SDL_JoystickGetButton joystick button))

(define (joystick-get-hat joystick hat)
  (assert-bounds hat 0 (sub1 (joystick-num-hats joystick))
                 "hat number out of bounds" 'joystick-get-hat)
  (joystick-hat-position->symbol
   (SDL_JoystickGetHat joystick hat)))

(define (joystick-get-hat-raw joystick hat)
  (assert-bounds hat 0 (sub1 (joystick-num-hats joystick))
                 "hat number out of bounds" 'joystick-get-hat-raw)
  (SDL_JoystickGetHat joystick hat))


(define (joystick-instance-id joystick)
  (let ((id (SDL_JoystickInstanceID joystick)))
    (if (negative? id)
        (abort (sdl-failure "SDL_JoystickInstanceID" id))
        id)))

(define (joystick-name joystick)
  (SDL_JoystickName joystick))

(define (joystick-name-for-index device-index)
  (SDL_JoystickNameForIndex device-index))


(define (joystick-get-device-guid device-index)
  (%autofree-struct!
   (SDL_JoystickGetDeviceGUID device-index)
   free-joystick-guid!))

(define (joystick-get-guid joystick)
  (%autofree-struct!
   (SDL_JoystickGetGUID joystick)
   free-joystick-guid!))

(define (joystick-get-guid-from-string guid-string)
  (%autofree-struct!
   (SDL_JoystickGetGUIDFromString guid-string)
   free-joystick-guid!))

(define (joystick-get-guid-string guid)
  (%joystick-get-guid-string guid))
