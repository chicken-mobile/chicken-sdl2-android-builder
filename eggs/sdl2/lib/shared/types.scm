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


;;; This file defines type aliases, so that procedure type
;;; declarations will be simpler.

;;; Strict definition. Only allows record types.
;;; This is good for return types, or argument types when you want to
;;; be strict.
(define-type sdl2:audio-cvt     (struct sdl2:audio-cvt))
(define-type sdl2:audio-spec    (struct sdl2:audio-spec))
(define-type sdl2:color         (struct sdl2:color))
(define-type sdl2:cursor        (struct sdl2:cursor))
(define-type sdl2:display-mode  (struct sdl2:display-mode))
(define-type sdl2:finger        (struct sdl2:finger))
(define-type sdl2:gl-context    (struct sdl2:gl-context))
(define-type sdl2:joystick      (struct sdl2:joystick))
(define-type sdl2:joystick-guid (struct sdl2:joystick-guid))
(define-type sdl2:palette       (struct sdl2:palette))
(define-type sdl2:pixel-format  (struct sdl2:pixel-format))
(define-type sdl2:point         (struct sdl2:point))
(define-type sdl2:rect          (struct sdl2:rect))
(define-type sdl2:renderer      (struct sdl2:renderer))
(define-type sdl2:renderer-info (struct sdl2:renderer-info))
(define-type sdl2:surface       (struct sdl2:surface))
(define-type sdl2:texture       (struct sdl2:texture))
(define-type sdl2:window        (struct sdl2:window))
(define-type sdl2:event         (struct sdl2:event))
(define-type sdl2:keysym        (struct sdl2:keysym))

;;; Loose definition. Also allows raw pointers or locatives.
;;; This is good for argument types, but not return types.
(define-type sdl2:audio-cvt*     (or pointer locative sdl2:audio-cvt))
(define-type sdl2:audio-spec*    (or pointer locative sdl2:audio-spec))
(define-type sdl2:color*         (or pointer locative sdl2:color))
(define-type sdl2:cursor*        (or pointer locative sdl2:cursor))
(define-type sdl2:display-mode*  (or pointer locative sdl2:display-mode))
(define-type sdl2:finger*        (or pointer locative sdl2:finger))
(define-type sdl2:gl-context*    (or pointer locative sdl2:gl-context))
(define-type sdl2:joystick*      (or pointer locative sdl2:joystick))
(define-type sdl2:joystick-guid* (or pointer locative sdl2:joystick-guid))
(define-type sdl2:palette*       (or pointer locative sdl2:palette))
(define-type sdl2:pixel-format*  (or pointer locative sdl2:pixel-format))
(define-type sdl2:point*         (or pointer locative sdl2:point))
(define-type sdl2:rect*          (or pointer locative sdl2:rect))
(define-type sdl2:renderer*      (or pointer locative sdl2:renderer))
(define-type sdl2:renderer-info* (or pointer locative sdl2:renderer-info))
(define-type sdl2:surface*       (or pointer locative sdl2:surface))
(define-type sdl2:texture*       (or pointer locative sdl2:texture))
(define-type sdl2:window*        (or pointer locative sdl2:window))
(define-type sdl2:event*         (or pointer locative sdl2:event))
(define-type sdl2:keysym*        (or pointer locative sdl2:keysym))

;;; Other types
(define-type enum      (or fixnum symbol))
(define-type enum-list (or fixnum (list-of symbol)))
