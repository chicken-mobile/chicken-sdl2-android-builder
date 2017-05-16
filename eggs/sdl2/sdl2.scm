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


(module sdl2 ()

(import scheme chicken)
(use sdl2-internals extras lolevel srfi-1 srfi-18)

;;; Automatically generated during installation.
(include "lib/_features.scm")

(include "lib/version.scm")
(include "lib/shared/error-helpers.scm")
(include "lib/shared/types.scm")

(include "lib/sdl2/helpers/with-temp-mem.scm")
(include "lib/sdl2/helpers/define-versioned.scm")

(include "lib/sdl2/reexports.scm")
(include "lib/sdl2/general.scm")
(include "lib/sdl2/events.scm")
(include "lib/sdl2/gl.scm")
(include "lib/sdl2/hints.scm")
(include "lib/sdl2/joystick.scm")
(include "lib/sdl2/keyboard.scm")
(include "lib/sdl2/palette.scm")
(include "lib/sdl2/pixel-format.scm")
(include "lib/sdl2/rect.scm")
(include "lib/sdl2/renderer.scm")
(include "lib/sdl2/renderer-draw.scm")
(include "lib/sdl2/rwops.scm")
(include "lib/sdl2/surface.scm")
(include "lib/sdl2/texture.scm")
(include "lib/sdl2/timer.scm")
(include "lib/sdl2/touch.scm")
(include "lib/sdl2/window.scm")

)
