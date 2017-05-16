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


(module sdl2-internals ()

(import scheme chicken foreign)
(use srfi-1 srfi-69 data-structures lolevel)

#>
#include "SDL.h"
#include "lib/sdl2-internals/custom-functions/math.c"
#include "lib/sdl2-internals/custom-functions/color.c"
#include "lib/sdl2-internals/custom-functions/point.c"
#include "lib/sdl2-internals/custom-functions/rect.c"
#include "lib/sdl2-internals/custom-functions/surface.c"
<#

;;; Automatically generated during installation.
(include "lib/_features.scm")

(include "lib/shared/macro-helpers.scm")
(include "lib/shared/types.scm")

(include "lib/sdl2-internals/helpers/define-foreign-constants.scm")
(include "lib/sdl2-internals/helpers/define-enum-mappings.scm")
(include "lib/sdl2-internals/helpers/define-enum-accessor.scm")
(include "lib/sdl2-internals/helpers/define-enum-mask-packer.scm")
(include "lib/sdl2-internals/helpers/define-enum-mask-unpacker.scm")
(include "lib/sdl2-internals/helpers/define-enum-mask-accessor.scm")
(include "lib/sdl2-internals/helpers/define-versioned-enum-mappings.scm")
(include "lib/sdl2-internals/helpers/guards.scm")
(include "lib/sdl2-internals/helpers/struct.scm")
(include "lib/sdl2-internals/helpers/event.scm")
(include "lib/sdl2-internals/helpers/c-array.scm")
(include "lib/sdl2-internals/helpers/define-function-binding.scm")
(include "lib/sdl2-internals/helpers/foreign-lambda-with-dynamic-body.scm")
(include "lib/sdl2-internals/helpers/foreign-value-with-dynamic-body.scm")
(include "lib/sdl2-internals/helpers/sdl-failure.scm")

;;; NOTE: The ordering of the includes below is important!

;;; 1. Record types, so that we have the pointer wrappers.
(include "lib/sdl2-internals/record-types/audio-cvt.scm")
(include "lib/sdl2-internals/record-types/audio-spec.scm")
(include "lib/sdl2-internals/record-types/color.scm")
(include "lib/sdl2-internals/record-types/cursor.scm")
(include "lib/sdl2-internals/record-types/display-mode.scm")
(include "lib/sdl2-internals/record-types/event.scm")
(include "lib/sdl2-internals/record-types/finger.scm")
(include "lib/sdl2-internals/record-types/gl-context.scm")
(include "lib/sdl2-internals/record-types/joystick-guid.scm")
(include "lib/sdl2-internals/record-types/joystick.scm")
(include "lib/sdl2-internals/record-types/keysym.scm")
(include "lib/sdl2-internals/record-types/palette.scm")
(include "lib/sdl2-internals/record-types/pixel-format.scm")
(include "lib/sdl2-internals/record-types/point.scm")
(include "lib/sdl2-internals/record-types/rect.scm")
(include "lib/sdl2-internals/record-types/renderer.scm")
(include "lib/sdl2-internals/record-types/renderer-info.scm")
(include "lib/sdl2-internals/record-types/rwops.scm")
(include "lib/sdl2-internals/record-types/surface.scm")
(include "lib/sdl2-internals/record-types/texture.scm")
(include "lib/sdl2-internals/record-types/window.scm")

;;; 2. Foreign types, which depend on the pointer wrappers.
(include "lib/sdl2-internals/foreign-types.scm")

;;; 3. Enums, which depend on foreign types defs.
(include "lib/sdl2-internals/enums/audio.scm")
(include "lib/sdl2-internals/enums/cursor.scm")
(include "lib/sdl2-internals/enums/events.scm")
(include "lib/sdl2-internals/enums/general.scm")
(include "lib/sdl2-internals/enums/gl.scm")
(include "lib/sdl2-internals/enums/hints.scm")
(include "lib/sdl2-internals/enums/joystick.scm")
(include "lib/sdl2-internals/enums/keycode.scm")
(include "lib/sdl2-internals/enums/mouse.scm")
(include "lib/sdl2-internals/enums/pixel-format.scm")
(include "lib/sdl2-internals/enums/renderer.scm")
(include "lib/sdl2-internals/enums/rwops.scm")
(include "lib/sdl2-internals/enums/scancode.scm")
(include "lib/sdl2-internals/enums/surface.scm")
(include "lib/sdl2-internals/enums/texture.scm")
(include "lib/sdl2-internals/enums/window.scm")

;;; 4a. Struct accessors, which depend on foreign types and enums.
(include "lib/sdl2-internals/accessors/audio-cvt.scm")
(include "lib/sdl2-internals/accessors/audio-spec.scm")
(include "lib/sdl2-internals/accessors/color.scm")
(include "lib/sdl2-internals/accessors/display-mode.scm")
(include "lib/sdl2-internals/accessors/events/common.scm")
(include "lib/sdl2-internals/accessors/events/controller-axis-event.scm")
(include "lib/sdl2-internals/accessors/events/controller-button-event.scm")
(include "lib/sdl2-internals/accessors/events/controller-device-event.scm")
(include "lib/sdl2-internals/accessors/events/dollar-gesture-event.scm")
(include "lib/sdl2-internals/accessors/events/drop-event.scm")
(include "lib/sdl2-internals/accessors/events/joy-axis-event.scm")
(include "lib/sdl2-internals/accessors/events/joy-ball-event.scm")
(include "lib/sdl2-internals/accessors/events/joy-button-event.scm")
(include "lib/sdl2-internals/accessors/events/joy-device-event.scm")
(include "lib/sdl2-internals/accessors/events/joy-hat-event.scm")
(include "lib/sdl2-internals/accessors/events/keyboard-event.scm")
(include "lib/sdl2-internals/accessors/events/mouse-button-event.scm")
(include "lib/sdl2-internals/accessors/events/mouse-motion-event.scm")
(include "lib/sdl2-internals/accessors/events/mouse-wheel-event.scm")
(include "lib/sdl2-internals/accessors/events/multi-gesture-event.scm")
(include "lib/sdl2-internals/accessors/events/quit-event.scm")
(include "lib/sdl2-internals/accessors/events/sys-wm-event.scm")
(include "lib/sdl2-internals/accessors/events/text-editing-event.scm")
(include "lib/sdl2-internals/accessors/events/text-input-event.scm")
(include "lib/sdl2-internals/accessors/events/touch-finger-event.scm")
(include "lib/sdl2-internals/accessors/events/user-event.scm")
(include "lib/sdl2-internals/accessors/events/window-event.scm")
(include "lib/sdl2-internals/accessors/finger.scm")
(include "lib/sdl2-internals/accessors/keysym.scm")
(include "lib/sdl2-internals/accessors/palette.scm")
(include "lib/sdl2-internals/accessors/pixel-format.scm")
(include "lib/sdl2-internals/accessors/point.scm")
(include "lib/sdl2-internals/accessors/rect.scm")
(include "lib/sdl2-internals/accessors/renderer-info.scm")
(include "lib/sdl2-internals/accessors/rwops.scm")
(include "lib/sdl2-internals/accessors/surface.scm")

;;; 4b. Array managers, which depend on foreign types.
(include "lib/sdl2-internals/arrays/color-array.scm")
(include "lib/sdl2-internals/arrays/event-array.scm")
(include "lib/sdl2-internals/arrays/number-array.scm")
(include "lib/sdl2-internals/arrays/point-array.scm")
(include "lib/sdl2-internals/arrays/rect-array.scm")

;;; 4c. Function bindings, which depend on foreign types.
(include "lib/sdl2-internals/functions/audio.scm")
(include "lib/sdl2-internals/functions/cursor.scm")
(include "lib/sdl2-internals/functions/events.scm")
(include "lib/sdl2-internals/functions/general.scm")
(include "lib/sdl2-internals/functions/gl.scm")
(include "lib/sdl2-internals/functions/hints.scm")
(include "lib/sdl2-internals/functions/joystick.scm")
(include "lib/sdl2-internals/functions/keyboard.scm")
(include "lib/sdl2-internals/functions/mouse.scm")
(include "lib/sdl2-internals/functions/pixel-format.scm")
(include "lib/sdl2-internals/functions/rect.scm")
(include "lib/sdl2-internals/functions/renderer.scm")
(include "lib/sdl2-internals/functions/renderer-draw.scm")
(include "lib/sdl2-internals/functions/rwops.scm")
(include "lib/sdl2-internals/functions/surface.scm")
(include "lib/sdl2-internals/functions/texture.scm")
(include "lib/sdl2-internals/functions/timer.scm")
(include "lib/sdl2-internals/functions/touch.scm")
(include "lib/sdl2-internals/functions/video-display-mode.scm")
(include "lib/sdl2-internals/functions/window.scm")

;;; 5. Extra procedures.

(include "lib/sdl2-internals/extras/color.scm")
(include "lib/sdl2-internals/extras/point.scm")
(include "lib/sdl2-internals/extras/rect.scm")

)
