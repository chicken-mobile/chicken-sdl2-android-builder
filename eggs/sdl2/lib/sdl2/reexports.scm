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


;;; This file re-exports all the sdl2-internals exports that are
;;; intended for users to use directly. They are re-exported so that
;;; users do not need to import the sdl2-internals module.


(reexport
 (only
  sdl2-internals

  ;;:;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; GENERAL

  ;; sdl2-internals/helpers/struct.scm
  struct-eq?
  struct-null?


  ;;:;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; AUDIO-CVT

  ;; ;; sdl2-internals/record-types/audio-cvt.scm
  ;; audio-cvt?

  ;; ;; sdl2-internals/accessors/audio-cvt.scm
  ;; audio-cvt-needed
  ;; audio-cvt-src-format
  ;; audio-cvt-src-format-raw
  ;; audio-cvt-dst-format
  ;; audio-cvt-dst-format-raw
  ;; audio-cvt-rate-incr
  ;; audio-cvt-buf-raw
  ;; audio-cvt-len
  ;; audio-cvt-len-cvt
  ;; audio-cvt-len-mult
  ;; audio-cvt-len-ratio


  ;;:;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; AUDIO-SPEC

  ;; ;; sdl2-internals/record-types/audio-spec.scm
  ;; audio-spec?

  ;; ;; sdl2-internals/accessors/audio-spec.scm
  ;; audio-spec-freq          audio-spec-freq-set!
  ;; audio-spec-format-raw    audio-spec-format-raw-set!
  ;; audio-spec-format        audio-spec-format-set!
  ;; audio-spec-channels      audio-spec-channels-set!
  ;; audio-spec-silence
  ;; audio-spec-samples       audio-spec-samples-set!
  ;; audio-spec-size
  ;; ;; Omitted: audio-spec-callback (not safe)
  ;; ;; Omitted: audio-spec-callback-set! (not safe)
  ;; audio-spec-userdata-raw  audio-spec-userdata-raw-set!


  ;;:;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; COLOR

  ;; sdl2-internals/record-types/color.scm
  color?
  free-color!
  colour?
  free-colour!

  ;; sdl2-internals/accessors/color.scm
  color-r  color-r-set!
  color-g  color-g-set!
  color-b  color-b-set!
  color-a  color-a-set!
  colour-r  colour-r-set!
  colour-g  colour-g-set!
  colour-b  colour-b-set!
  colour-a  colour-a-set!

  ;; sdl2-internals/extras/color.scm
  make-color        make-color*
  color-set!
  color->list       color->values
  color=?
  color-copy!       color-copy
  copy-color        copy-color*
  color-scale!      color-scale
  color-mult!       color-mult
  color-add!        color-add
  color-sub!        color-sub
  color-lerp!       color-lerp

  make-colour       make-colour*
  colour-set!
  colour->list      colour->values
  colour=?
  colour-copy!      colour-copy
  copy-colour       copy-colour*
  colour-scale!     colour-scale
  colour-mult!      colour-mult
  colour-add!       colour-add
  colour-sub!       colour-sub
  colour-lerp!      colour-lerp


  ;;:;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; CURSOR

  ;; ;; sdl2-internals/record-types/cursor.scm
  ;; cursor?


  ;;:;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; DISPLAY-MODE

  ;; sdl2-internals/record-types/display-mode.scm
  display-mode?
  free-display-mode!

  ;; sdl2-internals/accessors/display-mode.scm
  display-mode-format-raw    display-mode-format-raw-set!
  display-mode-format        display-mode-format-set!
  display-mode-w             display-mode-w-set!
  display-mode-h             display-mode-h-set!
  display-mode-refresh-rate  display-mode-refresh-rate-set!
  make-display-mode
  make-display-mode*


  ;;:;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; EVENTS

  ;; sdl2-internals/record-types/event.scm
  event?
  free-event!

  ;; sdl2-internals/accessors/events/common.scm
  make-event
  make-event*
  event-type-raw   event-type-raw-set!
  event-type       event-type-set!
  event-timestamp  event-timestamp-set!

  ;; sdl2-internals/accessors/events/controller-axis-event.scm
  controller-axis-event?
  controller-axis-event-which  controller-axis-event-which-set!
  controller-axis-event-axis   controller-axis-event-axis-set!
  controller-axis-event-value  controller-axis-event-value-set!

  ;; sdl2-internals/accessors/events/controller-button-event.scm
  controller-button-event?
  controller-button-event-which   controller-button-event-which-set!
  controller-button-event-button  controller-button-event-button-set!
  controller-button-event-state   controller-button-event-state-set!

  ;; sdl2-internals/accessors/events/controller-device-event.scm
  controller-device-event?
  controller-device-event-which  controller-device-event-which-set!

  ;; sdl2-internals/accessors/events/dollar-gesture-event.scm
  dollar-gesture-event?
  dollar-gesture-event-touch-id     dollar-gesture-event-touch-id-set!
  dollar-gesture-event-gesture-id   dollar-gesture-event-gesture-id-set!
  dollar-gesture-event-num-fingers  dollar-gesture-event-num-fingers-set!
  dollar-gesture-event-error        dollar-gesture-event-error-set!
  dollar-gesture-event-x            dollar-gesture-event-x-set!
  dollar-gesture-event-y            dollar-gesture-event-y-set!

  ;; sdl2-internals/accessors/events/drop-event.scm
  drop-event?
  drop-event-file  drop-event-file-set!

  ;; sdl2-internals/accessors/events/joy-axis-event.scm
  joy-axis-event?
  joy-axis-event-which  joy-axis-event-which-set!
  joy-axis-event-axis   joy-axis-event-axis-set!
  joy-axis-event-value  joy-axis-event-value-set!

  ;; sdl2-internals/accessors/events/joy-ball-event.scm
  joy-ball-event?
  joy-ball-event-which  joy-ball-event-which-set!
  joy-ball-event-ball   joy-ball-event-ball-set!
  joy-ball-event-xrel   joy-ball-event-xrel-set!
  joy-ball-event-yrel   joy-ball-event-yrel-set!

  ;; sdl2-internals/accessors/events/joy-button-event.scm
  joy-button-event?
  joy-button-event-which   joy-button-event-which-set!
  joy-button-event-button  joy-button-event-button-set!
  joy-button-event-state   joy-button-event-state-set!

  ;; sdl2-internals/accessors/events/joy-device-event.scm
  joy-device-event?
  joy-device-event-which  joy-device-event-which-set!

  ;; sdl2-internals/accessors/events/joy-hat-event.scm
  joy-hat-event?
  joy-hat-event-which      joy-hat-event-which-set!
  joy-hat-event-hat        joy-hat-event-hat-set!
  joy-hat-event-value-raw  joy-hat-event-value-raw-set!
  joy-hat-event-value      joy-hat-event-value-set!

  ;; sdl2-internals/accessors/events/keyboard-event.scm
  keyboard-event?
  keyboard-event-window-id     keyboard-event-window-id-set!
  keyboard-event-state         keyboard-event-state-set!
  keyboard-event-repeat        keyboard-event-repeat-set!
  keyboard-event-keysym        keyboard-event-keysym-set!
  keyboard-event-sym-raw       keyboard-event-sym-raw-set!
  keyboard-event-sym           keyboard-event-sym-set!
  keyboard-event-scancode-raw  keyboard-event-scancode-raw-set!
  keyboard-event-scancode      keyboard-event-scancode-set!
  keyboard-event-mod-raw       keyboard-event-mod-raw-set!
  keyboard-event-mod           keyboard-event-mod-set!

  ;; sdl2-internals/accessors/events/mouse-button-event.scm
  mouse-button-event?
  mouse-button-event-window-id   mouse-button-event-window-id-set!
  mouse-button-event-which       mouse-button-event-which-set!
  mouse-button-event-button-raw  mouse-button-event-button-raw-set!
  mouse-button-event-button      mouse-button-event-button-set!
  mouse-button-event-state       mouse-button-event-state-set!
  mouse-button-event-x           mouse-button-event-x-set!
  mouse-button-event-y           mouse-button-event-y-set!

  ;; sdl2-internals/accessors/events/mouse-motion-event.scm
  mouse-motion-event?
  mouse-motion-event-window-id  mouse-motion-event-window-id-set!
  mouse-motion-event-which      mouse-motion-event-which-set!
  mouse-motion-event-state-raw  mouse-motion-event-state-raw-set!
  mouse-motion-event-state      mouse-motion-event-state-set!
  mouse-motion-event-x          mouse-motion-event-x-set!
  mouse-motion-event-y          mouse-motion-event-y-set!
  mouse-motion-event-xrel       mouse-motion-event-xrel-set!
  mouse-motion-event-yrel       mouse-motion-event-yrel-set!

  ;; sdl2-internals/accessors/events/mouse-wheel-event.scm
  mouse-wheel-event?
  mouse-wheel-event-window-id  mouse-wheel-event-window-id-set!
  mouse-wheel-event-which      mouse-wheel-event-which-set!
  mouse-wheel-event-x          mouse-wheel-event-x-set!
  mouse-wheel-event-y          mouse-wheel-event-y-set!
  mouse-wheel-event-direction  mouse-wheel-event-direction-set!

  ;; sdl2-internals/accessors/events/multi-gesture-event.scm
  multi-gesture-event?
  multi-gesture-event-touch-id     multi-gesture-event-touch-id-set!
  multi-gesture-event-dtheta       multi-gesture-event-dtheta-set!
  multi-gesture-event-ddist        multi-gesture-event-ddist-set!
  multi-gesture-event-x            multi-gesture-event-x-set!
  multi-gesture-event-y            multi-gesture-event-y-set!
  multi-gesture-event-num-fingers  multi-gesture-event-num-fingers-set!

  ;; sdl2-internals/accessors/events/quit-event.scm
  quit-event?

  ;; sdl2-internals/accessors/events/sys-wm-event.scm
  sys-wm-event?
  sys-wm-event-msg-raw  sys-wm-event-msg-raw-set!

  ;; sdl2-internals/accessors/events/text-editing-event.scm
  text-editing-event?
  text-editing-event-window-id  text-editing-event-window-id-set!
  text-editing-event-text       text-editing-event-text-set!
  text-editing-event-start      text-editing-event-start-set!
  text-editing-event-length     text-editing-event-length-set!

  ;; sdl2-internals/accessors/events/text-input-event.scm
  text-input-event?
  text-input-event-window-id  text-input-event-window-id-set!
  text-input-event-text       text-input-event-text-set!

  ;; sdl2-internals/accessors/events/touch-finger-event.scm
  touch-finger-event?
  touch-finger-event-touch-id   touch-finger-event-touch-id-set!
  touch-finger-event-finger-id  touch-finger-event-finger-id-set!
  touch-finger-event-x          touch-finger-event-x-set!
  touch-finger-event-y          touch-finger-event-y-set!
  touch-finger-event-dx         touch-finger-event-dx-set!
  touch-finger-event-dy         touch-finger-event-dy-set!
  touch-finger-event-pressure   touch-finger-event-pressure-set!

  ;; sdl2-internals/accessors/events/user-event.scm
  user-event?
  user-event-window-id  user-event-window-id-set!
  user-event-code       user-event-code-set!
  user-event-data1-raw  user-event-data1-raw-set!
  user-event-data2-raw  user-event-data2-raw-set!

  ;; sdl2-internals/accessors/events/window-event.scm
  window-event?
  window-event-window-id  window-event-window-id-set!
  window-event-event-raw  window-event-event-raw-set!
  window-event-event      window-event-event-set!
  window-event-data1      window-event-data1-set!
  window-event-data2      window-event-data2-set!


  ;;:;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; FINGER

  ;; sdl2-internals/record-types/finger.scm
  finger?

  ;; sdl2-internals/accessors/finger.scm
  finger-id
  finger-x
  finger-y
  finger-pressure


  ;;:;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; GL-CONTEXT

  ;; sdl2-internals/record-types/gl-context.scm
  gl-context?


  ;;:;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; JOYSTICK

  ;; sdl2-internals/record-types/joystick.scm
  joystick?


  ;;:;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; JOYSTICK-GUID

  ;; sdl2-internals/record-types/joystick-guid.scm
  joystick-guid?
  free-joystick-guid!


  ;;:;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; KEYSYM

  ;; sdl2-internals/record-types/keysym.scm
  keysym?
  free-keysym!

  ;; sdl2-internals/accessors/keysym.scm
  keysym-scancode-raw  keysym-scancode-raw-set!
  keysym-sym-raw       keysym-sym-raw-set!
  keysym-mod-raw       keysym-mod-raw-set!
  keysym-scancode      keysym-scancode-set!
  keysym-sym           keysym-sym-set!
  keysym-mod           keysym-mod-set!
  make-keysym
  make-keysym*


  ;;:;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; PALETTE

  ;; sdl2-internals/record-types/palette.scm
  palette?
  free-palette!

  ;; sdl2-internals/accessors/palette.scm
  palette-ncolors
  palette-ncolours
  make-palette
  make-palette*


  ;;:;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; PIXEL-FORMAT

  ;; sdl2-internals/record-types/pixel-format.scm
  pixel-format?
  free-pixel-format!

  ;; sdl2-internals/accessors/pixel-format.scm
  pixel-format-format-raw
  pixel-format-format
  pixel-format-palette  pixel-format-palette-set!
  pixel-format-bits-per-pixel
  pixel-format-bytes-per-pixel
  pixel-format-rmask
  pixel-format-gmask
  pixel-format-bmask
  pixel-format-amask
  make-pixel-format
  make-pixel-format*


  ;;:;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; POINT

  ;; sdl2-internals/record-types/point.scm
  point?
  free-point!

  ;; sdl2-internals/accessors/point.scm
  point-x  point-x-set!
  point-y  point-y-set!

  ;; sdl2-internals/extras/point.scm
  make-point        make-point*
  point-set!
  point->list       point->values
  point=?
  point-copy!       point-copy
  copy-point        copy-point*

  point-scale!      point-scale
  point-unscale!    point-unscale
  point-move!       point-move
  point-add!        point-add
  point-sub!        point-sub
  point-lerp!       point-lerp


  ;;:;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; RECT

  ;; sdl2-internals/record-types/rect.scm
  rect?
  free-rect!

  ;; sdl2-internals/accessors/rect.scm
  rect-x  rect-x-set!
  rect-y  rect-y-set!
  rect-w  rect-w-set!
  rect-h  rect-h-set!

  ;; sdl2-internals/extras/rect.scm
  make-rect         make-rect*
  rect-set!
  rect->list        rect->values
  rect-copy!        rect-copy
  copy-rect         copy-rect*

  rect-scale!            rect-scale
  rect-unscale!          rect-unscale
  rect-move!             rect-move
  rect-add-point!        rect-add-point
  rect-sub-point!        rect-sub-point
  rect-grow!             rect-grow
  rect-grow/center!      rect-grow/center
  rect-lerp!             rect-lerp
  rect-lerp-xy!          rect-lerp-xy


  ;;:;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; RENDERER

  ;; sdl2-internals/record-types/renderer.scm
  renderer?


  ;;:;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; RENDERER-INFO

  ;; sdl2-internals/record-types/renderer-info.scm
  renderer-info?
  free-renderer-info!

  ;; sdl2-internals/accessors/renderer-info.scm
  renderer-info-name
  renderer-info-flags
  renderer-info-flags-raw
  renderer-info-num-texture-formats
  renderer-info-texture-formats
  renderer-info-max-texture-width
  renderer-info-max-texture-height


  ;;:;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; RWOPS

  ;; sdl2-internals/record-types/rwops.scm
  rwops?

  ;; sdl2-internals/accessors/rwops.scm
  rwops-type
  rwops-type-raw


  ;;:;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; SURFACE

  ;; sdl2-internals/record-types/surface.scm
  surface?

  ;; sdl2-internals/accessors/surface.scm
  surface-format
  surface-w
  surface-h
  surface-pitch
  surface-pixels-raw
  surface-userdata-raw  surface-userdata-raw-set!
  surface-refcount      surface-refcount-set!


  ;;:;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; TEXTURE

  ;; sdl2-internals/record-types/texture.scm
  texture?


  ;;:;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; WINDOW

  ;; sdl2-internals/record-types/window.scm
  window?

  ))
