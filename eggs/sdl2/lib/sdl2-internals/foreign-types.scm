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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GENERIC

(define-foreign-type float*  (c-pointer float))

(define-foreign-type Sint8   byte       (Sint8-guard  "Sint8 value"))
(define-foreign-type Sint16  short      (Sint16-guard "Sint16 value"))
(define-foreign-type Sint32  integer32  (Sint32-guard "Sint32 value"))
(define-foreign-type Sint64  integer64  (Sint64-guard "Sint64 value"))

(define-foreign-type Sint8*  (c-pointer byte))
(define-foreign-type Sint16* (c-pointer short))
(define-foreign-type Sint32* (c-pointer integer32))
(define-foreign-type Sint64* (c-pointer integer64))

(define-foreign-type Uint8   unsigned-byte       (Uint8-guard  "Uint8 value"))
(define-foreign-type Uint16  unsigned-short      (Uint16-guard "Uint16 value"))
(define-foreign-type Uint32  unsigned-integer32  (Uint32-guard "Uint32 value"))
(define-foreign-type Uint64  unsigned-integer64  (Uint64-guard "Uint64 value"))

(define-foreign-type Uint8*  (c-pointer unsigned-byte))
(define-foreign-type Uint16* (c-pointer unsigned-short))
(define-foreign-type Uint32* (c-pointer unsigned-integer32))
(define-foreign-type Uint64* (c-pointer unsigned-integer64))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ENUMS

(define-foreign-type SDL_AudioDeviceID Uint32)
(define-foreign-type SDL_AudioFormat Uint16)
(define-foreign-type SDL_AudioFormatEnum Sint32)
(define-foreign-type SDL_AudioStatus Sint32)

(define-foreign-type SDL_BlendMode Sint32)
(define-foreign-type SDL_BlendMode* (c-pointer "SDL_BlendMode"))
(define-foreign-type SDL_EventType Sint32)
(define-foreign-type SDL_FingerID Sint64)
(define-foreign-type SDL_GLattr Sint32)
(define-foreign-type SDL_GLcontextFlag Sint32)
(define-foreign-type SDL_GLcontextReleaseFlag Sint32)
(define-foreign-type SDL_GLprofile Sint32)
(define-foreign-type SDL_GestureID Sint64)
(define-foreign-type SDL_HintPriority Sint32)
(define-foreign-type SDL_JoystickHatPosition Sint32)
(define-foreign-type SDL_JoystickID Sint32)
(define-foreign-type SDL_JoystickPowerLevel Sint32)
(define-foreign-type SDL_Keycode Sint32)
(define-foreign-type SDL_Keymod Sint32)
(define-foreign-type SDL_MouseButton Sint32)
(define-foreign-type SDL_MouseButtonMask Sint32)
(define-foreign-type SDL_MouseWheelDirection Sint32)
(define-foreign-type SDL_RendererFlags Sint32)
(define-foreign-type SDL_RendererFlip Sint32)
(define-foreign-type SDL_RWopsWhenceEnum Sint32)
(define-foreign-type SDL_Scancode Sint32)
(define-foreign-type SDL_SystemCursor Sint32)
(define-foreign-type SDL_TouchID Sint64)
(define-foreign-type SDL_TextureAccess Sint32)
(define-foreign-type SDL_TextureModulate Sint32)
(define-foreign-type SDL_WindowEventID Sint32)
(define-foreign-type SDL_WindowFlags Sint32)
(define-foreign-type SDL_eventaction Sint32)

(define-foreign-type SDL_PixelFormatEnum Uint32)
(define-foreign-type SDL_PixelTypeEnum Sint32)
(define-foreign-type SDL_BitmapOrderEnum Sint32)
(define-foreign-type SDL_PackedOrderEnum Sint32)
(define-foreign-type SDL_ArrayOrderEnum Sint32)
(define-foreign-type SDL_PackedLayoutEnum Sint32)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; STRUCTS


(define-foreign-type SDL_AudioCVT*
  (nonnull-c-pointer "SDL_AudioCVT")
  unwrap-audio-cvt
  wrap-audio-cvt)

(define-foreign-type SDL_AudioSpec*
  (nonnull-c-pointer "SDL_AudioSpec")
  unwrap-audio-spec
  wrap-audio-spec)

(define-foreign-type SDL_AudioCallback
  (function void (c-pointer Uint8* Sint32)))

(define-foreign-type SDL_AudioFilter
  (function void (SDL_AudioCVT* SDL_AudioFormat)))


(define-foreign-type SDL_Color*
  (nonnull-c-pointer "SDL_Color")
  unwrap-color
  wrap-color)

(define-foreign-type SDL_Color*-or-null
  (c-pointer "SDL_Color")
  unwrap-color
  wrap-color)


(define-foreign-type SDL_Cursor*
  (nonnull-c-pointer "SDL_Cursor")
  unwrap-cursor
  wrap-cursor)

(define-foreign-type SDL_DisplayMode*
  (nonnull-c-pointer "SDL_DisplayMode")
  unwrap-display-mode
  wrap-display-mode)

(define-foreign-type SDL_Finger*
  (nonnull-c-pointer "SDL_Finger")
  unwrap-finger
  wrap-finger)


;;; NOTE: "SDL_GLContext" is a typedef alias of "void *" so properly
;;; this should be SDL_GLContext (no *), but I'm adding the * for
;;; semantic consistency. It is a pointer, after all.
(define-foreign-type SDL_GLContext*
  (c-pointer void)
  unwrap-gl-context
  wrap-gl-context)


(define-foreign-type SDL_Joystick*
  (nonnull-c-pointer "SDL_Joystick")
  unwrap-joystick
  wrap-joystick)

(define-foreign-type SDL_JoystickGUID*
  (nonnull-c-pointer "SDL_JoystickGUID")
  unwrap-joystick-guid
  wrap-joystick-guid)


(define-foreign-type SDL_Palette*
  (nonnull-c-pointer "SDL_Palette")
  unwrap-palette
  wrap-palette)

(define-foreign-type SDL_PixelFormat*
  (nonnull-c-pointer "SDL_PixelFormat")
  unwrap-pixel-format
  wrap-pixel-format)


(define-foreign-type SDL_Point*
  (nonnull-c-pointer "SDL_Point")
  unwrap-point
  wrap-point)

(define-foreign-type SDL_Point*-or-null
  (c-pointer "SDL_Point")
  unwrap-point
  wrap-point)


(define-foreign-type SDL_Rect*
  (nonnull-c-pointer "SDL_Rect")
  unwrap-rect
  wrap-rect)

(define-foreign-type SDL_Rect*-or-null
  (c-pointer "SDL_Rect")
  unwrap-rect
  wrap-rect)


(define-foreign-type SDL_Renderer*
  (nonnull-c-pointer "SDL_Renderer")
  unwrap-renderer
  wrap-renderer)

(define-foreign-type SDL_RendererInfo*
  (nonnull-c-pointer "SDL_RendererInfo")
  unwrap-renderer-info
  wrap-renderer-info)


(define-foreign-type SDL_RWops*
  (nonnull-c-pointer "SDL_RWops")
  unwrap-rwops
  wrap-rwops)


(define-foreign-type SDL_Surface*
  (nonnull-c-pointer "SDL_Surface")
  unwrap-surface
  wrap-surface)

(define-foreign-type SDL_Surface*-or-null
  (c-pointer "SDL_Surface")
  unwrap-surface
  wrap-surface)


(define-foreign-type SDL_Texture*
  (nonnull-c-pointer "SDL_Texture")
  unwrap-texture
  wrap-texture)

(define-foreign-type SDL_Texture*-or-null
  (c-pointer "SDL_Texture")
  unwrap-texture
  wrap-texture)


(define-foreign-type SDL_Window*
  (nonnull-c-pointer "SDL_Window")
  unwrap-window
  wrap-window)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EVENTS

(define-foreign-type SDL_Event*
  (c-pointer "SDL_Event")
  unwrap-event
  wrap-event)

(define-foreign-type SDL_EventFilter
  (function Sint32 (c-pointer SDL_Event*)))

(define-foreign-type SDL_EventFilter*
  (c-pointer (function Sint32 (c-pointer SDL_Event*))))

(define-foreign-type SDL_Keysym*
  (nonnull-c-pointer "SDL_Keysym")
  unwrap-keysym
  wrap-keysym)
