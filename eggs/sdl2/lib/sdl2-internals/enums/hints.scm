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


(export symbol->hint
        hints-alist)


(define-foreign-constants SDL_HintPriority
  SDL_HINT_DEFAULT
  SDL_HINT_NORMAL
  SDL_HINT_OVERRIDE)


;;; Hints are defined differently than most other enums in this
;;; library. Instead of using the value from SDL's headers at compile
;;; time, the values are hardcoded into this library. The reasons for
;;; doing it this way are:
;;;
;;; - So that developers do not need to check the compiled version of
;;;   SDL before setting a hint. Developers can safely set any hint
;;;   with any version of SDL (unrecognized hints are ignored).
;;;
;;; - Because the constant values are explicitly defined strings, not
;;;   enums which have implicit values determined at compile time.
;;;
;;; - Because new hints are often added in new SDL versions, and the
;;;   implementation would be much more complex if we needed to check
;;;   the SDL version for every constant.

(define (symbol->hint symbol #!optional not-found-callback)
  (or (alist-ref symbol hints-alist)
      (if not-found-callback
          (not-found-callback symbol)
          (error "Unrecognized hint name" symbol))))

(define hints-alist
  '(
    ;; Effective in SDL 2.0.0 and later
    (framebuffer-acceleration                     . "SDL_FRAMEBUFFER_ACCELERATION")
    (gamecontrollerconfig                         . "SDL_GAMECONTROLLERCONFIG")
    (grab-keyboard                                . "SDL_GRAB_KEYBOARD")
    (idle-timer-disabled                          . "SDL_IOS_IDLE_TIMER_DISABLED") ; irregular
    (joystick-allow-background-events             . "SDL_JOYSTICK_ALLOW_BACKGROUND_EVENTS")
    (orientations                                 . "SDL_IOS_ORIENTATIONS") ; irregular
    (render-driver                                . "SDL_RENDER_DRIVER")
    (render-opengl-shaders                        . "SDL_RENDER_OPENGL_SHADERS")
    (render-scale-quality                         . "SDL_RENDER_SCALE_QUALITY")
    (render-vsync                                 . "SDL_RENDER_VSYNC")
    (timer-resolution                             . "SDL_TIMER_RESOLUTION")
    (video-minimize-on-focus-loss                 . "SDL_VIDEO_MINIMIZE_ON_FOCUS_LOSS")
    (video-x11-xinerama                           . "SDL_VIDEO_X11_XINERAMA")
    (video-x11-xrandr                             . "SDL_VIDEO_X11_XRANDR")
    (video-x11-xvidmode                           . "SDL_VIDEO_X11_XVIDMODE")
    (xinput-enabled                               . "SDL_XINPUT_ENABLED")

    ;; Effective in SDL 2.0.1 and later
    (render-direct3d-threadsafe                   . "SDL_RENDER_DIRECT3D_THREADSAFE")
    (video-highdpi-disabled                       . "SDL_VIDEO_HIGHDPI_DISABLED")

    ;; Effective in SDL 2.0.2 and later
    (accelerometer-as-joystick                    . "SDL_ACCELEROMETER_AS_JOYSTICK")
    (mac-ctrl-click-emulate-right-click           . "SDL_MAC_CTRL_CLICK_EMULATE_RIGHT_CLICK")
    (mouse-relative-mode-warp                     . "SDL_MOUSE_RELATIVE_MODE_WARP")
    (video-allow-screensaver                      . "SDL_VIDEO_ALLOW_SCREENSAVER")
    (video-mac-fullscreen-spaces                  . "SDL_VIDEO_MAC_FULLSCREEN_SPACES")
    (video-window-share-pixel-format              . "SDL_VIDEO_WINDOW_SHARE_PIXEL_FORMAT")
    (video-win-d3dcompiler                        . "SDL_VIDEO_WIN_D3DCOMPILER")

    ;; Effective in SDL 2.0.3 and later
    (render-direct3d11-debug                      . "SDL_RENDER_DIRECT3D11_DEBUG")
    (winrt-handle-back-button                     . "SDL_WINRT_HANDLE_BACK_BUTTON")
    (winrt-privacy-policy-label                   . "SDL_WINRT_PRIVACY_POLICY_LABEL")
    (winrt-privacy-policy-url                     . "SDL_WINRT_PRIVACY_POLICY_URL")

    ;; Effective in SDL 2.0.4 and later
    (android-apk-expansion-main-file-version      . "SDL_ANDROID_APK_EXPANSION_MAIN_FILE_VERSION")
    (android-apk-expansion-patch-file-version     . "SDL_ANDROID_APK_EXPANSION_PATCH_FILE_VERSION")
    (android-separate-mouse-and-touch             . "SDL_ANDROID_SEPARATE_MOUSE_AND_TOUCH")
    (emscripten-keyboard-element                  . "SDL_EMSCRIPTEN_KEYBOARD_ELEMENT")
    (ime-internal-editing                         . "SDL_IME_INTERNAL_EDITING")
    (mac-background-app                           . "SDL_MAC_BACKGROUND_APP")
    (no-signal-handlers                           . "SDL_NO_SIGNAL_HANDLERS")
    (thread-stack-size                            . "SDL_THREAD_STACK_SIZE")
    (video-x11-net-wm-ping                        . "SDL_VIDEO_X11_NET_WM_PING")
    (windows-enable-messageloop                   . "SDL_WINDOWS_ENABLE_MESSAGELOOP")
    (windows-no-close-on-alt-f4                   . "SDL_WINDOWS_NO_CLOSE_ON_ALT_F4")
    (window-frame-usable-while-cursor-hidden      . "SDL_WINDOW_FRAME_USABLE_WHILE_CURSOR_HIDDEN")
    (xinput-use-old-joystick-mapping              . "SDL_XINPUT_USE_OLD_JOYSTICK_MAPPING")
    ))

