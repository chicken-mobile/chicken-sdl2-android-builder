# chicken-sdl2 enums

This document provides tables for the enums used in chicken-sdl2. It
lists the symbols used in chicken-sdl2, along with the corresponding C
constants from SDL.

**Table of contents:**

* [Init Flags](#init-flags)
* [Audio Formats](#audio-formats)
* [Blend Modes](#blend-modes)
* [Event Types](#event-types)
* [Hints](#hints)
* [Joystick Hat Position](#joystick-hat-position)
* [Joystick Power Level](#joystick-power-level)
* [Mouse Buttons](#mouse-buttons)
* [Mouse Button Masks](#mouse-button-masks)
* [OpenGL Attributes](#opengl-attributes)
    * [OpenGL Profiles](#opengl-profiles)
    * [OpenGL Context Flags](#opengl-context-flags)
    * [OpenGL Context Release Flags](#opengl-context-release-flags)
* [Pixel Formats](#pixel-formats)
* [Renderer Flags](#renderer-flags)
* [RWops Types](#rwops-types)
* [System Cursors](#system-cursors)
* [Texture Access](#texture-access)
* [Window Flags](#window-flags)
* [Window Event Types](#window-event-types)
* [Keyboard Modifiers](#keyboard-modifiers)
* [Keyboard Keycodes](#keyboard-keycodes)
* [Keyboard Scancodes](#keyboard-scancodes)


## Init Flags

A list of one or more of these is passed to `init!`,
`init-subsystem!`, `quit-subsystem!`, or `was-init`. A list of one or
more of these is returned by `was-init`.
See the remarks for [SDL_Init](https://wiki.libsdl.org/SDL_Init).

```
timer                         SDL_INIT_TIMER
audio                         SDL_INIT_AUDIO
video                         SDL_INIT_VIDEO
joystick                      SDL_INIT_JOYSTICK
haptic                        SDL_INIT_HAPTIC
game-controller               SDL_INIT_GAMECONTROLLER
events                        SDL_INIT_EVENTS
everything                    SDL_INIT_EVERYTHING
```


## Audio Formats

Returned by `audio-spec-format`, `audio-cvt-src-format`, and
`audio-cvt-dst-format`.
See [SDL_AudioFormat](https://wiki.libsdl.org/SDL_AudioFormat).

```
u8                            AUDIO_U8
s8                            AUDIO_S8
u16lsb                        AUDIO_U16LSB
s16lsb                        AUDIO_S16LSB
u16msb                        AUDIO_U16MSB
s16msb                        AUDIO_S16MSB
u16                           AUDIO_U16
s16                           AUDIO_S16
s32lsb                        AUDIO_S32LSB
s32msb                        AUDIO_S32MSB
s32                           AUDIO_S32
f32lsb                        AUDIO_F32LSB
f32msb                        AUDIO_F32MSB
f32                           AUDIO_F32
u16sys                        AUDIO_U16SYS
s16sys                        AUDIO_S16SYS
s32sys                        AUDIO_S32SYS
f32sys                        AUDIO_F32SYS
```


## Blend Modes

Used with `render-draw-blend-mode`, `surface-blend-mode`, and `texture-blend-mode`.
See [SDL_BlendMode](https://wiki.libsdl.org/SDL_BlendMode).

```
none                          SDL_BLENDMODE_NONE
blend                         SDL_BLENDMODE_BLEND
add                           SDL_BLENDMODE_ADD
mod                           SDL_BLENDMODE_MOD
```


## Event Types

Returned by `event-type`, or passed to `flush-events!`, `peek-events`,
and others. You may register up to 32767 additional event type symbols
using `register-events!`.
See [SDL_EventType](https://wiki.libsdl.org/SDL_EventType).


```
first                         SDL_FIRSTEVENT
quit                          SDL_QUIT
app-terminating               SDL_APP_TERMINATING
app-low-memory                SDL_APP_LOWMEMORY
app-will-enter-background     SDL_APP_WILLENTERBACKGROUND
app-did-enter-background      SDL_APP_DIDENTERBACKGROUND
app-will-enter-foreground     SDL_APP_WILLENTERFOREGROUND
app-did-enter-foreground      SDL_APP_DIDENTERFOREGROUND
window                        SDL_WINDOWEVENT
sys-wm                        SDL_SYSWMEVENT
key-down                      SDL_KEYDOWN
key-up                        SDL_KEYUP
text-editing                  SDL_TEXTEDITING
text-input                    SDL_TEXTINPUT
mouse-motion                  SDL_MOUSEMOTION
mouse-button-down             SDL_MOUSEBUTTONDOWN
mouse-button-up               SDL_MOUSEBUTTONUP
mouse-wheel                   SDL_MOUSEWHEEL
joy-axis-motion               SDL_JOYAXISMOTION
joy-ball-motion               SDL_JOYBALLMOTION
joy-hat-motion                SDL_JOYHATMOTION
joy-button-down               SDL_JOYBUTTONDOWN
joy-button-up                 SDL_JOYBUTTONUP
joy-device-added              SDL_JOYDEVICEADDED
joy-device-removed            SDL_JOYDEVICEREMOVED
controller-axis-motion        SDL_CONTROLLERAXISMOTION
controller-button-down        SDL_CONTROLLERBUTTONDOWN
controller-button-up          SDL_CONTROLLERBUTTONUP
controller-device-added       SDL_CONTROLLERDEVICEADDED
controller-device-removed     SDL_CONTROLLERDEVICEREMOVED
controller-device-remapped    SDL_CONTROLLERDEVICEREMAPPED
finger-down                   SDL_FINGERDOWN
finger-up                     SDL_FINGERUP
finger-motion                 SDL_FINGERMOTION
dollar-gesture                SDL_DOLLARGESTURE
dollar-record                 SDL_DOLLARRECORD
multi-gesture                 SDL_MULTIGESTURE
clipboard-update              SDL_CLIPBOARDUPDATE
drop-file                     SDL_DROPFILE
last                          SDL_LASTEVENT
```


## Hints

Passed as the hint name to `get-hint` and `set-hint!`.
Some hints only have an effect when using certain versions of SDL.
See [Hints (aka Configuration Variables)](https://wiki.libsdl.org/CategoryHints).

Effective in **SDL 2.0.0** and later:

```
framebuffer-acceleration                          SDL_HINT_FRAMEBUFFER_ACCELERATION
gamecontrollerconfig                              SDL_HINT_GAMECONTROLLERCONFIG
grab-keyboard                                     SDL_HINT_GRAB_KEYBOARD
idle-timer-disabled                               SDL_HINT_IDLE_TIMER_DISABLED
joystick-allow-background-events                  SDL_HINT_JOYSTICK_ALLOW_BACKGROUND_EVENTS
orientations                                      SDL_HINT_ORIENTATIONS
render-driver                                     SDL_HINT_RENDER_DRIVER
render-opengl-shaders                             SDL_HINT_RENDER_OPENGL_SHADERS
render-scale-quality                              SDL_HINT_RENDER_SCALE_QUALITY
render-vsync                                      SDL_HINT_RENDER_VSYNC
timer-resolution                                  SDL_HINT_TIMER_RESOLUTION
video-minimize-on-focus-loss                      SDL_HINT_VIDEO_MINIMIZE_ON_FOCUS_LOSS
video-x11-xinerama                                SDL_HINT_VIDEO_X11_XINERAMA
video-x11-xrandr                                  SDL_HINT_VIDEO_X11_XRANDR
video-x11-xvidmode                                SDL_HINT_VIDEO_X11_XVIDMODE
xinput-enabled                                    SDL_HINT_XINPUT_ENABLED
```

Effective in **SDL 2.0.1** and later:

```
render-direct3d-threadsafe                        SDL_HINT_RENDER_DIRECT3D_THREADSAFE
video-highdpi-disabled                            SDL_HINT_VIDEO_HIGHDPI_DISABLED
```

Effective in **SDL 2.0.2** and later:

```
accelerometer-as-joystick                         SDL_HINT_ACCELEROMETER_AS_JOYSTICK
mac-ctrl-click-emulate-right-click                SDL_HINT_MAC_CTRL_CLICK_EMULATE_RIGHT_CLICK
mouse-relative-mode-warp                          SDL_HINT_MOUSE_RELATIVE_MODE_WARP
video-allow-screensaver                           SDL_HINT_VIDEO_ALLOW_SCREENSAVER
video-mac-fullscreen-spaces                       SDL_HINT_VIDEO_MAC_FULLSCREEN_SPACES
video-window-share-pixel-format                   SDL_HINT_VIDEO_WINDOW_SHARE_PIXEL_FORMAT
video-win-d3dcompiler                             SDL_HINT_VIDEO_WIN_D3DCOMPILER
```

Effective in **SDL 2.0.3** and later:

```
render-direct3d11-debug                           SDL_HINT_RENDER_DIRECT3D11_DEBUG
winrt-handle-back-button                          SDL_HINT_WINRT_HANDLE_BACK_BUTTON
winrt-privacy-policy-label                        SDL_HINT_WINRT_PRIVACY_POLICY_LABEL
winrt-privacy-policy-url                          SDL_HINT_WINRT_PRIVACY_POLICY_URL
```

Effective in **SDL 2.0.4** and later:

```
android-apk-expansion-main-file-version           SDL_HINT_ANDROID_APK_EXPANSION_MAIN_FILE_VERSION
android-apk-expansion-patch-file-version          SDL_HINT_ANDROID_APK_EXPANSION_PATCH_FILE_VERSION
android-separate-mouse-and-touch                  SDL_HINT_ANDROID_SEPARATE_MOUSE_AND_TOUCH
emscripten-keyboard-element                       SDL_HINT_EMSCRIPTEN_KEYBOARD_ELEMENT
ime-internal-editing                              SDL_HINT_IME_INTERNAL_EDITING
mac-background-app                                SDL_HINT_MAC_BACKGROUND_APP
no-signal-handlers                                SDL_HINT_NO_SIGNAL_HANDLERS
thread-stack-size                                 SDL_HINT_THREAD_STACK_SIZE
video-x11-net-wm-ping                             SDL_HINT_VIDEO_X11_NET_WM_PING
windows-enable-messageloop                        SDL_HINT_WINDOWS_ENABLE_MESSAGELOOP
windows-no-close-on-alt-f4                        SDL_HINT_WINDOWS_NO_CLOSE_ON_ALT_F4
window-frame-usable-while-cursor-hidden           SDL_HINT_WINDOW_FRAME_USABLE_WHILE_CURSOR_HIDDEN
xinput-use-old-joystick-mapping                   SDL_HINT_XINPUT_USE_OLD_JOYSTICK_MAPPING
```


## Joystick Hat Position

Returned by `joystick-get-hat` and `joy-hat-event-value`. See the
remarks for [SDL_JoyHatEvent](https://wiki.libsdl.org/SDL_JoyHatEvent).

```
centered                      SDL_HAT_CENTERED
up                            SDL_HAT_UP
right                         SDL_HAT_RIGHT
down                          SDL_HAT_DOWN
left                          SDL_HAT_LEFT
right-up                      SDL_HAT_RIGHTUP
right-down                    SDL_HAT_RIGHTDOWN
left-up                       SDL_HAT_LEFTUP
left-down                     SDL_HAT_LEFTDOWN
```


## Joystick Power Level

Returned by `joystick-current-power-level` (SDL 2.0.4 and higher).
See [SDL_JoystickPowerLevel](https://wiki.libsdl.org/SDL_JoystickPowerLevel).

```
unknown                       SDL_JOYSTICK_POWER_UNKNOWN
empty                         SDL_JOYSTICK_POWER_EMPTY
low                           SDL_JOYSTICK_POWER_LOW
medium                        SDL_JOYSTICK_POWER_MEDIUM
full                          SDL_JOYSTICK_POWER_FULL
wired                         SDL_JOYSTICK_POWER_WIRED
max                           SDL_JOYSTICK_POWER_MAX
```


## Mouse Buttons

Returned by `mouse-button-event-button`. See the remarks for
[SDL_MouseButtonEvent](https://wiki.libsdl.org/SDL_MouseButtonEvent).

```
left                          SDL_BUTTON_LEFT
middle                        SDL_BUTTON_MIDDLE
right                         SDL_BUTTON_RIGHT
x1                            SDL_BUTTON_X1
x2                            SDL_BUTTON_X2
```


## Mouse Button Masks

A list of zero or more of these is returned by
`mouse-motion-event-state`. See the remarks for
[SDL_MouseMotionEvent](https://wiki.libsdl.org/SDL_MouseMotionEvent)

```
left                          SDL_BUTTON_LMASK
middle                        SDL_BUTTON_MMASK
right                         SDL_BUTTON_RMASK
x1                            SDL_BUTTON_X1MASK
x2                            SDL_BUTTON_X2MASK
```


## OpenGL Attributes

Passed to `gl-get-attribute` and `gl-set-attribute!`.
See [SDL_GLattr](https://wiki.libsdl.org/SDL_GLattr).

```
red-size                      SDL_GL_RED_SIZE
green-size                    SDL_GL_GREEN_SIZE
blue-size                     SDL_GL_BLUE_SIZE
alpha-size                    SDL_GL_ALPHA_SIZE
buffer-size                   SDL_GL_BUFFER_SIZE
doublebuffer                  SDL_GL_DOUBLEBUFFER
depth-size                    SDL_GL_DEPTH_SIZE
stencil-size                  SDL_GL_STENCIL_SIZE
accum-red-size                SDL_GL_ACCUM_RED_SIZE
accum-green-size              SDL_GL_ACCUM_GREEN_SIZE
accum-blue-size               SDL_GL_ACCUM_BLUE_SIZE
accum-alpha-size              SDL_GL_ACCUM_ALPHA_SIZE
stereo                        SDL_GL_STEREO
multisamplebuffers            SDL_GL_MULTISAMPLEBUFFERS
multisamplesamples            SDL_GL_MULTISAMPLESAMPLES
accelerated-visual            SDL_GL_ACCELERATED_VISUAL
context-major-version         SDL_GL_CONTEXT_MAJOR_VERSION
context-minor-version         SDL_GL_CONTEXT_MINOR_VERSION
context-flags                 SDL_GL_CONTEXT_FLAGS
context-profile-mask          SDL_GL_CONTEXT_PROFILE_MASK
share-with-current-context    SDL_GL_SHARE_WITH_CURRENT_CONTEXT
framebuffer-srgb-capable      SDL_GL_FRAMEBUFFER_SRGB_CAPABLE         (SDL 2.0.1 and higher)
context-release-behavior      SDL_GL_CONTEXT_RELEASE_BEHAVIOR         (SDL 2.0.4 and higher)

```

### OpenGL Profiles

One of these is used as the value when getting or setting the
`context-profile-mask` OpenGL attribute.
See [SDL_GLprofile](https://wiki.libsdl.org/SDL_GLprofile).

```
core                          SDL_GL_CONTEXT_PROFILE_CORE
compatibility                 SDL_GL_CONTEXT_PROFILE_COMPATIBILITY
es                            SDL_GL_CONTEXT_PROFILE_ES
```

### OpenGL Context Flags

A list of zero or more of these is used when getting or setting the
`context-flags` OpenGL attribute.
See [SDL_GLcontextFlag](https://wiki.libsdl.org/SDL_GLcontextFlag).

```
debug                         SDL_GL_CONTEXT_DEBUG_FLAG
forward-compatible            SDL_GL_CONTEXT_FORWARD_COMPATIBLE_FLAG
robust-access                 SDL_GL_CONTEXT_ROBUST_ACCESS_FLAG
reset-isolation               SDL_GL_CONTEXT_RESET_ISOLATION_FLAG
```

### OpenGL Context Release Flags

One of these is used as the value when getting or setting the
`context-release-behavior` OpenGL attribute (SDL 2.0.4 and higher).

```
none                          SDL_GL_CONTEXT_RELEASE_BEHAVIOR_NONE
flush                         SDL_GL_CONTEXT_RELEASE_BEHAVIOR_FLUSH
```


## Pixel Formats

Returned by `pixel-format-format` and `display-mode-format`. See
[SDL_PixelFormatEnum](https://wiki.libsdl.org/SDL_PixelFormatEnum).

```
unknown                       SDL_PIXELFORMAT_UNKNOWN
index1lsb                     SDL_PIXELFORMAT_INDEX1LSB
index1msb                     SDL_PIXELFORMAT_INDEX1MSB
index4lsb                     SDL_PIXELFORMAT_INDEX4LSB
index4msb                     SDL_PIXELFORMAT_INDEX4MSB
index8                        SDL_PIXELFORMAT_INDEX8
rgb332                        SDL_PIXELFORMAT_RGB332
rgb444                        SDL_PIXELFORMAT_RGB444
rgb555                        SDL_PIXELFORMAT_RGB555
bgr555                        SDL_PIXELFORMAT_BGR555
argb4444                      SDL_PIXELFORMAT_ARGB4444
rgba4444                      SDL_PIXELFORMAT_RGBA4444
abgr4444                      SDL_PIXELFORMAT_ABGR4444
bgra4444                      SDL_PIXELFORMAT_BGRA4444
argb1555                      SDL_PIXELFORMAT_ARGB1555
rgba5551                      SDL_PIXELFORMAT_RGBA5551
abgr1555                      SDL_PIXELFORMAT_ABGR1555
bgra5551                      SDL_PIXELFORMAT_BGRA5551
rgb565                        SDL_PIXELFORMAT_RGB565
bgr565                        SDL_PIXELFORMAT_BGR565
rgb24                         SDL_PIXELFORMAT_RGB24
bgr24                         SDL_PIXELFORMAT_BGR24
rgb888                        SDL_PIXELFORMAT_RGB888
rgbx8888                      SDL_PIXELFORMAT_RGBX8888
bgr888                        SDL_PIXELFORMAT_BGR888
bgrx8888                      SDL_PIXELFORMAT_BGRX8888
argb8888                      SDL_PIXELFORMAT_ARGB8888
rgba8888                      SDL_PIXELFORMAT_RGBA8888
abgr8888                      SDL_PIXELFORMAT_ABGR8888
bgra8888                      SDL_PIXELFORMAT_BGRA8888
argb2101010                   SDL_PIXELFORMAT_ARGB2101010
yv12                          SDL_PIXELFORMAT_YV12
iyuv                          SDL_PIXELFORMAT_IYUV
yuy2                          SDL_PIXELFORMAT_YUY2
uyvy                          SDL_PIXELFORMAT_UYVY
yvyu                          SDL_PIXELFORMAT_YVYU
```


## Renderer Flags

A list of zero or more of these is passed to `create-renderer!`.
See [SDL_RendererFlags](https://wiki.libsdl.org/SDL_RendererFlags).

```
software                      SDL_RENDERER_SOFTWARE
accelerated                   SDL_RENDERER_ACCELERATED
present-vsync                 SDL_RENDERER_PRESENTVSYNC
target-texture                SDL_RENDERER_TARGETTEXTURE
```


## RWops Types

Returned by `rwops-type`.
See the remarks for [SDL_RWops](https://wiki.libsdl.org/SDL_RWops).

```
unknown                       SDL_RWOPS_UNKNOWN
win-file                      SDL_RWOPS_WINFILE
std-file                      SDL_RWOPS_STDFILE
jni-file                      SDL_RWOPS_JNIFILE
memory                        SDL_RWOPS_MEMORY
memory-ro                     SDL_RWOPS_MEMORY_RO
```


## System Cursors

See [SDL_CreateSystemCursor](https://wiki.libsdl.org/SDL_CreateSystemCursor).

```
arrow                         SDL_SYSTEM_CURSOR_ARROW
ibeam                         SDL_SYSTEM_CURSOR_IBEAM
wait                          SDL_SYSTEM_CURSOR_WAIT
crosshair                     SDL_SYSTEM_CURSOR_CROSSHAIR
wait-arrow                    SDL_SYSTEM_CURSOR_WAITARROW
size-nwse                     SDL_SYSTEM_CURSOR_SIZENWSE
size-nesw                     SDL_SYSTEM_CURSOR_SIZENESW
size-we                       SDL_SYSTEM_CURSOR_SIZEWE
size-ns                       SDL_SYSTEM_CURSOR_SIZENS
size-all                      SDL_SYSTEM_CURSOR_SIZEALL
no                            SDL_SYSTEM_CURSOR_NO
hand                          SDL_SYSTEM_CURSOR_HAND
```


## Texture Access

Passed to `create-texture`, and returned by `query-texture` and `texture-access`.
See [SDL_TextureAccess](https://wiki.libsdl.org/SDL_TextureAccess).

```
static                        SDL_TEXTUREACCESS_STATIC
streaming                     SDL_TEXTUREACCESS_STREAMING
target                        SDL_TEXTUREACCESS_TARGET
```


## Window Flags

A list of zero or more of these can be passed to `create-window!`, and
is returned by `window-flags`.
See [SDL_WindowFlags](https://wiki.libsdl.org/SDL_WindowFlags).

```
fullscreen                    SDL_WINDOW_FULLSCREEN
fullscreen-desktop            SDL_WINDOW_FULLSCREEN_DESKTOP
opengl                        SDL_WINDOW_OPENGL
shown                         SDL_WINDOW_SHOWN
hidden                        SDL_WINDOW_HIDDEN
borderless                    SDL_WINDOW_BORDERLESS
resizable                     SDL_WINDOW_RESIZABLE
minimized                     SDL_WINDOW_MINIMIZED
maximized                     SDL_WINDOW_MAXIMIZED
input-grabbed                 SDL_WINDOW_INPUT_GRABBED
input-focus                   SDL_WINDOW_INPUT_FOCUS
mouse-focus                   SDL_WINDOW_MOUSE_FOCUS
foreign                       SDL_WINDOW_FOREIGN
allow-high-dpi                SDL_WINDOW_ALLOW_HIGHDPI           (SDL 2.0.1 and higher)
mouse-capture                 SDL_WINDOW_MOUSE_CAPTURE           (SDL 2.0.4 and higher)
```


## Window Event Types

Returned by `window-event-event` for events with the `window` event
type.
See [SDL_WindowEventID](https://wiki.libsdl.org/SDL_WindowEventID).

```
none                          SDL_WINDOWEVENT_NONE
shown                         SDL_WINDOWEVENT_SHOWN
hidden                        SDL_WINDOWEVENT_HIDDEN
exposed                       SDL_WINDOWEVENT_EXPOSED
moved                         SDL_WINDOWEVENT_MOVED
resized                       SDL_WINDOWEVENT_RESIZED
size-changed                  SDL_WINDOWEVENT_SIZE_CHANGED
minimized                     SDL_WINDOWEVENT_MINIMIZED
maximized                     SDL_WINDOWEVENT_MAXIMIZED
restored                      SDL_WINDOWEVENT_RESTORED
enter                         SDL_WINDOWEVENT_ENTER
leave                         SDL_WINDOWEVENT_LEAVE
focus-gained                  SDL_WINDOWEVENT_FOCUS_GAINED
focus-lost                    SDL_WINDOWEVENT_FOCUS_LOST
close                         SDL_WINDOWEVENT_CLOSE
```


## Keyboard Modifiers

A list of zero or more of these is returned by `mod-state`,
`keysym-mod`, and `keyboard-event-mod`.
See [SDL_Keymod](https://wiki.libsdl.org/SDL_Keymod)

```
none                          KMOD_NONE
lshift                        KMOD_LSHIFT
rshift                        KMOD_RSHIFT
shift                         KMOD_SHIFT
lctrl                         KMOD_LCTRL
rctrl                         KMOD_RCTRL
ctrl                          KMOD_CTRL
lalt                          KMOD_LALT
ralt                          KMOD_RALT
alt                           KMOD_ALT
lgui                          KMOD_LGUI
rgui                          KMOD_RGUI
gui                           KMOD_GUI
num                           KMOD_NUM
caps                          KMOD_CAPS
mode                          KMOD_MODE
```


## Keyboard Keycodes

Returned by `keysym-sym`, `keyboard-event-sym`, and others.
See [SDL_Keycode](https://wiki.libsdl.org/SDL_Keycode).

```
unknown                       SDLK_UNKNOWN
return                        SDLK_RETURN
escape                        SDLK_ESCAPE
backspace                     SDLK_BACKSPACE
tab                           SDLK_TAB
space                         SDLK_SPACE
exclaim                       SDLK_EXCLAIM
quote-dbl                     SDLK_QUOTEDBL
hash                          SDLK_HASH
percent                       SDLK_PERCENT
dollar                        SDLK_DOLLAR
ampersand                     SDLK_AMPERSAND
quote                         SDLK_QUOTE
left-paren                    SDLK_LEFTPAREN
right-paren                   SDLK_RIGHTPAREN
asterisk                      SDLK_ASTERISK
plus                          SDLK_PLUS
comma                         SDLK_COMMA
minus                         SDLK_MINUS
period                        SDLK_PERIOD
slash                         SDLK_SLASH
n-0                           SDLK_0
n-1                           SDLK_1
n-2                           SDLK_2
n-3                           SDLK_3
n-4                           SDLK_4
n-5                           SDLK_5
n-6                           SDLK_6
n-7                           SDLK_7
n-8                           SDLK_8
n-9                           SDLK_9
colon                         SDLK_COLON
semicolon                     SDLK_SEMICOLON
less                          SDLK_LESS
equals                        SDLK_EQUALS
greater                       SDLK_GREATER
question                      SDLK_QUESTION
at                            SDLK_AT

left-bracket                  SDLK_LEFTBRACKET
backslash                     SDLK_BACKSLASH
right-bracket                 SDLK_RIGHTBRACKET
caret                         SDLK_CARET
underscore                    SDLK_UNDERSCORE
backquote                     SDLK_BACKQUOTE

a                             SDLK_a
b                             SDLK_b
c                             SDLK_c
d                             SDLK_d
e                             SDLK_e
f                             SDLK_f
g                             SDLK_g
h                             SDLK_h
i                             SDLK_i
j                             SDLK_j
k                             SDLK_k
l                             SDLK_l
m                             SDLK_m
n                             SDLK_n
o                             SDLK_o
p                             SDLK_p
q                             SDLK_q
r                             SDLK_r
s                             SDLK_s
t                             SDLK_t
u                             SDLK_u
v                             SDLK_v
w                             SDLK_w
x                             SDLK_x
y                             SDLK_y
z                             SDLK_z

caps-lock                     SDLK_CAPSLOCK

f1                            SDLK_F1
f2                            SDLK_F2
f3                            SDLK_F3
f4                            SDLK_F4
f5                            SDLK_F5
f6                            SDLK_F6
f7                            SDLK_F7
f8                            SDLK_F8
f9                            SDLK_F9
f10                           SDLK_F10
f11                           SDLK_F11
f12                           SDLK_F12

print-screen                  SDLK_PRINTSCREEN
scroll-lock                   SDLK_SCROLLLOCK
pause                         SDLK_PAUSE
insert                        SDLK_INSERT
home                          SDLK_HOME
page-up                       SDLK_PAGEUP
delete                        SDLK_DELETE
end                           SDLK_END
page-down                     SDLK_PAGEDOWN
right                         SDLK_RIGHT
left                          SDLK_LEFT
down                          SDLK_DOWN
up                            SDLK_UP

num-lock-clear                SDLK_NUMLOCKCLEAR
kp-divide                     SDLK_KP_DIVIDE
kp-multiply                   SDLK_KP_MULTIPLY
kp-minus                      SDLK_KP_MINUS
kp-plus                       SDLK_KP_PLUS
kp-enter                      SDLK_KP_ENTER
kp-1                          SDLK_KP_1
kp-2                          SDLK_KP_2
kp-3                          SDLK_KP_3
kp-4                          SDLK_KP_4
kp-5                          SDLK_KP_5
kp-6                          SDLK_KP_6
kp-7                          SDLK_KP_7
kp-8                          SDLK_KP_8
kp-9                          SDLK_KP_9
kp-0                          SDLK_KP_0
kp-period                     SDLK_KP_PERIOD

application                   SDLK_APPLICATION
power                         SDLK_POWER
kp-equals                     SDLK_KP_EQUALS
f13                           SDLK_F13
f14                           SDLK_F14
f15                           SDLK_F15
f16                           SDLK_F16
f17                           SDLK_F17
f18                           SDLK_F18
f19                           SDLK_F19
f20                           SDLK_F20
f21                           SDLK_F21
f22                           SDLK_F22
f23                           SDLK_F23
f24                           SDLK_F24
execute                       SDLK_EXECUTE
help                          SDLK_HELP
menu                          SDLK_MENU
select                        SDLK_SELECT
stop                          SDLK_STOP
again                         SDLK_AGAIN
undo                          SDLK_UNDO
cut                           SDLK_CUT
copy                          SDLK_COPY
paste                         SDLK_PASTE
find                          SDLK_FIND
mute                          SDLK_MUTE
volume-up                     SDLK_VOLUMEUP
volume-down                   SDLK_VOLUMEDOWN
kp-comma                      SDLK_KP_COMMA
kp-equals-as400               SDLK_KP_EQUALSAS400

alt-erase                     SDLK_ALTERASE
sys-req                       SDLK_SYSREQ
cancel                        SDLK_CANCEL
clear                         SDLK_CLEAR
prior                         SDLK_PRIOR
return2                       SDLK_RETURN2
separator                     SDLK_SEPARATOR
out                           SDLK_OUT
oper                          SDLK_OPER
clear-again                   SDLK_CLEARAGAIN
crsel                         SDLK_CRSEL
exsel                         SDLK_EXSEL

kp-00                         SDLK_KP_00
kp-000                        SDLK_KP_000
thousands-separator           SDLK_THOUSANDSSEPARATOR
decimal-separator             SDLK_DECIMALSEPARATOR
currency-unit                 SDLK_CURRENCYUNIT
currency-subunit              SDLK_CURRENCYSUBUNIT
kp-left-paren                 SDLK_KP_LEFTPAREN
kp-right-paren                SDLK_KP_RIGHTPAREN
kp-left-brace                 SDLK_KP_LEFTBRACE
kp-right-brace                SDLK_KP_RIGHTBRACE
kp-tab                        SDLK_KP_TAB
kp-backspace                  SDLK_KP_BACKSPACE
kp-a                          SDLK_KP_A
kp-b                          SDLK_KP_B
kp-c                          SDLK_KP_C
kp-d                          SDLK_KP_D
kp-e                          SDLK_KP_E
kp-f                          SDLK_KP_F
kp-xor                        SDLK_KP_XOR
kp-power                      SDLK_KP_POWER
kp-percent                    SDLK_KP_PERCENT
kp-less                       SDLK_KP_LESS
kp-greater                    SDLK_KP_GREATER
kp-ampersand                  SDLK_KP_AMPERSAND
kp-dbl-ampersand              SDLK_KP_DBLAMPERSAND
kp-vertical-bar               SDLK_KP_VERTICALBAR
kp-dbl-vertical-bar           SDLK_KP_DBLVERTICALBAR
kp-colon                      SDLK_KP_COLON
kp-hash                       SDLK_KP_HASH
kp-space                      SDLK_KP_SPACE
kp-at                         SDLK_KP_AT
kp-exclam                     SDLK_KP_EXCLAM
kp-mem-store                  SDLK_KP_MEMSTORE
kp-mem-recall                 SDLK_KP_MEMRECALL
kp-mem-clear                  SDLK_KP_MEMCLEAR
kp-mem-add                    SDLK_KP_MEMADD
kp-mem-subtract               SDLK_KP_MEMSUBTRACT
kp-mem-multiply               SDLK_KP_MEMMULTIPLY
kp-mem-divide                 SDLK_KP_MEMDIVIDE
kp-plus-minus                 SDLK_KP_PLUSMINUS
kp-clear                      SDLK_KP_CLEAR
kp-clear-entry                SDLK_KP_CLEARENTRY
kp-binary                     SDLK_KP_BINARY
kp-octal                      SDLK_KP_OCTAL
kp-decimal                    SDLK_KP_DECIMAL
kp-hexadecimal                SDLK_KP_HEXADECIMAL

lctrl                         SDLK_LCTRL
lshift                        SDLK_LSHIFT
lalt                          SDLK_LALT
lgui                          SDLK_LGUI
rctrl                         SDLK_RCTRL
rshift                        SDLK_RSHIFT
ralt                          SDLK_RALT
rgui                          SDLK_RGUI

mode                          SDLK_MODE

audio-next                    SDLK_AUDIONEXT
audio-prev                    SDLK_AUDIOPREV
audio-stop                    SDLK_AUDIOSTOP
audio-play                    SDLK_AUDIOPLAY
audio-mute                    SDLK_AUDIOMUTE
media-select                  SDLK_MEDIASELECT
www                           SDLK_WWW
mail                          SDLK_MAIL
calculator                    SDLK_CALCULATOR
computer                      SDLK_COMPUTER
ac-search                     SDLK_AC_SEARCH
ac-home                       SDLK_AC_HOME
ac-back                       SDLK_AC_BACK
ac-forward                    SDLK_AC_FORWARD
ac-stop                       SDLK_AC_STOP
ac-refresh                    SDLK_AC_REFRESH
ac-bookmarks                  SDLK_AC_BOOKMARKS

brightness-down               SDLK_BRIGHTNESSDOWN
brightness-up                 SDLK_BRIGHTNESSUP
display-switch                SDLK_DISPLAYSWITCH
kbd-illum-toggle              SDLK_KBDILLUMTOGGLE
kbd-illum-down                SDLK_KBDILLUMDOWN
kbd-illum-up                  SDLK_KBDILLUMUP
eject                         SDLK_EJECT
sleep                         SDLK_SLEEP
```


## Keyboard Scancodes

Returned by `keysym-scancode`, `keyboard-event-scancode`, and others.
See [SDL_Scancode](https://wiki.libsdl.org/SDL_Scancode).

```
unknown                       SDL_SCANCODE_UNKNOWN

a                             SDL_SCANCODE_A
b                             SDL_SCANCODE_B
c                             SDL_SCANCODE_C
d                             SDL_SCANCODE_D
e                             SDL_SCANCODE_E
f                             SDL_SCANCODE_F
g                             SDL_SCANCODE_G
h                             SDL_SCANCODE_H
i                             SDL_SCANCODE_I
j                             SDL_SCANCODE_J
k                             SDL_SCANCODE_K
l                             SDL_SCANCODE_L
m                             SDL_SCANCODE_M
n                             SDL_SCANCODE_N
o                             SDL_SCANCODE_O
p                             SDL_SCANCODE_P
q                             SDL_SCANCODE_Q
r                             SDL_SCANCODE_R
s                             SDL_SCANCODE_S
t                             SDL_SCANCODE_T
u                             SDL_SCANCODE_U
v                             SDL_SCANCODE_V
w                             SDL_SCANCODE_W
x                             SDL_SCANCODE_X
y                             SDL_SCANCODE_Y
z                             SDL_SCANCODE_Z

n-1                           SDL_SCANCODE_1
n-2                           SDL_SCANCODE_2
n-3                           SDL_SCANCODE_3
n-4                           SDL_SCANCODE_4
n-5                           SDL_SCANCODE_5
n-6                           SDL_SCANCODE_6
n-7                           SDL_SCANCODE_7
n-8                           SDL_SCANCODE_8
n-9                           SDL_SCANCODE_9
n-0                           SDL_SCANCODE_0

return                        SDL_SCANCODE_RETURN
escape                        SDL_SCANCODE_ESCAPE
backspace                     SDL_SCANCODE_BACKSPACE
tab                           SDL_SCANCODE_TAB
space                         SDL_SCANCODE_SPACE

minus                         SDL_SCANCODE_MINUS
equals                        SDL_SCANCODE_EQUALS
left-bracket                  SDL_SCANCODE_LEFTBRACKET
right-bracket                 SDL_SCANCODE_RIGHTBRACKET
backslash                     SDL_SCANCODE_BACKSLASH
non-us-hash                   SDL_SCANCODE_NONUSHASH
semicolon                     SDL_SCANCODE_SEMICOLON
apostrophe                    SDL_SCANCODE_APOSTROPHE
grave                         SDL_SCANCODE_GRAVE
comma                         SDL_SCANCODE_COMMA
period                        SDL_SCANCODE_PERIOD
slash                         SDL_SCANCODE_SLASH

caps-lock                     SDL_SCANCODE_CAPSLOCK

f1                            SDL_SCANCODE_F1
f2                            SDL_SCANCODE_F2
f3                            SDL_SCANCODE_F3
f4                            SDL_SCANCODE_F4
f5                            SDL_SCANCODE_F5
f6                            SDL_SCANCODE_F6
f7                            SDL_SCANCODE_F7
f8                            SDL_SCANCODE_F8
f9                            SDL_SCANCODE_F9
f10                           SDL_SCANCODE_F10
f11                           SDL_SCANCODE_F11
f12                           SDL_SCANCODE_F12

print-screen                  SDL_SCANCODE_PRINTSCREEN
scroll-lock                   SDL_SCANCODE_SCROLLLOCK
pause                         SDL_SCANCODE_PAUSE
insert                        SDL_SCANCODE_INSERT
home                          SDL_SCANCODE_HOME
page-up                       SDL_SCANCODE_PAGEUP
delete                        SDL_SCANCODE_DELETE
end                           SDL_SCANCODE_END
page-down                     SDL_SCANCODE_PAGEDOWN
right                         SDL_SCANCODE_RIGHT
left                          SDL_SCANCODE_LEFT
down                          SDL_SCANCODE_DOWN
up                            SDL_SCANCODE_UP

num-lock-clear                SDL_SCANCODE_NUMLOCKCLEAR
kp-divide                     SDL_SCANCODE_KP_DIVIDE
kp-multiply                   SDL_SCANCODE_KP_MULTIPLY
kp-minus                      SDL_SCANCODE_KP_MINUS
kp-plus                       SDL_SCANCODE_KP_PLUS
kp-enter                      SDL_SCANCODE_KP_ENTER
kp-1                          SDL_SCANCODE_KP_1
kp-2                          SDL_SCANCODE_KP_2
kp-3                          SDL_SCANCODE_KP_3
kp-4                          SDL_SCANCODE_KP_4
kp-5                          SDL_SCANCODE_KP_5
kp-6                          SDL_SCANCODE_KP_6
kp-7                          SDL_SCANCODE_KP_7
kp-8                          SDL_SCANCODE_KP_8
kp-9                          SDL_SCANCODE_KP_9
kp-0                          SDL_SCANCODE_KP_0
kp-period                     SDL_SCANCODE_KP_PERIOD

non-us-backslash              SDL_SCANCODE_NONUSBACKSLASH
application                   SDL_SCANCODE_APPLICATION
power                         SDL_SCANCODE_POWER
kp-equals                     SDL_SCANCODE_KP_EQUALS
f13                           SDL_SCANCODE_F13
f14                           SDL_SCANCODE_F14
f15                           SDL_SCANCODE_F15
f16                           SDL_SCANCODE_F16
f17                           SDL_SCANCODE_F17
f18                           SDL_SCANCODE_F18
f19                           SDL_SCANCODE_F19
f20                           SDL_SCANCODE_F20
f21                           SDL_SCANCODE_F21
f22                           SDL_SCANCODE_F22
f23                           SDL_SCANCODE_F23
f24                           SDL_SCANCODE_F24
execute                       SDL_SCANCODE_EXECUTE
help                          SDL_SCANCODE_HELP
menu                          SDL_SCANCODE_MENU
select                        SDL_SCANCODE_SELECT
stop                          SDL_SCANCODE_STOP
again                         SDL_SCANCODE_AGAIN
undo                          SDL_SCANCODE_UNDO
cut                           SDL_SCANCODE_CUT
copy                          SDL_SCANCODE_COPY
paste                         SDL_SCANCODE_PASTE
find                          SDL_SCANCODE_FIND
mute                          SDL_SCANCODE_MUTE
volume-up                     SDL_SCANCODE_VOLUMEUP
volume-down                   SDL_SCANCODE_VOLUMEDOWN
kp-comma                      SDL_SCANCODE_KP_COMMA
kp-equals-as400               SDL_SCANCODE_KP_EQUALSAS400

international1                SDL_SCANCODE_INTERNATIONAL1
international2                SDL_SCANCODE_INTERNATIONAL2
international3                SDL_SCANCODE_INTERNATIONAL3
international4                SDL_SCANCODE_INTERNATIONAL4
international5                SDL_SCANCODE_INTERNATIONAL5
international6                SDL_SCANCODE_INTERNATIONAL6
international7                SDL_SCANCODE_INTERNATIONAL7
international8                SDL_SCANCODE_INTERNATIONAL8
international9                SDL_SCANCODE_INTERNATIONAL9
lang1                         SDL_SCANCODE_LANG1
lang2                         SDL_SCANCODE_LANG2
lang3                         SDL_SCANCODE_LANG3
lang4                         SDL_SCANCODE_LANG4
lang5                         SDL_SCANCODE_LANG5
lang6                         SDL_SCANCODE_LANG6
lang7                         SDL_SCANCODE_LANG7
lang8                         SDL_SCANCODE_LANG8
lang9                         SDL_SCANCODE_LANG9

alt-erase                     SDL_SCANCODE_ALTERASE
sys-req                       SDL_SCANCODE_SYSREQ
cancel                        SDL_SCANCODE_CANCEL
clear                         SDL_SCANCODE_CLEAR
prior                         SDL_SCANCODE_PRIOR
return2                       SDL_SCANCODE_RETURN2
separator                     SDL_SCANCODE_SEPARATOR
out                           SDL_SCANCODE_OUT
oper                          SDL_SCANCODE_OPER
clear-again                   SDL_SCANCODE_CLEARAGAIN
crsel                         SDL_SCANCODE_CRSEL
exsel                         SDL_SCANCODE_EXSEL

kp-00                         SDL_SCANCODE_KP_00
kp-000                        SDL_SCANCODE_KP_000
thousands-separator           SDL_SCANCODE_THOUSANDSSEPARATOR
decimal-separator             SDL_SCANCODE_DECIMALSEPARATOR
currency-unit                 SDL_SCANCODE_CURRENCYUNIT
currency-subunit              SDL_SCANCODE_CURRENCYSUBUNIT
kp-left-paren                 SDL_SCANCODE_KP_LEFTPAREN
kp-right-paren                SDL_SCANCODE_KP_RIGHTPAREN
kp-left-brace                 SDL_SCANCODE_KP_LEFTBRACE
kp-right-brace                SDL_SCANCODE_KP_RIGHTBRACE
kp-tab                        SDL_SCANCODE_KP_TAB
kp-backspace                  SDL_SCANCODE_KP_BACKSPACE
kp-a                          SDL_SCANCODE_KP_A
kp-b                          SDL_SCANCODE_KP_B
kp-c                          SDL_SCANCODE_KP_C
kp-d                          SDL_SCANCODE_KP_D
kp-e                          SDL_SCANCODE_KP_E
kp-f                          SDL_SCANCODE_KP_F
kp-xor                        SDL_SCANCODE_KP_XOR
kp-power                      SDL_SCANCODE_KP_POWER
kp-percent                    SDL_SCANCODE_KP_PERCENT
kp-less                       SDL_SCANCODE_KP_LESS
kp-greater                    SDL_SCANCODE_KP_GREATER
kp-ampersand                  SDL_SCANCODE_KP_AMPERSAND
kp-dbl-ampersand              SDL_SCANCODE_KP_DBLAMPERSAND
kp-vertical-bar               SDL_SCANCODE_KP_VERTICALBAR
kp-dbl-vertical-bar           SDL_SCANCODE_KP_DBLVERTICALBAR
kp-colon                      SDL_SCANCODE_KP_COLON
kp-hash                       SDL_SCANCODE_KP_HASH
kp-space                      SDL_SCANCODE_KP_SPACE
kp-at                         SDL_SCANCODE_KP_AT
kp-exclam                     SDL_SCANCODE_KP_EXCLAM
kp-mem-store                  SDL_SCANCODE_KP_MEMSTORE
kp-mem-recall                 SDL_SCANCODE_KP_MEMRECALL
kp-mem-clear                  SDL_SCANCODE_KP_MEMCLEAR
kp-mem-add                    SDL_SCANCODE_KP_MEMADD
kp-mem-subtract               SDL_SCANCODE_KP_MEMSUBTRACT
kp-mem-multiply               SDL_SCANCODE_KP_MEMMULTIPLY
kp-mem-divide                 SDL_SCANCODE_KP_MEMDIVIDE
kp-plus-minus                 SDL_SCANCODE_KP_PLUSMINUS
kp-clear                      SDL_SCANCODE_KP_CLEAR
kp-clear-entry                SDL_SCANCODE_KP_CLEARENTRY
kp-binary                     SDL_SCANCODE_KP_BINARY
kp-octal                      SDL_SCANCODE_KP_OCTAL
kp-decimal                    SDL_SCANCODE_KP_DECIMAL
kp-hexadecimal                SDL_SCANCODE_KP_HEXADECIMAL

lctrl                         SDL_SCANCODE_LCTRL
lshift                        SDL_SCANCODE_LSHIFT
lalt                          SDL_SCANCODE_LALT
lgui                          SDL_SCANCODE_LGUI
rctrl                         SDL_SCANCODE_RCTRL
rshift                        SDL_SCANCODE_RSHIFT
ralt                          SDL_SCANCODE_RALT
rgui                          SDL_SCANCODE_RGUI

mode                          SDL_SCANCODE_MODE

audio-next                    SDL_SCANCODE_AUDIONEXT
audio-prev                    SDL_SCANCODE_AUDIOPREV
audio-stop                    SDL_SCANCODE_AUDIOSTOP
audio-play                    SDL_SCANCODE_AUDIOPLAY
audio-mute                    SDL_SCANCODE_AUDIOMUTE
media-select                  SDL_SCANCODE_MEDIASELECT
www                           SDL_SCANCODE_WWW
mail                          SDL_SCANCODE_MAIL
calculator                    SDL_SCANCODE_CALCULATOR
computer                      SDL_SCANCODE_COMPUTER
ac-search                     SDL_SCANCODE_AC_SEARCH
ac-home                       SDL_SCANCODE_AC_HOME
ac-back                       SDL_SCANCODE_AC_BACK
ac-forward                    SDL_SCANCODE_AC_FORWARD
ac-stop                       SDL_SCANCODE_AC_STOP
ac-refresh                    SDL_SCANCODE_AC_REFRESH
ac-bookmarks                  SDL_SCANCODE_AC_BOOKMARKS

brightness-down               SDL_SCANCODE_BRIGHTNESSDOWN
brightness-up                 SDL_SCANCODE_BRIGHTNESSUP
display-switch                SDL_SCANCODE_DISPLAYSWITCH
kbd-illum-toggle              SDL_SCANCODE_KBDILLUMTOGGLE
kbd-illum-down                SDL_SCANCODE_KBDILLUMDOWN
kbd-illum-up                  SDL_SCANCODE_KBDILLUMUP
eject                         SDL_SCANCODE_EJECT
sleep                         SDL_SCANCODE_SLEEP

app1                          SDL_SCANCODE_APP1
app2                          SDL_SCANCODE_APP2
```
