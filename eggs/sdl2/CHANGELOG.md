
This document describes the changes in each version of chicken-sdl2.

This library follows "[semantic versioning](http://semver.org)".
Until version 1.0.0 is released, the API is not guaranteed to be "stable".
That means the maintainer reserves the right to change the API if needed,
possibly in ways that break backward compatibility with previous versions.
**Large backward-incompatible changes are unlikely**,
but there may be small tweaks and fixes to the API if problems are discovered.

After version 1.0.0 is released, the API is guaranteed to remain stable (no backward-incompatible changes)
until the next new major version (e.g. going from version 1.x to 2.0.0, or 2.x to 3.0.0).



# 0.2.0 (2016-02-13)

## Backward Incompatible Changes

There have been several backward incompatible changes to improve type
checks for integer arguments. These changes affect **all function
bindings and struct field setters that accept integer arguments**.
\[[#32](https://gitlab.com/chicken-sdl2/chicken-sdl2/issues/32)\]

- All procedures that accept integer arguments now accept inexact
  integers (e.g. 1.0) as well as exact integers (e.g. 1). Before, some
  procedures would signal an exception when given an inexact integer.

- Integer arguments are now checked to make sure they are integers.
  Before, some procedures would accept non-integer numbers (e.g. 1.99)
  and truncate them to integers (e.g. 1), which could cause unexpected
  results. Now, an exception will be signalled if the argument is not
  an integer.

- Integers arguments are now checked to make sure they are in the
  proper range for their data representation. E.g. 8-bit unsigned
  integers must be in the range [0, 255]. Before, integer arguments
  would sometimes overflow, which could cause unexpected results.
  Now, an exception will be signalled if the integer is too low or too
  high.

Other backward incompatible changes in this version:

- `get-window-from-id` now signals an exception of kind `(exn sdl2)`
  if there is no window with the given ID. Before, it returned a null
  sdl2:window, which was inconsistent with this egg's conventions.


## Deprecations

The following procedures have new names, for consistency with Scheme
naming conventions. The old names still exist for backward
compatibility, but will be removed in a future version.
You should migrate to the new names as soon as possible.

- Deprecated: `copy-color`.  Renamed to `color-copy`.
- Deprecated: `copy-colour`. Renamed to `colour-copy`.
- Deprecated: `copy-point`.  Renamed to `point-copy`.
- Deprecated: `copy-rect`.   Renamed to `rect-copy`.

The following procedures are deprecated because they are not very
useful and they are easy to define using other procedures if needed.
They will be removed in a future version.

- Deprecated: `copy-color*`.  Use `(color-copy! c (make-color*))` instead.
- Deprecated: `copy-colour*`. Use `(colour-copy! c (make-colour*))` instead.
- Deprecated: `copy-point*`.  Use `(point-copy! p (make-point*))` instead.
- Deprecated: `copy-rect*`.   Use `(rect-copy! r (make-rect*))` instead.


## Renderer (2D Accelerated Rendering)

Added the **sdl2:renderer** struct record type and many function bindings related to
[2D accelerated rendering](https://wiki.libsdl.org/CategoryRender)
\[[#1](https://gitlab.com/chicken-sdl2/chicken-sdl2/issues/1),
[#2](https://gitlab.com/chicken-sdl2/chicken-sdl2/issues/2),
[#3](https://gitlab.com/chicken-sdl2/chicken-sdl2/issues/3),
[#5](https://gitlab.com/chicken-sdl2/chicken-sdl2/issues/5)\]:

```
renderer?
create-renderer!              create-software-renderer!     destroy-renderer!
create-window-and-renderer!   get-renderer
get-renderer-info             get-renderer-info*
num-render-drivers            render-driver-info

renderer-output-size
render-clip-rect              render-clip-rect-set!         render-clip-enabled?
render-logical-size           render-logical-size-set!
render-scale                  render-scale-set!
render-viewport               render-viewport-set!

render-present!
render-copy!                  render-copy-ex!
render-target-supported?      render-target                 render-target-set!
render-read-pixels-raw

render-draw-blend-mode        render-draw-blend-mode-set!   render-draw-blend-mode-raw
render-draw-color             render-draw-color-set!
render-draw-colour            render-draw-colour-set!
render-clear!
render-draw-line!             render-draw-lines!
render-draw-point!            render-draw-points!
render-draw-rect!             render-draw-rects!
render-fill-rect!             render-fill-rects!
```


## Renderer Info

Added the **sdl2:renderer-info** struct record type
\[[#1](https://gitlab.com/chicken-sdl2/chicken-sdl2/issues/1)\]:

```
renderer-info?                     free-renderer-info!
renderer-info-name
renderer-info-flags                renderer-info-flags-raw
renderer-info-num-texture-formats
renderer-info-texture-formats      renderer-info-texture-formats-raw
renderer-info-max-texture-width    renderer-info-max-texture-height
```


## Texture

Added the **sdl2:texture** struct record type and many related function bindings
\[[#4](https://gitlab.com/chicken-sdl2/chicken-sdl2/issues/4)]:

```
texture?
create-texture                     create-texture*
create-texture-from-surface        create-texture-from-surface*
destroy-texture!
query-texture                      query-texture-raw
texture-format                     texture-access
texture-w                          texture-h
lock-texture-raw!                  unlock-texture!
update-texture-raw!                update-yuv-texture-raw!
texture-alpha-mod                  texture-alpha-mod-set!
texture-blend-mode                 texture-blend-mode-set!
texture-color-mod                  texture-color-mod-set!
texture-colour-mod                 texture-colour-mod-set!
gl-bind-texture!                   gl-unbind-texture!
```


## Hints

Added function bindings related to [hints](https://wiki.libsdl.org/CategoryHints).
\[[#7](https://gitlab.com/chicken-sdl2/chicken-sdl2/issues/7)\]

```
get-hint
set-hint!
clear-hints!
```


## SDL 2.0.4 Support

Added support for various feature that were added in SDL 2.0.4. These
are only available if the egg is compiled with SDL 2.0.4 or higher.
\[[#37](https://gitlab.com/chicken-sdl2/chicken-sdl2/issues/37)\]

- Added `grabbed-window` procedure.
- Added `point-in-rect?` procedure.
- Added `render-clip-enabled?` procedure.
- Added support for the `mouse-capture` window flag symbol.
- Added `mouse-wheel-event-direction` and
  `mouse-wheel-event-direction-set!` struct field accessors.
- Added support for the `context-release-behavior` OpenGL attribute.
  It can be set to the symbols `none` or `flush`.
- Added `joystick-from-instance-id`.
- Added `joystick-current-power-level`.


## Color, Point, and Rect Operations

- Added `color->values`, `point->values`, and `rect->values`. These
  return the components of those types as multiple values, so that you
  can easily destructure these types with `receive` or `let-values`.

- Renamed several procedures for consistency with Scheme conventions.
  The old names still exist for backward compatibility, but will be
  removed in a future version:

    - `copy-color`  renamed to `color-copy`
    - `copy-colour` renamed to `colour-copy`
    - `copy-point`  renamed to `point-copy`
    - `copy-rect`   renamed to `rect-copy`

- Added `color-copy!` (aka `colour-copy!`), `point-copy!`, and
  `rect-copy!`. They efficiently copy the values from the source
  struct into the destination struct, and return the modified
  destination struct.

- Added various color mathematical operations:

    ```
    color-scale     color-mult     color-add      color-sub
    color-scale!    color-mult!    color-add!     color-sub!
    color-lerp
    color-lerp!

    ("colour" aliases)
    colour-scale    colour-mult    colour-add     colour-sub
    colour-scale!   colour-mult!   colour-add!    colour-sub!
    colour-lerp
    colour-lerp!
    ```

- Added various point mathematical operations:

    ```
    point-scale     point-unscale       point-move
    point-scale!    point-unscale!      point-move!
    point-add       point-sub           point-lerp
    point-add!      point-sub!          point-lerp!
    ```

- Added various rect mathematical operations:

    ```
    rect-scale      rect-unscale
    rect-scale!     rect-unscale!
    rect-move       rect-add-point      rect-sub-point
    rect-move!      rect-add-point!     rect-sub-point!
    rect-grow       rect-grow/center
    rect-grow!      rect-grow/center!
    rect-lerp       rect-lerp-xy
    rect-lerp!      rect-lerp-xy!
    ```


## Performance Improvements

- Managed instances of sdl2:color, sdl2:event, sdl2:keysym,
  sdl2:point, and sdl2:rect are now allocated in CHICKEN-managed
  memory. This improves performance of garbage collecting managed
  instances of those types. Unmanaged instances are not affected.
  Thanks to Kooda Loutre for suggesting this improvement.
  \[[#30](https://gitlab.com/chicken-sdl2/chicken-sdl2/issues/30)\]

- Significantly improved the performance of initializing managed and
  unmanaged instances of sdl2:color, sdl2:point, and sdl2:rect.

- Significantly improved performance of `color-set!`, `point-set!`,
  and `rect-set!` when all arguments are specified. This performance
  improvement does not apply if any argument is omitted or #f.

- Fixed a performance bug in all struct field setters. Field guards
  were being created every time a field setter was called. Each guard
  is now created only once, at startup, as was originally intended.
  This improves the performance of all struct field setters.


## Feature Identifiers

Added feature identifiers which can be used to detect the versions of
the sdl2 egg and SDL at compile time or run time, for example using
the `cond-expand` macro, `#+foo` syntax, or the `feature?` procedure.

Some or all of the following feature identifiers will be registered
depending on the circumstances:

```
sdl2                Using any version of the sdl2 egg
sdl2-0.2.0+         Using sdl2 egg 0.2.0 or higher

libSDL-2.0.0+       sdl2 egg was compiled with SDL 2.0.0 or higher
libSDL-2.0.1+       ... SDL 2.0.1 or higher
libSDL-2.0.2+       ... SDL 2.0.2 or higher
libSDL-2.0.3+       ... SDL 2.0.3 or higher
libSDL-2.0.4+       ... SDL 2.0.4 or higher
```

These are cumulative, so if you are using sdl2 egg version 0.2.0 with
SDL version 2.0.4, all of the above identifiers will be defined.


## Other changes

- Added the `struct-eq?` procedure. You should use this procedure to
  compare the identities of struct instances. Other procedures, such
  as `eq?` or `equal?`, may not work correctly in the future due to
  changes in the implementation of struct records.

- Added the `egg-version` procedure, which returns a list of three
  integers indicating the version of the sdl2 egg itself. This is
  independent of the version of SDL.

- `fill-rects!` and `update-window-surface-rects!` can now accept a
  vector of sdl2:rects as well as a list of sdl2:rects. Before, they
  only accepted a list of sdl2:rects.

- `enclose-points` now can now accept a vector of sdl2:points as well
  as a list of sdl2:points. Before, it only accepted a list of
  sdl2:points.

- Added support for the `allow-high-dpi` window flag symbol.
  It requires SDL version 2.0.1 or higher.

- Added an install flag to enable profiling in the sdl2 egg. If you
  run chicken-install with the `-D sdl2-profile` flag, the sdl2 egg
  will be compiled with the `-profile` flag, which enables performance
  profiling of sdl2 procedures when any program uses the sdl2 egg.
  This can help you diagnose performance issues in the sdl2 egg and/or
  in your program. See the CHICKEN manual section
  "[Using the compiler](http://wiki.call-cc.org/man/4/Using%20the%20compiler)"
  for more information about profiling. Profiling greatly reduces
  performance, and creates a new "PROFILE.###" file each time the
  program is run, so you should only enable profiling when you are
  diagnosing performance issues.



# 0.1.1 (2015-12-22)

- Fixed a compile error when compiling with GCC 4:
  "‘for’ loop initial declarations are only allowed in C99 mode".
  Thanks to Evan Hanson for reporting this issue.
  \[[#29](https://gitlab.com/chicken-sdl2/chicken-sdl2/issues/29)\]



# 0.1.0 (2015-12-19)

Initial release. The following procedures were included:

## General

Initialization and cleanup:

```
set-main-ready!
init!                              init-subsystem!
quit!                              quit-subsystem!
was-init
```

Miscellaneous:

```
get-error                          set-error!                         clear-error!
get-platform
screen-saver-enabled?              screen-saver-enabled-set!
has-clipboard-text?                get-clipboard-text                 set-clipboard-text!
```

Struct memory management:

```
struct-null?
```


## Color

Related to the **sdl2:color** struct record type
(each procedure has an alias spelled as "colour"):

```
color?                   colour?
make-color               make-color*              free-color!
make-colour              make-colour*             free-colour!
color-r                  color-r-set!             colour-r                 colour-r-set!
color-g                  color-g-set!             colour-g                 colour-g-set!
color-b                  color-b-set!             colour-b                 colour-b-set!
color-a                  color-a-set!             colour-a                 colour-a-set!
color-set!                                        colour-set!
color->list                                       colour->list
color=?                                           colour=?
copy-color                                        copy-colour
copy-color*                                       copy-colour*
```


## Event

```
event-state                        event-state-set!
flush-event!                       flush-events!
has-event?                         has-events?                        quit-requested?
get-events!                        peek-events                        poll-event!
pump-events!                       push-event!
wait-event!                        wait-event-timeout!
register-events!
```

Related to the **sdl2:event** struct record type:

```
event?
make-event                         make-event*                        free-event!
event-type                         event-type-set!                    event-type-raw
event-timestamp                    event-timestamp-set!
```

Related to the **sdl2:controller-axis-event** variant:

```
controller-axis-event?
controller-axis-event-which        controller-axis-event-which-set!
controller-axis-event-axis         controller-axis-event-axis-set!
controller-axis-event-value        controller-axis-event-value-set!
```

Related to the **sdl2:controller-button-event** variant:

```
controller-button-event?
controller-button-event-which      controller-button-event-which-set!
controller-button-event-button     controller-button-event-button-set!
controller-button-event-state      controller-button-event-state-set!
```

Related to the **sdl2:controller-device-event** variant:

```
controller-device-event?
controller-device-event-which      controller-device-event-which-set!
```

Related to the **sdl2:dollar-gesture-event** variant:

```
dollar-gesture-event?
dollar-gesture-event-touch-id      dollar-gesture-event-touch-id-set!
dollar-gesture-event-gesture-id    dollar-gesture-event-gesture-id-set!
dollar-gesture-event-num-fingers   dollar-gesture-event-num-fingers-set!
dollar-gesture-event-error         dollar-gesture-event-error-set!
dollar-gesture-event-x             dollar-gesture-event-x-set!
dollar-gesture-event-y             dollar-gesture-event-y-set!
```

Related to the **sdl2:drop-event** variant:

```
drop-event?
drop-event-file                    drop-event-file-set!
```

Related to the **sdl2:joy-axis-event** variant:

```
joy-axis-event?
joy-axis-event-which               joy-axis-event-which-set!
joy-axis-event-axis                joy-axis-event-axis-set!
joy-axis-event-value               joy-axis-event-value-set!
```

Related to the **sdl2:joy-ball-event** variant:

```
joy-ball-event?
joy-ball-event-which               joy-ball-event-which-set!
joy-ball-event-ball                joy-ball-event-ball-set!
joy-ball-event-xrel                joy-ball-event-xrel-set!
joy-ball-event-yrel                joy-ball-event-yrel-set!
```

Related to the **sdl2:joy-button-event** variant:

```
joy-button-event?
joy-button-event-which             joy-button-event-which-set!
joy-button-event-button            joy-button-event-button-set!
joy-button-event-state             joy-button-event-state-set!
```

Related to the **sdl2:joy-device-event** variant:

```
joy-device-event?
joy-device-event-which             joy-device-event-which-set!
```

Related to the **sdl2:joy-hat-event** variant:

```
joy-hat-event?
joy-hat-event-which                joy-hat-event-which-set!
joy-hat-event-hat                  joy-hat-event-hat-set!
joy-hat-event-value                joy-hat-event-value-set!           joy-hat-event-value-raw
```

Related to the **sdl2:keyboard-event** variant:

```
keyboard-event?
keyboard-event-window-id           keyboard-event-window-id-set!
keyboard-event-state               keyboard-event-state-set!
keyboard-event-repeat              keyboard-event-repeat-set!
keyboard-event-keysym              keyboard-event-keysym-set!
keyboard-event-scancode            keyboard-event-scancode-set!       keyboard-event-scancode-raw
keyboard-event-sym                 keyboard-event-sym-set!            keyboard-event-sym-raw
keyboard-event-mod                 keyboard-event-mod-set!            keyboard-event-mod-raw
```

Related to the **sdl2:mouse-button-event** variant:

```
mouse-button-event?
mouse-button-event-window-id       mouse-button-event-window-id-set!
mouse-button-event-which           mouse-button-event-which-set!
mouse-button-event-button          mouse-button-event-button-set!     mouse-button-event-button-raw
mouse-button-event-state           mouse-button-event-state-set!
mouse-button-event-x               mouse-button-event-x-set!
mouse-button-event-y               mouse-button-event-y-set!
```

Related to the **sdl2:mouse-motion-event** variant:

```
mouse-motion-event?
mouse-motion-event-window-id       mouse-motion-event-window-id-set!
mouse-motion-event-which           mouse-motion-event-which-set!
mouse-motion-event-state           mouse-motion-event-state-set!      mouse-motion-event-state-raw
mouse-motion-event-x               mouse-motion-event-x-set!
mouse-motion-event-y               mouse-motion-event-y-set!
mouse-motion-event-xrel            mouse-motion-event-xrel-set!
mouse-motion-event-yrel            mouse-motion-event-yrel-set!
```

Related to the **sdl2:mouse-wheel-event** variant:

```
mouse-wheel-event?
mouse-wheel-event-window-id        mouse-wheel-event-window-id-set!
mouse-wheel-event-which            mouse-wheel-event-which-set!
mouse-wheel-event-x                mouse-wheel-event-x-set!
mouse-wheel-event-y                mouse-wheel-event-y-set!
```

Related to the **sdl2:multi-gesture-event** variant:

```
multi-gesture-event?
multi-gesture-event-touch-id       multi-gesture-event-touch-id-set!
multi-gesture-event-dtheta         multi-gesture-event-dtheta-set!
multi-gesture-event-ddist          multi-gesture-event-ddist-set!
multi-gesture-event-x              multi-gesture-event-x-set!
multi-gesture-event-y              multi-gesture-event-y-set!
multi-gesture-event-num-fingers    multi-gesture-event-num-fingers-set!
```

Related to the **sdl2:quit-event** variant:

```
quit-event?
```

Related to the **sdl2:sys-wm-event** variant:

```
sys-wm-event?
sys-wm-event-msg-raw               sys-wm-event-msg-raw-set!
```

Related to the **sdl2:text-editing-event** variant:

```
text-editing-event?
text-editing-event-window-id       text-editing-event-window-id-set!
text-editing-event-text            text-editing-event-text-set!
text-editing-event-start           text-editing-event-start-set!
text-editing-event-length          text-editing-event-length-set!
```

Related to the **sdl2:text-input-event** variant:

```
text-input-event?
text-input-event-window-id         text-input-event-window-id-set!
text-input-event-text              text-input-event-text-set!
```

Related to the **sdl2:touch-finger-event** variant:

```
touch-finger-event?
touch-finger-event-touch-id        touch-finger-event-touch-id-set!
touch-finger-event-finger-id       touch-finger-event-finger-id-set!
touch-finger-event-x               touch-finger-event-x-set!
touch-finger-event-y               touch-finger-event-y-set!
touch-finger-event-dx              touch-finger-event-dx-set!
touch-finger-event-dy              touch-finger-event-dy-set!
touch-finger-event-pressure        touch-finger-event-pressure-set!
```

Related to the **sdl2:user-event** variant:

```
user-event?
user-event-window-id               user-event-window-id-set!
user-event-code                    user-event-code-set!
user-event-data1-raw               user-event-data1-raw-set!
user-event-data2-raw               user-event-data2-raw-set!
```

Related to the **sdl2:window-event** variant:

```
window-event?
window-event-window-id             window-event-window-id-set!
window-event-event                 window-event-event-set!            window-event-event-raw
window-event-data1                 window-event-data1-set!
window-event-data2                 window-event-data2-set!
```


## Joystick

```
num-joysticks                      joystick-open!                     joystick-close!
joystick-update!                   joystick-event-state               joystick-event-state-set!
joystick-name-for-index
joystick-get-device-guid           joystick-get-guid-from-string
```

Related to the **sdl2:joystick** struct record type:

```
joystick?
joystick-attached?
joystick-name
joystick-num-axes                  joystick-get-axis
joystick-num-balls                 joystick-get-ball
joystick-num-buttons               joystick-get-button
joystick-num-hats                  joystick-get-hat                   joystick-get-hat-raw
joystick-instance-id               joystick-get-guid
```

Related to the **sdl2:joystick-guid** struct record type:

```
joystick-guid?                     free-joystick-guid!
joystick-get-guid-string
```


## Keyboard

```
get-key-from-name                  get-key-from-name-raw
get-key-from-scancode              get-key-from-scancode-raw
get-key-name
get-scancode-from-name             get-scancode-from-name-raw
get-scancode-from-key              get-scancode-from-key-raw
get-scancode-name
get-keyboard-focus
scancode-pressed?
mod-state                          mod-state-set!                     mod-state-raw
text-input-rect-set!
start-text-input!                  stop-text-input!                   text-input-active?
screen-keyboard-support?           screen-keyboard-shown?
```

Related to the **sdl2:keysym** struct record type:

```
keysym?
make-keysym                        make-keysym*                       free-keysym!
keysym-scancode                    keysym-scancode-set!               keysym-scancode-raw
keysym-sym                         keysym-sym-set!                    keysym-sym-raw
keysym-mod                         keysym-mod-set!                    keysym-mod-raw
```


## OpenGL integration

```
gl-create-context!                 gl-delete-context!                 gl-make-current!
gl-get-current-window              gl-get-current-context
gl-attribute                       gl-attribute-set!                  gl-reset-attributes!
gl-get-drawable-size
gl-swap-window!                    gl-swap-interval                   gl-set-swap-interval!
gl-extension-supported?
```

Related to the **sdl2:gl-context** struct record type:

```
gl-context?
```


## Palette

Related to the **sdl2:palette** struct record type:

```
palette?
make-palette                       make-palette*                      free-palette!
palette-ncolors                    palette-ncolours
palette-ref                        palette-set!
palette-colors                     palette-colours
palette-colors-set!                palette-colours-set!
```


## Pixel Format

```
map-rgb                            map-rgba
get-rgb                            get-rgba
pixel-format-enum-to-masks
```

Related to the **sdl2:pixel-format** struct record type:

```
pixel-format?
make-pixel-format                  make-pixel-format*                 free-pixel-format!
pixel-format-format                pixel-format-format-raw
pixel-format-palette               pixel-format-palette-set!
pixel-format-bits-per-pixel
pixel-format-bytes-per-pixel
pixel-format-rmask
pixel-format-gmask
pixel-format-bmask
pixel-format-amask
```


## Rect / Point

```
rect-empty?                        enclose-points                     has-intersection?
intersect-rect                     intersect-rect-and-line            union-rect
```

Related to the **sdl2:rect** struct record type:

```
rect?
make-rect                          make-rect*                         free-rect!
rect-x                             rect-x-set!
rect-y                             rect-y-set!
rect-w                             rect-w-set!
rect-h                             rect-h-set!
rect-set!                          rect->list                         rect=?
copy-rect                          copy-rect*
```

Related to the **sdl2:point** struct record type:

```
point?
make-point                         make-point*                        free-point!
point-x                            point-x-set!
point-y                            point-y-set!
point-set!                         point->list                        point=?
copy-point                         copy-point*
```


## RWops

```
rw-from-file                       rw-from-const-mem                  rw-from-mem
rw-from-blob                       rw-from-string
rw-close!
```

Related to the **sdl2:rwops** struct record type:

```
rwops?
rwops-type                         rwops-type-raw
```


## Surface

```
create-rgb-surface*                create-rgb-surface-from*           convert-surface
load-bmp                           load-bmp*
load-bmp-rw                        load-bmp-rw*
save-bmp!                          save-bmp-rw!
lock-surface!                      unlock-surface!                    must-lock?
blit-surface!                      blit-scaled!
fill-rect!                         fill-rects!
rotate-surface-90                  rotate-surface-90*
flip-surface                       flip-surface*
```

Related to the **sdl2:surface** struct record type:

```
surface?
make-surface                       make-surface*                      free-surface!
surface-format
surface-w
surface-h
surface-pitch
surface-pixels-raw
surface-userdata-raw               surface-userdata-raw-set!
surface-refcount                   surface-refcount-set!
surface-ref                        surface-set!                       surface-ref-raw
surface-clip-rect                  surface-clip-rect-set!
surface-color-key                  surface-color-key-set!             surface-color-key-raw
surface-colour-key                 surface-colour-key-set!            surface-colour-key-raw
surface-alpha-mod                  surface-alpha-mod-set!
surface-blend-mode                 surface-blend-mode-set!            surface-blend-mode-raw
surface-color-mod                  surface-colour-mod
surface-color-mod-set!             surface-colour-mod-set!
surface-palette                    surface-palette-set!
surface-rle-set!
```


## Timer

```
delay!                             get-ticks
get-performance-counter            get-performance-frequency
```


## Touch / Gesture

```
get-num-touch-devices              get-touch-device
get-num-touch-fingers              get-touch-finger
```

Related to the **sdl2:finger** struct record type:

```
finger?
finger-id
finger-x                           finger-y
finger-pressure
```


## Version

```
version-at-least?                  compiled-version                   current-version
```


## Window / Display Mode

```
create-window!                     destroy-window!                    get-window-from-id
update-window-surface!             update-window-surface-rects!
show-window!                       hide-window!                       raise-window!
maximize-window!                   minimize-window!                   restore-window!
```

Related to the **sdl2:window** struct record type:

```
window?
window-id
window-bordered?                   window-bordered-set!
window-brightness                  window-brightness-set!
window-display-index
window-display-mode                window-display-mode-set!
window-flags                       window-flags-raw
window-fullscreen                  window-fullscreen-set!
window-grab?                       window-grab-set!
window-icon-set!
window-maximum-size                window-maximum-size-set!
window-minimum-size                window-minimum-size-set!
window-pixel-format                window-pixel-format-raw
window-position                    window-position-set!
window-size                        window-size-set!
window-surface
window-title                       window-title-set!
```

Related to the **sdl2:display-mode** struct record type:

```
display-mode?
make-display-mode                  make-display-mode*                 free-display-mode!
display-mode-format                display-mode-format-set!           display-mode-format-raw
display-mode-w                     display-mode-w-set!
display-mode-h                     display-mode-h-set!
display-mode-refresh-rate          display-mode-refresh-rate-set!
```
