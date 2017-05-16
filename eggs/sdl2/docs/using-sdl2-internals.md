# Using sdl2-internals

The guide is for advanced users. It describes when and how to use
chicken-sdl2's non-public module, `sdl2-internals`.

chicken-sdl2 has two modules: the `sdl2` module, which defines the
public interface for the library, and `sdl2-internals`, which defines
the "under the hood" code. Most users will only need to use the `sdl2`
module, but for advanced use cases you may need or want to import
specific parts of the `sdl2-internals` module into your program.

**CAUTION:** The `sdl2-internals` module is not an official part of
the public API, and does not have the same guarantees of API
stability. Only the parts of the module described in this guide are
safe to use in your programs. Other parts of the module may change at
any time.

There are three categories of definitions in the `sdl2-internals`
module that are safe to use in your programs:

- Struct record type wrappers and unwrappers
- Integer constants
- Function and C macro bindings

These are described in more detail below.


## Struct record type wrappers and unwrappers

The `sdl2-internals` module exports procedures for wrapping and
unwrapping struct record types. Wrapping means taking a pointer and
creating a record type that holds the pointer. Unwrapping means
returning the pointer that a record type is holding.

Each struct record type has its own wrapper and unwrapper procedure,
which only work for that struct record type. As of this writing, the
following wrappers and unwrappers are defined:

```
wrap-audio-cvt       unwrap-audio-cvt
wrap-audio-spec      unwrap-audio-spec
wrap-color           unwrap-color
wrap-cursor          unwrap-cursor
wrap-display-mode    unwrap-display-mode
wrap-event           unwrap-event
wrap-finger          unwrap-finger
wrap-gl-context      unwrap-gl-context
wrap-joystick-guid   unwrap-joystick-guid
wrap-joystick        unwrap-joystick
wrap-keysym          unwrap-keysym
wrap-palette         unwrap-palette
wrap-pixel-format    unwrap-pixel-format
wrap-point           unwrap-point
wrap-rect            unwrap-rect
wrap-rwops           unwrap-rwops
wrap-surface         unwrap-surface
wrap-texture         unwrap-texture
wrap-window          unwrap-window
```

The following behaviors are guaranteed to remain stable:

- If you call a wrapper procedure with a pointer to an appropriate
  struct, or with a pointer to address 0 (aka a null pointer), it will
  return a struct record instance of the appropriate type.

- If you call an unwrapper procedure with a struct record instance of
  the appropriate type, it will return either a pointer (possibly to
  address 0) or `#f`.

Other behaviors, such as error responses when passing inappropriate
types, are not guaranteed to be stable.

Wrappers and unwrappers are designed to be compatible with CHICKEN's
foreign type system. You can define a foreign type using the wrappers
and unwrappers like so:

```scheme
(define-foreign-type SDL_Surface*
  (c-pointer "SDL_Surface")
  unwrap-surface
  wrap-surface)
```

This is useful when creating libraries that are designed to work with
chicken-sdl2, such as the chicken-sdl2-image library.


## Integer constants

The `sdl2-internals` module exports hundreds of SDL-related integer
constants, such as:

- Keycodes, scancodes, and key mods (e.g. `SDLK_a`, `SDL_SCANCODE_A`,
  `KMOD_LSHIFT`)
- Event types (e.g. `SDL_KEYDOWN`, `SDL_JOYHATMOTION`)
- Pixel and audio formats (e.g. `SDL_PIXELFORMAT_RGB888`, `AUDIO_U16`)
- Flags (e.g. `SDL_INIT_EVERYTHING`, `SDL_WINDOW_FULLSCREEN`)
- OpenGL attributes and flags (e.g. `SDL_GL_RED_SIZE`,
  `SDL_GL_CONTEXT_FORWARD_COMPATIBLE_FLAG`)
- And more

The integer constants all have the same name as in C. The values of
the constants are determined at compile time, and are guaranteed to
match the correct value as defined in the SDL headers. So if, for
whatever reason, you need or want to use these integer constants, you
can import them from the `sdl2-internals` module rather than defining
them again yourself.

**CAUTION:** Some integer constants only exist in certain versions of
SDL. If chicken-sdl2 is compiled with an older version of SDL, that
constant may not be defined in the `sdl2-internals` module, or it may
be defined with a nonsense value such as -1, `#f`, or `(void)`.
Constants are only guaranteed to have the correct value when
chicken-sdl2 is compiled with a version of SDL that defines them. You
should check the SDL version with `version-at-least?` before using a
constant that may not be defined.

In the `sdl2` module, symbols are preferred over integer constants.
But, there are certain "raw" procedures that return the original
integer constant. For example, the `event-type` procedure might return
the symbol `'key-down`, but the `event-type-raw` would return the
integer value of `SDL_KEYDOWN`. Most setter procedures in the `sdl2`
module that accept a symbol or list of symbols, are programmed to also
accept the equivalent integer value.

When might you want to use integer constants instead of symbols? Here
are some example situations:

- You need to integrate with a library that expects integers.
- You need to do bitwise math on a bitfield, such as a joystick hat
  position or a pixel format enum.
- You need to optimize a tight loop and can't afford the overhead of
  converting between integers and symbols.

In such situations, you can import the integer constants from
`sdl2-internals`, and use the "raw" procedures.


## Function and C macro bindings

The `sdl2-internals` module exports hundreds of bindings to SDL
functions and C macros. They are exported with their original C names,
e.g. `SDL_CreateRGBSurface` and `SDL_MUSTLOCK`. Functions and C macros
are both defined as foreign lambdas.

Funciton and C macro bindings are generally API-stable and very rarely
change, so it is usually safe to import and use them in your program.
However, on rare occasion the return type or argument type of the
binding might be changed, for example from int to bool.

**CAUTION:** Some functions and C macros only exist in certain
versions of SDL. If chicken-sdl2 is compiled with an older version of
SDL, the binding may not be defined in the `sdl2-internals` module, or
it may be defined to always return a nonsense value such as -1, 0, or
a null pointer. Bindings are only guaranteed to have correct behavior
when chicken-sdl2 is compiled with a version of SDL that defines them.
You should check the SDL version with `version-at-least?` before
calling a function or C macro that may not be defined.

Some functions are very easy to use, but others are quite a pain. You
may have to allocate some memory, pass a pointer to that memory as an
argument, then extract the value from the pointer afterwards to get
the result, and then free the memory you allocated. The `sdl2` module
is designed to spare you from this sort of pain, but if you are an
experienced programmer you can sometimes achieve greater efficiency by
directly calling the functions.

Because chicken-sdl2 defines foreign types using wrapper and
unwrappers (see above), you can pass either a struct record instance
(e.g. an `sdl2:surface`) or a raw pointer to most functions that expect
struct pointers. And, most functions that return a struct pointer will
actually return a struct record wrapping the pointer.
