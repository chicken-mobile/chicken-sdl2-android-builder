# Understanding chicken-sdl2's code

This guide is intended for people who are developing the code of
chicken-sdl2 itself. It provides helpful information to help you
understand how the code works, and why things are the way they are.
For example, it describes the structure of the codebase, and what the
important macros are.



## Module organization

chicken-sdl2 is organized into two modules:

- The `sdl2` module provides convenient, CHICKEN-idiomatic access to
  SDL functionality. It is intended for users to import into their own
  programs.
- The `sdl2-internals` module provides types, structures, enums, and
  low-level bindings to functions and C-macros. This module is the
  foundation for the `sdl2` module, and some parts of it can also be
  used by other libraries to integrate with chicken-sdl2.



## The `sdl2` module

The `sdl2` module is intended for users to interact with. It should be
convenient to use, idiomatic to (CHICKEN) Scheme, and require little
or no knowledge of FFI or C semantics. But, it should still closely
map onto the semantics of SDL. In other words, it is a convenient way
of accessing SDL, not a new library or framework on top of SDL.

Here are some specific guidelines:

- Procedures have Scheme-style names
  - Names are kebab-case, with no "SDL_" prefix. (The user can choose
    a prefix when importing the module. "sdl2:" is recommended.)
  - Boolean predicate procedures end with `?`
  - Procedures that cause a mutation or side effect end with `!`
    - Setters are of the form `{type}-{attr}-set!` (e.g.
      `rect-x-set!`)
  - Constructors are of the form `make-{type}` (e.g.
    `make-rect`)

- Instead of using integers for everything, use descriptive types
  - `#f` and `#t`, instead of using 0 and 1 as booleans
  - Symbols instead of integer enums (e.g. for keys)
  - Lists of symbols instead of bitfields (e.g. for flags)
  - **However**, procedures should *also* accept integers in these
    cases, for compatibility.

- Instead of using "output parameters" (pointers whose value is
  modified by the function), procedures properly return a value, or
  multiple values using the (values ...) construct, e.g. when
  retrieving the x and y of a point.
  - When returning multiple values, generally you should follow the
    same left-to-right order as the SDL function: the function's
    return type first (if any), followed by the left-most output
    parameter, then the next output parameter, and so on. However,
    there may be exceptions to this, for example if one of the output
    parameters is much more important than the function's return
    value, then maybe that output parameter should be the first value
    returned from the procedure.

- Trailing arguments can be made optional if there is an obvious and
  good default value for them. But, keep the same order of arguments
  as in SDL.

- In general, avoid keyword arguments when wrapping SDL functions. The
  minor convenience they might provide in some cases, is not worth
  making it less straightforward to follow SDL tutorials and examples.
  But, keyword arguments might be okay in certain procedures, such as
  complex constructors.

The general strategy for growing this module is:

1. Choose a useful function binding from `sdl2-internals`.
2. Define a new procedure in `sdl2` that calls the original function.
3. Repeat.

Step 2 allows us to change the function's name, interface, and
behavior to fit the guidelines above. Some functions are so simple
that the only thing that needs to be changed is the name. In such
cases, you should still create a new procedure that calls the original
function:

```
;; Recommended:
(define (show-window window)
  (SDL_ShowWindow window))

;; Not recommended (but works):
(define show-window SDL_ShowWindow)
```

Why do it the recommended way? This determines the procedure name and
argument names that are printed out in the REPL and in backtraces, so
it improves interactive development and debugging. There is a small
cost (an extra procedure call) to doing it this way, so it is possible
that we may change in the future, for efficiency. But for now, this is
how we do it.



## The `sdl2-internals` module

The `sdl2-internals` module is much larger and more complex than the
`sdl2` module. It provides many things:

1. Foreign type definiitons, which are used to specify the argument
   and return types of function bindings.
2. Enums and constants, plus procedures to convert them to/from
   symbols.
3. Record types (in the SRFI-9 sense) that are used to wrap struct
   pointers.
4. Struct record helpers for managing (e.g. allocating and freeing)
   record types that wrap struct pointers, and accessors for getting
   and setting field values.
5. Low-level bindings to functions and C-macros.

These things are described in more detail below.

The `sdl2-internals` module is not intended for users to import into
their programs. However, parts of it are available for other libraries
to use to integrate with chicken-sdl2. For example, a CHICKEN library
providing bindings to the SDL2_Image library could import wrappers and
unwrappers so that it could work with `sdl2:surface` struct records.


### Aside: "%" (percent sign) procedures

Before I go further, I should mention that this module has a lot of
procedures that start with "%". You may be wondering why!

The "%" is used to mark the fact that the procedure is not intended
for users. Many of these procedures *must* be exported, so that they
can be used by other chicken-sdl2 modules if needed. Because they are
exported, it is *technically possible* for users to use them, but
users should not do so. The procedures should only be used within the
implementation of chicken-sdl2.

The character "%" itself has no significance except that it makes the
procedure names look strange, so users will think twice about using it
in their code.


### Foreign type definitions

[Foreign type definitions](http://wiki.call-cc.org/man/4/Accessing%20external%20objects#define-foreign-type)
are used to declare a foreign type so that you can use it to specify
an argument or return value type in a foreign function binding. In
chicken-sdl2, foreign type definitions are used in several ways:

- Declaring type aliases for generic types, so that we can use the
  same type name in our bindings as is used in SDL, so it's easier to
  see that our bindings match SDL. E.g. the `Sint8` type is an alias
  for CHICKEN's standard foreign type `byte`.
- Declaring type aliases for enums and other integer-like types, for
  basically the same reason as above (and also to make bindings more
  descriptive). E.g. `SDL_EventType` is an alias for `Sint32`.
- Declaring type aliases for struct pointers, for auto-conversion
  and brevity. E.g. `SDL_Surface*` is an alias for `(c-pointer
  "SDL_Surface")`, but auto-converts to/from the `sdl2:surface` record
  type.

The last use case is the most interesting, because CHICKEN allows us
to define procedures that are used to automatically convert to or from
a foreign type. In chicken-sdl2, this is used to automatically convert
struct pointers to and from record types. This is why you can (for
example) pass an `sdl2:surface` record instance to a function that
expects an `SDL_Surface*` pointer, and why functions that return
`SDL_Surface*` pointers actually return `sdl2:surface` record
instances. More about record types below.


### Enums and integer constants

Enums and other integer constants are integer values that have special
significance. SDL uses integer constants for many things, such as:

- Event types
- Key and button IDs
- Window and surface flags
- Subsystem initialization flags
- Command modes (e.g. with `SDL_PeepEvents`)
- Status indicators (e.g. with `SDL_GetAudioDeviceStatus`)

In SDL, integer constants are defined either as an enum (with
`typedef enum`), or as a preprocessor definition (with `#define`). But in
chicken-sdl2, both kinds of integer constants are bound in the same
way: as Scheme definitions holding integer values. The values are
determined at compile time, so they automatically match what is in the
SDL headers.

The `define-foreign-constants` macro is used to do this simple form of
binding. It also exports the definitions, to avoid error-prone
repetition of hundreds of constant names.

But, integer constants are not a very good interface (although they
are probably the best available solution for a C library). The integer
value can become separated from its name, and thus lose meaning. For
example, if you query the `type` field of a particular event instance,
it may return the value 768, but it's not obvious that 768 means
`SDL_KEYDOWN`. Similarly, it is possible to accidentally use an
integer constant in the wrong context, e.g. using `SDL_BUTTON_RMASK`
(value 4) when you should have used `SDL_BUTTON_RIGHT` (value 3).
Since both are integers, the system would not detect the problem, but
would think you meant `SDL_BUTTON_X1` (value 4), causing your program
to behave incorrectly.

To avoid these issues, we want to allow users to use descriptive,
unambiguous types instead of integer constants. Symbols are a good
type for this, because they can have descriptive values, yet are
efficient to compare (unlike strings). We want to convert symbols to
integer values (for a given context), and vice versa. So, the user can
pass a symbol like `'right` as a mouse button ID, and it will be
treated as `SDL_BUTTON_RIGHT`. And, querying an event type would
return `'key-down` instead of 768.

The `define-enum-mappings` is meant to help with this conversion. It
defines and exports constants like `define-foreign-constants` does,
and also defines procedures for converting integer values to/from a
convenient symbol (e.g. converting 768 to the symbol `'key-down` or
vice versa).

`define-enum-mappings` implies that the constants defined together
*act like* an enum, i.e. they are related and do not collide with each
other. It is not necessary that the constants were defined in C using
`typedef enum`. For example, `define-enum-mappings` can be used with
`#define` style constants, too.

Some integer constants are intended to be used as bitmasks, for
example window flags and button masks. The bitmasks can be
bitwise-or'd together to create an integer bitfield, where each bit
has meaning. For example, in C you could specify the window flags
`SDL_WINDOW_FULLSCREEN | SDL_WINDOW_OPENGL` to request a fullscreen
OpenGL window. In chicken-sdl2, the best equivalent idiom is a list of
symbols, e.g. `'(fullscreen opengl)`.

The `define-enum-mask-packer` and `define-enum-mask-unpacker` macros
are used in such cases. They define procedures for "packing" a list of
symbols to get an integer bitfield, and for "unpacking" an integer
bitfield to get a list of symbols.

Unlike struct record types (described below), which are automatically
converted to/from struct pointers by the CHICKEN foreign type system,
enums are not automatically converted by the foreign type system.
Instead, they are converted by calling the conversion procedures
within procedures in the `sdl2` module (and in some accessors in the
`sdl2-internals` module).

The reason that enum conversion is not done automatically by the
foreign type system is that enum conversion is not always
straightforward. It sometimes depends on the context. The foreign type
system is not well-suited for handling this nuance.

The macros `define-enum-accessor` and `define-enum-mask-accessor` help
define struct filed getters and setters that automatically convert
enums to/from symbols (or lists of symbols).

For procedures and function bindings that return enum symbols, we
often provide "raw" versions that return the integer value instead of
a symbol. This allows the user to opt out of the (small) overhead of
converting from integer constant to symbol. Procedures and function
bindings that accept enum symbols, are also programmed to accept
integer values, again allowing the user to opt out of the conversion
overhead.


### Struct record types

Record types (from SRFI-9) are a data structure that has named fields
that can hold data. In chicken-sdl2, record types are mostly used to
wrap struct pointers. For example, an `sdl2:surface` record type
instance holds a pointer to an `SDL_Surface` struct instance.

As mentioned earlier, chicken-sdl2's foreign type definitions
automatically convert struct pointers to/from record types as
needed. So, it is generally not necessary to manually access the
pointer.

Why have record types? Why not just use pointers directly? Well,
record types have some advantages:

- They are generally safer than raw pointers (fewer ways for users to
  shoot themselves in the foot).
- They provide a type predicate procedure (like `surface?`) so you
  can test what type it is. Useful for type assertions (to catch bugs
  early), and type-based dispatch.
- They print nicely in the REPL (e.g. `#<sdl2:surface>`) and we can
  define record type printers to include extra information (e.g.
  `#<sdl2:rect (10 20 300 400)>`) to aid debugging and interactive
  development.

Record types add a little memory and computational overhead, but I
think the trade-off is worth it for most users.

There are several aspects to a struct record type (although not all
struct record types have all of these aspects):

1. Defining the struct record type itself.
2. Defining struct memory helpers to allocate and free the struct, if
   needed.
3. Defining a record printer to aid debugging and interactive
   development.
4. Defining struct field accessors to get and set the value of each
   field in the struct, if needed.
5. Defining extra convenience procedures, if needed.

#### Defining struct record types

A struct record type is simply a record type that holds a pointer to a
C struct. Because this pattern is so common in chicken-sdl2, we have
the `define-struct-record-type` macro to help.

Currently, all struct record types have only one slot, the pointer. In
the future, it is possible that we might want to add extra slots to
some types (e.g. to hold user data or metadata), but the pointer must
always be the first slot. The `struct-null?` helper procedure (and
possibly other helpers in the future) depends on the pointer being the
first slot.

Struct record types have a wrapper procedure and an unwrapper
procedure. The wrapper and unwrapper are mainly used by CHICKEN's
foreign type system to do automatic conversion to and from a pointer.
The wrapper just takes a pointer and creates an instance of the record
type that holds the pointer; usually this is the same as the record
type constructor. The unwrapper primarily does the reverse, extracting
the pointer from a record instance. But, the unwrapper can also accept
a raw pointer or #f (meaning null pointer). This is so that the
foreign type definition will be more flexible, in case a user wants to
use raw pointers (for efficiency or some other reason).

#### Defining struct memory helpers

Struct memory helpers help manage the memory for a struct record. In
particular, they allocate or free the memory for a struct. Struct
record helpers are defined with the `define-struct-memory-helpers`
macro. It is explained in detail in code comments.

The procedures defined by `define-struct-memory-helpers` only manage
the memory for the struct itself. They will not recursively allocate
or free the memory for pointer fields. For complex structs, you may
want to define custom memory helpers that allocate/free nested struct
pointers.

Some structs are allocated and/or freed using functiosn provided by
SDL. In such cases, there is no reason to define memory helpers. For
example, instances of `SDL_Surface` are created with e.g.
`SDL_CreateRGBSurface`, and freed using `SDL_FreeSurface`. So, we
wouldn't define allocators for surfaces. But, we would define
`free-surface!` (the wrapper around `SDL_FreeSurface`) to set the
struct record pointer to null after it is freed, for safety.

#### Defining struct field accessors

Struct field accessors are getters and setters, i.e. procedures that
you use to get or set the value of the field of a struct. E.g.
`rect-x` returns the x value of an `rect` record type
instance, while `rect-x-set!` sets the x value.

Struct field accessors are usually defined with the
`define-struct-field-accessor` macro. It is explained in detail in
code comments.

Some fields are documented in SDL as being for "internal use". Users
are not supposed to access those fields, so we do not define either
getters or setters for them.

Some fields are read-only, because it is not safe for users to set
them directly. For example, setting the w or h of an `sdl2:surface`
would cause bad problems. In such cases, we define only a getter, not
a setter.

For every field that has a setter, the getter is enhanced to work with
`set!`. E.g. `(set! (rect-x my-rect) 42)` is equivalent to
`(rect-x-set! my-rect 42)`.

Field setters sometimes have **guards**, which check that the value
being set is valid. For example, the guard for an unsigned 8-bit
integer (i.e. Uint8) field would assert that the value is an integer
between 0 and 255. If you try to set a field to an invalid value
(wrong type, or out of range), the guard will throw an error with an
informative message.

It is not possible to get or set a field if the struct pointer is
null. Attempting to do so will cause an error to be thrown. It is
advisable to test the struct with `struct-null?` before trying to
use it.

#### Defining struct record printers

Struct record printers define how to print a struct record, e.g.
`#<sdl2:rect (1 2 3 4)>`. The purpose of record printers is to aid
debugging and interactive development, so that developers can see
relevant information at a glance.

Struct record printers are usually defined with the
`define-struct-record-printer` macro. It is explained in detail in
code comments.

Every struct record printer shows at least the record type name, e.g.
`#<sdl2:rect>`.

Printers may also show the pointer address. This is configured on a
per-type basis. The pointer address is always shown for opaque types
(types which have no user-visible fields), so that users can
distinguish between instances. It is also usually shown for complex
types with many fields, unless there is a better way (like an ID
number) to distinguish between instances. The pointer address is
usually *not* shown for simple types where all of the fields can be
easily shown.

Printers may also print zero or more values, usually the values of
fields. These values may optionally have a label. For simple structs,
you may want to just print out *all* the field values. For more
complex structs, print only a few values that will be the most useful
to developers when debugging.

If the struct's pointer is null (address 0), the printer shows only
the record type and the word "NULL". This is because it is impossible
to get any field values when the pointer is null, so the only useful
information we can display is the record type and the fact that it is
null.

#### Defining extra convenience procedures

There are a few structs in SDL that are very simple: they just contain
some integers. For example, `SDL_Color`, `SDL_Rect`, `SDL_Point`. It
is common for users to manually construct and directly manipulate
instances of these structs, rather than doing so via functions
provided by SDL.

So, we provide some extra convenience procedures for working with
these simple structs:

- A constructor that takes zero or more field values in order. Omitted
  fields get their default value, which is usually 0. E.g.
  `(make-rect 1 2 3 4)`.
- A mass setter that sets zero or more fields, e.g.
  `(rect-set! my-rect 1 2 #f 4)`. The value `#f` means no change
  to that field.
- A procedure that returns all the field values as a list. E.g.
  `(rect->list my-rect)`

Other types may have their own extra convenience procedures, as
needed.


### Event types

SDL events are fairly complicated, and that complexity is reflected in
the way they are handled in chicken-sdl2. So, even though they are
fundamentally just record types and constructors and accessors like
others, they are complex enough to need their own explanation.

In SDL, `SDL_Event` is not a struct, but rather a union of all the
possible event structs. They have all have a `type` field, which is
used to distinguish between the different event types. Aside from
`type` and `timestamp`, each event type has different fields. So, you
must first check the event's `type` to see what event struct to use,
then type-cast the event pointer to be the correct event struct. Then,
finally, you can access the event's fields.

In chicken-sdl2, there is a single record type, `sdl2:event`, that
wraps pointers to events, regardless of type. So, the `event?`
predicate returns true for an event of any type.

But, we also provide a large number of event-type-specific predicates
and accessors for using the fields of each event type. The core of all
this is the `define-event-type` macro. It is used once for each
type of event (or group of related types, such as key-down and
key-up). It defines:

- A constructor for allocating and initializing an event of that type.
  It accepts as arguments the event type (so you can distinguish
  between e.g. key-up and key-down) and each field used by this event
  type.
- A predicate for checking whether on object is an event with a
  matching type.
- A record type printer that prints out certain fields, to aid
  debugging and interactive development.
- Getters and setters for each field. For safety, they assert that the
  object passes the predicate. So, you can't use a getter or setter on
  an event of the wrong type.

It's worth noting how the record type printers work. Technically there
is only one record type printer, because there is only one record
type, `sdl2:event`. But that printer performs a hash table lookup based
on the event type, so we have custom printing behavior per event type.


### Function and C-macro bindings

The `sdl2-internals` module defines low-level bindings to SDL functions and
C-macros. These bindings are exported using the full, C-style name of
the function, as it appears in SDL, e.g. `SDL_CreateWindow`.

The bindings are defined using custom macros that act as abstraction
layers around
[CHICKEN's built-in FFI support](http://wiki.call-cc.org/man/4/Interface%20to%20external%20functions%20and%20variables)
(so it is useful to be familiar with that too). The custom macros are
described later in this document. `define-function-binding` is the
most important macro for function bindings.

Although the bindings are very direct and low-level, they do utilize
CHICKEN's foreign type facilities to automatically "convert" (unwrap)
record types like `sdl2:surface`. In other words, you can pass an
`sdl2:surface` record instance to a function expecting an
`SDL_Surface*` pointer. This is a result of how the foreign types are
defined.

Boolean values are also converted at this level, thanks to CHICKEN's
`bool` foreign type. In other words, you can pass `#f` and `#t` in
places where 0 and 1 are used as booleans.

Some SDL functions use "output parameters" to "return" multiple values
(like `SDL_WindowPosition`'s x and y). To use a binding like that, you
must allocate a new value of the correct type, pass a pointer into the
function, then read the values out of the pointers afterwards. This is
awkward, so the `sdl2` module tries to spare users from it. See the
`window-position` function in the `sdl2` module for an example of
how output parameters are handled.

C-macros are bound by defining a function with a custom body (using
the `define-function-binding*` macro) that calls the C-macro and
returns the result. The defined function has the same name as the
C-macro in SDL, e.g. `SDL_MUSTLOCK`.

### Version-dependent bindings

Some functions and C-macros are only available in a certain version of
SDL or later. In such case, the binding should be defined using
`define-function-binding*` with a body like this:

```scheme
(define-function-binding* SDL_TICKS_PASSED
  return: (bool passed?)
  args: ((Uint32 a) (Uint32 b))
  body: "
#if SDL_VERSION_ATLEAST(2, 0, 1)
  C_return( SDL_TICKS_PASSED(a, b) );
#else
  C_return( 0 );
#endif")
```

This means that the call to `SDL_TICKS_PASSED` will be removed by the
C preprocessor, which avoids a compiler error when compiling
chicken-sdl2. The "else" branch returns any value that fits the return
type of the function. The value doesn't really matter because the
function won't be called.

Then in the sdl2 module, the wrapper is defined with the
`defined-versioned` macro, like so:

```scheme
(define-versioned (2 0 1) (ticks-passed? a b)
  (SDL_TICKS_PASSED a b))
```

Procedures defined this way will throw an informative error when
called with an unsupported version. The procedure body is only
executed if the version number is high enough.



## Overview of the macros

chicken-sdl2's internal implementation relies heavily on macros. Some
of the macros are quite complex and may be hard to understand at
first. This section is meant to give an overview of the important
macros, to help you understand why they exist and what they do.

This section does not go into the details of each macro. For that
information, there are (or should be) comments in the source code for
each macro. This section is meant to provide the context so that you
can better understand those comments.


### General rationale for macros

The SDL library has
[more than 500 functions and C-macros, more than 50 structs (thus hundreds of fields), and hundreds of enum values and constants](http://wiki.libsdl.org/CategoryAPI).
Creating bindings for even a fraction of SDL would be extremely
repetitive and nearly unmaintainable without the aid of macros.

Macros allow us to extract the common patterns involved, so that they
can be repeated with less effort and fewer mistakes. And, macros allow
us to make the code more clear and descriptive, so that you can easily
see the important details.

However, macros in chicken-sdl2 **must not** produce any identifiers
that were not explicitly provided in the macro invocation. It is very
tempting to make a macro that generates function names (e.g. combining
`rect` with `x` and `y` to make `rect-x` and `rect-y`),
but that is *too clever*, and makes the code harder to maintain. It
must be possible to locate the origin of each function, constant, etc.
by searching the codebase for its full name (aka
[the Grep Test](http://jamie-wong.com/2013/07/12/grep-test/)). Yes, it
is a bit tedious to type out every name explicitly, but that is better
than having a mysterious codebase where you can't find anything!


### Macro style (keyword arguments)

Many macros in chicken-sdl2 use keywords to label macro arguments.
This is meant to *resemble* the usage of keyword arguments when
calling procedures, but they are not truly keyword arguments. In
particular, **the order of arguments matters**, and **most arguments
are required**.

For example, the following macro call uses the keywords `types:`,
`pred:`, `print:`, `type:` `getter:`, and `setter:`.

```
(define-event-type "SDL_DropEvent"
  types: (SDL_DROPFILE)
  pred:  drop-event?
  print: ((file drop-event-file))
  (file
   type:   c-string
   getter: drop-event-file
   setter: drop-event-file-set!))
```

The keywords make the macro invocation more clear and descriptive.
This is very important, because many macro calls in chicken-sdl2
require numerous arguments. Imagine how difficult it would be to make
sense of the above macro call if there were no labels. And how
difficult it would be to notice if you got the order wrong.

The keywords also make it possible for us to make certain arguments
optional, without having ambiguous semantics. If we didn't use
keywords, it would only be possible to make arguments optional if they
were at the end. (That said, optional arguments tend to make the macro
implementations much more complex, so it's often best to make all
arguments required.)


### Macros for binding C functions

`define-function-binding` is the is the primary way of defining a
low-level binding to a C function in chicken-sdl2. It is essentially a
wrapper around CHICKEN's built-in
[`foreign-lambda`](http://wiki.call-cc.org/man/4/Accessing%20external%20objects#foreign-lambda),
but it also `define`s a name.

`define-function-binding*` is similar, except that it allows you to
specify a custom body of C code. It is based on `foreign-lambda*`
(note the asterisk). It is often used for wrapping C macros in a
function so that they can be called from Scheme. It is also used in
special cases where a custom function body is useful, for example to
do pointer stuff that would be much harder in Scheme.

`defined-versioned` is used to define procedures in the `sdl2` module
that require a certain minimum version of SDL. Depending on the
version of SDL that chicken-sdl2 was compiled with, such procedures
will either execute their body or throw an error.


### Macros for binding C enums and constants

`define-foreign-constants` is used to define one or more enum values
or constants (usually integers) so that you can access them from
within Scheme. It also automatically exports all the definitions.

`define-enum-mappings` is similar, but it also defines procedures for
converting an integer value to/from a convenient symbol. These
conversion procedures are used in the `sdl2` module to convert symbols
to/from integer constants, for user convenience.

`define-enum-mask-packer` and `define-enum-mask-unpacker` are used to
define procedures that convert a list of enum symbols to/from an
integer bitfield. They are used for enums that are used as bitmasks in
C, such as window flags and button masks.

`define-enum-accessor` and `define-enum-mask-accessor` are used to
define struct field getters and setters that automatically convert
enums to/from symbols (or lists of symbols).

These macros are described above in the section about enums and
integer constants, and documented in detail in code comments.


### Macros for wrapping C structs

`define-struct-record-type` is used to define a record type that wraps
a pointer to a C struct.

`define-struct-memory-helpers` is used to define helper procedures for
allocating and freeing memory for a struct record.

`define-struct-field-accessors` is used to define field accessors to
get and set the value of struct fields.

`define-struct-record-printer` is used to define a record printer for
a struct record, which determines how record instances are printed in
the REPL and error messages.

`define-event-type` is a specialized macro, used to define
constructors, predicates, and field accessors for SDL event structs.


### Other macros

`foreign-lambda*-with-dynamic-body` is like `foreign-lambda*`, except
the function body string is constructed dynamically (at macro
expansion time) using `sprintf`. It is used within other macros to
generate function bodies based on a template. For example, a type or
field name within the function body can be filled in based on the
arguments given to the high-level macro.
