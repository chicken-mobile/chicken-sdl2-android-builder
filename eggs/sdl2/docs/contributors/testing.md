# Testing chicken-sdl2

This document gives an overview of motivation and methods for testing
chicken-sdl2. It is intended for contributors working on chicken-sdl2.


## Why test chicken-sdl2?

The purpose of testing chicken-sdl2 is to verify that chicken-sdl2
itself is correct, and to find and prevent bugs in chicken-sdl2's
code. The purpose is **not** to verify that SDL is correct, but rather
that our bindings are correct, and that our own procedures and macros
behave as they should.

Testing helps us find existing bugs so that we can fix them, and to
catch future bugs as soon as possible. This improves code quality, so
that chicken-sdl2 will be useful, reliable, and not frustrating.
Testing also provides a safety net so that we can make changes to
chicken-sdl2 without worrying that we may have accidentally broken
something. Finally, tests allow users to quickly verify that
chicken-sdl2 is properly installed and working correctly on their
computer.


## Kinds of testing

There are several kinds of testing used on the chicken-sdl2 project:

- Informal testing in the REPL during development of a feature, to
  check that the feature seems to behave as expected.
- Demos and example programs that users can verify behave as expected.
- Guided test programs that ask a human tester to perform a series of
  actions and/or verify that the expected responses occur.
- Unit tests that automatically verify that each feature is behaving
  as expected.

Ideally, we would have unit tests for every feature that is possible
to test automatically, and guided test programs for every feature that
requires human interaction or verification. In reality, due to limited
time, energy, and motivation, we sometimes must temporarily rely on
the less rigorous methods of testing.

Informal testing in the REPL, and demos and example programs, are both
fairly self-explanatory, so they are not discussed further in this
guide.


## Guided test programs

Guided test programs are programs that ask a human tester to perform a
series of actions and/or verify that the expected responses occur. For
example:

- The program changes the window to fullscreen mode, and asks the user
  to verify that it worked.
- The program asks the user to resize the window, then the program
  verifies that the expected event occurs and has sensible values.
- The program asks the user to plug in a joystick, then asks the user
  to verify that the reported joystick name, number of buttons, etc.
  is correct.

To save human tester time, guided test programs should focus on
verifying that each feature generally works as expected, not
exercising every possibility and edge case.

Because guided test programs require more human time and effort to
run, they should only be used for features that *require* a human
tester to perform an action, verify a result, or both. Any feature
that can be performed and verified entirely by the computer should be
tested via unit tests instead.


## Unit tests

Unit tests are programs that automatically exercise each feature and
verify that the expected outcome occurs. Broadly speaking, there are
four common methods a unit test may use to verify the outcome:

1. **Return Values:** Verify that the expected values are returned.
2. **Exceptions:** Verifying that the expected exceptions are signaled.
3. **Side Effects:** Verify that the expected side effects occured.
4. **Spying:** Verify that another procedure was called.

Verifying the return value should be the first resort, because it is
cleanest and easiest. If the feature cannot be adequately tested by
verifying only the return value, then you should try to also verify
the side effects. If it is not possible to verify the side effects,
then as a last resort you should use spying to verify that another
procedure was called. And, all procedures that may signal an exception
should be tested by verifying exceptions.

These methods are not mutually exclusive. For example, it is common to
test a feature by verifying its return values, side effects, *and*
exceptions.

An additional method, mocking, is described below. Mocking is not a
verification method, but rather is a set-up method that can help you
test circumstances that are unpredictable or difficult to set up.


### Verifying return values

When testing a procedure that returns a value, you can write a unit
test that verifies that the expected value is returned under certain
circumstances.

Most procedures that have no side effects can be adequately tested
this way. For example, `rect-x` either returns the correct value or it
doesn't. It does not have any side effects or depend on any global
state, it only depends on the argument given.

Other procedures require some set-up to prepare the global state. For
example, when testing `was-init`, you might first verify that it
returns an empty list (before SDL has not been initialized), then call
`sdl-init!` with some flags and verify that `was-init` now returns the
expected flags, then call `quit-subsystem!` and verify that `was-init`
reflects that change, and so on. (When features are closely tied to
each other, it may be simplest to test them all together.)


### Verifying side effects

Some features cannot be adequately tested by verifying the return
value alone. For example, to adequately test that `rect-set!` works,
you need to verify that the rect's fields were indeed modified as
expected. (In the case of `rect-set!`, you would *also* verify the
return value: that it returns the same rect object it was given.)

Other examples of verifying side effects:

- Verifying that `surface-ref` returns the expected pixel values after
  calling `blit-surface!`.
- Verifying that the dest-rect argument was modified after calling
  `blit-surface!` under certain circumstances.
- Verifying that a file was created on disk after calling `save-bmp!`.
- Verifying that `poll-event!` (eventually) returns the equivalent
  event that was pushed with `push-event!`.
- Verifying that an event can be created with a user event symbol
  after calling `register-events!`.
- Verifying that `window-size` returns the expected values after
  calling `window-size-set!`


### Verifying exceptions

Most procedures will signal an exception under certain circumstances.
The exceptions that a procedure signals are part of its public API, so
it is important to test that the expected exceptions occur under
various circumstances.

For example:

- Verifying that an `(exn type)` exception is signalled when given an
  invalid argument type.
- Verifying that an `(exn arity)` exception is signalled when given
  the wrong number of arguments.
- Verifying that an `(exn bounds)` exception is signalled when given
  an out-of-bounds value.
- Verifying that an `(exn i/o file)` exception is signalled when a
  file cannot be loaded or saved.
- Verifying that an `(exn sdl2)` exception is signaled when a SDL
  function fails for some other reason.

It is usually not worthwhile to verify the exact wording of error
messages, because the wording might be tweaked over time. But, it is
sometimes be worthwhile to verify that the error message contains
certain key words.

For example a procedure that takes two arguments, one sdl2:surface and
one sdl2:rect, might signal `(exn type)` if either argument is the
wrong type, but only the error message indicates which argument the
exception was related to. In such a case, you might verify that the
message contains the word "surface" (when testing the sdl2:surface
argument) or the word "rect" (when testing the sdl2:rect argument).


### Spying

Spying is the last resort in unit testing chicken-sdl2, for features
that cannot be automatically verified by either return values or side
effects.

The idea behind spying is that when you call procedure A, you expect
procedure B to also be called. As a bad example, a test might call
`init!` with certain symbol flags, then verify that `SDL_Init` was
called with the corresponding integer flags. Thus, we can at least be
sure that `init!` itself was programmed correctly, even if we cannot
directly verify that `SDL_Init` worked. (This is a bad example,
because we actually *could* verify the side effects, by calling
`was-init` and checking that the expected subsystems are initialized.)

As of this writing, no tests have needed to use spying, so we have not
yet created the tooling to easily do spying. But, the basic idea is
that before calling procedure A, you would use
[`mutate-procedure!`](http://wiki.call-cc.org/man/4/Unit%20lolevel#mutate-procedure)
to temporarily replace procedure B. The "spy B" would be programmed so
that if it is called, it causes a verifiable effect, such as signaling
an exception or modifying a variable. Then you call procedure A, and
if the effect occurs then you know that procedure B was called. After
the test, you reset procedure B to its original definition.

Ideally, spying should be used to verify that the procedure was called
with the correct arguments, not just the fact that the procedure was
called at all.

The reason spying is the last resort, is because it is a lot of effort
to set up, can break other tests if you don't properly reset the
procedures afterwards, and it only performs a "shallow" sort of
verification. But, it is better than having no verification at all.


### Mocking

Mocking is similar to spying, but it is used to *modify the behavior*
of a secondary procedure, not verify that the procedure was called.
Mocking is not a verification method, but rather a set-up method that
can help you test circumstances that are unpredictable or difficult to
set up through other means.

The idea behind mocking is to simulate a certain circumstance, for
example to test the behavior of `load-bmp` when an image fails to
load. Instead of actually creating an image that will fail to load, we
could override the behavior of `SDL_LoadBMP` to simulate it, and then
verify that `load-bmp` behaves as expected. For example, we could use
`mutate-procedure!` to replace `SDL_LoadBMP` with a procedure that
calls `set-error!` and then returns a null sdl2:surface, simulating
the actual behavior of `SDL_LoadBMP` when an image fails to load. Then
we could call `load-bmp` and verify that it handles that circumstance
as expected. Of course, after the test you must be sure to reset the
mocked procedure to its real behavior.

The benefit of mocking is that it allows you to easily test
circumstances that are unpredictable or difficult to set up through
other means. But the risk of mocking is that the test can become out
of touch with reality. For example, suppose we mistakenly believed
that `SDL_LoadBMP` returns -1 if loading fails, and wrote our mock
procedure to have that behavior. Our test would then be invalid,
because it is not testing realistic behavior. Even worse, someone
might assume the test is valid, and change the behavior of `load-bmp`
to make the test pass, thereby introducing a bug!

So, mocking should be used with caution, and only if testing the real
circumstance is not practical.
