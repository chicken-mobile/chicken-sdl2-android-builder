# Contributing to chicken-sdl2

This guide provides information about the many ways you can contribute
to chicken-sdl2. This project is a volunteer effort, and your help is
appreciated!

**Table of contents:**

- [**Conduct**](#conduct) - our expectations regarding participant
  conduct
- [**Getting involved**](#getting-involved) - there are many ways to help
- [**Filing issues**](#filing-issues)
  - [**General guidelines for filing issues**](#general-guidelines-for-filing-issues) - guidelines for issues of any type
  - [**Filing support requests**](#filing-support-requests) - when you need help
  - [**Filing feature requests**](#filing-feature-requests) - when you need a feature
  - [**Filing bug reports**](#filing-bug-reports) - when you find a bug
  - [**Filing documentation issues**](#filing-documentation-issues) - when the docs need improvement
- [**Issue labels**](#issue-labels) - how issues are organized
- [**Contributing code**](#contributing-code) - things you should know


## Conduct

We are committed to making participation in this project a welcoming
and harassment-free experience.

All project participants are expected to abide by the
[Contributor Code of Conduct](CODE_OF_CONDUCT.md). This includes (but
is not limited to) anyone filing an issue or posting a comment. See
the Contributor Code of Conduct for details.


## Getting involved

There are many ways to get involved in the project, whether you are an
experienced programmer or not. You don't have to make a commitment
&mdash; one-time or occasional involvement is perfectly fine.

Here are some ways you could help:

- Writing tutorials, guides, and reference documentation
- Creating demos and small example games
- Responding to support requests (answering questions, helping people
  install, etc.)
- Triaging and organizing issues (applying appropriate labels,
  checking that bug reports have enough info, etc.)
- Writing unit tests
- Investigating and fixing bugs
- Adding convenience bindings to the `sdl2` module
- Adding low-level bindings to the `sdl2-internals` module
- Refactoring, cleaning up, or optimizing the codebase

If you would like to get involved, please email a maintainer (email
address listed in the [README](README.md)). Please tell us how you
would like to help, and a little about yourself (e.g. related skills
and experience), so that we can suggest some tasks or activities that
would be a good fit for you. :)


## Filing issues

Filing issues in
[the project issue tracker](https://gitlab.com/chicken-sdl2/chicken-sdl2/issues)
is the best way to report bugs, request features, ask for help, or
tell us about anything else that will help us improve the project.

We are pretty relaxed about how you file issues. As long as you
provide enough information for us to understand and resolve the issue,
it's good. But, following the guidelines below will help keep the
project running smoothly and efficiently, so we appreciate it if you
make the effort to follow them.

If you have participated in open source projects before, most of these
guidelines will be familiar to you. They are explained for the benefit
of anyone who is new to open source participation, or not sure what to
do.


### General guidelines for filing issues

Before filing an issue, make sure you are using (or trying to use) the
most recent version of chicken-sdl2. Then, spend a few minutes
searching the
[open and closed project issues](https://gitlab.com/chicken-sdl2/chicken-sdl2/issues?scope=all&sort=created_desc&state=all),
to check whether someone has already requested the same feature,
reported the same bug, had the same problem, etc.

If you see an *open* issue that is relevant to you, post a comment
like "+1" or "I need this feature too" or "This bug affects me too."
This helps us gauge interest/impact so we can prioritize issues. Of
course, you can also include other details in your comment. For
example, if a bug affects you, please mention what operating system
and versions of chicken-sdl2, SDL, and CHICKEN you are using.

Please be aware that project members may edit the title and/or
description of issues that you file. Don't worry, this does not mean
you filed the issue badly. It is just our way of staying organized so
we can work most effectively. For example, we might rephrase your
issue for clarity, add more information, or include common search
terms so other people will be able to find it more easily.


### Filing support requests

Everybody needs help sometimes. If the help you need is related to
installing or using chicken-sdl2, you may want to file a support
request.

(Be sure to read the
[general guidelines for filing issues](#general-guidelines-for-filing-issues)
above, if you have not already done so.)

Here are some examples of situations where filing a support request
would be appropriate:

- When you are having trouble installing chicken-sdl2 (or related
  software like SDL or CHICKEN Scheme)
- When you can't figure out how to use a certain feature or accomplish
  a certain goal using chicken-sdl2
- When you wrote a program that uses chicken-sdl2, but your program is
  not working correctly, and you can't figure out what is wrong

**Before you file a support request**, we ask that you please spend
one hour trying to figure it out yourself. For example, you might try
one or more of these approaches:

- Reading the [chicken-sdl2 reference docs](http://api.call-cc.org/doc/sdl2)
- Studying the
  [chicken-sdl2 demos](https://gitlab.com/chicken-sdl2/chicken-sdl2/tree/master/demos)
  and
  [examples](https://gitlab.com/chicken-sdl2/chicken-sdl2-examples)
- Searching the [open and closed support requests for chicken-sdl2](https://gitlab.com/chicken-sdl2/chicken-sdl2/issues?scope=all&sort=created_desc&state=all&label_name=support)
- Browsing the [SDL 2 reference docs](http://wiki.libsdl.org/APIByCategory)
- Reading some [SDL 2 tutorials](http://wiki.libsdl.org/Tutorials)

If you are still stuck after one hour, **it may be our fault, not
yours**. Our documentation may need to be improved, or you may have
found a bug that we need to fix. *Please file a support request* so we
can help you, and so we can improve the project so that other people
will not have the same problem in the future.

The **best way to file a support request** is to
[create a new issue](https://gitlab.com/chicken-sdl2/chicken-sdl2/issues/new)
on the project issue tracker. This makes it possible for many people
to help, so a solution can be found more quickly. Plus, when a
solution is found, it will be available online to help other people
who have the same problem you did.

So that we can help you most effectively, please provide as much of
the following information as you can:

- What you are trying to do
- What problem you are having
- What you have already tried doing to solve it
- What versions of chicken-sdl2, SDL, and CHICKEN Scheme you are
  using
- What operating system (including version) you are using
- Any error messages, warnings, or other messages that appeared
- If the support request is related to some code you wrote:
  - If the code is shorter than (roughly) 100 lines, paste the entire
    code directly in the issue description.
  - Otherwise, if the code is available in a public online location
    (e.g. a public repository), provide a link to it.
  - Otherwise, put the code in a
    [snippet](https://gitlab.com/snippets) (or other pastebin service)
    and provide a link to it.

**Tip:** Use
[GitLab's code and syntax highlighting](https://gitlab.com/help/markdown/markdown.md#code-and-syntax-highlighting)
to make example code, error messages, and logs easier to read.

We may have follow-up questions for you. Please either
[set up email notifications in your GitLab profile settings](https://gitlab.com/profile/emails),
or bookmark your issue and check back every few days until it is
resolved.

Please **allow several days for someone to respond** to your support
request. Everyone here is a volunteer, and may have a busy job or
other responsibilities. If no one has responded after one week, please
email a project maintainer directly (email address listed in the
[README](README.md)) with a link to your support request issue.

If someone posts a solution that solves your problem, please add a
comment saying that it worked. If you discover a solution to your own
support request, please add a comment explaining what you did to solve
it. This helps other people who may have the same problem in the
future.

Support request issues will be closed after the person who filed it
says their problem has been solved, or if there has been no new
activity for over one month.


### Filing feature requests

If you need a feature from SDL, but chicken-sdl2 does not provide
access to it yet, please file a feature request so we know that you
need it, and can prioritize our efforts accordingly.

(Be sure to read the
[general guidelines for filing issues](#general-guidelines-for-filing-issues)
above, if you have not already done so.)

Each feature request should be focused on a single feature, i.e. one
or more **closely related** functions, macros, structures, and/or
constants.

For example, the functions `SDL_GetRelativeMouseMode` and
`SDL_SetRelativeMouseMode` are closely related. They are both related
to the same functionality, and it would not make sense to add one
without the other. So, they belong together in one feature request.

For another example, the functions `SDL_CaptureMouse` and
`SDL_ShowCursor` are *not* closely related. Although they are both in
the broad category of "mouse support," they are related to distinct
and independent aspects of mouse support. So, they would go in two
separate feature requests.

If you are not sure whether to file one request or multiple requests,
it is better to file one request. A project member will review it, and
possibly split it into multiple requests if appropriate.

In the issue description, please list the name of every function,
macro, structure, or constant that is part of the requested feature,
using the original "C style" names from SDL (e.g. `SDL_Foo` or
`SDL_FOO`). This makes the feature request easier to search for.

#### Example feature request

> **Title:** Clipboard handling
>
> Support for handling clipboard text should be added.
>
> * SDL_GetClipboardText
> * SDL_HasClipboardText
> * SDL_SetClipboardText


### Filing bug reports

If you encounter a crash, error, or other strange behavior in
chicken-sdl2, please file a bug report to tell us about it.

(Be sure to read the
[general guidelines for filing issues](#general-guidelines-for-filing-issues)
above, if you have not already done so.)

To help us diagnose and fix the problem quickly, please provide as
much of the following information as you can:

- A detailed description of what happened, and what should have
  happened instead
- How often it happens (every time? half the time? only once?)
- Example code that reproduces the bug (i.e. makes it happen). Try to
  narrow the code down to a small, self-contained example.
- Any error messages that appeared in the console. Copy and paste as
  much of the message as seems relevant (if in doubt, include it all).
- What versions of chicken-sdl2, SDL, and CHICKEN Scheme you are
  using. If you installed chicken-sdl2 from the source repository,
  please mention which Git commit ID (run `git log -1` to find out).
- What operating system (including version) you are using

**Tip:** Use
[GitLab's code and syntax highlighting](https://gitlab.com/help/markdown/markdown.md#code-and-syntax-highlighting)
to make example code, error messages, and logs easier to read.

We may have follow-up questions for you. Please either
[set up email notifications in your GitLab profile settings](https://gitlab.com/profile/emails),
or bookmark your issue and check back every few days until it is
resolved.


#### Example bug report

> **Title:** create-window fails when given empty string
>
> When I call create-window with an empty string as the first
> argument, it throws an error (see below). The error happens every
> time. It should instead create a window with no title.
>
> Example:
>
> ```
> (require-extension sdl2)
> (create-window "" 0 0 800 600)
> ```
>
> This is what is displayed in the terminal:
>
> ```
> Error: bad argument ""
>
>	Call history:
>
>	example.scm:1: ##sys#require
>	example.scm:1: ##sys#require
>	example.scm:2: sdl2#create-window	  	<--
> ```
>
> I am using:
>
> * chicken-sdl2 version 0.1 (commit a1b2c3d4)
> * SDL 2.0.1
> * CHICKEN Scheme 4.9.0.1
> * Mac OS X Yosemite 10.10.4


## Filing documentation issues

If you notice that something in the official documentation is missing,
wrong, unclear, confusing, out of date, has a typo or broken link, or
otherwise needs improvement, please file an issue to let us know.

(Be sure to read the
[general guidelines for filing issues](#general-guidelines-for-filing-issues)
above, if you have not already done so.)

The official documentation includes (or hopefully will include in the
future):

- **API reference docs**: specific information about each individual
  function, macro, etc.
- **Guides and tutorials**: educational articles, often focused on a
  certain feature or goal
- **Demos**: example programs that demonstrate certain features
- **Example games**: small playable games or toy programs that provide
  an example of how to "put the pieces together" to make a complete
  application
- **Project-related documents**: the README, installation
  instructions, contribution guide, etc.

Bugs in demos or example games are consider documentation issues. They
do not get the "bug" issue label, which is reserved for bugs in the
code of one of chicken-sdl2's modules.

We can only improve documentation that is hosted in a project space
(e.g. the repository or wiki). If you notice a problem in
documentation hosted elsewhere (e.g. a tutorial on someone's blog),
you should contact the author of that documentation instead.


## Issue labels

Project issues are organized by attaching labels to them.

In most cases, you do not attach labels when filing an issue. Instead,
a project member will attach appropriate labels to the issue during
triage. However, project members may attach labels to issues they
file, to save time. (If you want to help with issue triage, please
contact a project maintainer so we can add you.)

Here are the
[labels used in this project](https://gitlab.com/chicken-sdl2/chicken-sdl2/labels),
and what they mean.

- Issue type labels:
  - **bug**: The issue is a bug report about an unexpected behavior or
    mistake in the library code.
  - **discussion**: The issue is intended primarily for discussion,
    e.g. for making high-level decisions about the project.
  - **feature**: The issue is a request to add a new feature.
  - **improvement**: The issue is a suggestion to improve existing
    code, e.g. adding new behavior, optimization, or code cleanup.
  - **support**: The issue is a support request.

- Aspect labels (what aspect(s) of the project is it related to):
  - **documentation**: The issue is related to documentation, such as
    reference docs, guides, tutorials, demos, and example games.
  - **tests**: The issue is related to the unit tests or other
    automated tests.

- Triage labels:
  - **duplicate**: The issue is a duplicate of another existing issue.
    An explanation and link to the original issue should be posted in
    a comment.
  - **needs-info**: The issue needs more information (usually from the
    reporter or other affected person) before we can proceed. What
    info is needed, and why, should be explained in a comment.
  - **priority-high**: The issue is high priority, e.g. due to popular
    demand (feature), or severe/widespread impact (bug).
  - **priority-low**: The issue is low priority, e.g. due to low
    demand (feature), or minimal/rare impact (bug).

Here are some example issues, and what labels would be appropriate:

- There is a bug in a function. Label: bug.
- There is a much-wanted feature. Labels: feature, priority-high.
- There is missing reference documentation for a function. Label:
  documentation.
- There is a typo in a tutorial. Label: documentation.
- There is a bug in a demo or example game. Label: documentation. (The
  "bug" label only applies to library code.)


## Contributing code

If you are interested in working on chicken-sdl2's code, you should
read the guide,
"[Understanding chicken-sdl2's code](docs/contributors/understanding-the-code.md)".
It provides helpful information to help you understand how the code
works, and why things are the way they are. For example, it describes
the structure of the codebase, and what the important macros are.
