
Things to do before, during, and after releasing a new version of the egg.

- Make sure there are no open issues for the current milestone.
  If there are, either finish them or change their milestone.

- Update sdl2.setup.
    - `known-egg-versions`
    - `known-sdl-versions` if a new SDL version has been released

- Make sure unit tests, demos, examples, and related libraries work,
  using a clean build (`make uninstall clean install test`).

- Make sure the in-repo docs are complete, accurate, and up to date.
    - README
    - CHANGELOG
    - docs/enums.md
    - Others

- Make sure [the wiki page](wiki.call-cc.org/eggref/4/sdl2) is complete, accurate, and up to date.
    - Docs for all new features in the latest version.
    - Indicate which egg version and SDL version each new feature requires.
    - Version feature identifiers for latest egg version and SDL version (if needed).

- Update the wiki page "version history" section, including the release date.

- Copy the final wiki page contents to docs/sdl2.wiki.txt (preserving the notice at the top).

- Update the version number and release date.
    - README
    - CHANGELOG
    - lib/version.scm
    - sdl2.release-info

- Commit, tag, and push.

- Close the milestone.

- Announce.
    - chicken-users mailing list
    - Blog
    - Social networks
