# Contributing

Before starting to work on a feature or bug fix for Gren, it might be a good idea
to check if the change you're intending to make is a good fit for the project.

We use the `Help Wanted` tag on github issues to indicate that a PR would be
welcome. If you cannot find an issue for the change you're intending to make,
head over to our [Zulip](https://gren.zulipchat.com) and start a new topic for your
idea in either the `#language-design` or `#api-design` streams.

We like to talk things through before commiting to a change, so that's a good way to go about
suggesting new features. Also, we're a friendly bunch, so don't be afraid to say hi.

All PRs will be considered, but by going through the above process you increase
your chances for a merge significantly.

## Local development commands

- build: `cabal build -f dev`
- run tests: `cabal test -f dev`
- format files: `ormolu --mode inplace $(git ls-files '*.hs')`
