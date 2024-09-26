# Contributing

Before starting to work on a feature or bug fix for Gren, it might be a good idea
to check if the change you're intending to make is a good fit for the project.

We use the `Help Wanted` tag on github issues to indicate that a PR would be
welcome. If you cannot find an issue for the change you're intending to make,
head over to the [Discord server](https://discord.gg/Chb9YB9Vmh).

We're a friendly bunch, so don't be afraid to drop in to say hi or ask questions!

We also like to talk things through before commiting to a change, so it's a good
idea to bring up an idea on Discord before making a PR.

[forum-channel-what]: https://support.discord.com/hc/en-us/articles/6208479917079-Forum-Channels-FAQ#h_01G69FJQWTWN88HFEHK7Z6X79N

To do so:

1. Join the Discord server
2. Find the server's left-hand sidebar
3. Scroll down to the `Development` category
4. Choose one of the following [forum channels][forum-channel-what] beneath it:

   - [`#compiler`](https://discord.com/channels/1250584603085766677/1250591099681247332)
   - [`#language`](https://discord.com/channels/1250584603085766677/1250591320335188099)
   - [`#core-packages`](https://discord.com/channels/1250584603085766677/1250591260159377490)
   - [`#www`](https://discord.com/channels/1250584603085766677/1250592392646492283)

5. Make your post:

   - Click in the textbox labelled with 'Search or create a post...'
   - Enter a title
   - Follow any additional steps as prompted

All PRs will be considered, but going through the above process significantly
improves your chances of a merge!

## Local development commands

- build: `cabal build -f dev`
- run tests: `cabal test -f dev`
- format files: `ormolu --mode inplace $(git ls-files '*.hs')`

## Back-up / Historic Archives

There's also a [Zulip](https://gren.zulipchat.com) with older
`#language-design` or `#api-design` streams.

This may act as a fallback for the time being, but it may be
sunset since Discord seems preferred.
