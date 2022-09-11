# Documentation
Read the [User's Manual](http://hspec.github.io/)!

# Development

Preparing a release:

```
$ util/release
```
or
```
$ util/release major
```
This will bump the version and update `CHANGES.markdown`.

Releases happen automatically when a new version end up on the `main` branch.

Whenever [`.github/workflows/publish.yml`](.github/workflows/publish.yml) sees
a new version that has no corresponding tag it:

- creates the tag
- publishes to Hackage
