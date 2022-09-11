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

Releases happen automatically when a new version ends up on `main`.

Whenever [`.github/workflows/publish.yml`](.github/workflows/publish.yml) detects
a new version without a corresponding tag it:

- creates a tag
- publishes to Hackage
