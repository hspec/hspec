## Building the documentation locally

A working ruby setup, including [Bundler](https://bundler.io/), is required
to build the documentation locally; the `bundle` command has to be on your
`PATH`. Also [tinc](https://github.com/sol/tinc) is needed.

If you are building the documentation for the first time, run:

```console
$ make setup
```

To build the documentation, run:

```console
$ make
```

To automatically rebuild the documentation on changes, run:

```console
$ make watch
```
