---
layout: default
---

# Frequently asked questions

**Q.: I tried to print something to `stdout` for debugging, but Hspec seems to
suppress any output.  What can I do?**

By default, Hspec suppresses output to `stdout`.  You can disable this by
passing the command-line option `--verbose` when running your spec (note that
command-line options also work in GHCi, see TODO).

An other option is to print to `stderr` instead of `stdout`.

If you use `hspecWith` to run your spec, verbosity can be set by modifying the
passed `Config` value.
