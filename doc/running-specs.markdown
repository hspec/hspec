---
layout: default
---

## Running Tests with Hspec

The most common way to run a spec is with {{'hspec'|id}}, e.g.:

```
main = hspec spec
```

It is possible to customize how a spec is run by providing command-line flags.
You can get a list of supported flags by passing `--help` to your test driver:

<pre>
<kbd class="shell-input">runhaskell Spec.hs --help</kbd>
<samp>{{"_includes/Help.hs --help"|runhaskell}}</samp>
</pre>

Using {{'hspecWith'|id}} instead of `hspec` gives even more control over how a
spec is run.
