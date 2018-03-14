---
layout: default
title: Running individual spec items
---


You can run individual spec items that match a given pattern by passing
`--match PATTERN` to your test driver.  The `PATTERN` is matched against

- the *`/`-separated path* of the spec item
- the *pretty-printed path* of the spec item as it appears in test output

To understand what exactly this means let's look at an example.

### Example

{% highlight hspec %}
{% include match/Spec.hs %}
{% endhighlight %}

<pre>
<kbd class="shell-input">runhaskell Spec.hs</kbd>
<samp>{{ "_includes/match/Spec.hs --html --seed 921447365 --ignore-dot-hspec" | runhaskell | replace: '_includes/match/', '' }}</samp></pre>

This spec contains one failing spec item.

The `/`-separated path of that spec item is:

```
Prelude/reverse/reverses a list
```

The pretty-printed path as it appears in the test output is:

```
Prelude.reverse reverses a list
```

You can rerun the failing spec item by either matching on the `/`-separated
path or the pretty-printed path.  Both of the following options lead to the
same result:

<pre><kbd class="shell-input">runhaskell Spec.hs -m "Prelude/reverse/reverses a list"</kbd></pre>

<pre><kbd class="shell-input">runhaskell Spec.hs -m "Prelude.reverse reverses a list"</kbd></pre>


Or you can match on any substring of one of the above, e.g.:


<pre><kbd class="shell-input">runhaskell Spec.hs -m reverse</kbd></pre>

{% note Do not rely on the pretty-printed path for scripting purpose; use the `/`-separated path instead! %}
