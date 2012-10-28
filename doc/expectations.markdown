---
layout: default
---


## Expecting exceptions

### Expecting exceptions from pure code

[`evaluate`][v:evaluate] can be used to expect exceptions from pure code:

```hspec
evaluate (1 `div` 0) `shouldThrow` anyArithException
```

However, `evaluate` only forces its argument to _weak head normal form_.  To
better understand what that means, let's look at an other example:

```hspec
evaluate ('a' : undefined) `shouldThrow` errorCall "Prelude.undefined"
```

Here `evaluate` does not force the exception.  It only evaluates `'a' :
undefined` until it encounters the first constructor &mdash; the `:` &mdash; and is done.
It does not look at the arguments of that contructor.

[`$!!`][v:deep-apply] can be used to force the exception:

```hspec
(return $!! 'a' : undefined) `shouldThrow` errorCall "Prelude.undefined"
```

<div class="example">
<h4 class="example-heading">show example code</h4>

<div>

{% highlight hspec %}
-- file Spec.hs
{% include ExceptionsFromPureCode.hs %}
{% endhighlight %}

<pre>
<code>$ runhaskell Spec.hs</code>
<samp>{{ "-i_includes/introduction/step4/ _includes/ExceptionsFromPureCode.hs --html" | runhaskell }}</samp></pre>

</div>
</div>

[v:evaluate]:   http://hackage.haskell.org/packages/archive/base/latest/doc/html/Control-Exception.html#v:evaluate
[v:deep-apply]: http://hackage.haskell.org/packages/archive/deepseq/1.3.0.1/doc/html/Control-DeepSeq.html#v:-36--33--33-
