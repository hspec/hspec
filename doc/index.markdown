---
layout: default
---

## Getting started

### Step 1: Describe your desired behavior
{% highlight hspec %}
{% include introduction/MathSpec.hs %}
{% endhighlight %}

### Step 2: Write some code
{% highlight hspec %}
{% include introduction/step2/Math.hs %}
{% endhighlight %}

### Step 3: Run and watch it fail
<pre>
<code>$ runhaskell MathSpec.hs</code>
<samp>{{ "-i_includes/introduction/step2/ _includes/introduction/MathSpec.hs --html" | runhaskell }}</samp></pre>

### Step 4: Implement your desired behavior

{% highlight hspec %}
{% include introduction/step4/Math.hs %}
{% endhighlight %}

### Step 5: Run again and see it pass
<pre>
<code>$ runhaskell MathSpec.hs</code>
<samp>{{ "-i_includes/introduction/step4/ _includes/introduction/MathSpec.hs --html" | runhaskell }}</samp></pre>

## Where to go from here?

* Have a look at the [API documentation][api-docs].
* Use [QuickCheck][api-quickcheck] and [HUnit][api-hunit] for your examples.
* Use [RSpec-style expectation terminology](expectations.html) for your examples.
* Learn about [automatic spec discovery][hspec-discover]
* Have a look at a [report of hspec's behavior][hspec-report].

[hspec-report]:     report.html
[api-docs]:  http://hackage.haskell.org/packages/archive/hspec/latest/doc/html/Test-Hspec.html
                      "Haddock documentation for the non-monadic API"
[api-hunit]:        http://hackage.haskell.org/packages/archive/hspec/latest/doc/html/Test-Hspec-HUnit.html
                      "Haddock documentation for HUnit integartion"
[api-quickcheck]:   http://hackage.haskell.org/packages/archive/hspec/latest/doc/html/Test-Hspec-QuickCheck.html
                      "Haddock documentation for QuickCheck integartion"
[hspec-discover]:   https://github.com/hspec/hspec/tree/master/hspec-discover#readme
                      "README for hspec-discover"
