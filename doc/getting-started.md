---
layout: default
title: Five-step introduction
---

## Step 1: Describe your desired behavior
{% highlight hspec %}
{% include introduction/MathSpec.hs %}
{% endhighlight %}

## Step 2: Write some code
{% highlight hspec %}
{% include introduction/step2/Math.hs %}
{% endhighlight %}

## Step 3: Run and watch it fail
<pre>
$ <kbd>runhaskell MathSpec.hs</kbd>
<samp>{{ "-i_includes/introduction/step2/ _includes/introduction/MathSpec.hs --html --seed 921447365 --ignore-dot-hspec" | runhaskell }}</samp></pre>

## Step 4: Implement your desired behavior

{% highlight hspec %}
{% include introduction/step4/Math.hs %}
{% endhighlight %}

## Step 5: Run again and see it pass
<pre>
$ <kbd>runhaskell MathSpec.hs</kbd>
<samp>{{ "-i_includes/introduction/step4/ _includes/introduction/MathSpec.hs --html --seed 921447365 --ignore-dot-hspec" | runhaskell }}</samp></pre>
