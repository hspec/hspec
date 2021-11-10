---
layout: default
---

# User's Manual

Hspec is a testing framework for [Haskell](http://www.haskell.org/).  Some of
Hspec's distinctive features are:

 * a friendly DSL for defining tests
 * integration with QuickCheck, SmallCheck, and HUnit
 * parallel test execution
 * automatic discovery of test files

Here is an example for the impatient:

<pre><kbd class="shell-input">cabal update && cabal install --package-env=. --lib hspec hspec-contrib QuickCheck HUnit</kbd></pre>

{% inline_example Example.hs %}

## Contents
<ul>
{% for name in site.data.contents %}
  {% assign item_url = name | prepend: "/" | append: ".html" %}
  {% assign item = site.pages | where: "url", item_url | first %}
  <li>
  {% if page.url == item.url %}
    {{ item.title }}
    {{ page.content | toc }}
  {% else %}
    <a href="{{ item.url }}">{{ item.title }}</a>
  {% endif %}
  </li>
{% endfor %}
</ul>

## Other resources

* [Hspec on GitHub](https://github.com/hspec/hspec)
* [Hspec on Hackage](http://hackage.haskell.org/package/hspec)
* [API documentation](http://hackage.haskell.org/packages/archive/hspec/latest/doc/html/Test-Hspec.html)
