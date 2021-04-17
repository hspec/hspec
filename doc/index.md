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

An example is worth a thousand words, so here we go:

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

## Extensions

* [hspec-expectations-lens](http://hackage.haskell.org/package/hspec-expectations-lens) — Hspec expectations for the _lens_ stuff
* [hspec-webdriver](http://hackage.haskell.org/package/hspec-webdriver) — write end2end web application tests using _webdriver_ and hspec
* [hspec-snap](http://hackage.haskell.org/package/hspec-snap) — a library for testing with Hspec and the _Snap Web Framework_
* [hspec-checkers](http://hackage.haskell.org/package/hspec-checkers) — allows to use _checkers_ properties from hspec
* [hspec-smallcheck](http://hackage.haskell.org/package/hspec-smallcheck) — support for _SmallCheck_
* [hspec-laws](http://hackage.haskell.org/package/hspec-laws) — document and test laws for standard type classes
* [hspec-attoparsec](http://hackage.haskell.org/package/hspec-attoparsec) — utility functions for testing your _attoparsec_ parsers with hspec
* [hspec-megaparsec](http://hackage.haskell.org/package/hspec-megaparsec) — utility functions for testing your _megaparsec_ parsers with hspec
* [hspec-jenkins](http://hackage.haskell.org/package/hspec-jenkins) — Jenkins-friendly XML formatter for Hspec
* [hspec-expectations-lifted](http://hackage.haskell.org/package/hspec-expectations-lifted) — a version of _hspec-expectations_ generalized to MonadIO
* [hspec-expectations-pretty](http://hackage.haskell.org/package/hspec-expectations-pretty) — _hspec-expectations_ with pretty printing on failure
* [hspec-test-framework](http://hackage.haskell.org/package/hspec-test-framework) and [hspec-test-framework-th](http://hackage.haskell.org/package/hspec-test-framework-th) — run _test-framework_ tests with Hspec

## Blogosphere

* [Writing controller specs for a Warp server](http://begriffs.com/posts/2014-10-19-warp-server-controller-test.html)
* [Testing attoparsec parsers with hspec](http://alpmestan.com/posts/2014-06-18-testing-attoparsec-parsers-with-hspec.html)

## Other resources

* [Hspec on GitHub](https://github.com/hspec/hspec)
* [Hspec on Hackage](http://hackage.haskell.org/package/hspec)
* [API documentation](http://hackage.haskell.org/packages/archive/hspec/latest/doc/html/Test-Hspec.html)
* [Vim syntax file for Hspec](https://github.com/hspec/hspec.vim#readme)
* [Arion - Watcher and runner for hspec](http://github.com/karun012/arion)
* [Guard plugin for Hspec](http://rubygems.org/gems/guard-haskell)
* [Sublime Text snippets for Hspec](https://github.com/ayakovlenko/hspec-sublime-snippets)
