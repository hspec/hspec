---
layout: default
title: Speeding up the edit-compile-test cycle
---

{% require 2.4.2 %}

## Rerunning failed tests only


When running tests, it is possible to only run tests that have failed in a
previous test run.
This can help to:

 - speed up the edit-compile-test cycle
 - focus on a fixed subset of failing tests

Two options are required for this.

Instruct Hspec to write/read failure reports to/from a file:

<pre><kbd class="shell-input">echo --failure-report .hspec-failures >> ~/.hspec</kbd></pre>

Enable `--rerun`:

<pre><kbd class="shell-input">echo --rerun >> ~/.hspec</kbd></pre>


In addition `--rerun-all-on-success` is useful in this scenario:

<pre><kbd class="shell-input">echo --rerun-all-on-success >> ~/.hspec</kbd></pre>

You may want to have a look at the [section on options](/options.html) for more details.
