|Build status|

Running tests
#############

Cabal
=====

Just run

.. sourcecode:: bash

    cabal test

and return code will indicate success or failure.
However, you won't get colors on stdout [1]_

Runhaskel
=========

.. sourcecode:: bash

    runhaskell -isrc -itest test/Spec.hs

.. |Build status| image:: https://travis-ci.org/sol/hspec-example.png
                  :alt: Build Status
                  :target: https://travis-ci.org/sol/hspec-example

.. [1] https://github.com/sol/hspec-example/issues/3
