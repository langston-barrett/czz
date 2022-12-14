=========
Scripting
=========

czz can be scripted using `Scheme`_. The API is unstable, and not yet even
particularly well-documented.

API Reference
=============

``czz``
-------

``czz-bv-lit``
**************

.. code-block:: haskell

  ExprBuilder -> Number -> Number -> Expr

``czz-default-fuzz-config``
***************************

.. code-block:: haskell

  FuzzConfig

``czz-fuzz``
************

.. code-block:: haskell

  Fuzzer -> FuzzConfig -> Either FuzzError State

``word``
--------

Corresponding to Haskell's ``Data.Word``.

``word-bit-reverse-8``
**********************

.. code-block:: haskell

  Fuzzer -> FuzzConfig -> Either FuzzError State

.. _Scheme: http://justinethier.github.io/husk-scheme/manual/index.html
