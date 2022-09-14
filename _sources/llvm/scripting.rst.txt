=========
Scripting
=========

czz-llvm can be scripted using `Scheme`_.

API Reference
=============

``czz-llvm-default-config``
***************************

.. code-block:: haskell

  LLVMConfig

``czz-llvm-from-what4``
***********************

.. code-block:: haskell

  Expr -> SVal

``czz-llvm-fuzzer``
*******************

.. code-block:: haskell

  Number -> LLVMConfig -> LLVMTranslation -> List LLVMOverride -> Fuzzer

``czz-llvm-override``
*********************

.. code-block:: haskell

  Translation ->
  String ->
  (ExprBuilder -> Mem -> [Val] -> (Mem, Val)) ->
  Maybe Override

``czz-llvm-translate``
**********************

.. code-block:: haskell

  LLVMConfig -> LLVMTranslation

.. _Scheme: http://justinethier.github.io/husk-scheme/manual/index.html
