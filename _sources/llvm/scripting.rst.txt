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

``czz-llvm-fuzzer``
*******************

.. code-block:: haskell

  Number -> LLVMConfig -> LLVMTranslation -> Fuzzer

``czz-llvm-translate``
**********************

.. code-block:: haskell

  LLVMConfig -> LLVMTranslation

.. _Scheme: http://justinethier.github.io/husk-scheme/manual/index.html
