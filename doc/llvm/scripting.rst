=========
Scripting
=========

czz-llvm can be scripted using `Scheme`_.

Examples
========

Override a Checksum to Always Pass
----------------------------------

.. code-block:: scheme

  (define test-c "
  #include <stdbool.h>
  #include <stdio.h>

  bool __attribute__((noinline)) passes_checksum(int x, int checksum) {
    return (x % 128 == checksum);
  }

  int main(int argc, char* argv[]) {
    if (passes_checksum(argc, 73)) {
      printf(\"~~here~~\\n\");
    }
    return 0;
  }
  ")

  ;; Write string test-c to file test.c
  (call-with-output-file "test.c" (lambda (out) (display test-c out)))

  ;; Compile to LLVM bitcode
  (system "clang -g -O1 -emit-llvm -fno-discard-value-names -Wall -c test.c -o in.bc")

  ;; Override the behavior of "passes_checksum" to always return true
  (define (passes-checksum sym mem args)
    (list mem (czz-llvm-bool sym #t)))

  ;; Helper of type `Maybe a -> [a]`
  (define (maybe-to-list mb) (maybe-maybe (list) (lambda (x) (list x)) mb))

  (let* ((llvm-config (czz-llvm-default-config))
        ;; Translate the program into czz's IR
        (trans (czz-llvm-translate llvm-config))
        ;; Create an override for the LLVM passes_checksum procedure from the
        ;; Scheme passes-checksum function
        (ovs (maybe-to-list (czz-llvm-override trans "passes_checksum" passes-checksum)))
        ;; Create a fuzzer with the given overrides and configuration
        (fuzzer (czz-llvm-fuzzer 1 llvm-config trans ovs)))
    ;; Start fuzzing
    (czz-fuzz fuzzer (czz-default-fuzz-config)))

API Reference
=============

``czz-llvm-default-config``
---------------------------

.. code-block:: haskell

  LLVMConfig

``czz-llvm-bool``
-----------------

.. code-block:: haskell

  ExprBuilder -> Bool -> SVal

``czz-llvm-expr``
-----------------

.. code-block:: haskell

  ExprBuilder -> Expr -> SVal

``czz-llvm-fuzzer``
-------------------

.. code-block:: haskell

  Number -> LLVMConfig -> LLVMTranslation -> List LLVMOverride -> Fuzzer

``czz-llvm-i1``
---------------

.. code-block:: haskell

  ExprBuilder -> Number -> SVal

``czz-llvm-i8``
---------------

.. code-block:: haskell

  ExprBuilder -> Number -> SVal

``czz-llvm-i16``
----------------

.. code-block:: haskell

  ExprBuilder -> Number -> SVal

``czz-llvm-i32``
----------------

.. code-block:: haskell

  ExprBuilder -> Number -> SVal

``czz-llvm-i64``
----------------

.. code-block:: haskell

  ExprBuilder -> Number -> SVal

``czz-llvm-override``
---------------------

.. code-block:: haskell

  Translation ->
  String ->
  (ExprBuilder -> Mem -> [Val] -> (Mem, Val)) ->
  Maybe Override

``czz-llvm-translate``
----------------------

.. code-block:: haskell

  LLVMConfig -> LLVMTranslation

.. _Scheme: http://justinethier.github.io/husk-scheme/manual/index.html
