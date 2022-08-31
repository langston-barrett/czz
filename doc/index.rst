==========================
czz: Whole-Program Fuzzing
==========================

..
  This section is duplicated in the README and overview.rst.

czz is a *whole-program* coverage-guided mutational fuzzer. Instead of feeding
input to the target program via a file or stdin, czz executes target from
``main`` and provides it with manufactured data by intercepting calls to library
functions like ``recv``, ``fopen``, and ``rand``. This approach has two primary
benefits:

- It requires very little setup. In particular, it does not require users to
  write a fuzzing harness.
- It can exercise effectful, non-deterministic code that is not amenable to
  traditional fuzzing techniques.

czz currently targets languages that compile to LLVM (e.g., C, C++, Rust, etc.),
but is built on the language-agnostic
`Crucible <https://github.com/GaloisInc/crucible>`_ library, and also includes
a proof-of-concept fuzzer for JVM code.

Read more at :doc:`overview`.

.. toctree::
   :hidden:
   :caption: Start Here

   overview

.. toctree::
   :hidden:
   :caption: LLVM

   llvm/model
   llvm/status


.. toctree::
   :hidden:
   :caption: JVM

   jvm/status

==================
Indices and tables
==================

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`
