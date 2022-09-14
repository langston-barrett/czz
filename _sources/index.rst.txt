==========================
czz: Whole-Program Fuzzing
==========================

..
  This section is duplicated in the README and overview.rst.

czz is a *whole-program*, *scriptable*, *multi-language*, coverage-guided
fuzzer.

**Whole-program**: Instead of feeding input to the target program via a file or
stdin, czz executes target from ``main`` and provides it with manufactured data
by intercepting calls to library functions like ``recv``, ``fopen``, and
``rand``. This approach does not require users to write a fuzzing harness and
can exercise effectful, non-deterministic code that is not amenable to
traditional fuzzing techniques.

**Scriptable**: czz can be scripted in Scheme. Capabilities include overriding
the behavior of functions in the target program, e.g., to :ref:`make a checksum
function always pass <checksum>`. Use-cases that `we plan to support in the
future <https://github.com/langston-barrett/czz/issues/124>`_ include writing
custom power schedules and mutations.

**Multi-language**: czz currently targets languages that compile to LLVM (e.g.,
C, C++, Rust, etc.), but is built on the language-agnostic `Crucible
<https://github.com/GaloisInc/crucible>`_ library, and also includes a
proof-of-concept fuzzer for JVM code. Webassembly support is `planned
<https://github.com/langston-barrett/czz/issues/109>`_.

Read more at the :doc:`overview`.

.. toctree::
   :hidden:
   :caption: Start Here

   overview
   quickstart

.. toctree::
   :hidden:
   :caption: Reference

   architecture
   fuzzing
   scripting

.. toctree::
   :hidden:
   :caption: czz-llvm Usage

   llvm/config
   llvm/tui

.. toctree::
   :hidden:
   :caption: czz-llvm Reference

   llvm/model
   llvm/scripting
   llvm/status


.. toctree::
   :hidden:
   :caption: czz-jvm Reference

   jvm/status

==================
Indices and tables
==================

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`
