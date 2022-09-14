===
czz
===

..
  This paragraph is duplicated in the README and index.rst.

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

See the `documentation <TODO>`_ for more information about czz.
