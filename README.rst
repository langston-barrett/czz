===
czz
===

..
  This paragraph is duplicated in the README and index.rst.

czz is a *whole-program* coverage-guided mutational fuzzer. Instead of feeding
input to the target program via a file or stdin, czz executes the target from
``main`` and provides random data by intercepting calls to library functions
like ``recv``, ``fopen``, and ``rand``. This approach has two primary benefits:

- It requires very little setup; it does not require users to write a fuzzing
  harness.
- It can exercise effectful, non-deterministic code that is not amenable to
  traditional fuzzing techniques.

czz currently targets languages that compile to LLVM (e.g., C, C++, Rust, etc.),
but is built on the language-agnostic
`Crucible <https://github.com/GaloisInc/crucible>`_ library, and also includes
a proof-of-concept fuzzer for JVM code.

See the `documentation <TODO>`_ for more information about czz.
