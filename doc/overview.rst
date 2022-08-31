========
Overview
========

..
  This section is duplicated in the README and index.rst.

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

Introduction
============

.. note::

  This section compares czz to AFL-style mutational, coverage-guided fuzzers. If
  you're not familiar with how these work, you may want to read `the AFL
  whitepaper <https://lcamtuf.coredump.cx/afl/technical_details.txt>`_ before
  continuing.

Mutational, coverage-guided fuzzers like AFL, honggfuzz, and libFuzzer place
strict requirements on their targets: they must take a single string of bytes as
input and run deterministically. Side-effects in the target, like accessing the
network or file system, can introduce non-determinism that adversely affects the
fuzzing algorithm. Since most programs don't take a single file as an input and
do in fact use such side-effects, in practice these requirements mean that
developers have to write a *harness* that reads the input from the fuzzer and
passes it on to some mostly-side-effect-free subset of the target.

.. figure:: img/classic.svg
   :scale: 35

   Classic coverage-guided fuzzing

czz works differently. Instead of executing code directly on the host CPU and
OS, it acts as an *interpreter* for target program (like QEMU User Mode
Emulation). This allows czz to completely control the target's environment,
responding to library calls with generated data.

.. figure:: img/czz.svg
   :scale: 35

   The czz approach

In this paradigm, side-effects from library calls present no obstacle to
mutational fuzzing---they don't introduce non-determinism, because czz chooses
how to respond to them. Consider the following target:

.. code-block:: c

  #include <stdlib.h>
  #include <time.h>
  void harness(char *data, size_t size) {
    srand(time(NULL)); // set seed for random number generator based on time
    if (rand()) {
      do_a_thing(data, size);
    } else {
      something_else(data, size);
    }
  }

For AFL, this target would present a challenge - When mutating the input, it
wouldn't be clear if new coverage arises from the mutation, or from a different
result from ``rand``. czz, on the other hand, can choose to *either* respond to
``rand`` identically when mutating the input, or to keep the *input* constant
while changing the output of ``rand``. In either case, the effect of the
mutation on coverage is clear.

How it Works
============

Here's what happens when running czz-llvm on a new target. For the sake of
simplicity, assume no corpus is available.

First, czz-llvm translates the target LLVM code into its internal representation
(the Crucible-LLVM IR). To start running the target at ``main``, czz-llvm must
supply arguments to ``main``, namely ``argc`` and ``argv``. Assume that it sets
``argc`` to ``0`` and ``argv`` to ``NULL``.

After starting ``main``, the target will make some number of library calls,
which czz-llvm must respond to. For example, the program might call ``time``,
and czz-llvm might return a random integer, or the program could call ``send``,
and ``czz-llvm`` would check that it's sending to a valid socket and return some
random number indicating the number of bytes sent. At some point, the target
will exit normally or crash.

After the target exits, czz records the :ref:`coverage <coverage>` and the
inputs it generated:

1. Command-line arguments
2. Initial environment, including environment variables and virtual file system
3. The sequence of responses to system calls

These form a *seed*. czz-llvm then *mutates* this seed to try to find new
coverage. It might:

- Add, drop, or alter a command-line argument
- Add, drop, or alter an environment variable
- Add, drop, or alter a file in the initial file system
- Alter the response to a library call

After mutating the seed, czz-llvm executes the program again in the new
environment. If it mutated the response to a library call, it will respond
identically to all library calls that precede it (forcing this portion of the
execution to be deterministic), and then respond differently to that call.

If this new seed generates additional coverage, czz-llvm will add it to the
*seed pool*, the collection of seeds that are candidates for mutation.
Otherwise, it will discard it. This process of generating and evaluating seeds
continues indefinitely.

.. _limitations:

Limitations
===========

While whole-program fuzzing has some benefits, it also has its drawbacks:

- Modeling the standard library and host OS is challenging.

    * Some library calls may not be supported (e.g. ``stat``), and czz won't be
      able to fuzz the parts of the target that use them.

    * It's possible (though it should be considered a bug in czz) that some of
      czz's models are :ref:`unsound <soundness>`, meaning it can report bugs
      that can't actually occur.

- Interpreting programs is *much* slower than running them natively on the host
  OS and CPU. This means fewer executions, fewer mutations, and less coverage
  for your CPU time. czz will never compete with traditional fuzzers on code
  which is suitable for traditional fuzzing.

czz-llvm
--------

- czz-llvm only works on programs that can be statically compiled to a single
  LLVM module with Clang.

- czz-llvm does not work for parallel code (e.g., using ``pthreads``).

- czz-llvm inherits `the limitations of Crucible-LLVM
  <https://github.com/GaloisInc/crucible/blob/master/crucible-llvm/doc/limitations.md>`_.
  Notably:

    * It `can't handle <https://github.com/GaloisInc/crucible/issues/857>`_
      variable-arity functions (other than overrides like ``printf``,
      ``snprintf`` and friends).

    * It often lags a few versions behind the latest LLVM release.

.. _coverage:

Coverage
========

To determine whether or to keep a seed in the seed pool, czz tracks the
*coverage* that the seed achieves. This tracking is configurable. Fine-grained
coverage tracking results in a larger seed pool, which is beneficial if the
difference in coverage reflects an interesting difference between the seeds, but
can be harmful if it ends up adding fundamentally similar, redundant seeds to
the pool.

When executing the target with the seed, czz tracks how many times the program
executes each *k*-length chain of edges between basic blocks. Particular choices
of *k* reduce to more familiar coverage tracking schemes:

- *k* = 1: Basic block coverage
- *k* = 2: Edge coverage (like AFL)

Higher values of *k* correspond to more fine-grained distinctions in coverage.

Like AFL, czz tracks not just whether an edge was covered, but further tracks
the *hit counts* of each *k*-edge chain (i.e., how many times the chain was
executed). These hit counts are *bucketed* at the end of the execution, meaning
they are collapsed into a more granular form. czz currently provides two
bucketing strategies:

- log2: Take the (integer) logarithm base 2 of each hit count
- zero-one-many: Record only whether the edge was hit one or more than one time

log2 is more fine-grained than zero-one-many.

.. TODO(lb): examples

.. _soundness:

Soundness
=========
