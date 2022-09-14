========
Overview
========

..
  This section is duplicated in the README and index.rst.

czz is a *whole-program*, *scriptable*, *multi-language*, coverage-guided
fuzzer.

*Whole-program*: Instead of feeding input to the target program via a file or
stdin, czz executes target from ``main`` and provides it with manufactured data
by intercepting calls to library functions like ``recv``, ``fopen``, and
``rand``. This approach does not require users to write a fuzzing harness and
can exercise effectful, non-deterministic code that is not amenable to
traditional fuzzing techniques.

*Scriptable*: czz can be scripted in Scheme. Capabilities include overriding
the behavior of functions in the target program, e.g., to :ref:`make a checksum
function always pass <checksum>`. Use-cases that `we plan to support in the
future <https://github.com/langston-barrett/czz/issues/124>`_ include writing
custom power schedules and mutations.

*Multi-language*: czz currently targets languages that compile to LLVM (e.g.,
C, C++, Rust, etc.), but is built on the language-agnostic `Crucible
<https://github.com/GaloisInc/crucible>`_ library, and also includes a
proof-of-concept fuzzer for JVM code. Webassembly support is `planned
<https://github.com/langston-barrett/czz/issues/109>`_.

czz also has some notable :ref:`drawbacks and limitations <limitations>`.

Introduction
============

.. note::

  This section compares czz to AFL-style mutational, coverage-guided fuzzers. If
  you're not familiar with how these work, you may want to read `the fuzzing
  book <https://www.fuzzingbook.org/>`_ up through chapter 2 ("Lexical Fuzzing")
  and `the AFL whitepaper
  <https://lcamtuf.coredump.cx/afl/technical_details.txt>`_ before continuing.

Mutational, coverage-guided fuzzers like AFL, honggfuzz, and libFuzzer place
strict requirements on their targets: they must take a single string of bytes as
input and run deterministically. Side-effects in the target, like accessing the
network or file system, can introduce non-determinism that adversely affects the
fuzzing algorithm. Since most programs don't take a single file as an input and
do in fact use such side-effects, in practice these requirements mean that
developers have to write a *harness* that reads the input from the fuzzer and
passes it on to some mostly-side-effect-free subset of the target.

.. figure:: img/classic.svg

   Classic coverage-guided fuzzing

czz works differently. Instead of executing code directly on the host, it acts
as an *interpreter* for target program (like QEMU User Mode Emulation). This
allows czz to completely control the target's environment, responding to library
calls with generated data. It also allows czz to be orchestrated and extensively
customized with user-provided Scheme scripts.

.. figure:: img/czz.svg

   The czz approach

In this paradigm, side-effects from library calls present no obstacle to
mutational fuzzing---they don't introduce non-determinism, because czz chooses
how to respond to them. Consider the following target:

.. code-block:: c

  #include <stdlib.h>
  #include <time.h>
  void harness(char *data, size_t size) {
    // set seed of random number generator to current time
    srand(time(NULL));
    if (rand() % 2 == 0) {
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
which czz-llvm must respond to (to a first approximation, "respond" means
"generate a return value and possibly mutate program memory"). For example, the
program might call ``time``, and czz-llvm might return a random integer, or the
program could call ``send``, and ``czz-llvm`` would check that it's sending to a
valid socket and return some random number indicating the number of bytes sent.
At some point, the target will exit normally or crash.

After the target exits, czz records the :ref:`coverage <coverage>` and the
inputs it generated:

1. Command-line arguments
2. Initial environment, including environment variables and virtual file system
3. The sequence of responses to library calls

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

.. _model:

Modeling the Environment
========================

It's easy for czz to respond appropriately to library calls like ``rand``: it
has the freedom to choose an arbitrary ``int`` and return it to the program.
Other library calls require more care. Consider ``getenv``:

.. code-block:: c

  #include <stdlib.h>
  int main(int argc, char *argv[]) {
    char *x = malloc(1);
    if (strcmp(getenv("SHELL"), getenv("SHELL")) != 0) {
      free(x);  // unreachable
    }
    free(x);
    return 0;
  }

This program doesn't have a double-free---``getenv`` will return the same value
when given the same argument twice in a row. czz-llvm needs to do the same to
avoid *unsoundness*, that is, reporting a "false positive", a "bug" that can't
actually arise in practice. In particular, czz can't simply respond completely
randomly to each library call.

The situation gets even more complicated when considering ``setenv``: ``getenv``
must return the *latest* value of each environment variable, meaning czz-llvm
must maintain *state* during the program's execution. Similarly, ``getenv``
should agree with ``envp`` (the third argument to ``main``, for programs that
take such an argument) on the values of the environment variables.

To maintain soundness, czz must *under-approximate* the behavior of the standard
library and host OS. Every response that czz generates for a library call must
be a *possible* response that the standard library and host OS might generate.
The test suite compares the behavior of programs that make library calls when
interpreted by czz-llvm to when they're compiled by Clang and executed on the
host, to ensure fidelity of czz-llvm's models.

See :doc:`llvm/model` for more information about czz-llvm's modeling.

.. _limitations:

Limitations
===========

While whole-program fuzzing has some benefits, it also has its drawbacks:

- Modeling the standard library and host OS is challenging.

  * Some library calls may not be supported (e.g. ``stat``), and czz won't be
    able to fuzz the parts of the target that use them.

  * It's possible (though it should be considered a bug in czz) that some of
    czz's models are unsound (see :ref:`model`), meaning it can report bugs that
    can't actually occur.

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

.. _comparison:

Comparisons to Other Tools
==========================

This list is meant to help you understand how czz fits into the broader
landscape, and figure out whether czz or one of these tools is more appropriate
for your use. It is based on the author's limited experience and understanding,
and in no way meant to criticize the excellent work that has gone into the tools
in the list.

AFL, etc.
---------

There are many coverage-guided mutational fuzzers such as AFL, Honggfuzz, and
libFuzzer. If these tools work for your program, you should absolutely use them.

Advantages over czz: These tools are reliable and *actually find bugs in real
programs*.

Disadvantages vs. czz:

- Can't handle effectful code well
- Generally handle a single programming language (or a few, via LLVM)
- Limited customizability
- AFL's instrumentation `can record misleading coverage
  <https://ieeexplore.ieee.org/document/8418631>`_, czz avoids this issue

KLEE
----

czz is much akin to KLEE, they both analyze LLVM bitcode and provide models of
library calls.

Advantages over czz:

- Support for symbolic execution
- More developed, including probably more reliable

Disadvantages vs. czz:

- Symbolic execution suffers from solver limitations and path explosion
- Only works on LLVM
- Limited customizability
