===========
Quick Start
===========

This guide is intended to get you started fuzzing a C program with czz-llvm as
soon as possible. For more detail on the various steps taken here, refer to the
rest of the documentation.

Dependencies
============

The following tools must be present on your ``$PATH`` to run czz-llvm:

* ``z3``, the `Z3 SMT solver <https://github.com/Z3Prover/z3/releases>`_
* ``llvm-link`` (version <= 12), shipped with `LLVM`_

On Ubuntu, these can be installed with

.. code-block:: bash

  sudo apt-get install -y llvm z3

Obtaining Binaries
==================

You can either download pre-built binaries, or build them yourself from source.

.. _download:

Download Pre-Built Binaries
---------------------------

The CI system uploads artifacts to GitHub. To download a pre-built binary,
navigate to `the "Actions" tab
<https://github.com/langston-barrett/czz/actions?query=branch%3Amain+event%3Apush>`_
of `the czz GitHub repo <https://github.com/langston-barrett/czz>`_, select the
latest completed build of ``main``, and click on the appropriate "bdist" (binary
distribution) artifact for your OS.

Build from Source
-----------------

To build czz you'll need GHC and ``cabal``, which can be installed with
`ghcup`_.

.. code-block:: bash

  git clone https://github.com/langston-barrett/czz.git
  cd czz
  git submodule update --init
  cabal build exe:czz-llvm-tui

You can locate the built executable with ``cabal list-bin exe:czz-llvm-tui``, or
install it under ``~/.cabal/bin`` with ``cabal install
exe:czz-llvm-tui --overwrite-policy=always``.

Configure and Run
=================

Build the Target
----------------

To run czz-llvm, you'll need to compile your program to LLVM bitcode. You can do
this with `gllvm`_ or `build-bom`_, or for single-file C programs you can run

.. code-block:: bash

   clang -g -emit-llvm -fno-discard-value-names -c prog.c -o prog.bc

(To view the resulting LLVM module, try using ``llvm-dis``.)

Run the TUI
-----------

Then you can run the TUI (Terminal User Interface) like so:

.. code-block:: bash

   czz-llvm-tui prog.bc

Press ESC to exit the TUI.

Configure
---------

Some interesting options:

* ``--jobs``: Control the number of fuzzing threads
* ``--tries``

.. _LLVM: https://llvm.org/
.. _ghcup: https://www.haskell.org/ghcup/
.. _gllvm: https://github.com/SRI-CSL/gllvm
.. _build-bom: https://github.com/travitch/build-bom
