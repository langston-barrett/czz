============
Architecture
============

.. important::
   This page is intended for the developers of czz and the very curious---it is
   not necessary for users.

czz is implemented as a collection of Haskell libraries. There are four
first-party libraries, which correspond directly to some of the top-level
directories in the czz source tree.

- czz, AKA czz core:

  * Description: generic :doc:`fuzzing algorithm <fuzzing>`, language-agnostic
    mutations

- czz-jvm:

  * Description: JVM-specific wrapper for czz core
  * Depends on: czz

- czz-llvm:

  * Description: :doc:`LLVM-specific system model <llvm/model>`, mutations,
    wrapper for czz core
  * Depends on: czz

- czz-llvm-tui:

  * Description: terminal user interface (TUI) for czz-llvm
  * Depends on: czz, czz-llvm

The dependencies of these libraries include the Haskell standard library
``base``, a few utility libraries such as ``containers`` and ``time``, and, most
notably, Crucible.

`Crucible <https://github.com/GaloisInc/crucible>`_ is a library for the
symbolic execution of imperative programs. It provides a core Crucible
intermediate representation (IR) with language-specific extensions. czz-jvm
translates programs into the Crucible-JVM IR, whereas czz-llvm translates them
into the Crucible-LLVM IR. The functionality in czz core is polymorphic over
(i.e., agnostic to) the Crucible IR extension. While Crucible is capable of
symbolic execution, czz only uses for concrete execution.
