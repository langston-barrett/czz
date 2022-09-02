============
System Model
============

This page describes how czz-llvm :ref:`models various aspects of libc and the
host operating system <model>`.

To maintain soundness, czz must *under-approximate* the behavior of the standard
library and host OS. Every response that czz generates for a library call must
be a *possible* response that the standard library and host OS might generate.
The test suite compares the behavior of programs that make library calls when
interpreted by czz-llvm to when they're compiled by Clang and executed on the
host, to ensure fidelity of czz-llvm's models.

TODO(lb)
