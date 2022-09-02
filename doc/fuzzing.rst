================
Fuzzing Approach
================

This page describes aspects of czz's fuzzing algorithm.

Generally, czz is inspired by AFL, `the AFL whitepaper
<https://lcamtuf.coredump.cx/afl/technical_details.txt>`_ contains helpful
background for understanding czz.

.. _coverage:

Coverage
========

To determine whether or to keep a seed in the seed pool, czz tracks the
*coverage* that the seed achieves. This tracking is configurable. Fine-grained
coverage tracking results in a larger seed pool, which is beneficial if the
difference in coverage reflects an interesting difference between the seeds, but
can be detrimental if it ends up adding fundamentally similar, redundant seeds
to the pool.

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

TODO(lb)

Mutations
=========

TODO(lb)

Power Schedule
==============

czz does not yet support interesting power schedules, it picks seeds to mutate
uniformly at random from the seed pool.
