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

Overrides
=========

czz-llvm's models of library functions are called *overrides*. The following
table provides a high-level view of these overrides.

The "Impl." column is short for "implementation", for which there are several
possible options:

- "None": no override is available
- "czz": the override is implemented as part of czz-llvm.
- "cru.": the override is implemented as part of Crucible-LLVM.
  These overrides are generally suitable for program verification, so they are
  almost always sound.
- "musl": the definition of this function is compiled by Clang from musl libc
  and linked into the LLVM module before execution by czz-llvm. This is sound up
  to bugs in musl, Clang, and ``llvm-link``.

The "Sound" column is short for "soundness", for which there are several possible
options:

- "Yes?": AFAIK this is sound, but more research may be needed to verify this.
- "Yes": This is very likely sound based on my reading of the docs.
- "No": This is known to be unsound (see documentation for specific overrides).

.. TODO(lb): What's up with the crucible-llvm math ones? Do they work for concrete values?
.. https://github.com/GaloisInc/crucible/blob/master/crucible-llvm/src/Lang/Crucible/LLVM/Intrinsics.hs
.. :ref:`Math <model_math>`           ``ciel``           n/a   cru.  Yes   n/a

================================== ================== ===== ===== ===== ============================
Category                           Name               Tests Impl. Sound Docs or Issue
================================== ================== ===== ===== ===== ============================
:ref:`Env. vars. <model_env_vars>` ``getenv``         2     czz   Yes?  :ref:`Docs <getenv>`
:ref:`Env. vars. <model_env_vars>` ``setenv``         0     None  n/a   `#29`_
:ref:`Env. vars. <model_env_vars>` ``unsetenv``       0     None  n/a   `#30`_
:ref:`Files <model_files>`         ``open``           0     None  n/a   TODO(lb)
:ref:`Files <model_files>`         ``creat``          0     None  n/a   TODO(lb)
:ref:`Files <model_files>`         ``unlink``         0     None  n/a   TODO(lb)
:ref:`Files <model_files>`         ``remove``         0     None  n/a   TODO(lb)
:ref:`Files <model_files>`         ``close``          0     None  n/a   TODO(lb)
:ref:`Files <model_files>`         ``read``           0     None  n/a   TODO(lb)
:ref:`Files <model_files>`         ``write``          0     None  n/a   TODO(lb)
:ref:`Files <model_files>`         ``fclose``         0     None  n/a   `#43`_
:ref:`Files <model_files>`         ``fopen``          0     None  n/a   `#42`_
:ref:`Files <model_files>`         ``fprintf``              czz   n/a   :ref:`Docs <fprintf>`
:ref:`Files <model_files>`         ``fread``          0     None  n/a   `#44`_
:ref:`Files <model_files>`         ``fwrite``         0     None  n/a   `#45`_
:ref:`Formatting <model_format>`   ``__printf_chk``   n/a   cru.  No    :ref:`Docs <printf>`
:ref:`Formatting <model_format>`   ``printf``         n/a   cru.  No    :ref:`Docs <printf>`
:ref:`Formatting <model_format>`   ``sprintf``              czz   No    :ref:`Docs <printf>`
:ref:`Formatting <model_format>`   ``snprintf``             czz   No    :ref:`Docs <printf>`
:ref:`Formatting <model_format>`   ``__sprintf_chk``        czz   No    :ref:`Docs <printf>`
:ref:`Formatting <model_format>`   ``__snprintf_chk``       czz   No    :ref:`Docs <printf>`
:ref:`Math <model_math>`           ``abs``            n/a   cru.  Yes   n/a
:ref:`Math <model_math>`           ``labs``           n/a   cru.  Yes   n/a
:ref:`Math <model_math>`           ``llabs``          n/a   cru.  Yes   n/a
:ref:`Memory <model_memory>`       ``__memcpy_chk``   n/a   cru.  Yes   n/a
:ref:`Memory <model_memory>`       ``__memset_chk``   n/a   cru.  Yes   n/a
:ref:`Memory <model_memory>`       ``calloc``         n/a   cru.  Yes   n/a
:ref:`Memory <model_memory>`       ``free``           n/a   cru.  Yes   n/a
:ref:`Memory <model_memory>`       ``htonl``          n/a   cru.  Yes   n/a
:ref:`Memory <model_memory>`       ``htons``          n/a   cru.  Yes   n/a
:ref:`Memory <model_memory>`       ``malloc``         n/a   cru.  Yes   n/a
:ref:`Memory <model_memory>`       ``memcmp``         n/a   musl  Yes   n/a
:ref:`Memory <model_memory>`       ``memcpy``         n/a   cru.  Yes   n/a
:ref:`Memory <model_memory>`       ``memmove``        n/a   cru.  Yes   n/a
:ref:`Memory <model_memory>`       ``memrchr``        n/a   musl  Yes   n/a
:ref:`Memory <model_memory>`       ``memset``         n/a   cru.  Yes   n/a
:ref:`Memory <model_memory>`       ``ntohl``          n/a   cru.  Yes   n/a
:ref:`Memory <model_memory>`       ``ntohs``          n/a   cru.  Yes   n/a
:ref:`Memory <model_memory>`       ``posix_memalign`` n/a   cru.  Yes   n/a
:ref:`Memory <model_memory>`       ``realloc``        n/a   cru.  Yes   n/a
:ref:`Misc <model_misc>`           ``__lctrans_cur``  n/a   musl  Yes   n/a
:ref:`Misc <model_misc>`           ``__lctrans``      n/a   musl  Yes   n/a
:ref:`Misc <model_misc>`           ``atoi``           n/a   musl  Yes   n/a
:ref:`Misc <model_misc>`           ``getopt_long``    n/a   musl  Yes   n/a
:ref:`Misc <model_misc>`           ``mbtowc``         n/a   musl  Yes   n/a
:ref:`Misc <model_misc>`           ``rand``           n/a   musl  Yes   n/a
:ref:`Misc <model_misc>`           ``srand``          n/a   musl  Yes   n/a
:ref:`Misc <model_misc>`           ``signal``               czz   Yes?  :ref:`Docs <signal>`
:ref:`Network <model_network>`     ``accept``               czz   No    :ref:`Docs <accept>`
:ref:`Network <model_network>`     ``bind``                 czz   No    :ref:`Docs <bind>`
:ref:`Network <model_network>`     ``listen``               czz   No    :ref:`Docs <listen>`
:ref:`Network <model_network>`     ``recv``                 czz   No    :ref:`Docs <recv>`
:ref:`Network <model_network>`     ``send``                 czz   No    :ref:`Docs <send>`
:ref:`Network <model_network>`     ``setsockopt``           czz   No    :ref:`Docs <setsockopt>`
:ref:`Network <model_network>`     ``socket``               czz   No    :ref:`Docs <socket>`
:ref:`Standard I/O <model_stdio>`  ``putchar``        n/a   cru.  No    n/a
:ref:`Standard I/O <model_stdio>`  ``puts``           n/a   cru.  No    n/a
:ref:`Strings <model_strings>`     ``stpncpy``        n/a   musl  Yes   n/a
:ref:`Strings <model_strings>`     ``strcmp``         n/a   musl  Yes   n/a
:ref:`Strings <model_strings>`     ``strdup``         n/a   musl  Yes   n/a
:ref:`Strings <model_strings>`     ``strlen``         n/a   cru.  Yes   n/a
:ref:`Strings <model_strings>`     ``strcpy``               czz   Yes   None
:ref:`Strings <model_strings>`     ``strncpy``        n/a   musl  Yes   n/a
:ref:`Strings <model_strings>`     ``strrchr``        n/a   musl  Yes   n/a
:ref:`Time <model_time>`           ``gettimeofday``   1     czz   Yes   :ref:`Docs <gettimeofday>`
:ref:`Time <model_time>`           ``time``           1     czz   Yes?  :ref:`Docs <time>`
================================== ================== ===== ===== ===== ============================

.. _model_env_vars:

Environment Variables
=====================

czz-llvm models environment variables as an array of null-terminated strings.
Each seed stores an initial array of environment variables that is propagated to
the ``envp`` parameter of ``main`` (if present). This array is part of the
interpreter state, it may be modified by calls to ``setenv`` and ``unsetenv``
and these modifications will be reflected in subsequent calls to ``getenv``.

..
  TODO(lb): Describe memory allocation strategy: non-continguous, disjoint,
  writeable (?) stack (?) allocations.

Overrides
---------

.. _getenv:

``getenv``
----------

``getenv(s)`` scans through the array of environment variables, tries to find
one of the form ``${s}=${v}`` (where ``${s}`` is the value of ``s``). If such a
value is not found, it returns ``NULL``. Otherwise, it allocates fresh memory
big enough to contain ``v``, writes ``v`` to it, and returns a pointer to that
allocation.

..
  TODO(lb): Behavior when ${v} is empty? Writability and region of memory
  allocated?

.. _model_files:

Files
=====

.. _fprintf:

``fprintf``
-----------

Only works for ``stdout`` and ``stderr``. See also :ref:`printf`.

.. _model_format:

Formatting
==========

The ``_chk`` overrides don't do any extra checking, but this might be OK since
Crucible-LLVM will detect any memory errors or undefined behavior.

.. _printf:

``printf``
----------

See `the upstream docs <https://github.com/GaloisInc/crucible/blob/ce682842f8908a04036cf651df38b131736d7068/crucible-llvm/doc/limitations.md#printf-accuracy>`_.

.. _model_math:

Math
====

.. _model_memory:

Memory
======

See `upstream documentation <https://github.com/GaloisInc/crucible/blob/master/crucible-llvm/doc/memory-model.md>`_.

.. _model_misc:

Miscellaneous
=============

.. _signal:

``signal``
----------

czz-llvm never sends signals to the target. Thus, it's sound to ignore the
signal handler passed to ``signal``, and this is what czz-llvm does. It returns
a null pointer, which may not actually be sound (null pointers are not mentioned
in the docs).

.. _model_network:

Network
=======

.. _model_stdio:

.. _accept:

``accept``
----------

This override is unsound:

- It always returns zero, instead of a new file descriptor.
- It doesn't check for usage errors, such as ``EINVAL`` or ``EOPNOTSUPP``.

It is also incomplete; it doesn't model exceptional system states like
``EAGAIN``, ``ENOMEM``, or ``EPERM``.

.. _bind:

``bind``
--------

This override is unsound; it doesn't check for usage errors, such as ``EINVAL``
or ``EADDRINUSE``.

It is also incomplete; it doesn't model exceptional system states like
``EACCES`` or ``ELOOP``.

.. _listen:

``listen``
----------

This override is unsound; it doesn't check for usage errors, such as
``EOPNOTSUPP`` or ``EADDRINUSE``.

It is also incomplete; it doesn't model failure and always returns zero.

.. _recv:

``recv``
--------

This override writes a completely random string of bytes to the input buffer.

This override is unsound; it doesn't check for usage errors, such as ``EINVAL``
or ``ENOTCONN``.

It is also incomplete:

- It doesn't model exceptional system states like ``ECONNREFUSED`` or
  ``EAGAIN``.
- It asserts that ``flags`` is zero.
- It always returns the number of bytes written, never -1.

.. _send:

``send``
--------

This override returns a random number of bytes sent.

This override is unsound; it doesn't check for usage errors, such as ``EINVAL``
or ``ENOTCONN``.

It is also incomplete; it doesn't model exceptional system states like
``ECONNRESET`` or ``ENOBUFS``. It always returns the number of bytes written,
never -1.

.. _setsockopt:

``setsockopt``
--------------

This override is unsound; it doesn't check for usage errors, such as ``EINVAL``
or ``ENOTSOCK``. It always returns zero.

.. _socket:

``socket``
----------

This override is unsound; it doesn't check for usage errors, such as ``EINVAL``.

It is also incomplete:

- It doesn't model exceptional system states like ``EPROTONOSUPPORT`` or
  ``ENOBUFS``.
- It only supports ``AF_INET``, ``SOCK_STREAM``, and a protocol of zero.

Standard I/O
============

.. _model_strings:

Strings
=======

.. _model_time:

Time
====

.. _gettimeofday:

``gettimeofday``
----------------

``gettimeofday`` always returns 0. This is sound, see the manpage:

    The time returned by gettimeofday() is affected by discontinuous jumps in the
    system time (e.g., if the sys‐ tem administrator manually changes the system
    time).

.. _time:

``time``
--------

``time`` always returns 0.

.. _#29: https://github.com/langston-barrett/czz/issues/29
.. _#30: https://github.com/langston-barrett/czz/issues/30
.. _#42: https://github.com/langston-barrett/czz/issues/42
.. _#43: https://github.com/langston-barrett/czz/issues/43
.. _#44: https://github.com/langston-barrett/czz/issues/44
.. _#45: https://github.com/langston-barrett/czz/issues/45
