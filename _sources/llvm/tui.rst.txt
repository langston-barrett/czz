=======================
Terminal User Interface
=======================

Here's what you'll see when opening up the TUI:

.. code-block::

       ┌────────────────────czz───────────────────┐
       │┌────────────────────────────────────────┐│
       ││start:               2022-09-02 15:14:55││
       ││now:                 2022-09-02 15:14:57││
       ││duration:                             1s││
       ││execs:                               100││
       ││execs/sec:                           100││
       ││last new:                             1s││
       ││pool:                                  1││
       ││missing:                                ││
       │├────────────────────────────────────────┤│
       ││stuck:                                  ││
       ││<start>:                               1││
       │└────────────────────────────────────────┘│
       └──────────────────────────────────────────┘

Press the up arrow key to show the TUI help:

.. code-block::

      ┌─────────────────────czz─────────────────────┐
      │┌───────────────────────────────────────────┐│
      ││start: time when czz was started           ││
      ││now: current time                          ││
      ││duration: difference between now and start ││
      ││execs: total number of executions of target││
      ││execs/sec: executions per second           ││
      ││last new: time since last new coverage     ││
      ││pool: number of seeds in seed pool         ││
      ││missing: library functions with no model   ││
      │├───────────────────────────────────────────┤│
      ││ESC: exit                                  ││
      ││up: toggle help                            ││
      │└───────────────────────────────────────────┘│
      └─────────────────────────────────────────────┘
