Microcluster
============

A suite of tools to distribute your programs to a cluster of microcontrollers, for scientific computation and machine learning purposes.

[Microcluster Execute](./microcluster_exec) is a Python interpreter that dissects your program and orchestrates distributed tasks with a micro-cluster. It is written in OCaml.

[MicroPython Remote](./mpremote) is an OCaml reimplementation of MicroPython's `mpremote` program with first-class programmatic API and concurrent communication. 

[The ports directory](./ports) contains drivers for specific microcontrollers and devices.

[Microcluster Execute 2.0](./microcluster_exec_2_0) is an OCaml interpreter for parallelizing abstract vector math via distributing tasks to a micro-cluster.
