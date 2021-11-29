# BenchmarkCC

Benchmark for the Correlation Clustering Resolution methods

* Copyright 2020-21 Nejat Arınık

*BenchmarkCC* is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation. For source availability and license information see the file `LICENCE`

* GitHub repo: https://github.com/arinik9/BenchmarkCC
* Contact: Nejat Arınık <arinik9@gmail.com>

## Description

*BenchmarkCC* aims to regroup exact and heuristic methods in the same repository in order to solve the *Correlation Clustering (CC)* problem. Exact methods can also enumerate all optimal solutions. Moreover, we can also evaluate heuristic methods based on how they explore the space of optimal solutions. See Chapters 2 and 5 in *[Arınık'21]* for more details.

Exact methods rely on two different ILP formulation types: 1) decision variables defined on vertex-pair (*Fv*: "*vertex*" formulation type) or 2) edge (*Fe*: "*edge*" formulation type). If we denote "*n*" by the number of vertices in the graph and "*m*" by the number of edges, there are *(n(n-1)/2)* variables in *Fv*, whereas there are *m* variables in *Fe*. 

The formulation *Fv* contains *n\*(n-1)\*(n-2)/2* triangle inequalities. In the literature, the authors of *[Miyauchi'18]* show that we do not need to consider all these triangle inequalities, when searching for a single optimal solution. We call F\*v this reduced form of formulation. Finally, we call F\*e the formulation based on *Fe* and defined only on cycle inequalities with a single negative edge. See Chapter 2 in *[Arınık'21]* for more details.

## Exact methods

* Finding a single optimal solution of a given instance
  * **C&B(Fv):** Cut&Branch based on the Fv formulation ([source code](https://github.com/arinik9/ExCC))
  * **C&B(F\*v):** Cut&Branch based on the F\*v formulation ([source code](https://github.com/arinik9/ExCC))
  * **B&C(Fe):** Branch&Cut based on the Fe formulation ([source code](https://github.com/arinik9/ExCC))
  * **BDCC(F\*e):** Benders Decomposition  based on the F\*e formulation ([available upon request from the authors](https://doi.org/10.1109/MLHPCAI4S51975.2020.00009))
    * Note that BDCC fails to complete the execution process for some signed graphs, it is not very stable.
* Enumerating all optimal solutions of a given instance
  * **OneTreeCC** ([source code](https://github.com/arinik9/ExCC))
    * **OneTreeCC(Fv):** OneTreeCC based on the Fv formulation
    * **OneTreeCC(F\*v):** OneTreeCC based on the F\*v formulation
    * **OneTreeCC(Fe):** OneTreeCC based on the Fe formulation
  * **EnumCC** ([source code](https://github.com/arinik9/EnumCC))
    * **EnumCC(v, k):** EnumCC based on the Fv formulation
    * **EnumCC(F\*v, k):** EnumCC based on the F\*v formulation
    * **EnumCC(Fe, k):** EnumCC based on the Fe formulation

## Heuristic methods

* **(stochastic) Iterated Local Search:** ILS-CC_l1_a<ALPHA>_g<GAIN>_p3_t<TIME_LIMIT>_i<ITER_NUMBER>  ([available upon request from the authors](https://doi.org/10.1007/s13675-017-0082-6))
* **(stochastic) Greedy Randomized Adaptive Search Procedure:** GRASP-CC_l1_a<ALPHA>_g<GAIN>_t<TIME_LIMIT>_i<ITER_NUMBER>_n1  ([available upon request from the authors](https://doi.org/10.1007/s13675-017-0082-6))
* **(stochastic) Variable neighborhood search:** Brusco-VNS-CC_t<TIME_LIMIT> ([source code](https://github.com/arinik9/HeuristicsCC))
* **(stochastic) Tabu Search:** TS-CC_t<TIME_LIMIT>  ([source code](https://github.com/arinik9/HeuristicsCC))
* **(stochastic) Simulated Annealing:** SA-CC_t<TIME_LIMIT> ([source code](https://github.com/arinik9/HeuristicsCC))
* **(stochastic) Memetic:** MLMSB-CC_r1_t<TIME_LIMIT>  ([available upon request from the authors](https://doi.org/10.1016/j.knosys.2015.05.006))
* **(deterministic) Message Passing, followed by Greedy Additive Edge Contraction and Kernighan-Lin with joins:** MP-GAEC-KLj-CC_t<TIME_LIMIT> ([source code](https://github.com/LPMP/))
* **(deterministic) Iterative Cycle Packing, followed by Greedy Additive Edge Contraction and Kernighan-Lin with joins:** ICP-GAEC-KLj-CC_t<TIME_LIMIT> ([source code](https://github.com/LPMP/))
* **(deterministic) Greedy Additive Edge Contraction, followed by Kernighan-Lin with joins:** GAEC-KLj-CC_t<TIME_LIMIT> ([source code](https://github.com/LPMP/))

## Data

Input signed networks are generated through [this](https://github.com/CompNet/SignedBenchmark) random signed network generator. You can also download already obtained partition results from Datasets 2.2 and 5.1 on [Figshare](https://doi.org/10.6084/m9.figshare.14551113), and place them into the `out` folder.

## Installation
1. Install the [`R` language](https://www.r-project.org/)
2. Install the following R packages:
   * [`igraph`](http://igraph.org/r/) Tested with the version 1.2.6.
   * [`XML`](https://cran.r-project.org/web/packages/XML/index.html)
   * [`alluvial`](https://cran.r-project.org/web/packages/alluvial/)
   * [`cluster`](https://cran.r-project.org/web/packages/cluster/)
   * [`stringr`](https://cran.r-project.org/web/packages/stringr/)
   * [`plotrix`](https://cran.r-project.org/web/packages/plotrix/)
   * [`RColorBrewer`](https://cran.r-project.org/web/packages/RColorBrewer/)
   * [`vioplot`](https://cran.r-project.org/web/packages/vioplot/)
   * [`clues`](https://cran.r-project.org/web/packages/clues/)
   * [`NMF`](https://cran.r-project.org/web/packages/NMF/)
   * [`entropy`](https://cran.r-project.org/web/packages/entropy/)
   * [`e1071`](https://cran.r-project.org/web/packages/e1071/)
   * [`parallel`](https://cran.r-project.org/web/packages/parallel/)
   * [`doParallel`](https://cran.r-project.org/web/packages/doParallel/)
   * [`iterators`](https://cran.r-project.org/web/packages/iterators/)
   * [`bigmemory`](https://cran.r-project.org/web/packages/bigmemory/)
   * [`expm`](https://cran.r-project.org/web/packages/expm/)
3. Install [`IBM CPlex`](https://www.ibm.com/developerworks/community/blogs/jfp/entry/CPLEX_Is_Free_For_Students?lang=en). Tested with the version 12.8 and 20.1. Set correctly the variable `CPLEX.BIN.PATH` in `define-algos.R` (e.g. `/opt/ibm/ILOG/CPLEX_Studio128/cplex/bin/x86-64_linux/`). Note that BDCC runs only with Cplex 12.8.
   * For ubuntu, type the following command:
     * `sudo ./cplex_studio<YOUR_VERSION>.linux-x86-64.bin` 
       * The default installation location for education version is: `/opt/ibm/ILOG/CPLEX_Studio<YOUR_VERSION`.
       * The default installation location for trial version is: `/opt/ibm/ILOG/CPLEX_Studio_Community<YOUR_VERSION/cplex/bin/x86-64_linux/`.
4. Download exact and heuristic methods, as indicated above, and place them into the `lib` folder.


## Use
1. Set correctly the variable `CPLEX.BIN.PATH` in the file `src/define-algos.R`.
2. Set correctly the MATLAB installation path in `get.MLMSB.command()`, `get.BDCC.command()` and `get.ZONOCC.command()` in the file `src/define-algos.R` 
3. Open the `R` console.
4. Set the current directory as the working directory, using `setwd("<my directory>")`.
5. Run the main script `src/main.R`.

## References
* **[Arınık'21]** N. Arınık, [*Multiplicity in the Partitioning of Signed Graphs*](https://www.theses.fr/2021AVIG0285). PhD thesis in Avignon Université (2021).

* **[Miyauchi'18]** A. Miyauchi, T. Sonobe, and N. Sukegawa,  *Exact Clustering via Integer Programming and Maximum Satisfiability*, in: AAAI Conference on Artificial Intelligence 32.1 (2018).
