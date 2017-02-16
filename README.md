# Virtual Machine Consolidation in Haskell

The purpose of this project if for me to develop some Haskell experience, as
well as exploring the current Haskell SAT/SMT ecosystem.

This is a solution to a variant of the problem
specified [here](./spec/project-1.pdf) using SAT/SMT. This problem was part of the
2016/2017 course on
[Algorithms for Computational Logic](https://fenix.tecnico.ulisboa.pt/disciplinas/ALC9/2016-2017/1-semestre)
at IST, University of Lisbon. You can find an already existing solution to this
problem
[here](https://github.com/rodamber/alc)
using SAT (C++ and MiniSat), SMT (Python3 and Z3) and CSP (Python3 and MiniZinc).

The input files can be found in "./input".

As of now, I have a solution to the problem using the 
[picosat package](https://hackage.haskell.org/package/picosat-0.1.4).
In the future I intend to also use
[ersatz](https://hackage.haskell.org/package/ersatz)
and
[sbv](http://leventerkok.github.io/sbv/).

## **NOTE**

The solution does not assume that "for each VM ν ∈ V , cpu_req(ν) = 1 and ram_req(ν) = 1"
as stated at the beginning of page 2 of the [spec](./spec/project-1.pdf).
