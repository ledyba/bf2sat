# 非決定性BrainfuckからSATへの多項式時間還元

日本語の説明スライドは[こちら](https://docs.google.com/presentation/d/1f91sMZT_KuBW2vNO-CmrvjVrshrVqfo6YuYo5T5Glo0/edit?usp=sharing)

# Brainfuck to SAT Converter

A polynomial-time reduction from a Brainfuck program to a SAT instance.

Any non-deterministic Turing machine that solves any NP-problem in polynomial-time is reducible to  a SAT instance in polynomial-time. Any NP problem that is reducible from SAT can be solved by non-det Turing machine, therefore, they are also reducible to SAT. Hence, All problems that are reduced from SAT are reducible to each other. This is a key idea of **NP-complete**.

Brainfuck is Turing-complete, that is, Brainfuck programs and Turing-machines have the same power. Hence, Brainfuck programs that solve NP-problems are also reducible to SAT instance.

## Build

You need [Haskell Platform](https://www.haskell.org/platform/)

```
cabal sandbox init
cabal install
```

## Demo: simple sample

Here is a very simple sample:

```
in: [0, 0, 0, 0]
value-bits: 8
addr-bits: 4
steps: 18
out-addr-bits: 2

++[->++<]
```

Make a SAT problem:

```
> cabal run create simple.bf

** Brainfuck 2 SAT **
-- Setting --
  src:++[->++<]
  ast:[ValInc,ValInc,LoopBegin 9,ValDec,PtInc,ValInc,ValInc,PtDec,LoopEnd 3]
  in: [0, 0, 0, 0]
  value-bits: 8
  addr-bits:4
  out-addr-bits:2
  sim-steps:18
-- Create SAT problem --
To CNF...
313372 clauses, 4088273 literals
Aliasing...
84933 uniq predicates
writ to file
All done, have fun.

> less sat.txt
```

You can solve this SAT instance by any SAT solver as you like, for example, minisat.

```
> minisat sat.txt ans.txt

WARNING: for repeatability, setting FPU to use double precision
============================[ Problem Statistics ]=============================
|                                                                             |
|  Number of variables:         84933                                         |
|  Number of clauses:          307111                                         |
|  Parse time:                   0.23 s                                       |
|  Eliminated clauses:           0.61 Mb                                      |
|  Simplification time:          8.53 s                                       |
|                                                                             |
============================[ Search Statistics ]==============================
| Conflicts |          ORIGINAL         |          LEARNT          | Progress |
|           |    Vars  Clauses Literals |    Limit  Clauses Lit/Cl |          |
===============================================================================
===============================================================================
restarts              : 1
conflicts             : 0              (0 /sec)
decisions             : 1              (0.00 % random) (0 /sec)
propagations          : 5391           (615 /sec)
conflict literals     : 0              (-nan % deleted)
Memory used           : 88.00 MB
CPU time              : 8.768 s

SATISFIABLE
```

You can decode the answer to IDs of Brainfuck.

```
> cabal run decode simple.bf

** Brainfuck 2 SAT **
-- Setting --
  src:++[->++<]
  ast:[ValInc,ValInc,LoopBegin 9,ValDec,PtInc,ValInc,ValInc,PtDec,LoopEnd 3]
  in: [0, 0, 0, 0]
  value-bits: 8
  addr-bits:4
  out-addr-bits:2
  sim-steps:18
-- Result --
         Source:++[->++<]
          Input: [0, 0, 0, 0]
Estimated Input: [0, 0, 0, 0]
Estimated IDs:
0: ID {getPC = 0, getMem = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], getPT = 0, getIC = 0, getOC = 0, getOut = []}
1: ID {getPC = 1, getMem = [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], getPT = 0, getIC = 0, getOC = 0, getOut = []}
2: ID {getPC = 2, getMem = [2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], getPT = 0, getIC = 0, getOC = 0, getOut = []}
3: ID {getPC = 3, getMem = [2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], getPT = 0, getIC = 0, getOC = 0, getOut = []}
4: ID {getPC = 4, getMem = [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], getPT = 0, getIC = 0, getOC = 0, getOut = []}
5: ID {getPC = 5, getMem = [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], getPT = 1, getIC = 0, getOC = 0, getOut = []}
6: ID {getPC = 6, getMem = [1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0], getPT = 1, getIC = 0, getOC = 0, getOut = []}
7: ID {getPC = 7, getMem = [1,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0], getPT = 1, getIC = 0, getOC = 0, getOut = []}
8: ID {getPC = 8, getMem = [1,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0], getPT = 0, getIC = 0, getOC = 0, getOut = []}
9: ID {getPC = 3, getMem = [1,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0], getPT = 0, getIC = 0, getOC = 0, getOut = []}
10: ID {getPC = 4, getMem = [0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0], getPT = 0, getIC = 0, getOC = 0, getOut = []}
11: ID {getPC = 5, getMem = [0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0], getPT = 1, getIC = 0, getOC = 0, getOut = []}
12: ID {getPC = 6, getMem = [0,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0], getPT = 1, getIC = 0, getOC = 0, getOut = []}
13: ID {getPC = 7, getMem = [0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0], getPT = 1, getIC = 0, getOC = 0, getOut = []}
14: ID {getPC = 8, getMem = [0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0], getPT = 0, getIC = 0, getOC = 0, getOut = []}
15: ID {getPC = 9, getMem = [0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0], getPT = 0, getIC = 0, getOC = 0, getOut = []}
16: ID {getPC = 9, getMem = [0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0], getPT = 0, getIC = 0, getOC = 0, getOut = []}
17: ID {getPC = 9, getMem = [0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0], getPT = 0, getIC = 0, getOC = 0, getOut = []}
All done, have fun.
```

## Introducing non-determinism

If you set input values to -1, bf2sat set them as "?" and a SAT solver fills the blank.

```
in: [-1, 0, 0, 0]
value-bits: 8
addr-bits: 4
steps:22
out-addr-bits: 2

,[>++<-]>----[]
```

This program stops if and only if first byte of input value is 2, and reduced SAT instance is satisfied if and only if the corresponded logical variables mean "2".

Here is the result:

```
-- Result --
         Source:,[>++<-]>----[]
          Input: [(?), 0, 0, 0]
Estimated Input: [2, 0, 0, 0] <- SAT solver decides the suitable answer.
Estimated IDs:
0: ID {getPC = 0, getMem = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], getPT = 0, getIC = 0, getOC = 0, getOut = []}
1: ID {getPC = 1, getMem = [2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], getPT = 0, getIC = 1, getOC = 0, getOut = []}
2: ID {getPC = 2, getMem = [2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], getPT = 0, getIC = 1, getOC = 0, getOut = []}
3: ID {getPC = 3, getMem = [2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], getPT = 1, getIC = 1, getOC = 0, getOut = []}

....

20: ID {getPC = 15, getMem = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], getPT = 1, getIC = 1, getOC = 0, getOut = []}
21: ID {getPC = 15, getMem = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], getPT = 1, getIC = 1, getOC = 0, getOut = []}
All done, have fun.
```
