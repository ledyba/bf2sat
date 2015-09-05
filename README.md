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
in: [0, 0, 0, 0] <- input data
value-bits: 8 <- Bit width of the value registers
addr-bits: 4 <- Bit width of the memory address space
steps: 18 <- Number of execution steps.
out-addr-bits: 2 <- Length of output must be less than 2^(out-addr-bits)

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

## prime factorization

Brainfuck programs that solve a NP-complete problem are too big to reduce.

These programs are long and need many steps to stop, therefore you need too huge RAM to convert. Even if you could reduce to SAT instance, SAT solver could not solve it in realistic time today.

Instead, let's solve prime factorization:

```
in: [-1,-1]
value-bits: 4
addr-bits: 3
steps:280
out-addr-bits: 0

,>,<
[>[>+>+<<-]>>[<<+>>-]<<<-]
>>---------------[]
```

This program stops if and only if multiplication of given inputs values is 15. Last "-" sequence correspondeds that. So If 15 is composite number, this program can be satisfiable, and if 15 is prime number, it's not satisfiable.

So let's ask to SAT solver:

```
> cabal run create mult.bf
** Brainfuck 2 SAT **
...
-- Create SAT problem --
To CNF...
4518175 clauses, 71314123 literals
Aliasing...
1232952 uniq predicates
write to file
All done, have fun.

> minisat sat.txt ans.txt || true
WARNING: for repeatability, setting FPU to use double precision
============================[ Problem Statistics ]=============================
|                                                                             |
|  Number of variables:       1232952                                         |
|  Number of clauses:         4513889                                         |
|  Parse time:                   3.89 s                                       |
|  Eliminated clauses:          62.90 Mb                                      |
|  Simplification time:         99.15 s                                       |
|                                                                             |
============================[ Search Statistics ]==============================
| Conflicts |          ORIGINAL         |          LEARNT          | Progress |
|           |    Vars  Clauses Literals |    Limit  Clauses Lit/Cl |          |
===============================================================================
|       100 |  250944  1437484 19489906 |   527077      100      8 |  0.089 % |
|       250 |  250944  1437484 19489906 |   579785      250      7 |  0.089 % |
|       475 |  250944  1437484 19489906 |   637763      475      7 |  0.089 % |
|       812 |  250944  1437484 19489906 |   701540      812      8 |  0.089 % |
|      1318 |  250944  1437484 19489906 |   771694     1318      8 |  0.089 % |
|      2077 |  250944  1437484 19489906 |   848863     2077      9 |  0.089 % |
|      3216 |  250944  1437484 19489906 |   933749     3216     10 |  0.089 % |
|      4924 |  250944  1437484 19489906 |  1027124     4924     10 |  0.089 % |
|      7486 |  250943  1437484 19489906 |  1129837     7485     10 |  0.089 % |
|     11330 |  250939  1437484 19489906 |  1242821    11328     10 |  0.089 % |
|     17096 |  250939  1437484 19489906 |  1367103    17094     11 |  0.089 % |
|     25745 |  250939  1437484 19489906 |  1503813    25743     12 |  0.089 % |
|     38719 |  250939  1437484 19489906 |  1654194    38717     12 |  0.089 % |
|     58180 |  250939  1437484 19489906 |  1819614    58178     12 |  0.089 % |
|     87372 |  250939  1437484 19489906 |  2001575    87370     12 |  0.089 % |
|    131161 |  250939  1437484 19489906 |  2201733   131159     13 |  0.089 % |
|    196845 |  250939  1437484 19489906 |  2421906   196843     14 |  0.089 % |
|    295371 |  250939  1437098 19487159 |  2664097   295348     15 |  0.089 % |
|    443160 |  250937  1436961 19486440 |  2930507   443129     15 |  0.089 % |
===============================================================================
restarts              : 1387
conflicts             : 645440         (4119 /sec)
decisions             : 74913312       (0.00 % random) (478068 /sec)
propagations          : 148256497      (946117 /sec)
conflict literals     : 10255868       (26.77 % deleted)
Memory used           : 1293.00 MB
CPU time              : 156.7 s

SATISFIABLE
```

This program stops in some input value sets, so 15 is composite number.

Now I can get the prime factors:

```
> cabal run decode mult.bf
...

-- Result --
          Input: [(?), (?)]
Estimated Input: [3, 5]
Estimated IDs:
0: ID {getPC = 0, getMem = [0,0,0,0,0,0,0,0], getPT = 0, getIC = 0, getOC = 0, getOut = []}
...
279: ID {getPC = 49, getMem = [0,5,0,0,0,0,0,0], getPT = 2, getIC = 2, getOC = 0, getOut = []}
All done, have fun.
```

15=3x5. That's it.
