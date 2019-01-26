# scala-mix
A MIX emulator written in Scala.

## About

This project simulates the MIX machine described by Donald Knuth in Volume 1 of _The Art of Computer Programming_ (TAOCP) [1]. The project also includes an assembler for MIXAL (the MIX assembly language).

**I am still in the process of writing this but it successfully assembles some MIX programs and some of them even work!** _Most operations have been implemented but not throughly tested yet. There's probably a lot of refactoring to do!_

## Running

To run the emulator, do

```bash
sbt "run program.mix"
```

where `program.mix` is the file name of a text file containing assembler code.

Assembler code requires formatting according to the rules defined in TAOCP, including ensuring that the location section is ten columns wide and that columns 11 and 16 are blank. For example, this is the basic program given by Knuth in section 1.3.2:

```
X          EQU  1000
           ENT1 10
           JMP  MAXIMUM
           STA  MAX
           HLT
           ORIG 1000
           CON  2
           CON  6
           CON  3
           CON  12
           CON  62
           CON  36
           CON  28
           CON  10
           CON  46
           CON  37
           ORIG 3000
MAXIMUM    STJ  EXIT
INIT       ENT3 0,1
           JMP  CHANGEM
LOOP       CMPA X,3
           JGE  *+3
CHANGEM    ENT2 0,3
           LDA  X,3
           DEC3 1
           J3P  LOOP
EXIT       JMP  *
```
_**Program 1.3.2M.** Find the maximum_

The program has been modified to store ten values in memory locations 1000 through 1009.

Running the program will print the state of the machine after assembling the code into memory, then compute the program and then print the final state of the machine. For Program 1.3.2M, the initial state looks like

```
+-----------------------------------------------------------------------------------------------------------------------+
|                                                          MIX                                                          |
+-----------------------------------------------------------------------------------------------------------------------+
|     rA = + 00 00 (    0) 00 00 00 [          0]   rX = + 00 00 (    0) 00 00 00 [          0]   rJ =   00 00 (    0)  |
|    rI1 = + 00 00 (    0)                         rI2 = + 00 00 (    0)                         rI3 = + 00 00 (    0)  |
|    rI4 = + 00 00 (    0)                         rI5 = + 00 00 (    0)                         rI6 = + 00 00 (    0)  |
|     pc =     0                              overflow = Off                              comparison =   Equal          |
+-----------------------------------------------------------------------------------------------------------------------+
| ->            0000 = + 00 10 (   10) 00 02 49 (ENT1) [    2621617]    x =        0    m =        1                    |
|               0001 = + 46 56 ( 3000) 00 00 39 ( JMP) [  786432039]    x =        0    m =        1                    |
|               0003 = + 00 00 (    0) 00 02 05 ( HLT) [        133]    x =        0    m =        1                    |
|               1000 = + 00 00 (    0) 00 00 02 (    ) [          2]    x =        0    m =        1                    |
|               1001 = + 00 00 (    0) 00 00 06 (    ) [          6]    x =        0    m =        1                    |
|               1002 = + 00 00 (    0) 00 00 03 (    ) [          3]    x =        0    m =        1                    |
|               1003 = + 00 00 (    0) 00 00 12 ( LD4) [         12]    x =        0    m =        1                    |
|               1004 = + 00 00 (    0) 00 00 62 (    ) [         62]    x =        0    m =        1                    |
|               1005 = + 00 00 (    0) 00 00 36 (    ) [         36]    x =        0    m =        1                    |
|               1006 = + 00 00 (    0) 00 00 28 ( ST4) [         28]    x =        0    m =        1                    |
|               1007 = + 00 00 (    0) 00 00 10 ( LD2) [         10]    x =        0    m =        1                    |
|               1008 = + 00 00 (    0) 00 00 46 ( J6N) [         46]    x =        0    m =        1                    |
|               1009 = + 00 00 (    0) 00 00 37 ( OUT) [         37]    x =        0    m =        1                    |
|               3000 = + 47 01 ( 3009) 00 02 32 ( STJ) [  788791456]    x =        0    m =        1                    |
|               3001 = + 00 00 (    0) 01 02 51 (ENT3) [       4275]    x =        0    m =        1                    |
|               3002 = + 46 61 ( 3005) 00 00 39 ( JMP) [  787742759]    x =        0    m =        1                    |
|               3003 = + 15 40 ( 1000) 03 05 56 (CMPA) [  262156664]    x =        0    m =        1                    |
|               3004 = + 46 63 ( 3007) 00 07 39 ( JGE) [  788267495]    x =        0    m =        1                    |
|               3005 = + 00 00 (    0) 03 02 50 (ENT2) [      12466]    x =        0    m =        1                    |
|               3006 = + 15 40 ( 1000) 03 05 08 ( LDA) [  262156616]    x =        0    m =        1                    |
|               3007 = + 00 01 (    1) 00 01 51 (DEC3) [     262259]    x =        0    m =        1                    |
|               3008 = + 46 59 ( 3003) 00 02 43 ( J3P) [  787218603]    x =        0    m =        1                    |
|               3009 = + 47 01 ( 3009) 00 00 39 ( JMP) [  788791335]    x =        0    m =        1                    |
+-----------------------------------------------------------------------------------------------------------------------+
```

and the final state looks like (note the result of 62 in `rA` and the index offset 4 from memory location 1000 is in `rI2`):

```
+-----------------------------------------------------------------------------------------------------------------------+
|                                                          MIX                                                          |
+-----------------------------------------------------------------------------------------------------------------------+
|     rA = + 00 00 (    0) 00 00 62 [         62]   rX = + 00 00 (    0) 00 00 00 [          0]   rJ =   47 02 (    2)  |
|    rI1 = + 00 10 (   10)                         rI2 = + 00 04 (    4)                         rI3 = + 00 00 (    0)  |
|    rI4 = + 00 00 (    0)                         rI5 = + 00 00 (    0)                         rI6 = + 00 00 (    0)  |
|     pc =     3                              overflow = Off                              comparison = Greater          |
+-----------------------------------------------------------------------------------------------------------------------+
|               0000 = + 00 10 (   10) 00 02 49 (ENT1) [    2621617]    x =        1    m =        1                    |
|               0001 = + 46 56 ( 3000) 00 00 39 ( JMP) [  786432039]    x =        1    m =        1                    |
| ->            0003 = + 00 00 (    0) 00 02 05 ( HLT) [        133]    x =        1    m =        1                    |
|               1000 = + 00 00 (    0) 00 00 02 (    ) [          2]    x =        0    m =        1                    |
|               1001 = + 00 00 (    0) 00 00 06 (    ) [          6]    x =        0    m =        1                    |
|               1002 = + 00 00 (    0) 00 00 03 (    ) [          3]    x =        0    m =        1                    |
|               1003 = + 00 00 (    0) 00 00 12 ( LD4) [         12]    x =        0    m =        1                    |
|               1004 = + 00 00 (    0) 00 00 62 (    ) [         62]    x =        0    m =        1                    |
|               1005 = + 00 00 (    0) 00 00 36 (    ) [         36]    x =        0    m =        1                    |
|               1006 = + 00 00 (    0) 00 00 28 ( ST4) [         28]    x =        0    m =        1                    |
|               1007 = + 00 00 (    0) 00 00 10 ( LD2) [         10]    x =        0    m =        1                    |
|               1008 = + 00 00 (    0) 00 00 46 ( J6N) [         46]    x =        0    m =        1                    |
|               1009 = + 00 00 (    0) 00 00 37 ( OUT) [         37]    x =        0    m =        1                    |
|               3000 = + 47 01 ( 3009) 00 02 32 ( STJ) [  788791456]    x =        1    m =        1                    |
|               3001 = + 00 00 (    0) 01 02 51 (ENT3) [       4275]    x =        1    m =        1                    |
|               3002 = + 46 61 ( 3005) 00 00 39 ( JMP) [  787742759]    x =        1    m =        1                    |
|               3003 = + 15 40 ( 1000) 03 05 56 (CMPA) [  262156664]    x =        9    m =        1                    |
|               3004 = + 46 63 ( 3007) 00 07 39 ( JGE) [  788267495]    x =        9    m =        1                    |
|               3005 = + 00 00 (    0) 03 02 50 (ENT2) [      12466]    x =        4    m =        1                    |
|               3006 = + 15 40 ( 1000) 03 05 08 ( LDA) [  262156616]    x =        4    m =        1                    |
|               3007 = + 00 01 (    1) 00 01 51 (DEC3) [     262259]    x =       10    m =        1                    |
|               3008 = + 46 59 ( 3003) 00 02 43 ( J3P) [  787218603]    x =       10    m =        1                    |
|               3009 = + 00 02 (    2) 00 00 39 ( JMP) [     524327]    x =        1    m =        2                    |
+-----------------------------------------------------------------------------------------------------------------------+
```

The display shows the values of the various registers and status flags in the top part:
* `rA` - register A - a signed word
* `rX` - register X - a signed word
* `rI1` - register I1 (index) - two bytes with sign
* `rI2` - register I2 (index) - two bytes with sign
* `rI3` - register I3 (index) - two bytes with sign
* `rI4` - register I4 (index) - two bytes with sign
* `rI5` - register I5 (index) - two bytes with sign
* `rI6` - register I6 (index) - two bytes with sign
* `rJ` - register J (jump) - two bytes without sign
* `pc` - program counter (or location counter)
* `overflow` - flag to indicate overflow status, either `On` or `Off`
* `comparison` - comparison flag, one of `Less`, `Equal` or `Greater`

The bottom part shows the memory of the machine. The `->` indicates the memory location that the location counter is at, the memory cell number, the value of the memory cell, the number of times that memory cell has been executed (`x = ...`) and the number of times that memory cell has been modified (`m = ...`).

A word is displayed as, e.g. `+ 00 00 (    0) 01 02 51 (ENT3) [       4275]`. The sign is shown as well as the five decimal bytes (without parentheses or brackets). The first parenthesised number is the decimal value of the first two bytes (the location). The second parenthesis includes the instruction that the memory cell represents (note that some cells may just "appear" to include an instruction, such as memory cell 1003 above). The square brackets contain the value if all five bytes were treated as a single decimal number.

## Assembly process

The assembly process includes two passes over the program. This is to allow forward symbol references to be resolved. In order to do this, the `AssemblerState` includes a map of unevaluated operations which will be resolved in the second pass. The `Mix` state includes maps of the symbols as well as local symbols (`1H..9H`).

The assembler first lexes the code and the parses it into an abstract syntax tree. This is done using Scala Parse Combinators [2].

## IO Devices

Coming soon...

## References

[1] Knuth, Donald E., _The Art of Computer Programming_, Vol. 1, pp. 124-185, 3rd Edition, 1997, Addison-Wesley

[2] Scala Parse Combinators. https://github.com/scala/scala-parser-combinators