package org.jstott.mix

import org.jstott.mix.assembler.Assembler
import org.scalatest.{FlatSpec, Matchers}

class MixSpec extends FlatSpec with Matchers {

  "Program 1.2.3M" should "run successfully" in {
    var state = new Assembler(`program 1.2.3M`).assemble.withProgramCounter(0)
    println(state.toString)
    do {
      state = state.compute
    } while (!state.terminate)
    println(state.toString)
    state.accumulator.intValue shouldBe 62
    state.index1.intValue shouldBe 10
    state.index2.intValue shouldBe 4
  }

  val `program 1.2.3M`: String =
    """X          EQU  1000
      |           ENT1 10
      |           JMP  MAXIMUM
      |           STA  MAX
      |           HLT
      |           ORIG 1000
      |           CON  2
      |           CON  6
      |           CON  3
      |           CON  12
      |           CON  62
      |           CON  36
      |           CON  28
      |           CON  10
      |           CON  46
      |           CON  37
      |           ORIG 3000
      |MAXIMUM    STJ  EXIT
      |INIT       ENT3 0,1
      |           JMP  CHANGEM
      |LOOP       CMPA X,3
      |           JGE  *+3
      |CHANGEM    ENT2 0,3
      |           LDA  X,3
      |           DEC3 1
      |           J3P  LOOP
      |EXIT       JMP  *
    """.stripMargin.trim

  "Program 1.2.3P" should "run successfully" in {
    var state = new Assembler(`program 1.2.3P`).assemble
    println(state.toString)
    do {
      val newState = state.compute
      state = newState
    } while (!state.terminate)
    println(state.toString)
    state.accumulator.intValue shouldBe 62
  }

  val `program 1.2.3P`: String =
    """* EXAMPLE PROGRAM  ...  TABLE OF PRIMES
      |*
      |L          EQU  500
      |PRINTER    EQU  18
      |PRIME      EQU  -1
      |BUF0       EQU  2000
      |BUF1       EQU  BUF0+25
      |           ORIG 3000
      |START      IOC  0(PRINTER)
      |           LD1  =1-L=
      |           LD2  =3=
      |2H         INC1 1
      |           ST2  PRIME+L,1
      |           J1Z  2F
      |4H         INC2 2
      |           ENT3 2
      |6H         ENTA 0
      |           ENTX 0,2
      |           DIV  PRIME,3
      |           JXZ  4B
      |           CMPA PRIME,3
      |           INC3 1
      |           JG   6B
      |           JMP  2B
      |2H         OUT  TITLE(PRINTER)
      |           ENT4 BUF1+10
      |           ENT5 -50
      |2H         INC5 L+1
      |4H         LDA  PRIME,5
      |           CHAR
      |           STX  0,4(1:4)
      |           DEC4 1
      |           DEC5 50
      |           J5P  4B
      |           OUT  0,4(PRINTER)
      |           LD4  24,4
      |           J5N  2B
      |           HLT
      |* INITIAL CONTENTS OF TABLES AND BUFFERS
      |           ORIG PRIME+1
      |           CON  2
      |           ORIG BUF0-5
      |TITLE      ALF  FIRST
      |           ALF   FIVE
      |           ALF   HUND
      |           ALF  RED P
      |           ALF  RIMES
      |           ORIG BUF0+24
      |           CON  BUF1+10
      |           ORIG BUF1+24
      |           CON  BUF0+10
      |           END  START
    """.stripMargin.trim

}
