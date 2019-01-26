package org.jstott.mix.assembler

import org.scalatest.{FlatSpec, Matchers}

class AssemblerSpec extends FlatSpec with Matchers {

  behavior of "Assembler"

  it should "assemble" in {
    val assembler = new Assembler(`program 1.2.3M`)
    assembler.assemble
  }

  val `program 1.2.3M`: String =
    """X          EQU  1000
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
      |EXIT       HLT
    """.stripMargin.trim

}
