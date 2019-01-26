package org.jstott.mix.assembler

import org.scalatest.{FlatSpec, Matchers}

class MixParserSpec extends FlatSpec with Matchers {

  behavior of "MixLexer"

  it should "parse 1" in {
    val program = """X          EQU  1000
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
                    |EXIT       HLT""".stripMargin
    println(program)
    val result = MixParser.apply(program)
    val expected = Right(
      List(
        MixProgramLine(
          Some(LocationToken("X")),
          EquToken,
          Some(ValueExpression(Number(1000)))
        ),
        MixProgramLine(
          None,
          OrigToken,
          Some(ValueExpression(Number(3000)))
        ),
        MixProgramLine(
          Some(LocationToken("MAXIMUM")),
          StjToken,
          Some(ValueExpression(Symbol("EXIT")))
        ),
        MixProgramLine(
          Some(LocationToken("INIT")),
          Ent3Token,
          Some(ValueExpression(Number(0), Number(1)))
        ),
        MixProgramLine(
          None,
          JmpToken,
          Some(ValueExpression(Symbol("CHANGEM")))
        ),
        MixProgramLine(
          Some(LocationToken("LOOP")),
          CmpaToken,
          Some(ValueExpression(Symbol("X"), Number(3)))
        ),
        MixProgramLine(
          None,
          JgeToken,
          Some(AddExpression(ValueExpression(LocationCounter), ValueExpression(Number(3))))
        ),
        MixProgramLine(
          Some(LocationToken("CHANGEM")),
          Ent2Token,
          Some(ValueExpression(Number(0), Number(3)))
        ),
        MixProgramLine(
          None,
          LdaToken,
          Some(ValueExpression(Symbol("X"), Number(3)))
        ),
        MixProgramLine(
          None,
          Dec3Token,
          Some(ValueExpression(Number(1)))
        ),
        MixProgramLine(
          None,
          J3pToken,
          Some(ValueExpression(Symbol("LOOP")))
        ),
        MixProgramLine(
          Some(LocationToken("EXIT")),
          HltToken,
          None
        )
      )
    )
    result shouldBe expected
  }

}
