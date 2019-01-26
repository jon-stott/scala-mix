package org.jstott.mix.assembler

import org.jstott.mix.{Minus, Plus}
import org.scalatest.{FlatSpec, Matchers}

class AddressParserSpec extends FlatSpec with Matchers {

  behavior of "AddressParser"

  it should "parse 'PRIME'" in {
    val expected = Right(
      ValueExpression(SignedAtomicExpression(Plus, Symbol("Prime")))
    )
    AddressParser.apply(List(SymbolToken("Prime"))) shouldBe expected
  }

  it should "parse '1'" in {
    val tokens = List(NumberToken(1))
    val expected = Right(
      ValueExpression(SignedAtomicExpression(Plus, Number(1)))
    )
    AddressParser.apply(tokens) shouldBe expected
  }

  it should "parse '-1'" in {
    val tokens = List(MinusToken, NumberToken(1))
    val expected = Right(
      ValueExpression(SignedAtomicExpression(Minus, Number(1)))
    )
    AddressParser.apply(tokens) shouldBe expected
  }

  it should "parse '1,2'" in {
    val tokens = List(NumberToken(1), CommaToken, NumberToken(2))
    val expected = Right(
      ValueExpression(SignedAtomicExpression(Plus, Number(1)), SignedAtomicExpression(Plus, Number(2)))
    )
    AddressParser.apply(tokens) shouldBe expected
  }

  it should "parse '1(2:5)'" in {
    val tokens = List(NumberToken(1), OpenParenthesisToken, NumberToken(2), ColonToken, NumberToken(5),
      CloseParenthesisToken)
    val expected = Right(
      ValueExpression(
        SignedAtomicExpression(Plus, Number(1)),
        Vacuous,
        FieldExpression(Number(2), Number(5))
      )
    )
    AddressParser.apply(tokens) shouldBe expected
  }

  it should "parse '1,2(3:5)'" in {
    val tokens = List(NumberToken(1), CommaToken, NumberToken(2), OpenParenthesisToken, NumberToken(3), ColonToken,
      NumberToken(5), CloseParenthesisToken)
    val expected = Right(
      ValueExpression(
        SignedAtomicExpression(Plus, Number(1)),
        SignedAtomicExpression(Plus, Number(2)),
        FieldExpression(Number(3), Number(5))
      )
    )
    AddressParser.apply(tokens) shouldBe expected
  }

  it should "parse '1+5-X'" in {
    val result = AddressParser.apply(List(NumberToken(1), PlusToken, NumberToken(5), MinusToken, SymbolToken("X")))
    val expected = Right(
      ValueExpression(
        MinusExpression(
          AddExpression(
            SignedAtomicExpression(Plus, Number(1)),
            SignedAtomicExpression(Plus, Number(5))
          ),
          SignedAtomicExpression(Plus, Symbol("X"))
        )
      )
    )
    result shouldBe expected
  }

  it should "parse '1+5*20/6'" in {
    val result = AddressParser.apply(List(NumberToken(1), PlusToken, NumberToken(5), AsteriskToken, NumberToken(20), DivideToken, NumberToken(6)))
    val expected = Right(
      ValueExpression(
        DivideExpression(
          MultiplyExpression(
            AddExpression(
              SignedAtomicExpression(Plus, Number(1)),
              SignedAtomicExpression(Plus, Number(5))
            ),
            SignedAtomicExpression(Plus, Number(20))
          ),
          SignedAtomicExpression(Plus, Number(6))
        )
      )
    )
    result shouldBe expected
  }

  it should "parse '1//3'" in {
    val result = AddressParser.apply(List(NumberToken(1), FractionToken, NumberToken(3)))
    val expected = Right(
      ValueExpression(
        FractionExpression(
          SignedAtomicExpression(Plus, Number(1)),
          SignedAtomicExpression(Plus, Number(3))
        )
      )
    )
    result shouldBe expected
  }

  it should "parse '1:3'" in {
    val result = AddressParser.apply(List(NumberToken(1), ColonToken, NumberToken(3)))
    val expected = Right(
      ValueExpression(
        FieldExpression(
          SignedAtomicExpression(Plus, Number(1)),
          SignedAtomicExpression(Plus, Number(3))
        )
      )
    )
    result shouldBe expected
  }

  it should "parse '*-3'" in {
    val result = AddressParser.apply(List(AsteriskToken, MinusToken, NumberToken(3)))
    val expected = Right(
      ValueExpression(
        MinusExpression(
          SignedAtomicExpression(Plus, LocationCounter),
          SignedAtomicExpression(Plus, Number(3))
        )
      )
    )
    result shouldBe expected
  }

  it should "parse '=1-L='" in {
    val result = AddressParser.apply(List(EqualsToken, NumberToken(1), MinusToken, SymbolToken("L"), EqualsToken))
    val expected = Right(
      LiteralExpression(
        MinusExpression(
          SignedAtomicExpression(Plus, Number(1)),
          SignedAtomicExpression(Plus, Symbol("L"))
        )
      )
    )
    result shouldBe expected
  }

  it should "parse '***'" in {
    val result = AddressParser.apply(List(AsteriskToken, AsteriskToken, AsteriskToken))
    val expected = Right(
      ValueExpression(
        MultiplyExpression(
          SignedAtomicExpression(Plus, LocationCounter),
          SignedAtomicExpression(Plus, LocationCounter)
        )
      )
    )
    result shouldBe expected
  }

  it should "parse '1,1000(0:2)'" in {
    val result = AddressParser.apply(List(NumberToken(1), CommaToken, NumberToken(1000), OpenParenthesisToken,
      NumberToken(0), ColonToken, NumberToken(2), CloseParenthesisToken))
    val expected = Right(
      ValueExpression(
        SignedAtomicExpression(Plus, Number(1)),
        SignedAtomicExpression(Plus, Number(1000)),
        FieldExpression(Number(0), Number(2))
      )
    )
    result shouldBe expected
  }

  it should "parse 'PRIME+L,1'" in {
    val result = AddressParser.apply(List(SymbolToken("PRIME"), PlusToken, SymbolToken("L"), CommaToken, NumberToken(1)))
    val expected = Right(
      ValueExpression(
        a = AddExpression(
          SignedAtomicExpression(Plus, Symbol("PRIME")),
          SignedAtomicExpression(Plus, Symbol("L"))
        ),
        i = SignedAtomicExpression(Plus, Number(1)),
        f = Vacuous
      )
    )
    result shouldBe expected
  }

}
