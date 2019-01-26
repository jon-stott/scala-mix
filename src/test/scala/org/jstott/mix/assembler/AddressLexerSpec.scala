package org.jstott.mix.assembler

import org.scalatest.{FlatSpec, Matchers}

class AddressLexerSpec extends FlatSpec with Matchers {

  behavior of "AddressLexer"

  it should "lex 1+5" in {
    val result = AddressLexer.apply("1+5")
    result shouldBe Right(List(NumberToken(1), PlusToken, NumberToken(5)))
  }

  it should "lex '0,1'" in {
    val result = AddressLexer.apply("0,1")
    result shouldBe Right(List(NumberToken(0), CommaToken, NumberToken(1)))
  }

  it should "lex '*+3'" in {
    val result = AddressLexer.apply("*+3")
    result shouldBe Right(List(AsteriskToken, PlusToken, NumberToken(3)))
  }

  it should "lex '=1-L='" in {
    val result = AddressLexer.apply("=1-L=")
    result shouldBe Right(List(EqualsToken, NumberToken(1), MinusToken, SymbolToken("L"), EqualsToken))
  }

  it should "lex symbols" in {
    AddressLexer.apply("PRIME") shouldBe Right(List(SymbolToken("PRIME")))
    AddressLexer.apply("TEMP") shouldBe Right(List(SymbolToken("TEMP")))
    AddressLexer.apply("20BY20") shouldBe Right(List(SymbolToken("20BY20")))
    AddressLexer.apply("1H") shouldBe Right(List(SymbolToken("1H")))
    AddressLexer.apply("1B") shouldBe Right(List(SymbolToken("1B")))
    AddressLexer.apply("9F") shouldBe Right(List(SymbolToken("9F")))
  }

  it should "lex numbers" in {
    AddressLexer.apply("1") shouldBe Right(List(NumberToken(1)))
    AddressLexer.apply("1234567890") shouldBe Right(List(NumberToken(1234567890)))
    AddressLexer.apply("00052") shouldBe Right(List(NumberToken(52)))
  }

  it should "lex '-1'" in {
    val result = AddressLexer.apply("-1")
    result shouldBe Right(List(MinusToken, NumberToken(1)))
  }

}
