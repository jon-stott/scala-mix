package org.jstott.mix

import org.scalatest.{FlatSpec, Matchers}

class WordSpec extends FlatSpec with Matchers {

  behavior of "withByte"

  it should "replace a byte" in {
    val base = Word(sign = Plus, bytes = Seq.fill(5)(MixByte(0)))
    base.withByte(MixByte(2), 1).bytes should contain theSameElementsInOrderAs Seq(MixByte(2), MixByte(0), MixByte(0), MixByte(0), MixByte(0))
    base.withByte(MixByte(2), 5).bytes should contain theSameElementsInOrderAs Seq(MixByte(0), MixByte(0), MixByte(0), MixByte(0), MixByte(2))
    base.withBytes(Seq(MixByte(2), MixByte(3)), 1).bytes should contain theSameElementsInOrderAs Seq(MixByte(2), MixByte(3), MixByte(0), MixByte(0), MixByte(0))
    base.withBytes(Seq(MixByte(2), MixByte(3)), 4).bytes should contain theSameElementsInOrderAs Seq(MixByte(0), MixByte(0), MixByte(0), MixByte(2), MixByte(3))
  }

  behavior of "apply(Int)"

  it should "handle a 6-bit number" in {
    Word.apply(10) shouldBe Word(Plus, Seq(MixByte(0), MixByte(0), MixByte(0), MixByte(0), MixByte(10)))
    Word.apply(-10) shouldBe Word(Minus, Seq(MixByte(0), MixByte(0), MixByte(0), MixByte(0), MixByte(10)))
  }

  it should "handle a 12-bit number" in {
    Word.apply(74) shouldBe Word(Plus, Seq(MixByte(0), MixByte(0), MixByte(0), MixByte(1), MixByte(10)))
    Word.apply(-74) shouldBe Word(Minus, Seq(MixByte(0), MixByte(0), MixByte(0), MixByte(1), MixByte(10)))
  }

  it should "handle an 18-bit number" in {
    Word.apply(4200) shouldBe Word(Plus, Seq(MixByte(0), MixByte(0), MixByte(1), MixByte(1), MixByte(40)))
    Word.apply(-4200) shouldBe Word(Minus, Seq(MixByte(0), MixByte(0), MixByte(1), MixByte(1), MixByte(40)))
  }

  it should "handle a 24-bit number" in {
    Word.apply(300000) shouldBe Word(Plus, Seq(MixByte(0), MixByte(1), MixByte(9), MixByte(15), MixByte(32)))
    Word.apply(-300000) shouldBe Word(Minus, Seq(MixByte(0), MixByte(1), MixByte(9), MixByte(15), MixByte(32)))
  }

  it should "handle a 30-bit number" in {
    Word.apply(20000000) shouldBe Word(Plus, Seq(MixByte(1), MixByte(12), MixByte(18), MixByte(52), MixByte(0)))
    Word.apply(-20000000) shouldBe Word(Minus, Seq(MixByte(1), MixByte(12), MixByte(18), MixByte(52), MixByte(0)))
  }

  it should "handle a 36-bit number (will overflow)" in {
    Word.apply(1073741824) shouldBe Word(Plus, Seq(MixByte(0), MixByte(0), MixByte(0), MixByte(0), MixByte(0)))
    Word.apply(-1073741825) shouldBe Word(Minus, Seq(MixByte(0), MixByte(0), MixByte(0), MixByte(0), MixByte(1)))
  }

  behavior of "maxValue"

  it should "create a Word with the highest possible value" in {
    val word = Word(Word.maxValue)
    word.sign shouldBe Plus
    word.bytes should contain theSameElementsInOrderAs Seq(MixByte(63), MixByte(63), MixByte(63), MixByte(63), MixByte(63))
  }

  behavior of "minValue"

  it should "create a Word with the lowest possible value" in {
    val word = Word(Word.minValue)
    word.sign shouldBe Minus
    word.bytes should contain theSameElementsInOrderAs Seq(MixByte(63), MixByte(63), MixByte(63), MixByte(63), MixByte(63))
  }

}
