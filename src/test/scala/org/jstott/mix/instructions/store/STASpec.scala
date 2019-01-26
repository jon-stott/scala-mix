package org.jstott.mix.instructions.store

import org.jstott.mix._
import org.scalatest.{FlatSpec, Matchers}

class STASpec extends FlatSpec with Matchers {

  behavior of "apply"

  it should "create an STA instruction when not specifying the field" in {
    val result = STA.apply(TwoSignedBytes.apply(10))
    result.A shouldBe TwoSignedBytes(Plus, Seq(MixByte(0), MixByte(10)))
    result.I shouldBe MixByte(0)
    result.F.asField shouldBe Field(0, 5)
    result.C shouldBe MixByte(24)
  }

  it should "create an STA instruction when specifying the field" in {
    val result = STA.apply(TwoSignedBytes.apply(10), Field(2, 3))
    result.A shouldBe TwoSignedBytes(Plus, Seq(MixByte(0), MixByte(10)))
    result.I shouldBe MixByte(0)
    result.F.asField shouldBe Field(2, 3)
    result.C shouldBe MixByte(24)
  }

  behavior of "compute"

  it should "correctly update the accumulator with field (0:5)" in {
    val initialAccumulator = Word(Plus, Seq(MixByte(6), MixByte(7), MixByte(8), MixByte(9), MixByte(0)))
    val theWord = Word(Minus, Seq(MixByte(1), MixByte(2), MixByte(3), MixByte(4), MixByte(5)))
    val mix = Mix(accumulator = initialAccumulator).withMemoryUpdated(theWord, 2000)
    val sta = STA(TwoSignedBytes.apply(2000))
    sta.toString shouldBe "STA 2000"
    val result = sta.compute(mix)
    result.memory(2000) shouldBe initialAccumulator
    result.accumulator shouldBe initialAccumulator
  }

  it should "correctly update the accumulator with field (1:5)" in {
    val initialAccumulator = Word(Plus, Seq(MixByte(6), MixByte(7), MixByte(8), MixByte(9), MixByte(0)))
    val theWord = Word(Minus, Seq(MixByte(1), MixByte(2), MixByte(3), MixByte(4), MixByte(5)))
    val mix = Mix(accumulator = initialAccumulator).withMemoryUpdated(theWord, 2000)
    val sta = STA(TwoSignedBytes.apply(2000), Field(1, 5))
    sta.toString shouldBe "STA 2000(1:5)"
    val result = sta.compute(mix)
    val expectedWord = Word(Minus, Seq(MixByte(6), MixByte(7), MixByte(8), MixByte(9), MixByte(0)))
    result.memory(2000) shouldBe expectedWord
    result.accumulator shouldBe initialAccumulator
  }

  it should "correctly update the accumulator with field (5:5)" in {
    val initialAccumulator = Word(Plus, Seq(MixByte(6), MixByte(7), MixByte(8), MixByte(9), MixByte(0)))
    val theWord = Word(Minus, Seq(MixByte(1), MixByte(2), MixByte(3), MixByte(4), MixByte(5)))
    val mix = Mix(accumulator = initialAccumulator).withMemoryUpdated(theWord, 2000)
    val sta = STA(TwoSignedBytes.apply(2000), Field(5, 5))
    sta.toString shouldBe "STA 2000(5:5)"
    val result = sta.compute(mix)
    val expectedWord = Word(Minus, Seq(MixByte(1), MixByte(2), MixByte(3), MixByte(4), MixByte(0)))
    result.memory(2000) shouldBe expectedWord
    result.accumulator shouldBe initialAccumulator
  }

  it should "correctly update the accumulator with field (2:2)" in {
    val initialAccumulator = Word(Plus, Seq(MixByte(6), MixByte(7), MixByte(8), MixByte(9), MixByte(0)))
    val theWord = Word(Minus, Seq(MixByte(1), MixByte(2), MixByte(3), MixByte(4), MixByte(5)))
    val mix = Mix(accumulator = initialAccumulator).withMemoryUpdated(theWord, 2000)
    val sta = STA(TwoSignedBytes.apply(2000), Field(2, 2))
    sta.toString shouldBe "STA 2000(2:2)"
    val result = sta.compute(mix)
    val expectedWord = Word(Minus, Seq(MixByte(1), MixByte(0), MixByte(3), MixByte(4), MixByte(5)))
    result.memory(2000) shouldBe expectedWord
    result.accumulator shouldBe initialAccumulator
  }

  it should "correctly update the accumulator with field (2:3)" in {
    val initialAccumulator = Word(Plus, Seq(MixByte(6), MixByte(7), MixByte(8), MixByte(9), MixByte(0)))
    val theWord = Word(Minus, Seq(MixByte(1), MixByte(2), MixByte(3), MixByte(4), MixByte(5)))
    val mix = Mix(accumulator = initialAccumulator).withMemoryUpdated(theWord, 2000)
    val sta = STA(TwoSignedBytes.apply(2000), Field(2, 3))
    sta.toString shouldBe "STA 2000(2:3)"
    val result = sta.compute(mix)
    val expectedWord = Word(Minus, Seq(MixByte(1), MixByte(9), MixByte(0), MixByte(4), MixByte(5)))
    result.memory(2000) shouldBe expectedWord
    result.accumulator shouldBe initialAccumulator
  }

  it should "correctly update the accumulator with field (0:1)" in {
    val initialAccumulator = Word(Plus, Seq(MixByte(6), MixByte(7), MixByte(8), MixByte(9), MixByte(0)))
    val theWord = Word(Minus, Seq(MixByte(1), MixByte(2), MixByte(3), MixByte(4), MixByte(5)))
    val mix = Mix(accumulator = initialAccumulator).withMemoryUpdated(theWord, 2000)
    val sta = STA(TwoSignedBytes.apply(2000), Field(0, 1))
    sta.toString shouldBe "STA 2000(0:1)"
    val result = sta.compute(mix)
    val expectedWord = Word(Plus, Seq(MixByte(0), MixByte(2), MixByte(3), MixByte(4), MixByte(5)))
    result.memory(2000) shouldBe expectedWord
    result.accumulator shouldBe initialAccumulator
  }

}
