package org.jstott.mix.instructions.load

import org.jstott.mix._
import org.scalatest.{FlatSpec, Matchers}

class LD1Spec extends FlatSpec with Matchers {

  behavior of "apply"

  it should "create an LD1 instruction when not specifying the field" in {
    val result = LD1.apply(TwoSignedBytes.apply(10))
    result.A shouldBe TwoSignedBytes(Plus, Seq(MixByte(0), MixByte(10)))
    result.I shouldBe MixByte(0)
    result.F.asField shouldBe Field(0, 5)
    result.C shouldBe MixByte(9)
  }

  it should "create an LD1 instruction when specifying the field" in {
    val result = LD1.apply(TwoSignedBytes.apply(10), Field(2, 3))
    result.A shouldBe TwoSignedBytes(Plus, Seq(MixByte(0), MixByte(10)))
    result.I shouldBe MixByte(0)
    result.F.asField shouldBe Field(2, 3)
    result.C shouldBe MixByte(9)
  }

  behavior of "compute"

  it should "correctly update the accumulator with field (0:5)" in {
    val theWord = Word(Plus, Seq(MixByte(1), MixByte(2), MixByte(3), MixByte(4), MixByte(5)))
    val mix = Mix().withMemoryUpdated(theWord, 200)
    val ld1 = LD1(TwoSignedBytes.apply(200))
    val result = ld1.compute(mix)
    result.memory(200) shouldBe theWord
    result.index1 shouldBe TwoSignedBytes(Plus, Seq(MixByte(4), MixByte(5)))
  }

  it should "correctly update the accumulator with field (1:4)" in {
    val theWord = Word(Plus, Seq(MixByte(1), MixByte(2), MixByte(3), MixByte(4), MixByte(5)))
    val mix = Mix().withMemoryUpdated(theWord, 200)
    val ld1 = LD1(TwoSignedBytes.apply(200), Field(1, 4))
    ld1.toString shouldBe "LD1 200(1:4)"
    val result = ld1.compute(mix)
    result.memory(200) shouldBe theWord
    result.index1 shouldBe TwoSignedBytes(Plus, Seq(MixByte(3), MixByte(4)))
  }

}
