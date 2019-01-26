package org.jstott.mix.instructions.conversion

import org.jstott.mix._
import org.scalatest.{FlatSpec, Matchers}

class CHARSpec extends FlatSpec with Matchers {

  behavior of "compute"

  it should "convert a negative value to characters" in {
    val mix = Mix(accumulator = Word(-12977699))
    val result = CHAR(TwoSignedBytes(0)).compute(mix)
    result.accumulator shouldBe Word(Minus, Seq(MixByte(30), MixByte(30), MixByte(31), MixByte(32), MixByte(39)))
    result.extension shouldBe Word(Plus, Seq(MixByte(37), MixByte(37), MixByte(36), MixByte(39), MixByte(39)))
  }

  it should "convert a positive value to characters" in {
    val mix = Mix(accumulator = Word(12977699))
    val result = CHAR(TwoSignedBytes(0)).compute(mix)
    result.accumulator shouldBe Word(Plus, Seq(MixByte(30), MixByte(30), MixByte(31), MixByte(32), MixByte(39)))
    result.extension shouldBe Word(Plus, Seq(MixByte(37), MixByte(37), MixByte(36), MixByte(39), MixByte(39)))
  }

}
