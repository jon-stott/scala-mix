package org.jstott.mix.instructions.transfer

import org.jstott.mix._
import org.scalatest.{FlatSpec, Matchers}

class INCASpec extends FlatSpec with Matchers {

  behavior of "compute"

  it should "increment without overflow" in {
    val mix = Mix().withRegister(Accumulator, Word()).withOverflow(Off)
    val inca = INCA(TwoSignedBytes.apply(1))
    val result = inca.compute(mix)
    result.accumulator shouldBe Word(1)
    result.overflow shouldBe Off
  }

  it should "increment with address and index without overflow" in {
    val mix = Mix().withRegister(Accumulator, Word()).withRegister(Index1, TwoSignedBytes(2)).withOverflow(Off)
    val inca = INCA(TwoSignedBytes.apply(1), MixByte(1))
    val result = inca.compute(mix)
    result.accumulator shouldBe Word(3)
    result.overflow shouldBe Off
  }

  it should "increment with overflow" in {
    val mix = Mix().withRegister(Accumulator, Word(Word.maxValue)).withOverflow(Off)
    val inca = INCA(TwoSignedBytes.apply(1))
    val result = inca.compute(mix)
    result.accumulator shouldBe Word(0)
    result.overflow shouldBe On
  }

}
