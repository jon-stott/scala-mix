package org.jstott.mix.instructions.transfer

import org.jstott.mix._
import org.scalatest.{FlatSpec, Matchers}

class DECXSpec extends FlatSpec with Matchers {

  behavior of "compute"

  it should "decrement without overflow" in {
    val mix = Mix().withRegister(Extension, Word()).withOverflow(Off)
    val decx = DECX(TwoSignedBytes.apply(1))
    val result = decx.compute(mix)
    result.extension shouldBe Word(-1)
    result.overflow shouldBe Off
  }

  it should "decrement with address and index without overflow" in {
    val mix = Mix().withRegister(Extension, Word()).withRegister(Index1, TwoSignedBytes(2)).withOverflow(Off)
    val decx = DECX(TwoSignedBytes.apply(1), MixByte(1))
    val result = decx.compute(mix)
    result.extension shouldBe Word(-3)
    result.overflow shouldBe Off
  }

  it should "decrement with overflow" in {
    val mix = Mix().withRegister(Extension, Word(Word.minValue)).withOverflow(Off)
    val decx = DECX(TwoSignedBytes.apply(1))
    val result = decx.compute(mix)
    result.extension shouldBe Word(0).copy(sign = Minus)
    result.overflow shouldBe On
  }

}
