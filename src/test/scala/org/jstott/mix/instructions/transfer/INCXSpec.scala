package org.jstott.mix.instructions.transfer

import org.jstott.mix._
import org.scalatest.{FlatSpec, Matchers}

class INCXSpec extends FlatSpec with Matchers {

  behavior of "compute"

  it should "increment without overflow" in {
    val mix = Mix().withRegister(Extension, Word()).withOverflow(Off)
    val incx = INCX(TwoSignedBytes.apply(1))
    val result = incx.compute(mix)
    result.extension shouldBe Word(1)
    result.overflow shouldBe Off
  }

  it should "increment with address and index without overflow" in {
    val mix = Mix().withRegister(Extension, Word()).withRegister(Index1, TwoSignedBytes(2)).withOverflow(Off)
    val incx = INCX(TwoSignedBytes.apply(1), MixByte(1))
    val result = incx.compute(mix)
    result.extension shouldBe Word(3)
    result.overflow shouldBe Off
  }

  it should "increment with overflow" in {
    val mix = Mix().withRegister(Extension, Word(Word.maxValue)).withOverflow(Off)
    val incx = INCX(TwoSignedBytes.apply(1))
    val result = incx.compute(mix)
    result.extension shouldBe Word(0)
    result.overflow shouldBe On
  }

}
