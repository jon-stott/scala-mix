package org.jstott.mix.instructions.transfer

import org.jstott.mix._
import org.scalatest.{FlatSpec, Matchers}

class INC1Spec extends FlatSpec with Matchers {

  behavior of "compute"

  it should "increment without overflow" in {
    val mix = Mix().withRegister(Index1, TwoSignedBytes()).withOverflow(Off)
    val inc1 = INC1(TwoSignedBytes.apply(1))
    val result = inc1.compute(mix)
    result.index1 shouldBe TwoSignedBytes(1)
    result.overflow shouldBe Off
  }

  it should "increment with address and index without overflow" in {
    val mix = Mix().withRegister(Index1, TwoSignedBytes()).withRegister(Index2, TwoSignedBytes(2)).withOverflow(Off)
    val inc1 = INC1(TwoSignedBytes.apply(1), MixByte(2))
    val result = inc1.compute(mix)
    result.index1 shouldBe TwoSignedBytes(3)
    result.overflow shouldBe Off
  }

  it should "increment with overflow" in {
    val mix = Mix().withRegister(Index1, TwoSignedBytes(TwoSignedBytes.maxValue)).withOverflow(Off)
    val inc1 = INC1(TwoSignedBytes.apply(1))
    an [UnexpectedOverflowException] should be thrownBy inc1.compute(mix)
  }

}
