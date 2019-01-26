package org.jstott.mix.instructions.transfer

import org.jstott.mix._
import org.scalatest.{FlatSpec, Matchers}

class DEC1Spec extends FlatSpec with Matchers {

  behavior of "compute"

  it should "decrement without overflow" in {
    val mix = Mix().withRegister(Index1, TwoSignedBytes()).withOverflow(Off)
    val dec1 = DEC1(TwoSignedBytes.apply(1))
    val result = dec1.compute(mix)
    result.index1 shouldBe TwoSignedBytes(-1)
    result.overflow shouldBe Off
  }

  it should "decrement with overflow" in {
    val mix = Mix().withRegister(Index1, TwoSignedBytes(TwoSignedBytes.minValue)).withOverflow(Off)
    val dec1 = DEC1(TwoSignedBytes.apply(1))
    an [UnexpectedOverflowException] should be thrownBy dec1.compute(mix)
  }

}
