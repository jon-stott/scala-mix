package org.jstott.mix.instructions.jump

import org.jstott.mix._
import org.scalatest.{FlatSpec, Matchers}

class J3PSpec extends FlatSpec with Matchers {

  behavior of "compute"

  it should "jump if rI3 is positive" in {
    val mix = Mix().withRegister(Index3, TwoSignedBytes(1))
    val j3p = J3P(TwoSignedBytes.apply(200))
    val result = j3p.compute(mix)
    result.programCounter shouldBe 200
  }

  it should "not jump if rI3 is zero" in {
    val mix = Mix().withRegister(Index3, TwoSignedBytes(0))
    val j3p = J3P(TwoSignedBytes.apply(200))
    val result = j3p.compute(mix)
    result.programCounter shouldBe 1
  }

  it should "not jump if rI3 is negative" in {
    val mix = Mix().withRegister(Index3, TwoSignedBytes(-1))
    val j3p = J3P(TwoSignedBytes.apply(200))
    val result = j3p.compute(mix)
    result.programCounter shouldBe 1
  }

}
