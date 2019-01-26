package org.jstott.mix.instructions.arithmetic

import org.jstott.mix._
import org.scalatest.{FlatSpec, Matchers}

class DIVSpec extends FlatSpec with Matchers {

  behavior of "compute"

  it should "work for 4/2" in {
    val mix = Mix().withRegister(Accumulator, Word(0)).withRegister(Extension, Word(4)).withMemoryUpdated(Word(2), 0)
    val div = DIV(TwoSignedBytes(0))
    val result = div.compute(mix)
    result.accumulator shouldBe Word(2)
    result.extension shouldBe Word(0)
  }

  it should "work for 5/2" in {
    val mix = Mix().withRegister(Accumulator, Word(0)).withRegister(Extension, Word(5)).withMemoryUpdated(Word(2), 0)
    val div = DIV(TwoSignedBytes(0))
    val result = div.compute(mix)
    result.accumulator shouldBe Word(2)
    result.extension shouldBe Word(1)
  }

}
