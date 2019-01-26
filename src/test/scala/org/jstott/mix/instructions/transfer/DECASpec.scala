package org.jstott.mix.instructions.transfer

import org.jstott.mix._
import org.scalatest.{FlatSpec, Matchers}

class DECASpec extends FlatSpec with Matchers {

  behavior of "compute"

  it should "decrement without overflow" in {
    val mix = Mix().withRegister(Accumulator, Word()).withOverflow(Off)
    val deca = DECA(TwoSignedBytes.apply(1))
    val result = deca.compute(mix)
    result.accumulator shouldBe Word(-1)
    result.overflow shouldBe Off
  }

  it should "decrement with overflow" in {
    val mix = Mix().withRegister(Accumulator, Word(Word.minValue)).withOverflow(Off)
    val deca = DECA(TwoSignedBytes.apply(1))
    val result = deca.compute(mix)
    result.accumulator shouldBe Word(0).copy(sign = Minus)
    result.overflow shouldBe On
  }

}
