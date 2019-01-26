package org.jstott.mix

import org.scalatest.{FlatSpec, Matchers}

class FieldSpec extends FlatSpec with Matchers {

  behavior of "apply(Int)"

  it should "handle a value less than 8" in {
    Field.apply(5) shouldBe Field(0, 5)
  }

  it should "handle a value equal to 8" in {
    Field.apply(8) shouldBe Field(1, 0)
  }

  it should "handle a value greater than 8" in {
    Field.apply(10) shouldBe Field(1, 2)
  }

}
