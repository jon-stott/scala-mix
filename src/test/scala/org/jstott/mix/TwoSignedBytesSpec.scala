package org.jstott.mix

import org.scalatest.{FlatSpec, Matchers}

class TwoSignedBytesSpec extends FlatSpec with Matchers {

  behavior of "intValue"

  it should "calculate the correct value" in {
    TwoSignedBytes(Plus, Seq(MixByte(0), MixByte(10))).intValue shouldBe 10
    TwoSignedBytes(Plus, Seq(MixByte(1), MixByte(10))).intValue shouldBe 74
    TwoSignedBytes(Plus, Seq(MixByte(0), MixByte(1))).intValue shouldBe 1
    TwoSignedBytes(Plus, Seq(MixByte(0), MixByte(0))).intValue shouldBe 0
    TwoSignedBytes(Minus, Seq(MixByte(0), MixByte(1))).intValue shouldBe -1
  }

  behavior of "apply"

  it should "produce a valid value" in {
    TwoSignedBytes.apply(10).sign shouldBe Plus
    TwoSignedBytes.apply(10).bytes should contain theSameElementsInOrderAs Array(MixByte(0), MixByte(10))
    TwoSignedBytes.apply(100).sign shouldBe Plus
    TwoSignedBytes.apply(100).bytes should contain theSameElementsInOrderAs Array(MixByte(1), MixByte(36))
    TwoSignedBytes.apply(-100).sign shouldBe Minus
    TwoSignedBytes.apply(-100).bytes should contain theSameElementsInOrderAs Array(MixByte(1), MixByte(36))
  }

}
