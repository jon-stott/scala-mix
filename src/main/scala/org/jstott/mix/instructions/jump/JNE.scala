package org.jstott.mix.instructions.jump

import org.jstott.mix._

case class JNE(override val value: Word) extends J {

  val name = "JNE"

  def compute(implicit mix: Mix): Mix = doJump(mix, mix.comparisonIndicator != Equal)

}

object JNE {

  val operationCode = MixByte(39)

  val field = Field(0, 8)

  def apply(address: TwoSignedBytes): JNE = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): JNE =
    JNE(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}