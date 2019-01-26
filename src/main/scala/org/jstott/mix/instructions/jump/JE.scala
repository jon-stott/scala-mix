package org.jstott.mix.instructions.jump

import org.jstott.mix._

case class JE(override val value: Word) extends J {

  val name = "JE"

  def compute(implicit mix: Mix): Mix = doJump(mix, mix.comparisonIndicator == Equal)

}

object JE {

  val operationCode = MixByte(39)

  val field = Field(0, 5)

  def apply(address: TwoSignedBytes): JE = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): JE =
    JE(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}