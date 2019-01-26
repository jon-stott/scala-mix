package org.jstott.mix.instructions.jump

import org.jstott.mix._

case class JL(override val value: Word) extends J {

  val name = "JL"

  def compute(implicit mix: Mix): Mix = doJump(mix, mix.comparisonIndicator == Less)

}

object JL {

  val operationCode = MixByte(39)

  val field = Field(0, 4)

  def apply(address: TwoSignedBytes): JL = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): JL =
    JL(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}