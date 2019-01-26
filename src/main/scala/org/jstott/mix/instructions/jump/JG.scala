package org.jstott.mix.instructions.jump

import org.jstott.mix._

case class JG(override val value: Word) extends J {

  val name = "JG"

  def compute(implicit mix: Mix): Mix = doJump(mix, mix.comparisonIndicator == Greater)

}

object JG {

  val operationCode = MixByte(39)

  val field = Field(0, 6)

  def apply(address: TwoSignedBytes): JG = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): JG =
    JG(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}