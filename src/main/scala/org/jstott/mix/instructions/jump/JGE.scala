package org.jstott.mix.instructions.jump

import org.jstott.mix._

case class JGE(override val value: Word) extends J {

  val name = "JGE"

  def compute(implicit mix: Mix): Mix = doJump(mix, mix.comparisonIndicator != Less)

}

object JGE {

  val operationCode = MixByte(39)

  val field = Field(0, 7)

  def apply(address: TwoSignedBytes): JGE = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): JGE =
    JGE(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}