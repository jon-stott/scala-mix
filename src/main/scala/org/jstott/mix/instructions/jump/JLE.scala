package org.jstott.mix.instructions.jump

import org.jstott.mix._

case class JLE(override val value: Word) extends J {

  val name = "JLE"

  def compute(implicit mix: Mix): Mix = doJump(mix, mix.comparisonIndicator != Greater)

}

object JLE {

  val operationCode = MixByte(39)

  val field = Field(0, 9)

  def apply(address: TwoSignedBytes): JLE = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): JLE =
    JLE(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}