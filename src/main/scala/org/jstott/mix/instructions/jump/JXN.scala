package org.jstott.mix.instructions.jump

import org.jstott.mix._

case class JXN(override val value: Word) extends J {

  val name = "JXN"

  def compute(implicit mix: Mix): Mix = j_n(mix, mix.extension)

}

object JXN {

  val operationCode = MixByte(47)

  val field = Field(0, 0)

  def apply(address: TwoSignedBytes): JXN = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): JXN =
    JXN(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}