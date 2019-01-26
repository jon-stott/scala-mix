package org.jstott.mix.instructions.jump

import org.jstott.mix._

case class JAN(override val value: Word) extends J {

  val name = "JAN"

  def compute(implicit mix: Mix): Mix = j_n(mix, mix.a)

}

object JAN {

  val operationCode = MixByte(40)

  val field = Field(0, 0)

  def apply(address: TwoSignedBytes): JAN = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): JAN =
    JAN(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}