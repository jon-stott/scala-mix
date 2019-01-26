package org.jstott.mix.instructions.jump

import org.jstott.mix._

case class JXNP(override val value: Word) extends J {

  val name = "JXNP"

  def compute(implicit mix: Mix): Mix = j_np(mix, mix.extension)

}

object JXNP {

  val operationCode = MixByte(47)

  val field = Field(0, 5)

  def apply(address: TwoSignedBytes): JXNP = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): JXNP =
    JXNP(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}