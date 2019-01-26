package org.jstott.mix.instructions.jump

import org.jstott.mix._

case class JXNN(override val value: Word) extends J {

  val name = "JXNN"

  def compute(implicit mix: Mix): Mix = j_nn(mix, mix.extension)

}

object JXNN {

  val operationCode = MixByte(47)

  val field = Field(0, 3)

  def apply(address: TwoSignedBytes): JXNN = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): JXNN =
    JXNN(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}