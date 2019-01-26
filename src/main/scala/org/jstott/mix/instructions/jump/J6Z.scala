package org.jstott.mix.instructions.jump

import org.jstott.mix._

case class J6Z(override val value: Word) extends J {

  val name = "J6Z"

  def compute(implicit mix: Mix): Mix = j_z(mix, mix.index6)

}

object J6Z {

  val operationCode = MixByte(46)

  val field = Field(0, 1)

  def apply(address: TwoSignedBytes): J6Z = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): J6Z =
    J6Z(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}