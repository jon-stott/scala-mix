package org.jstott.mix.instructions.jump

import org.jstott.mix._

case class J6NZ(override val value: Word) extends J {

  val name = "J6NZ"

  def compute(implicit mix: Mix): Mix = j_nz(mix, mix.index6)

}

object J6NZ {

  val operationCode = MixByte(46)

  val field = Field(0, 4)

  def apply(address: TwoSignedBytes): J6NZ = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): J6NZ =
    J6NZ(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}