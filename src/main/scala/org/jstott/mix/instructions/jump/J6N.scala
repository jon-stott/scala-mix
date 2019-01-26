package org.jstott.mix.instructions.jump

import org.jstott.mix._

case class J6N(override val value: Word) extends J {

  val name = "J6N"

  def compute(implicit mix: Mix): Mix = j_n(mix, mix.index6)

}

object J6N {

  val operationCode = MixByte(46)

  val field = Field(0, 0)

  def apply(address: TwoSignedBytes): J6N = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): J6N =
    J6N(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}