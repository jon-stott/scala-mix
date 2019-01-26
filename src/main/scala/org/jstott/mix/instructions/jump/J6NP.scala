package org.jstott.mix.instructions.jump

import org.jstott.mix._

case class J6NP(override val value: Word) extends J {

  val name = "J6NP"

  def compute(implicit mix: Mix): Mix = j_np(mix, mix.index6)

}

object J6NP {

  val operationCode = MixByte(46)

  val field = Field(0, 5)

  def apply(address: TwoSignedBytes): J6NP = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): J6NP =
    J6NP(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}