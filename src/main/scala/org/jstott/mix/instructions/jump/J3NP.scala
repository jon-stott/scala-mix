package org.jstott.mix.instructions.jump

import org.jstott.mix._

case class J3NP(override val value: Word) extends J {

  val name = "J3NP"

  def compute(implicit mix: Mix): Mix = j_np(mix, mix.index3)

}

object J3NP {

  val operationCode = MixByte(43)

  val field = Field(0, 5)

  def apply(address: TwoSignedBytes): J3NP = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): J3NP =
    J3NP(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}