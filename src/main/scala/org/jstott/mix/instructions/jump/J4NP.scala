package org.jstott.mix.instructions.jump

import org.jstott.mix._

case class J4NP(override val value: Word) extends J {

  val name = "J4NP"

  def compute(implicit mix: Mix): Mix = j_np(mix, mix.index4)

}

object J4NP {

  val operationCode = MixByte(44)

  val field = Field(0, 5)

  def apply(address: TwoSignedBytes): J4NP = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): J4NP =
    J4NP(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}