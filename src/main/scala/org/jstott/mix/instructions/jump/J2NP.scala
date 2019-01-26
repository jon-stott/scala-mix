package org.jstott.mix.instructions.jump

import org.jstott.mix._

case class J2NP(override val value: Word) extends J {

  val name = "J2NP"

  def compute(implicit mix: Mix): Mix = j_np(mix, mix.index2)

}

object J2NP {

  val operationCode = MixByte(42)

  val field = Field(0, 5)

  def apply(address: TwoSignedBytes): J2NP = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): J2NP =
    J2NP(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}