package org.jstott.mix.instructions.jump

import org.jstott.mix._

case class J5NP(override val value: Word) extends J {

  val name = "J5NP"

  def compute(implicit mix: Mix): Mix = j_np(mix, mix.index5)

}

object J5NP {

  val operationCode = MixByte(45)

  val field = Field(0, 5)

  def apply(address: TwoSignedBytes): J5NP = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): J5NP =
    J5NP(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}