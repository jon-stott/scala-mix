package org.jstott.mix.instructions.jump

import org.jstott.mix._

case class J1NP(override val value: Word) extends J {

  val name = "J1NP"

  def compute(implicit mix: Mix): Mix = j_np(mix, mix.index1)

}

object J1NP {

  val operationCode = MixByte(41)

  val field = Field(0, 5)

  def apply(address: TwoSignedBytes): J1NP = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): J1NP =
    J1NP(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}