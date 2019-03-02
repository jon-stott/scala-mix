package org.jstott.mix.instructions.comparison

import org.jstott.mix._

case class CMP1(override val value: Word) extends CMP {

  val name = "CMP1"

  def compute(implicit mix: Mix): Mix = {
    mix.copy(comparisonIndicator = computeNewComparisonIndicator(mix, mix.a.intValue)).incrementProgramCounter
  }

}

object CMP1 {

  val operationCode = MixByte(57)

  def apply(address: TwoSignedBytes): CMP1 = apply(address, MixByte(0), Field.normal.toByte)
  def apply(address: TwoSignedBytes, index: MixByte): CMP1 = apply(address, index, Field.normal.toByte)
  def apply(address: TwoSignedBytes, field: Field): CMP1 = apply(address, MixByte(0), field.toByte)
  def apply(address: TwoSignedBytes, index: MixByte, field: MixByte): CMP1 =
    CMP1(value = Word(address.sign, address.bytes ++ Seq(index, field, operationCode)))

}
