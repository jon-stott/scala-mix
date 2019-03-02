package org.jstott.mix.instructions.comparison

import org.jstott.mix._

case class CMP5(override val value: Word) extends CMP {

  val name = "CMP5"

  def compute(implicit mix: Mix): Mix = {
    mix.copy(comparisonIndicator = computeNewComparisonIndicator(mix, mix.i5.intValue)).incrementProgramCounter
  }

}

object CMP5 {

  val operationCode = MixByte(61)

  def apply(address: TwoSignedBytes): CMP5 = apply(address, MixByte(0), Field.normal.toByte)
  def apply(address: TwoSignedBytes, index: MixByte): CMP5 = apply(address, index, Field.normal.toByte)
  def apply(address: TwoSignedBytes, field: Field): CMP5 = apply(address, MixByte(0), field.toByte)
  def apply(address: TwoSignedBytes, index: MixByte, field: MixByte): CMP5 =
    CMP5(value = Word(address.sign, address.bytes ++ Seq(index, field, operationCode)))

}