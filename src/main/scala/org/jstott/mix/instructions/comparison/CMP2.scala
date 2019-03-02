package org.jstott.mix.instructions.comparison

import org.jstott.mix._

case class CMP2(override val value: Word) extends CMP {

  val name = "CMP2"

  def compute(implicit mix: Mix): Mix = {
    mix.copy(comparisonIndicator = computeNewComparisonIndicator(mix, mix.i2.intValue)).incrementProgramCounter
  }

}

object CMP2 {

  val operationCode = MixByte(58)

  def apply(address: TwoSignedBytes): CMP2 = apply(address, MixByte(0), Field.normal.toByte)
  def apply(address: TwoSignedBytes, index: MixByte): CMP2 = apply(address, index, Field.normal.toByte)
  def apply(address: TwoSignedBytes, field: Field): CMP2 = apply(address, MixByte(0), field.toByte)
  def apply(address: TwoSignedBytes, index: MixByte, field: MixByte): CMP2 =
    CMP2(value = Word(address.sign, address.bytes ++ Seq(index, field, operationCode)))

}