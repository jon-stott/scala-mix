package org.jstott.mix.instructions.comparison

import org.jstott.mix._

case class CMP6(override val value: Word) extends CMP {

  val name = "CMP6"

  def compute(implicit mix: Mix): Mix = {
    mix.copy(comparisonIndicator = computeNewComparisonIndicator(mix, mix.a.intValue)).incrementProgramCounter
  }

}

object CMP6 {

  val operationCode = MixByte(62)

  def apply(address: TwoSignedBytes): CMP6 = apply(address, MixByte(0), Field.normal.toByte)
  def apply(address: TwoSignedBytes, index: MixByte): CMP6 = apply(address, index, Field.normal.toByte)
  def apply(address: TwoSignedBytes, field: Field): CMP6 = apply(address, MixByte(0), field.toByte)
  def apply(address: TwoSignedBytes, index: MixByte, field: MixByte): CMP6 =
    CMP6(value = Word(address.sign, address.bytes ++ Seq(index, field, operationCode)))

}