package org.jstott.mix.instructions.comparison

import org.jstott.mix.{MixByte, Field, Mix, TwoSignedBytes, Word}

case class CMPA(override val value: Word) extends CMP {

  val name = "CMPA"

  def compute(implicit mix: Mix): Mix = {
    mix.copy(comparisonIndicator = computeNewComparisonIndicator(mix, mix.a.intValue)).incrementProgramCounter
  }

}

object CMPA {

  val operationCode = MixByte(56)

  def apply(address: TwoSignedBytes): CMPA = apply(address, MixByte(0), Field.normal.toByte)
  def apply(address: TwoSignedBytes, index: MixByte): CMPA = apply(address, index, Field.normal.toByte)
  def apply(address: TwoSignedBytes, field: Field): CMPA = apply(address, MixByte(0), field.toByte)
  def apply(address: TwoSignedBytes, index: MixByte, field: MixByte): CMPA =
    CMPA(value = Word(address.sign, address.bytes ++ Seq(index, field, operationCode)))

}