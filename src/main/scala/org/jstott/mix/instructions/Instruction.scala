package org.jstott.mix.instructions

import org.jstott.mix.instructions.arithmetic.DIV
import org.jstott.mix.instructions.comparison.CMPA
import org.jstott.mix.instructions.conversion.{CHAR, NUM}
import org.jstott.mix.instructions.io.{IOC, OUT}
import org.jstott.mix.instructions.jump._
import org.jstott.mix.instructions.load._
import org.jstott.mix.instructions.misc.{HLT, NOP}
import org.jstott.mix.instructions.store._
import org.jstott.mix.instructions.transfer._
import org.jstott.mix.{MixByte, Mix, Plus, TwoSignedBytes, Word}

trait Instruction {

  def name: String

  def value: Word

  def address: TwoSignedBytes = TwoSignedBytes(value.sign, value.bytes.slice(0, 2))

  def operationCode: MixByte = value.bytes(4)

  def modification: MixByte = value.bytes(3)

  def indexSpecification: MixByte = value.bytes(2)

  def compute(implicit mix: Mix): Mix

  require(I.value >= 0 && I.value <= 6, s"index specification (I) [$I] is not 0..6")

  require(C.value >= 0 && C.value <= 63, s"operation code (C) [$C] $value is not 0..63")

  def A: TwoSignedBytes = address
  def I: MixByte = indexSpecification
  def F: MixByte = modification
  def C: MixByte = operationCode
  def M: TwoSignedBytes = address

  protected def applyFieldToWord(word: Word, invertSign: Boolean = false): Word = {
    val field = F.asField
    val sign = if (field.left == 0) word.sign else Plus
    val possiblyInvertedSign = if (invertSign) sign.invert else sign
    val bytesToCopy = word.bytes.slice(Math.max(field.left, 1) - 1, field.right)
    Word(possiblyInvertedSign).withBytes(bytesToCopy, 6 - bytesToCopy.length)
  }

  protected def addressWithIndex(mix: Mix): Int = {
    A.intValue + mix.i(I).intValue
  }

  override def toString: String = {
    s"$name ${A.intValue}${if (I > 0) s",$I" else ""}${F.asField}"
  }

}

object Instruction {

  def fromMemory(w: Word): Option[Instruction] = {
    val field = w.bytes(3).value
    try {
      w.bytes.last match {
        case MixByte(0) => Some(NOP(w))
        //      case Byte(1) if field != 6 => Some(ADD(w))
        //      case Byte(1) => Some(FADD(w))
        //      case Byte(2) if field != 6 => Some(SUB(w))
        //      case Byte(2) => Some(FSUB(w))
        //      case Byte(3) if field != 6 => Some(MUL(w))
        //      case Byte(3) => Some(FMUL(w))
        case MixByte(4) if field != 6 => Some(DIV(w))
        //      case Byte(4) => Some(FDIV(w))
        case MixByte(5) if field == 0 => Some(NUM(w))
        case MixByte(5) if field == 1 => Some(CHAR(w))
        case MixByte(5) if field == 2 => Some(HLT(w))
        //      case Byte(6) if field == 0 => Some(SLA(w))
        //      case Byte(6) if field == 1 => Some(SRA(w))
        //      case Byte(6) if field == 2 => Some(SLAX(w))
        //      case Byte(6) if field == 3 => Some(SRAX(w))
        //      case Byte(6) if field == 4 => Some(SLC(w))
        //      case Byte(6) if field == 5 => Some(SRC(w))
        //      case Byte(7) => Some(MOVE(w))
        case MixByte(8) => Some(LDA(w))
        case MixByte(9) => Some(LD1(w))
        case MixByte(10) => Some(LD2(w))
        case MixByte(11) => Some(LD3(w))
        case MixByte(12) => Some(LD4(w))
        case MixByte(13) => Some(LD5(w))
        case MixByte(14) => Some(LD6(w))
        case MixByte(15) => Some(LDX(w))
        case MixByte(16) => Some(LDAN(w))
        case MixByte(17) => Some(LD1N(w))
        case MixByte(18) => Some(LD2N(w))
        case MixByte(19) => Some(LD3N(w))
        case MixByte(20) => Some(LD4N(w))
        case MixByte(21) => Some(LD5N(w))
        case MixByte(22) => Some(LD6N(w))
        case MixByte(23) => Some(LDXN(w))
        case MixByte(24) => Some(STA(w))
        case MixByte(25) => Some(ST1(w))
        case MixByte(26) => Some(ST2(w))
        case MixByte(27) => Some(ST3(w))
        case MixByte(28) => Some(ST4(w))
        case MixByte(29) => Some(ST5(w))
        case MixByte(30) => Some(ST6(w))
        case MixByte(31) => Some(STX(w))
        case MixByte(32) => Some(STJ(w))
        case MixByte(33) => Some(STZ(w))
//        case Byte(34) => Some(JBUS(w))
        case MixByte(35) => Some(IOC(w))
//        case Byte(36) => Some(IN(w))
        case MixByte(37) => Some(OUT(w))
//        case Byte(38) if field == 0 => Some(JRED(w))
        case MixByte(39) if field == 0 => Some(JMP(w))
        case MixByte(39) if field == 1 => Some(JSJ(w))
        case MixByte(39) if field == 2 => Some(JOV(w))
        case MixByte(39) if field == 3 => Some(JNOV(w))
        case MixByte(39) if field == 4 => Some(JL(w))
        case MixByte(39) if field == 5 => Some(JE(w))
        case MixByte(39) if field == 6 => Some(JG(w))
        case MixByte(39) if field == 7 => Some(JGE(w))
        case MixByte(39) if field == 8 => Some(JNE(w))
        case MixByte(39) if field == 9 => Some(JLE(w))
        case MixByte(40) if field == 0 => Some(JAN(w))
        case MixByte(40) if field == 1 => Some(JAZ(w))
        case MixByte(40) if field == 2 => Some(JAP(w))
        case MixByte(40) if field == 3 => Some(JANN(w))
        case MixByte(40) if field == 4 => Some(JANZ(w))
        case MixByte(40) if field == 5 => Some(JANP(w))
        case MixByte(41) if field == 0 => Some(J1N(w))
        case MixByte(41) if field == 1 => Some(J1Z(w))
        case MixByte(41) if field == 2 => Some(J1P(w))
        case MixByte(41) if field == 3 => Some(J1NN(w))
        case MixByte(41) if field == 4 => Some(J1NZ(w))
        case MixByte(41) if field == 5 => Some(J1NP(w))
        case MixByte(42) if field == 0 => Some(J2N(w))
        case MixByte(42) if field == 1 => Some(J2Z(w))
        case MixByte(42) if field == 2 => Some(J2P(w))
        case MixByte(42) if field == 3 => Some(J2NN(w))
        case MixByte(42) if field == 4 => Some(J2NZ(w))
        case MixByte(42) if field == 5 => Some(J2NP(w))
        case MixByte(43) if field == 0 => Some(J3N(w))
        case MixByte(43) if field == 1 => Some(J3Z(w))
        case MixByte(43) if field == 2 => Some(J3P(w))
        case MixByte(43) if field == 3 => Some(J3NN(w))
        case MixByte(43) if field == 4 => Some(J3NZ(w))
        case MixByte(43) if field == 5 => Some(J3NP(w))
        case MixByte(44) if field == 0 => Some(J4N(w))
        case MixByte(44) if field == 1 => Some(J4Z(w))
        case MixByte(44) if field == 2 => Some(J4P(w))
        case MixByte(44) if field == 3 => Some(J4NN(w))
        case MixByte(44) if field == 4 => Some(J4NZ(w))
        case MixByte(44) if field == 5 => Some(J4NP(w))
        case MixByte(45) if field == 0 => Some(J5N(w))
        case MixByte(45) if field == 1 => Some(J5Z(w))
        case MixByte(45) if field == 2 => Some(J5P(w))
        case MixByte(45) if field == 3 => Some(J5NN(w))
        case MixByte(45) if field == 4 => Some(J5NZ(w))
        case MixByte(45) if field == 5 => Some(J5NP(w))
        case MixByte(46) if field == 0 => Some(J6N(w))
        case MixByte(46) if field == 1 => Some(J6Z(w))
        case MixByte(46) if field == 2 => Some(J6P(w))
        case MixByte(46) if field == 3 => Some(J6NN(w))
        case MixByte(46) if field == 4 => Some(J6NZ(w))
        case MixByte(46) if field == 5 => Some(J6NP(w))
        case MixByte(47) if field == 0 => Some(JXN(w))
        case MixByte(47) if field == 1 => Some(JXZ(w))
        case MixByte(47) if field == 2 => Some(JXP(w))
        case MixByte(47) if field == 3 => Some(JXNN(w))
        case MixByte(47) if field == 4 => Some(JXNZ(w))
        case MixByte(47) if field == 5 => Some(JXNP(w))
        case MixByte(48) if field == 0 => Some(INCA(w))
        case MixByte(48) if field == 1 => Some(DECA(w))
        case MixByte(48) if field == 2 => Some(ENTA(w))
        case MixByte(48) if field == 3 => Some(ENNA(w))
        case MixByte(49) if field == 0 => Some(INC1(w))
        case MixByte(49) if field == 1 => Some(DEC1(w))
        case MixByte(49) if field == 2 => Some(ENT1(w))
        case MixByte(49) if field == 3 => Some(ENN1(w))
        case MixByte(50) if field == 0 => Some(INC2(w))
        case MixByte(50) if field == 1 => Some(DEC2(w))
        case MixByte(50) if field == 2 => Some(ENT2(w))
        case MixByte(50) if field == 3 => Some(ENN2(w))
        case MixByte(51) if field == 0 => Some(INC3(w))
        case MixByte(51) if field == 1 => Some(DEC3(w))
        case MixByte(51) if field == 2 => Some(ENT3(w))
        case MixByte(51) if field == 3 => Some(ENN3(w))
        case MixByte(52) if field == 0 => Some(INC4(w))
        case MixByte(52) if field == 1 => Some(DEC4(w))
        case MixByte(52) if field == 2 => Some(ENT4(w))
        case MixByte(52) if field == 3 => Some(ENN4(w))
        case MixByte(53) if field == 0 => Some(INC5(w))
        case MixByte(53) if field == 1 => Some(DEC5(w))
        case MixByte(53) if field == 2 => Some(ENT5(w))
        case MixByte(53) if field == 3 => Some(ENN5(w))
        case MixByte(54) if field == 0 => Some(INC6(w))
        case MixByte(54) if field == 1 => Some(DEC6(w))
        case MixByte(54) if field == 2 => Some(ENT6(w))
        case MixByte(54) if field == 3 => Some(ENN6(w))
        case MixByte(55) if field == 0 => Some(INCX(w))
        case MixByte(55) if field == 1 => Some(DECX(w))
        case MixByte(55) if field == 2 => Some(ENTX(w))
        case MixByte(55) if field == 3 => Some(ENNX(w))
        case MixByte(56) if field != 6 => Some(CMPA(w))
        //      case Byte(56) => Some(FCMPA(w))
        //      case Byte(57) => Some(CMP1(w))
        //      case Byte(58) => Some(CMP2(w))
        //      case Byte(59) => Some(CMP3(w))
        //      case Byte(60) => Some(CMP4(w))
        //      case Byte(61) => Some(CMP5(w))
        //      case Byte(62) => Some(CMP6(w))
        //      case Byte(63) => Some(CMPX(w))
        case _ => None
      }
    } catch {
      case _: Exception => None
    }
  }

}