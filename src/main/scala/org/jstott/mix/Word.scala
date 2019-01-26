package org.jstott.mix

import org.jstott.mix.instructions.Instruction

case class Word(sign: Sign = Plus, override val bytes: Seq[MixByte] = Seq.fill(5)(MixByte(0))) extends SignedBytes {

  require(bytes.length == 5, s"must be 5 bytes - got [${bytes.length}]")

  def +(o: MixByte): Word = Word.apply(intValue + o.value)
  def +(o: TwoSignedBytes): Word = Word.apply(intValue + o.intValue)

//  def valueBySpecification(specification: SByte)

  def valueBySpecification(left: Int, right: Int) = {
    //valueBySpecification((left * 8) + right)
    if (left == 0 && right == 0) {
      sign
    } else if (left == 0) {
      bytes.slice(left, right)
    } else {
      bytes.slice(left + 1, right)
    }
  }

  def withSign(s: Sign): Word = copy(sign = s)

  def invertSign: Word = copy(sign = if (sign == Plus) Minus else Plus)

  /**
    * @param index the index to replace, counting from 1 through 5 (0 means the sign).
    */
  def withByte(b: MixByte, index: Int): Word = withBytes(Seq(b), index)

  /**
    * @param index the index to replace from, counting from 1 through 5 (0 means the sign).
    */
  def withBytes(bs: Seq[MixByte], index: Int): Word = {
    if (index < 1 || index > 5) {
      throw new IllegalArgumentException(s"index [$index] must be in range 1..5")
    }
    // FIXME need to trim bs if it would cause too many bytes in the new word.
    copy(bytes = bytes.patch(index - 1, bs, bs.length))
  }

  def intValue: Int = {
    signAsMultiplier *
      (bytes(4).value +
      (bytes(3).value << 6) +
      (bytes(2).value << 12) +
      (bytes(1).value << 18) +
      (bytes.head.value << 24))
  }

  def asCharacters: String = {
    bytes.map(_.asCharacter).mkString
  }

  override def toString: String = toString(suppressOperation = false)

  def toString(suppressOperation: Boolean): String = {
    val instructionStr = Instruction.fromMemory(this).map(_.name).getOrElse("")
    val op = if (suppressOperation) "" else f" ($instructionStr%4s)"
    val wordValueStr = f" [$intValue%11d]"
    f"$sign ${bytes.head} ${bytes(1)} (${TwoSignedBytes(sign, bytes.take(2)).intValue}%5d) ${bytes(2)} ${bytes(3)} ${bytes(4)}$op$wordValueStr"
  }

}

object Word {

  val maxValue: Int = Math.pow(2, MixByte.bits * 5).toInt - 1
  val minValue: Int = -1 * maxValue

  def apply(i: Int): Word = {
    val abs = if (i < 0) i * -1 else i
    Word(
      sign = if (i >= 0) Plus else Minus,
      bytes = Seq(MixByte((abs >> 24) & 63), MixByte((abs >> 18) & 63), MixByte((abs >> 12) & 63), MixByte((abs >> 6) & 63), MixByte(abs & 63))
    )
  }

  def apply(t: TwoSignedBytes): Word = Word(
    sign = t.sign,
    bytes = Seq(MixByte(0), MixByte(0), MixByte(0), t.bytes.head, t.bytes(1))
  )

  def apply(t: TwoUnsignedBytes): Word = Word(
    sign = Plus,
    bytes = Seq(MixByte(0), MixByte(0), MixByte(0), t.bytes.head, t.bytes(1))
  )

}