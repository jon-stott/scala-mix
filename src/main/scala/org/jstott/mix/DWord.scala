package org.jstott.mix

import org.jstott.mix.instructions.Instruction

case class DWord(sign: Sign = Plus, override val bytes: Seq[MixByte] = Seq.fill(10)(MixByte(0))) extends SignedBytes {

  require(bytes.length == 10, s"must be 10 bytes - got [${bytes.length}]")

  def +(o: MixByte): DWord = DWord.apply(intValue + o.value)
  def +(o: TwoSignedBytes): DWord = DWord.apply(intValue + o.intValue)

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

  def withSign(s: Sign): DWord = copy(sign = s)

  def intValue: Int = {
    signAsMultiplier *
      (bytes(9).value +
        (bytes(8).value << 6) +
        (bytes(7).value << 12) +
        (bytes(6).value << 18) +
        (bytes(5).value << 24) +
        (bytes(4).value << 30) +
        (bytes(3).value << 36) +
        (bytes(2).value << 42) +
        (bytes(1).value << 48) +
        (bytes.head.value << 54))
  }

  override def longValue: Long = {
    signAsMultiplier *
      (bytes(9).value +
      (bytes(8).value << 6) +
      (bytes(7).value << 12) +
      (bytes(6).value << 18) +
      (bytes(5).value << 24) +
      (bytes(4).value << 30) +
      (bytes(3).value << 36) +
      (bytes(2).value << 42) +
      (bytes(1).value << 48) +
      (bytes.head.value << 54))
  }

  def asCharacters: String = {
    bytes.map(_.asCharacter).mkString
  }

  override def toString: String = {
    s"$sign ${bytes.map(_.toString).mkString(" ")}"
  }

}

object DWord {

  val maxValue: Int = Math.pow(2, MixByte.bits * 10).toInt - 1
  val minValue: Int = -1 * maxValue

  def apply(i: Long): DWord = {
    val abs = if (i < 0) i * -1 else i
    DWord(
      sign = if (i >= 0) Plus else Minus,
      bytes = Seq(MixByte(((abs >> 54) & 63).toInt), MixByte(((abs >> 48) & 63).toInt), MixByte(((abs >> 42) & 63).toInt),
        MixByte(((abs >> 36) & 63).toInt), MixByte(((abs >> 30) & 63).toInt), MixByte(((abs >> 24) & 63).toInt),
        MixByte(((abs >> 18) & 63).toInt), MixByte(((abs >> 12) & 63).toInt), MixByte(((abs >> 6) & 63).toInt),
        MixByte((abs & 63).toInt))
    )
  }

  def apply(w1: Word, w2: Word): DWord = {
    DWord(w1.sign, w1.bytes ++ w2.bytes)
  }

  def apply(t: TwoSignedBytes): DWord = DWord(
    sign = t.sign,
    bytes = Seq(MixByte(0), MixByte(0), MixByte(0), MixByte(0), MixByte(0), MixByte(0), MixByte(0), MixByte(0), t.bytes.head, t.bytes(1))
  )

  def apply(t: TwoUnsignedBytes): DWord = DWord(
    sign = Plus,
    bytes = Seq(MixByte(0), MixByte(0), MixByte(0), MixByte(0), MixByte(0), MixByte(0), MixByte(0), MixByte(0), t.bytes.head, t.bytes(1))
  )

}