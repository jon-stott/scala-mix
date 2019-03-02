package org.jstott.mix

import org.jstott.mix.assembler.AddressAST
import org.jstott.mix.instructions.Instruction
import org.jstott.mix.io.IoDevices

case class Mix(
                accumulator: Word = Word(),
                extension: Word = Word(),
                index1: TwoSignedBytes = TwoSignedBytes(),
                index2: TwoSignedBytes = TwoSignedBytes(),
                index3: TwoSignedBytes = TwoSignedBytes(),
                index4: TwoSignedBytes = TwoSignedBytes(),
                index5: TwoSignedBytes = TwoSignedBytes(),
                index6: TwoSignedBytes = TwoSignedBytes(),
                jump: TwoUnsignedBytes = TwoUnsignedBytes(),
                overflow: Overflow = Off,
                comparisonIndicator: ComparisonIndicator = Equal,
                memory: Seq[Word] = Seq.fill(4000)(Word()),
                ioDevices: IoDevices = new IoDevices,
                memoryModificationCount: Seq[Long] = Seq.fill(4000)(0L),
                memoryExecutionCount: Seq[Long] = Seq.fill(4000)(0L),
                programCounter: Int = 0,
                symbols: Map[String, Either[AddressAST, Int]] = Map.empty,
                localSymbols: Map[Int, Set[Int]] = Map.empty,
                numExecutedInstructions: Int = 0,
                executedInstructions: Seq[(Int, Instruction)] = Seq.empty,
                terminate: Boolean = false
              ) {

  def a: Word = accumulator

  def x: Word = extension

  def i1: TwoSignedBytes = index1

  def i2: TwoSignedBytes = index2

  def i3: TwoSignedBytes = index3

  def i4: TwoSignedBytes = index4

  def i5: TwoSignedBytes = index5

  def i6: TwoSignedBytes = index6

  def i(index: MixByte): TwoSignedBytes = index match {
    case MixByte(0) => TwoSignedBytes.zero
    case MixByte(1) => i1
    case MixByte(2) => i2
    case MixByte(3) => i3
    case MixByte(4) => i4
    case MixByte(5) => i5
    case MixByte(6) => i6
    case _ => throw new IllegalArgumentException(s"index [$index] is not 0..6")
  }

  def j: TwoUnsignedBytes = jump

  def pc: Int = programCounter

  def contents(t: TwoSignedBytes): Word = contents(t.intValue)

  def contents(t: TwoSignedBytes, index: MixByte): Word = contents(t.intValue + i(index).intValue)

  def contents(m: Int): Word = memory(m)

  def withRegister(register: Register, value: Bytes): Mix = {
    register match {
      case Accumulator if value.isInstanceOf[Word] => copy(accumulator = value.asInstanceOf[Word])
      case Extension if value.isInstanceOf[Word] => copy(extension = value.asInstanceOf[Word])
      case Index1 if value.isInstanceOf[TwoSignedBytes] => copy(index1 = value.asInstanceOf[TwoSignedBytes])
      case Index2 if value.isInstanceOf[TwoSignedBytes] => copy(index2 = value.asInstanceOf[TwoSignedBytes])
      case Index3 if value.isInstanceOf[TwoSignedBytes] => copy(index3 = value.asInstanceOf[TwoSignedBytes])
      case Index4 if value.isInstanceOf[TwoSignedBytes] => copy(index4 = value.asInstanceOf[TwoSignedBytes])
      case Index5 if value.isInstanceOf[TwoSignedBytes] => copy(index5 = value.asInstanceOf[TwoSignedBytes])
      case Index6 if value.isInstanceOf[TwoSignedBytes] => copy(index6 = value.asInstanceOf[TwoSignedBytes])
      case Jump if value.isInstanceOf[TwoUnsignedBytes] => copy(jump = value.asInstanceOf[TwoUnsignedBytes])
      case _ => this
    }
  }

  def withMemoryUpdated(w: Word, index: Int): Mix = {
    val newMMC = memoryModificationCount.updated(index, memoryModificationCount(index) + 1)
    copy(memory = memory.updated(index, w), memoryModificationCount = newMMC)
  }

  def withProgramCounter(newProgramCounter: Int): Mix = copy(programCounter = newProgramCounter)

  def incrementProgramCounter: Mix = copy(programCounter = programCounter + 1)

  def withOverflow(o: Overflow): Mix = copy(overflow = o)

  def compute: Mix = {
    val newMEC = memoryExecutionCount.updated(pc, memoryExecutionCount(pc) + 1)
    val instruction = Instruction.fromMemory(contents(pc))
    val newState = instruction match {
      case Some(i) => i.compute(copy(
        memoryExecutionCount = newMEC,
        numExecutedInstructions = numExecutedInstructions + 1,
        executedInstructions = executedInstructions :+ (pc, i)
      ))
      case _ => this
    }
    newState
  }

  override def toString: String = {
    f"""+-----------------------------------------------------------------------------------------------------------------------+
       £|                                                          MIX                                                          |
       £+-----------------------------------------------------------------------------------------------------------------------+
       £|     rA = ${a.toString(suppressOperation = true)}   rX = ${x.toString(suppressOperation = true)}   rJ = ${j.toString}  |
       £|    rI1 = ${i1.toString}                         rI2 = ${i2.toString}                         rI3 = ${i3.toString}  |
       £|    rI4 = ${i4.toString}                         rI5 = ${i5.toString}                         rI6 = ${i6.toString}  |
       £|     pc = $pc%7d                            overflow = $overflow%3s                              comparison = $comparisonIndicator%7s          |
       £| # exec = $numExecutedInstructions%7d                                                                                                      |
       £+-----------------------------------------------------------------------------------------------------------------------+
       £$memoryToString
       £+-----------------------------------------------------------------------------------------------------------------------+
       £|Line Printer                                                                                                           |
       £ ${ioDevices.linePrinter}
       £+-----------------------------------------------------------------------------------------------------------------------+
     """.stripMargin('£')
  }

  private def memoryToString: String = {
    memory.zipWithIndex.filter(_._1.intValue != 0).map { case (w, i) =>
      val pcStr = if (i == pc) "->" else "  "
      val symStr = localSymbols.find { case (_, ls) =>
          ls.contains(i)
      }.map { case (memLoc, _) => s"${memLoc}H" }.getOrElse("")
      val wStr = w.toString(suppressOperation = false)
      val xStr = memoryExecutionCount(i)
      val mStr = memoryModificationCount(i)
      f"| $pcStr $symStr%10s $i%04d = $wStr    x = $xStr%8d    m = $mStr%8d                    |"
    }.mkString("\n")
  }

}

