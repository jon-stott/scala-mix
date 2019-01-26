package org.jstott.mix.io

import org.jstott.mix.Mix

class IoDevices {

  val tapeUnits: Seq[TapeUnit] = Seq.fill(8)(new TapeUnit)

  val diskUnits: Seq[DiskUnit] = Seq.fill(8)(new DiskUnit)

  val cardReader: CardReader = new CardReader

  val cardPunch: CardPunch = new CardPunch

  val linePrinter: LinePrinter = new LinePrinter

  val typewriter: Typewriter = new Typewriter

  val paperTape: PaperTape = new PaperTape

  def getIoDevice(mix: Mix, unitNumber: Int): IoDevice = {
    unitNumber match {
      case x if x >= 0 && x <= 7 => mix.ioDevices.tapeUnits(x)
      case x if x >= 8 && x <= 15 => mix.ioDevices.diskUnits(x)
      case 16 => mix.ioDevices.cardReader
      case 17 => mix.ioDevices.cardPunch
      case 18 => mix.ioDevices.linePrinter
      case 19 => mix.ioDevices.typewriter
      case 20 => mix.ioDevices.paperTape
    }
  }

}
