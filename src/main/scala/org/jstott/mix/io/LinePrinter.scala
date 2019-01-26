package org.jstott.mix.io

import org.jstott.mix.{Mix, Word}

class LinePrinter extends IoDevice {

  val paper: StringBuilder = new StringBuilder

  def ioc(mix: Mix): Unit = {

  }

  def out(mix: Mix, address: Int): Unit = {
    val toPrint = mix.memory.slice(address, address + 24)
    toPrint.foreach { word: Word =>
      paper.append(word.asCharacters)
    }
    paper.append("\n")
  }

  override def toString: String = {
    paper.toString
  }

}
