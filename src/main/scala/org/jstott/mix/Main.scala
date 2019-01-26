package org.jstott.mix

import org.jstott.mix.assembler.Assembler

import scala.io.Source

object Main extends App {

  if (args.length == 1) {
    val program = Source.fromFile(args.head).mkString
    var state = new Assembler(program).assemble
    println(state.toString)
    try {
      do state = state.compute while (!state.terminate)
    } catch {
      case e: Exception =>
        e.printStackTrace()
    }
    println(state.toString)
  } else {
    println("File not specified.")
  }

}
