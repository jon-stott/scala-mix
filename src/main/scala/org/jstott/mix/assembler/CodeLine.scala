package org.jstott.mix.assembler

case class CodeLine(location: String, operation: String, address: String)

object CodeLine {

  def apply(line: String): CodeLine = {
    if (line.length <= 17) new IllegalArgumentException("a code line must be at least 17 characters long")
    if (line(10) != ' ' || line(15) != ' ') new IllegalArgumentException("columns 11 and 16 must be blank")
    if (line.length > 80) new IllegalArgumentException("a code line must be no more than 80 characteters long")
    CodeLine(line.substring(0, 9).trim, line.substring(11, 14).trim, line.substring(16).trim)
  }

}