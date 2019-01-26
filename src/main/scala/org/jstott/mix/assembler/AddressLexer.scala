package org.jstott.mix.assembler

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

object AddressLexer extends RegexParsers {

  override def skipWhitespace: Boolean = true

  override val whiteSpace: Regex = "[ \t\r\f]+".r

  def identifier: Parser[SymbolToken] = {
    "\\d*[A-Z][A-Z0-9]*".r ^^ { s => SymbolToken(s) }
  }

  def number: Parser[NumberToken] = {
    "[0-9]{1,10}".r ^^ { s => NumberToken(s.toInt) }
  }

  def plus: Parser[PlusToken.type] = "\\+".r ^^ { s => PlusToken }
  def minus: Parser[MinusToken.type] = "-".r ^^ { s => MinusToken }
  def asterisk: Parser[AsteriskToken.type] = "\\*".r ^^ { s => AsteriskToken }
  def divide: Parser[DivideToken.type] = "/[^/]".r ^^ { s => DivideToken }
  def fraction: Parser[FractionToken.type] = "//".r ^^ { s => FractionToken }
  def colon: Parser[ColonToken.type] = ":".r ^^ { s => ColonToken }
  def comma: Parser[CommaToken.type] = ",".r ^^ { s => CommaToken }
  def openParenthesis: Parser[OpenParenthesisToken.type] = "\\(".r ^^ { s => OpenParenthesisToken }
  def closeParenthesis: Parser[CloseParenthesisToken.type] = "\\)".r ^^ { s => CloseParenthesisToken }
  def equals: Parser[EqualsToken.type] = "\\=".r ^^ { s => EqualsToken }

  def tokens: Parser[List[AddressToken]] = {
    phrase(
      rep1(
        identifier | number | plus | minus | asterisk | divide | fraction | colon | comma | openParenthesis |
          closeParenthesis | equals
      )
    )
  }

  def apply(code: String): Either[String, List[AddressToken]] = {
    parse(tokens, code) match {
      case NoSuccess(msg, next) => Left(s"Error: $msg")
      case Success(result, next) => Right(result)
    }
  }

}
