package org.jstott.mix.assembler

import org.jstott.mix.{Minus, Plus}
import org.jstott.mix.assembler.AddressParser.opt

import scala.util.parsing.combinator.Parsers

object AddressParser extends Parsers {

  override type Elem = AddressToken

  def symbol: Parser[Symbol] = accept("symbol", { case _ @ SymbolToken(s) => Symbol(s) })

  def number: Parser[Number] = accept("number", { case _ @ NumberToken(s) => Number(s) })

  def signedNumber: Parser[Number] = {
    opt(MinusToken) ~ number ^^ {
      case Some(_) ~ Number(num) => Number(-1 * num)
      case _ ~ Number(num) => Number(num)
    }
  }

  def locationCounter: Parser[LocationCounter.type] = accept("location counter", { case _ @ AsteriskToken => LocationCounter })

  def atomicExpression: Parser[AtomicExpression] = signedNumber | symbol | locationCounter

  def signedAtomicExpression: Parser[SignedAtomicExpression] = opt(MinusToken) ~ atomicExpression ^^ {
    case Some(MinusToken) ~ x => SignedAtomicExpression(Minus, x)
    case _ ~ x                => SignedAtomicExpression(Plus, x)
  }

  def fieldExpression: Parser[FieldExpression] = {
    OpenParenthesisToken ~ atomicExpression ~ opt(ColonToken ~ atomicExpression) ~ CloseParenthesisToken ^^ {
      case _ ~ left ~ Some(_ ~ right) ~ _ => FieldExpression(left, right)
      case _ ~ left ~ _ ~ _ => FieldExpression(Number(0), left)
    }
  }

  def literalExpression: Parser[LiteralExpression] = {
    EqualsToken ~ expression ~ EqualsToken ^^ {
      case _ ~ expression ~ _ => LiteralExpression(expression)
    }
  }

  def expression: Parser[Expression] = {
    signedAtomicExpression ~
    opt(
      rep(
        (PlusToken | MinusToken | AsteriskToken | DivideToken | ColonToken | FractionToken) ~
          signedAtomicExpression
      )
    ) ^^ {
      case left ~ None => left
      case left ~ Some(rights) =>
        rights.foldLeft(left.asInstanceOf[Expression]) {
          case (x, PlusToken ~ y) => AddExpression(x, y)
          case (x, MinusToken ~ y) => MinusExpression(x, y)
          case (x, AsteriskToken ~ y) => MultiplyExpression(x, y)
          case (x, DivideToken ~ y) => DivideExpression(x, y)
          case (x, ColonToken ~ y) => FieldExpression(x, y)
          case (x, FractionToken ~ y) => FractionExpression(x, y)
        }
    }
  }

  def valueExpression: Parser[OperatorExpression] = {
    literalExpression |
      (
        expression ~
        opt(
          CommaToken ~ expression
        ) ~
        opt(
          expression | fieldExpression
        )
      ) ^^ {
      case aPart ~ Some(_ ~ iPart) ~ Some(fPart) =>
        ValueExpression(aPart, iPart, fPart)
      case aPart ~ Some(_ ~ iPart) ~ _ =>
        ValueExpression(aPart, iPart, Vacuous)
      case aPart ~ _ ~ Some(fPart) =>
        ValueExpression(aPart, Vacuous, fPart)
      case aPart ~ _ ~ _ =>
        ValueExpression(aPart, Vacuous, Vacuous)
    }
  }

  def address: Parser[AddressAST] = {
    phrase(valueExpression)
  }

  def apply(tokens: Seq[AddressToken]): Either[String, AddressAST] = {
    val reader = new AddressTokenReader(tokens)
    address(reader) match {
      case NoSuccess(msg, next) => Left(msg)
      case Success(result, next) => Right(result)
    }
  }

}
