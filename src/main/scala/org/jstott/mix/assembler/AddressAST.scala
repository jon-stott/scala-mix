package org.jstott.mix.assembler

import org.jstott.mix.{Minus, Sign}

import scala.util.matching.Regex

case class EvaluatedAddress(v: Int = 0, index: Int = 0, field: Int = 0)

sealed trait AddressAST {

  type Evaluated = Either[String, Int]

  def evaluate(state: AssemblerState): Evaluated

  def address(state: AssemblerState, operation: OperationToken): Either[String, EvaluatedAddress]

  protected def evaluate(left: Evaluated, right: Evaluated, fn: (Int, Int) => Int): Evaluated = {
    left match {
      case error@Left(_) => error
      case Right(l) => right match {
        case error@Left(_) => error
        case Right(r) => Right(fn(l, r))
      }
    }
  }

}

sealed trait Expression extends AddressAST

sealed trait AtomicExpression extends Expression

case class Symbol(s: String) extends AtomicExpression {

  override def evaluate(state: AssemblerState): Evaluated = {
    s match {
      case Symbol.localSymbolPattern(n, bfh) =>
        findLocalSymbolAddress(state, n.toInt, bfh)
      case _ =>
        state.mix.symbols.get(s) match {
          case Some(Right(v)) => Right(v)
          case _ => Left(s"Can't dereference symbol $s")
        }
    }

  }

  override def address(state: AssemblerState, operation: OperationToken): Either[String, EvaluatedAddress] = {
    s match {
      case Symbol.localSymbolPattern(n, bfh) =>
        findLocalSymbolAddress(state, n.toInt, bfh) match {
          case Right(a) => Right(EvaluatedAddress(a))
          case Left(l) => Left(l)
        }
      case _ =>
        state.mix.symbols.get(s) match {
          case Some(Right(v)) => Right(EvaluatedAddress(v))
          case _ => Left(s"Can't dereference symbol $s")
        }
    }
  }

  private def findLocalSymbolAddress(state: AssemblerState, n: Int, bfh: String): Either[String, Int] = {
    val result = state.mix.localSymbols.get(n) match {
      case Some(symbolLocations) =>
        val diffs = symbolLocations.map(_ - state.locationCounter).toSeq
        bfh match {
          case "B" =>
            val filtered = diffs.filter(_ < 0)
            if (filtered.nonEmpty) {
              Right(state.locationCounter + filtered.sorted.max)
            } else {
              Left(s"Can't dereference symbol $s")
            }
          case "F" =>
            val filtered = diffs.filter(_ >= 0)
            if (filtered.nonEmpty) {
              Right(state.locationCounter + filtered.sorted.min)
            } else {
              Left(s"Can't dereference symbol $s")
            }
          case "H" =>
            Left(s"Unexpected local symbol $s")
        }
      case _ => Left(s"Can't dereference symbol $s")
    }
    result
  }

}

object Symbol {
  val localSymbolPattern: Regex = "^(\\d)([BFH])$".r
}

case class Number(n: Int) extends AtomicExpression {
  override def evaluate(state: AssemblerState): Evaluated = Right(n)

  override def address(state: AssemblerState, operation: OperationToken): Either[String, EvaluatedAddress] =
    Right(EvaluatedAddress(n))
}

case object LocationCounter extends AtomicExpression {
  override def evaluate(state: AssemblerState): Evaluated = Right(state.locationCounter)

  override def address(state: AssemblerState, operation: OperationToken): Either[String, EvaluatedAddress] =
    Right(EvaluatedAddress(state.locationCounter))
}

case object Vacuous extends AtomicExpression {
  override def evaluate(state: AssemblerState): Evaluated = Right(0)

  override def address(state: AssemblerState, operation: OperationToken): Either[String, EvaluatedAddress] =
    Right(EvaluatedAddress())
}

case class SignedAtomicExpression(sign: Sign, atomicExpression: AtomicExpression) extends Expression {
  override def evaluate(state: AssemblerState): Evaluated = {
    atomicExpression.evaluate(state) match {
      case l @ Left(_) => l
      case r @ Right(x) => if (sign == Minus) Right(-1 * x) else r
    }
  }
  override def address(state: AssemblerState, operation: OperationToken): Either[String, EvaluatedAddress] = {
    atomicExpression.address(state, operation)
    // FIXME does the result need to account for the sign?
  }
}

case class AddExpression(left: Expression, right: Expression) extends Expression {
  override def evaluate(state: AssemblerState): Evaluated = {
    evaluate(left.evaluate(state), right.evaluate(state), { (l, r) => l + r })
  }

  override def address(state: AssemblerState, operation: OperationToken): Either[String, EvaluatedAddress] = {
    evaluate(state) match {
      case Left(error) => Left(error)
      case Right(v) => Right(EvaluatedAddress(v, field = operation.defaultField.intValue))
    }
  }
}

case class MinusExpression(left: Expression, right: Expression) extends Expression {
  override def evaluate(state: AssemblerState): Evaluated = {
    evaluate(left.evaluate(state), right.evaluate(state), { (l, r) => l - r })
  }

  override def address(state: AssemblerState, operation: OperationToken): Either[String, EvaluatedAddress] = {
    evaluate(state) match {
      case Left(error) => Left(error)
      case Right(v) => Right(EvaluatedAddress(v, field = operation.defaultField.intValue))
    }
  }
}

case class MultiplyExpression(left: Expression, right: Expression) extends Expression {
  override def evaluate(state: AssemblerState): Evaluated = {
    evaluate(left.evaluate(state), right.evaluate(state), { (l, r) => l * r })
  }

  override def address(state: AssemblerState, operation: OperationToken): Either[String, EvaluatedAddress] = {
    evaluate(state) match {
      case Left(error) => Left(error)
      case Right(v) => Right(EvaluatedAddress(v, field = operation.defaultField.intValue))
    }
  }
}

case class DivideExpression(left: Expression, right: Expression) extends Expression {
  override def evaluate(state: AssemblerState): Evaluated = {
    evaluate(left.evaluate(state), right.evaluate(state), { (l, r) => l / r })
  }

  override def address(state: AssemblerState, operation: OperationToken): Either[String, EvaluatedAddress] = {
    evaluate(state) match {
      case Left(error) => Left(error)
      case Right(v) => Right(EvaluatedAddress(v, field = operation.defaultField.intValue))
    }
  }
}

sealed trait OperatorExpression extends Expression

case class LiteralExpression(expression: Expression) extends OperatorExpression {
  override def evaluate(state: AssemblerState): Evaluated = {
    expression.evaluate(state)
  }

  override def address(
                        state: AssemblerState,
                        operation: OperationToken
                      ): Either[String, EvaluatedAddress] = {
    evaluate(state) match {
      case Left(error) => Left(error)
      case Right(v) => Right(EvaluatedAddress(v, field = operation.defaultField.intValue))
    }
  }
}

case class FieldExpression(left: Expression, right: Expression) extends Expression {
  override def evaluate(state: AssemblerState): Evaluated = {
    evaluate(evaluate(left.evaluate(state), Right(8), { (l, r) => l * r }), right.evaluate(state), { (l, r) => l + r })
  }

  override def address(state: AssemblerState, operation: OperationToken): Either[String, EvaluatedAddress] = {
    ???
  }
}

case class FractionExpression(left: Expression, right: Expression) extends Expression {
  override def evaluate(state: AssemblerState): Evaluated = Left("Can't evaluate fraction")

  override def address(state: AssemblerState, operation: OperationToken): Either[String, EvaluatedAddress] =
    Left("Can't evaluate fraction")
}

case class ValueExpression(a: Expression, i: Expression = Vacuous, f: Expression = Vacuous) extends OperatorExpression {
  override def evaluate(state: AssemblerState): Evaluated = {
    //    val ei = i match {
    //      case Vacuous => None
    //      case x => x.evaluate(state)
    //    }
    //    val ef = f match {
    //      case Vacuous => None
    //      case x => x.evaluate(state)
    //    }
    a.evaluate(state) match {
      case Left(e) => Left(e)
      case Right(v) => Right(v)
    }
    //    Right(EvaluatedValue(a.evaluate(state), ei, ef))
  }

  override def address(state: AssemblerState, operation: OperationToken): Either[String, EvaluatedAddress] = {
    a.evaluate(state) match {
      case Left(error) => Left(error)
      case Right(aVal) => i.evaluate(state) match {
        case Left(error) => Left(error)
        case Right(iVal) => f.evaluate(state) match {
          case Left(error) => Left(error)
          case Right(_) if f == Vacuous => Right(EvaluatedAddress(aVal, iVal, operation.defaultField.intValue))
          case Right(fVal) => Right(EvaluatedAddress(aVal, iVal, fVal))
        }
      }
    }
  }
}
