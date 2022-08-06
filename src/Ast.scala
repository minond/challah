package challah
package ast

import source.Span
import utils.Print


sealed trait Token(val span: Span)
case class OpenParen(override val span: Span) extends Token(span)
case class CloseParen(override val span: Span) extends Token(span)
case class OpenCurly(override val span: Span) extends Token(span)
case class CloseCurly(override val span: Span) extends Token(span)
case class Eq(override val span: Span) extends Token(span)
case class Plus(override val span: Span) extends Token(span)
case class Minus(override val span: Span) extends Token(span)

sealed trait Expr
case class Id(lexeme: String, override val span: Span) extends Expr, Token(span), Print(lexeme)
case class Num(lexeme: String, override val span: Span) extends Expr, Token(span), Print(lexeme)
case class Val(name: Id, value: Expr) extends Expr, Print(s"(val $name $value)")
case class Binop(lhs: Expr, rhs: Expr, op: BinaryOperator) extends Expr, Print(s"($op $lhs $rhs)")

enum BinaryOperator:
  case Plus extends BinaryOperator, Print("+")
  case Minus extends BinaryOperator, Print("-")
