package challah
package ast

import source.Span


sealed trait Token(val span: Span)
case class OpenParen(override val span: Span) extends Token(span)
case class CloseParen(override val span: Span) extends Token(span)
case class OpenCurly(override val span: Span) extends Token(span)
case class CloseCurly(override val span: Span) extends Token(span)
case class Eq(override val span: Span) extends Token(span)
case class Plus(override val span: Span) extends Token(span)
case class Minus(override val span: Span) extends Token(span)

sealed trait Expr
case class Id(lexeme: String, override val span: Span) extends Expr, Token(span)
case class Num(lexeme: String, override val span: Span) extends Expr, Token(span)
case class Val(name: Id, value: Expr) extends Expr
case class Binop(lhs: Expr, rhs: Expr, op: BinaryOperator) extends Expr

enum BinaryOperator:
  case Plus, Minus
