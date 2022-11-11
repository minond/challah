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
case class Comma(override val span: Span) extends Token(span)
case class Plus(override val span: Span) extends Token(span)
case class Minus(override val span: Span) extends Token(span)

sealed trait Stmt
case class Module(name: Id, symbols: List[Id], span: Span) extends Stmt, Print(s"(module $name (${symbols.mkString(" ")}))")
case class Val(name: Id, value: Expr) extends Stmt, Print(s"(val $name $value)")

sealed trait Expr extends Stmt
case class Id(lexeme: String, override val span: Span) extends Expr, Token(span), Print(lexeme)
case class Num(lexeme: String, override val span: Span) extends Expr, Token(span), Print(lexeme)
case class Str(lexeme: String, override val span: Span) extends Expr, Token(span), Print(s""""$lexeme"""")
case class Binop(op: BinaryOperator, lhs: Expr, rhs: Expr) extends Expr, Print(s"($op $lhs $rhs)")
case class Uniop(op: UnaryOperator, rhs: Expr) extends Expr, Print(s"($op $rhs)")

enum UnaryOperator:
  case Minus extends UnaryOperator, Print("-")

enum BinaryOperator:
  case Plus extends BinaryOperator, Print("+")
  case Minus extends BinaryOperator, Print("-")
