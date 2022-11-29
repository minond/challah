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

sealed trait Node

sealed trait Stmt extends Node
case class Module(name: Id, symbols: List[Id], span: Span) extends Stmt, Print(s"(module $name (${symbols.mkString(" ")}))")
case class Import(name: Id, symbols: List[Id], span: Span) extends Stmt, Print(s"(import $name (${symbols.mkString(" ")}))")
case class Val(name: Id, value: Expr) extends Stmt, Print(s"(val $name $value)")

sealed trait Expr extends Node
case class Id(lexeme: String, override val span: Span) extends Expr, Token(span), Print(lexeme)
case class Num(lexeme: String, override val span: Span) extends Expr, Token(span), Print(lexeme)
case class Str(lexeme: String, override val span: Span) extends Expr, Token(span), Print(s""""$lexeme"""")
case class Binop(op: BinaryOperator, lhs: Expr, rhs: Expr) extends Expr, Print(s"($op $lhs $rhs)")
case class Uniop(op: UnaryOperator, rhs: Expr) extends Expr, Print(s"($op $rhs)")

enum UnaryOperator(val id: Id):
  case Minus(override val id: Id) extends UnaryOperator(id), Print("-")

enum BinaryOperator(val id: Id):
  case Plus(override val id: Id) extends BinaryOperator(id), Print("+")
  case Minus(override val id: Id) extends BinaryOperator(id), Print("-")
