package challah
package ast

import source.Span


sealed trait Token(span: Span)
case class Id(lexeme: String, span: Span) extends Token(span)
case class Num(lexeme: String, span: Span) extends Token(span)
case class OpenParen(span: Span) extends Token(span)
case class CloseParen(span: Span) extends Token(span)
case class OpenCurly(span: Span) extends Token(span)
case class CloseCurly(span: Span) extends Token(span)
case class Eq(span: Span) extends Token(span)
case class Plus(span: Span) extends Token(span)
case class Minus(span: Span) extends Token(span)
