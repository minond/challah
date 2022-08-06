package challah
package parser

import ast._
import source._
import utils._

import scala.reflect.ClassTag


sealed trait Err
case class UnknownCharErr(char: Char, span: Span) extends Err
case class UnexpectedToken[Expected](token: Token) extends Err
case class UnexpectedEof[Expected](source: Source) extends Err

type CharStream = BufferedIterator[(Char, Int)]
type TokenStream = BufferedIterator[Token]


def parse(source: Source, project: Project): Either[Err, List[Expr]] =
  tokenize(source, project).flatMap { tokens => parse(tokens.iterator.buffered, project) }
def parse(tokens: TokenStream, project: Project): Either[Err, List[Expr]] =
  tokens
    .map { (curr) => parseTop(curr, tokens, project) }
    .squished

def parseTop(curr: Token, tokens: TokenStream, project: Project) = curr match
  case Id("val", _) =>
    for
      name  <- parseId(tokens.next, tokens, project)
      _     <- eat[Eq](tokens, curr.span.source)
      value <- parseExpr(tokens.next, tokens, project)
    yield
      Val(name, value)
  case _ => parseExpr(curr, tokens, project)

def parseExpr(curr: Token, tokens: TokenStream, project: Project) =
  parsePrimary(curr, tokens, project).flatMap { lhs =>
    tokens.headOption match
      case Some(_: Plus) =>
        tokens.next
        parsePrimary(tokens.next, tokens, project).map { rhs =>
          Binop(lhs, rhs, BinaryOperator.Plus)
        }
      case Some(_: Minus) =>
        tokens.next
        parsePrimary(tokens.next, tokens, project).map { rhs =>
          Binop(lhs, rhs, BinaryOperator.Minus)
        }
      case _ => Right(lhs)
  }

def parsePrimary(curr: Token, tokens: TokenStream, project: Project): Either[Err, Expr] = curr match
  case _: Id  => parseId(curr, tokens, project)
  case _: Num => parseNumber(curr, tokens, project)
  case _      => ???

def parseId(curr: Token, tokens: TokenStream, project: Project): Either[Err, Id] = curr match
  case id: Id => Right(id)
  case bad => Left(UnexpectedToken[Id](curr))

def parseNumber(curr: Token, tokens: TokenStream, project: Project): Either[Err, Num] = curr match
  case num: Num => Right(num)
  case bad => Left(UnexpectedToken[Num](curr))

def eat[Expected: ClassTag](tokens: TokenStream, source: Source): Either[Err, Expected] = tokens.headOption match
  case Some(token: Expected) =>
    tokens.next
    Right(token)
  case Some(bad) => Left(UnexpectedToken[Expected](bad))
  case None => Left(UnexpectedEof(source))


def tokenize(source: Source, project: Project): Either[Err, List[Token]] =
  tokenize(project.getContent(source).iterator.zipWithIndex.buffered, source, project)
def tokenize(stream: CharStream, source: Source, project: Project): Either[Err, List[Token]] =
  stream
    .filter { (c, _) => !c.isWhitespace }
    .map { (char, offset) => nextToken(char, offset, stream, source) }
    .squished

def nextToken(char: Char, offset: Int, stream: CharStream, source: Source): Either[Err, Token] = char match
  case '=' =>
    Right(Eq(Span(source, offset)))

  case '+' =>
    Right(Plus(Span(source, offset)))

  case '-' =>
    val tail = takeWhile(stream, isNumTail).mkString
    if tail.isEmpty
    then Right(Minus(Span(source, offset)))
    else Right(Num("-" + tail, Span(source, offset)))

  case head if isLetter(head) =>
    val tail = takeWhile(stream, isIdTail).mkString
    Right(Id(s"$head$tail", Span(source, offset)))

  case head if isNumeric(head) =>
    val tail = takeWhile(stream, isNumeric).mkString
    Right(Num(s"$head$tail", Span(source, offset)))

  case bad =>
    Left(UnknownCharErr(bad, Span(source, offset)))


type Pred[T] = T => Boolean
type Predcond = (Boolean, Boolean) => Boolean

def flpreds[T](preds: Seq[Pred[T]], id: Boolean = true)(cond: Predcond) =
  (c: T) => preds.foldLeft(id)((acc, pred) => cond(acc, pred(c)))

def ge[T <: Char](x: T) = (c: T) => c >= x
def le[T <: Char](x: T) = (c: T) => c <= x
def is[T <: Char](x: T) = (c: T) => c == x
def oneof[T <: Char](xs: T*) = (c: T) => xs.contains(c)
def not[T <: Char](x: T) = (c: T) => c != x
def not[T <: Char](f: Pred[T]) = flpreds(Seq(f))(_ && !_)
def and[T <: Char](fs: Pred[T]*) = flpreds(fs)(_ && _)
def or[T <: Char](fs: Pred[T]*) = flpreds(fs, false)(_ || _)

val isWhitespace = oneof(' ', '\t', '\r', '\n', '\f')
val isLetter = or(and(ge('a'), le('z')),
                  and(ge('A'), le('Z')))
val isNumeric = and(ge('0'),
                    le('9'))
val isNumTail = or(isNumeric,
                   is('.'))
val isIdTail = and(not(isWhitespace),
                   or(isNumeric,
                      isLetter,
                      is('_')))


def takeWhile[T](source: BufferedIterator[(T, _)], pred: Pred[T]): List[T] =
  def aux(acc: List[T]): List[T] =
    if source.isEmpty
    then acc
    else
      val curr = source.head
      if pred(curr._1)
      then aux(acc :+ source.next._1)
      else acc
  aux(List.empty)
