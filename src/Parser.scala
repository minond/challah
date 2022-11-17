package challah
package parser

import ast._
import err.{SyntaxErr, UnknownCharErr, UnexpectedToken, UnexpectedEof}
import project.Project
import source.{Span, Source}
import utils.squished

import scala.reflect.ClassTag


type CharStream = BufferedIterator[(Char, Int)]
type TokenStream = BufferedIterator[Token]


def parse(source: Source, project: Project): Either[SyntaxErr, List[Stmt]] =
  tokenize(source, project).flatMap { tokens => parse(tokens.iterator.buffered, project) }
def parse(tokens: TokenStream, project: Project): Either[SyntaxErr, List[Stmt]] =
  tokens.map { (curr) => parseTop(curr, tokens, project) }
        .squished

def parseTop(curr: Token, tokens: TokenStream, project: Project) = curr match
  case start @ Id("module", _) => parseModule(start, tokens, project)
  case start @ Id("import", _) => parseImport(start, tokens, project)
  case start @ Id("val", _)    => parseVal(start, tokens, project)
  case _                       => parseExpr(curr, tokens, project)

def parseModule(start: Id, tokens: TokenStream, project: Project): Either[SyntaxErr, Module] =
  for
    name <- parseId(tokens.next, tokens, project)
    ids  <- parseOptionalIds(tokens.headOption, tokens, project)
  yield
    Module(name, ids, start.span)

def parseImport(start: Id, tokens: TokenStream, project: Project): Either[SyntaxErr, Import] =
  for
    name <- parseId(tokens.next, tokens, project)
    ids  <- parseOptionalIds(tokens.headOption, tokens, project)
  yield
    Import(name, ids, start.span)

def parseOptionalIds(headOption: Option[Token], tokens: TokenStream, project: Project): Either[SyntaxErr, List[Id]] = headOption match
  case Some(OpenParen(_)) =>
    tokens.next
    takeFromByUntil(
      tokens,
      { (head, stream) => parseId(head, stream, project) },
      { case Comma(_) => true
        case _        => false },
      { case CloseParen(_) => true
        case _             => false },
    )

  case _ => Right(List.empty)

def parseVal(start: Id, tokens: TokenStream, project: Project): Either[SyntaxErr, Val] =
  for
    name  <- parseId(tokens.next, tokens, project)
    _     <- eat[Eq](tokens, start.span.source)
    value <- parseExpr(tokens.next, tokens, project)
  yield
    Val(name, value)

def parseExpr(curr: Token, tokens: TokenStream, project: Project) = curr match
  case _: Minus => parseUniop(tokens.next, tokens, project, UnaryOperator.Minus)
  case _ =>
    parsePrimary(curr, tokens, project).flatMap { lhs =>
      tokens.headOption match
        case Some(_: Plus)  => tokens.next; parseBinop(tokens.next, tokens, project, lhs, BinaryOperator.Plus)
        case Some(_: Minus) => tokens.next; parseBinop(tokens.next, tokens, project, lhs, BinaryOperator.Minus)
        case _              => Right(lhs)
    }

def parseBinop(curr: Token, tokens: TokenStream, project: Project, lhs: Expr, op: BinaryOperator): Either[SyntaxErr, Binop] =
  parseExpr(curr, tokens, project).map { rhs => structureBinop(op, lhs, rhs) }

def structureBinop(op: BinaryOperator, lhs: Expr, rhs: Expr): Binop = rhs match
  case Binop(rhsOp, rhsLhs, rhsRhs) => Binop(rhsOp, structureBinop(op, lhs, rhsLhs), rhsRhs)
  case _ => Binop(op, lhs, rhs)

def parseUniop(curr: Token, tokens: TokenStream, project: Project, op: UnaryOperator): Either[SyntaxErr, Uniop] =
  parseExpr(curr, tokens, project).map { rhs =>
    Uniop(op, rhs)
  }

def parsePrimary(curr: Token, tokens: TokenStream, project: Project): Either[SyntaxErr, Expr] = curr match
  case _: Id    => parseId(curr, tokens, project)
  case _: Num   => parseNum(curr, tokens, project)
  case _: Str   => parseStr(curr, tokens, project)
  case _        => throw Exception(s"curr = ${curr}")

def parseId(curr: Token, tokens: TokenStream, project: Project): Either[SyntaxErr, Id] = curr match
  case id: Id => Right(id)
  case bad => Left(UnexpectedToken[Id](curr))

def parseNum(curr: Token, tokens: TokenStream, project: Project): Either[SyntaxErr, Num] = curr match
  case num: Num => Right(num)
  case bad => Left(UnexpectedToken[Num](curr))

def parseStr(curr: Token, tokens: TokenStream, project: Project): Either[SyntaxErr, Str] = curr match
  case str: Str => Right(str)
  case bad => Left(UnexpectedToken[Str](curr))

def eat[Expected: ClassTag](tokens: TokenStream, source: Source): Either[SyntaxErr, Expected] = tokens.headOption match
  case Some(token: Expected) =>
    tokens.next
    Right(token)
  case Some(bad) => Left(UnexpectedToken[Expected](bad))
  case None => Left(UnexpectedEof(source))

def takeFromByUntil[T, E, R](
  stream: BufferedIterator[T],
  take: (head: T, stream: BufferedIterator[T]) => Either[E, R],
  by: (head: T) => Boolean,
  until: (head: T) => Boolean,
  allowTrailing: Boolean = false
): Either[E, List[R]] =
  def aux(acc: List[R]): Either[E, List[R]] =
    if until(stream.head) then
      stream.next
      Right(acc)
    else if by(stream.head) && allowTrailing then
      stream.next
      aux(acc)
    else if by(stream.head) then
      stream.next
      take(stream.next, stream).flatMap { r => aux(acc :+ r) }
    else
      take(stream.next, stream).flatMap { r => aux(acc :+ r) }
  aux(List.empty)


def tokenize(source: Source, project: Project): Either[SyntaxErr, List[Token]] =
  tokenize(project.getContent(source).iterator.zipWithIndex.buffered, source, project)
def tokenize(stream: CharStream, source: Source, project: Project): Either[SyntaxErr, List[Token]] =
  stream
    .filter { (c, _) => !c.isWhitespace }
    .map { (char, offset) => nextToken(char, offset, stream, source) }
    .squished

def nextToken(char: Char, offset: Int, stream: CharStream, source: Source): Either[SyntaxErr, Token] = char match
  case '(' => Right(OpenParen(Span(source, offset)))
  case ')' => Right(CloseParen(Span(source, offset)))
  case ',' => Right(Comma(Span(source, offset)))
  case '=' => Right(Eq(Span(source, offset)))
  case '+' => Right(Plus(Span(source, offset)))
  case '-' => Right(Minus(Span(source, offset)))

  case '"' =>
    val lexeme = takeUntil(stream, is('"')).mkString
    Right(Str(lexeme, Span(source, offset)))

  case head if isLetter(head) =>
    val tail = takeWhile(stream, isIdTail).mkString
    Right(Id(s"$head$tail", Span(source, offset)))

  case head if isNumeric(head) =>
    val tail = takeWhile(stream, isNumTail).mkString
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

def takeUntil[T](source: BufferedIterator[(T, _)], pred: Pred[T]): List[T] =
  def aux(acc: List[T]): List[T] =
    if source.isEmpty
    then acc
    else
      val curr = source.head
      if pred(curr._1)
      then
        source.next
        acc
      else aux(acc :+ source.next._1)
  aux(List.empty)
