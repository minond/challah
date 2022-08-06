package challah
package parser

import ast._
import source._
import utils._


sealed trait Err
case class UnknownCharErr(char: Char, span: Span) extends Err


def tokenize(source: Source, project: Project): Either[Err, List[Token]] =
  val stream = project.getContent(source).iterator.zipWithIndex.buffered

  stream
    .filter { (c, _) => !c.isWhitespace }
    .map { (c, i) =>
      c match
        case '=' =>
          Right(Eq(Span(source, i)))

        case '-' =>
          val tail = takeWhile(stream, isNumTail).mkString
          if tail.isEmpty
          then Right(Minus(Span(source, i)))
          else Right(Num("-" + tail, Span(source, i)))

        case head if isLetter(head) =>
          val tail = takeWhile(stream, isIdTail).mkString
          Right(Id(s"$head$tail", Span(source, i)))

        case head if isNumeric(head) =>
          val tail = takeWhile(stream, isNumeric).mkString
          Right(Num(s"$head$tail", Span(source, i)))

        case bad =>
          Left(UnknownCharErr(bad, Span(source, i)))
    }
    .squished

type Pred[T] = T => Boolean
type Predcond = (Boolean, Boolean) => Boolean

def flpreds[T](preds: Seq[Pred[T]], id: Boolean = true)(cond: Predcond) =
  (c: T) => preds.foldLeft(id)((acc, pred) => cond(acc, pred(c)))

def ge[T <: Char](x: T) = (c: T) => c >= x
def le[T <: Char](x: T) = (c: T) => c <= x
def is[T <: Char](x: T) = (c: T) => c == x
def aint[T <: Char](x: T) = (c: T) => c != x
def oneof[T <: Char](xs: T*) = (c: T) => xs.contains(c)
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
