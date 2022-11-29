package challah
package ir
package linked

import err._
import utils.{Print, ids, squished}


sealed trait Node
case class Module(id: DefinitionId) extends Node, Print(s"(module $id)")
case class Import(id: DefinitionId, expr: ast.Id) extends Node, Print(s"(import ${expr.lexeme}:$id)")
case class Val(name: Id, value: Node, expr: ast.Val) extends Node, Print(s"(val $name $value)")
case class Id(id: DefinitionId, expr: ast.Id) extends Node, Print(s"${expr.lexeme}:$id")
case class Num(value: Double, expr: ast.Num) extends Node, Print(value.toString)
case class Str(value: String, expr: ast.Str) extends Node, Print(s""""$value"""")
case class App(fn: Node, args: List[Node]) extends Node, Print(if args.size > 0 then s"($fn ${args.mkString(" ")})" else s"($fn)")


case class ModuleId(val value: String):
  override def toString: String = value


case class DefinitionId():
  val value = ids.next.head
  override def toString: String = s"id#${value}"


def transform(node: ast.Node): Either[LinkingErr, Node] = node match
  case expr: ast.Id => transformId(expr)
  case expr: ast.Str => Right(Str(expr.lexeme, expr))
  case expr: ast.Num => Right(Num(expr.lexeme.toDouble, expr))
  case expr: ast.Uniop => transformApp(expr.op.id, List(expr.rhs))
  case expr: ast.Binop => transformApp(expr.op.id, List(expr.rhs, expr.lhs))
  case stmt: ast.Val => transformVal(stmt)
  case _ => println(node); ???

def transformId(expr: ast.Id): Either[LinkingErr, Id] =
  Right(Id(DefinitionId(), expr))

def transformVal(stmt: ast.Val): Either[LinkingErr, Val] =
  for
    id <- transformId(stmt.name)
    value <- transform(stmt.value)
  yield
    Val(id, value, stmt)

def transformApp(fn: ast.Node, args: List[ast.Node]): Either[LinkingErr, App] =
  for
    fn <- transform(fn)
    args <- args.map { arg => transform(arg) }.squished
  yield
    App(fn, args)
