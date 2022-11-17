package challah
package source

import ast.{Stmt, Module}


case class Source(name: String, id: Int)
case class Span(source: Source, offset: Int)

case class Tree(stmts: List[Stmt], module: Option[Module], imports: List[_])
object Tree:
  def load(xs: List[Stmt]): Tree =
    val (stmts, module) = xs.foldLeft[(List[Stmt], Option[Module])]((List.empty, None)) {
      case ((stmts, module), stmt) => stmt match
        case module: Module =>
          (stmts, Some(module))
        case _ =>
          (stmts :+ stmt, module)
    }

    Tree(stmts, module, List.empty)
