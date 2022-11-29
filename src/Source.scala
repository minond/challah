package challah
package source

import utils.ids

case class SourceId():
  val value = ids.next.head
  override def toString: String = s"source#${value}"

case class SourceMapping(name: String):
  val id = SourceId()

case class Span(sourceMapping: SourceMapping, offset: Int)

case class Source(module: Option[ast.Module], imports: List[ast.Import], val statements: List[ast.Node]):
  override def toString(): String =
    s"""
     |${module.getOrElse("")}
     |
     |${imports.mkString("\n")}
     |
     |${statements.mkString("\n")}
    """.trim.stripMargin('|')

object Source:
  def load(xs: List[ast.Node]): Source =
    val (module, imports, statements) = xs.foldLeft[(Option[ast.Module], List[ast.Import], List[ast.Node])]((None, List.empty, List.empty)) {
      case ((module, imports, statements), stmt) => stmt match
        case mod: ast.Module => (Some(mod), imports, statements)
        case imp: ast.Import => (module, imports :+ imp, statements)
        case _ => (module, imports, statements :+ stmt)
    }

    Source(module, imports, statements)
