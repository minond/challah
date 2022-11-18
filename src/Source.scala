package challah
package source

import ast.{Module, Import, Stmt}


case class Span(sourceMapping: SourceMapping, offset: Int)
case class SourceMapping(name: String, id: Int)

case class Source(module: Option[Module], imports: List[Import], statements: List[Stmt]):
  override def toString(): String =
    s"""
     |${module.getOrElse("")}
     |
     |${imports.mkString("\n")}
     |
     |${statements.mkString("\n")}
    """.trim.stripMargin('|')

object Source:
  def load(xs: List[Stmt]): Source =
    val (module, imports, statements) = xs.foldLeft[(Option[Module], List[Import], List[Stmt])]((None, List.empty, List.empty)) {
      case ((module, imports, statements), stmt) => stmt match
        case mod: Module => (Some(mod), imports, statements)
        case imp: Import => (module, imports :+ imp, statements)
        case _ => (module, imports, statements :+ stmt)
    }

    Source(module, imports, statements)
