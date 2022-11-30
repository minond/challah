package challah
package module

import utils.ids
import ir.linked
import source.Source

class Module(val source: Source):
  override def toString(): String =
    s"""
     |${source.module.getOrElse("")}
     |
     |${source.imports.mkString("\n")}
     |
     |${source.statements.mkString("\n")}
    """.trim.stripMargin('|')

  lazy val name = source.module
    .map({ node => node.name.lexeme })
    .getOrElse({ s"module#${ids.next.head}" })

  lazy val ir = for stmt <- source.statements
    yield linked.transform(stmt)

object Module:
  def fromSource(nodes: List[ast.Node]) =
    Module(Source.load(nodes))
