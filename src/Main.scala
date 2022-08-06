package challah

import source._


def main(args: Array[String]) =
  val project = Project().withSource("test", """
    val x = 123
    val y = -1777.23
    val z = 4 - 53
  """)

  for
    exprs <- parser.parse(project.sources.head, project)
    expr  <- exprs
  do
    println(expr)
