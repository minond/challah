package challah

import source._


def main(args: Array[String]) =
  val project = Project()
    .withSource("Test", """
      module Test (x, y, z)

      val x = 123
      val y = -1777.23
      val z = 4 - 53

      val a = "hi hi 3 2 1"
      val b = x + y
      val c = -b
      val d = ----c
      val e = x + y + z - 1
      val f = "1" + "2" + "3" + "4"

    """)

  parser.parse(project.sources.head, project) match
    case Left(err) =>
      println(err)
    case Right(exprs) =>
      exprs.foreach { expr => println(expr) }
