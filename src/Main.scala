package challah

import source._


def main(args: Array[String]) =
  val project = Project().withSource("test", """
    val x = 123
    val y = -1777
    val z = 4 - 53
  """)

  print(parser.tokenize(project.sources.head, project))
