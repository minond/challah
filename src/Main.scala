package challah

import project.Project


def main(args: Array[String]) =
  val project = Project().withFile("examples/Test1.ch")
  println(project)
