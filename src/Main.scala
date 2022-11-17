package challah

import source._


def main(args: Array[String]) =
  val project = Project().withFile("examples/Test1.ch")

  project.load match
    case None =>
    case Some(err) => println(s"error: ${err}")
