package challah

import project.Project
import utils.squished
import ir.linked


def main(args: Array[String]) =
  val project = Project().withFile("examples/test1.ch")

  val main = project.findModule("test1").getOrElse(???)
  println("source =============")
  println(main)
  println("source =============")

  println("ir =================")
  val ir = main.ir.squished.getOrElse(???)
  for node <- ir do
    println(node)
  println("ir =================")
