package challah

import project.Project
import utils.squished
import ir.linked


def main(args: Array[String]) =
  val project = Project().withFile("examples/test1.ch")
  val main = project.findModule("test1").getOrElse(???)
  val ir = main.ir.squished.getOrElse(???)
  for node <- ir do
    println(node)
    // node match
    //   case v: ast.Val =>
    //     println(v.id)
    //   case _ =>
