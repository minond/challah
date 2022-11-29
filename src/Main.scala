package challah

import project.Project
import ir.linked


def main(args: Array[String]) =
  val project = Project().withFile("examples/test1.ch")
  println(project.entrySource)
  for stmt <- project.entrySource.statements do
    println(linked.transform(stmt))
    // stmt match
    //   case v: ast.Val =>
    //     println(v.id)
    //   case _ =>
