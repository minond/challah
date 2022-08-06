package challah
package test
package utils

import source.Project


def parse(content: String) =
  val project = Project().withSource("test", content)
  val tree = parser.parse(project.sources.head, project)
  tree.getOrElse({
    println("Error: unhandled parsing error in ParsingMatcher")
    ???
  }).head.toString
