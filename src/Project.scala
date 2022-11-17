package challah
package project

import ast.Stmt
import source.{Source, Tree}
import parser.parse
import err.Err

import scala.io.Source.fromFile
import scala.collection.mutable.{ArrayBuffer, Map => MMap, Set => MSet}


val sourceContentIds = LazyList.from(1).sliding(1)

class Project():
  private val sources: ArrayBuffer[Source] = ArrayBuffer.empty
  private val sourceContentMap = MMap.empty[Int, String]
  private val sourceAstMap = MMap.empty[Int, Tree]
  private val loadedFiles = MSet.empty[String]

  def withModule(name: String) =
    withFile(s"examples/${name}.ch")

  def withFile(path: String) =
    if ! loadedFiles.contains(path) then
      loadedFiles.addOne(path)
      withSource(path, fromFile(path).getLines.toList.mkString("\n"))

    this

  def withSource(name: String, content: String) =
    val id = sourceContentIds.next.head
    val source = Source(name, id)
    sources += source
    sourceContentMap.addOne((id, content))
    load(source)
    this

  def getContent(source: Source) =
    sourceContentMap(source.id)

  def load(source: Source): Option[Err] =
    parser.parse(source, this) match
      case Left(err) =>
        Some(err)
      case Right(stmts) =>
        val tree = Tree.load(stmts)
        sourceAstMap.addOne(source.id, tree)
        tree.imports.foreach { imp => withModule(imp.name.lexeme) }
        None

  override def toString(): String =
    sourceAstMap.mkString("\n\n\n")
