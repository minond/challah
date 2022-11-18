package challah
package project

import ast.Stmt
import source.{SourceMapping, Source}
import parser.parse
import err.Err

import scala.io.Source.fromFile
import scala.collection.mutable.{ArrayBuffer, Map => MMap, Set => MSet}


val sourceMappingContentIds = LazyList.from(1).sliding(1)

class Project():
  private val sourceMappings: ArrayBuffer[SourceMapping] = ArrayBuffer.empty
  private val sourceMappingContentMap = MMap.empty[Int, String]
  private val sourceMappingAstMap = MMap.empty[Int, Source]
  private val loadedFiles = MSet.empty[String]

  def withModule(name: String) =
    withFile(s"examples/${name}.ch")

  def withFile(path: String) =
    if ! loadedFiles.contains(path) then
      loadedFiles.addOne(path)
      withSource(path, fromFile(path).getLines.toList.mkString("\n"))

    this

  def withSource(name: String, content: String) =
    val id = sourceMappingContentIds.next.head
    val sourceMapping = SourceMapping(name, id)
    sourceMappings += sourceMapping
    sourceMappingContentMap.addOne((id, content))
    load(sourceMapping)
    this

  def getContent(sourceMapping: SourceMapping) =
    sourceMappingContentMap(sourceMapping.id)

  def load(sourceMapping: SourceMapping): Option[Err] =
    parser.parse(sourceMapping, this) match
      case Left(err) =>
        Some(err)
      case Right(stmts) =>
        val tree = Source.load(stmts)
        sourceMappingAstMap.addOne(sourceMapping.id, tree)
        tree.imports.foreach { imp => withModule(imp.name.lexeme) }
        None

  override def toString(): String =
    sourceMappingAstMap.mkString("\n\n\n")
