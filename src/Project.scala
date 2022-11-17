package challah
package project

import ast.Stmt
import source.Source
import parser.parse
import err.Err

import scala.io.Source.fromFile
import scala.collection.mutable.{ArrayBuffer, Map => MMap}


val sourceContentIds = LazyList.from(1).sliding(1)

class Project():
  private val sources: ArrayBuffer[Source] = ArrayBuffer.empty
  private val sourceContentMap: MMap[Int, String] = MMap.empty
  private val sourceAstMap: MMap[Int, List[Stmt]] = MMap.empty

  def withFile(path: String) =
    val content = fromFile(path).getLines.toList.mkString("\n")
    withSource(path, content)
    this

  def withSource(name: String, content: String) =
    val id = sourceContentIds.next.head
    sources += Source(name, id)
    sourceContentMap.addOne((id, content))
    this

  def getContent(source: Source) =
    sourceContentMap(source.id)

  def load: Option[Err] =
    load(sources.head)

  def load(source: Source): Option[Err] =
    parser.parse(source, this) match
      case Left(err) =>
        Some(err)
      case Right(stmts) =>
        sourceAstMap.addOne(source.id, stmts)
        None
