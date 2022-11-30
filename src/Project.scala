package challah
package project

import ast.Stmt
import source.{SourceMapping, SourceId}
import module.Module
import parser.parse
import err.Err

import scala.io.Source.fromFile
import scala.collection.mutable.{ArrayBuffer, Map => MMap, Set => MSet}


class Project():
  private val sourceMappings: ArrayBuffer[SourceMapping] = ArrayBuffer.empty
  private val sourceMappingContentMap = MMap.empty[SourceId, String]
  private val modules = MSet.empty[Module]
  private val loadedFiles = MSet.empty[String]

  def withModule(name: String) =
    withFile(s"examples/${name}.ch")

  def withFile(path: String) =
    if ! loadedFiles.contains(path) then
      loadedFiles.addOne(path)
      withSource(path, fromFile(path).getLines.toList.mkString("\n"))

    this

  def withSource(name: String, content: String) =
    val sourceMapping = SourceMapping(name)
    sourceMappings += sourceMapping
    sourceMappingContentMap.addOne((sourceMapping.id, content))
    load(sourceMapping)
    this

  def getContent(sourceMapping: SourceMapping) =
    sourceMappingContentMap(sourceMapping.id)

  def load(sourceMapping: SourceMapping): Option[Err] =
    parser.parse(sourceMapping, this) match
      case Left(err) =>
        Some(err)
      case Right(stmts) =>
        val mod = Module.fromSource(stmts)
        modules.addOne(mod)
        mod.source.imports.foreach { imp => withModule(imp.name.lexeme) }
        None

  def findModule(name: String): Option[Module] =
    modules.find { case mod => mod.name == name }
