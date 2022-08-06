package challah
package source


val sourceContentIds = LazyList.from(1).sliding(1)

case class Project(val sources: List[Source] = List.empty, sourceContentMap: Map[Int, String] = Map.empty):
  def withSource(name: String, content: String) =
    val id = sourceContentIds.next.head
    Project(sources :+ Source(name, id),
      sourceContentMap + (id -> content))

  def getContent(source: Source) =
    sourceContentMap(source.contentId)


case class Source(name: String, contentId: Int)
case class Span(source: Source, offset: Int)
