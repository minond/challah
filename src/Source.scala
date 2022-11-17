package challah
package source

case class Source(name: String, id: Int)
case class Span(source: Source, offset: Int)
