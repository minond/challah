package challah
package err

import ast.Token
import source.{Span, SourceMapping}


sealed trait Err

sealed trait SyntaxErr extends Err
case class UnknownCharErr(char: Char, span: Span) extends SyntaxErr
case class UnexpectedToken[Expected](token: Token) extends SyntaxErr
case class UnexpectedEof[Expected](sourceMapping: SourceMapping) extends SyntaxErr
