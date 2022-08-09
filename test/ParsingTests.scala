package challah
package test

import source._
import matchers.parseAs

import org.scalatest._
import flatspec._
import matchers._
import should._


class ParsingTests extends AnyFlatSpec with Matchers:
  it should "parse valid val statements" in {
    val statements = Map(
      """module Test""" -> """(module Test ())""",
      """module Test ()""" -> """(module Test ())""",
      """module Test (a)""" -> """(module Test (a))""",
      """module Test (a, b, c)""" -> """(module Test (a b c))""",
      """module Test (a,b,c)""" -> """(module Test (a b c))""",
      """module Test (abc)""" -> """(module Test (abc))""",
      """val num1 = 123""" -> """(val num1 123)""",
      """val num2 = -777""" -> """(val num2 -777)""",
      """val num3 = 3 + 4""" -> """(val num3 (+ 3 4))""",
      """val str1 = """"" -> """(val str1 "")""",
      """val str2 = "1"""" -> """(val str2 "1")""",
      """val str3 = "1 2"""" -> """(val str3 "1 2")""",
      """val str4 = "abc abc abc"""" -> """(val str4 "abc abc abc")""",
    )

    statements.foreach { (code, result) =>
      code should parseAs (result)
    }
  }
