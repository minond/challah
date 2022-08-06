package challah
package test

import source._
import matchers.parseAs

import org.scalatest._
import flatspec._
import matchers._
import should._


class ParsingTests extends AnyFlatSpec with Matchers:
  it should "parse valid expressions" in {
    "val x = 123" should parseAs ("(val x 123)")
    "val y = -777" should parseAs ("(val y -777)")
    "val z = 3 + 4" should parseAs ("(val z (+ 3 4))")
  }
