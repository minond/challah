package challah
package test
package matchers

import test.utils.parse

import org.scalatest._
import matchers._


def parseAs(expected: String) =
  new Matcher[String]:
    def apply(source: String) =
      val result = parse(source) 
      MatchResult(
        result == expected,
        s"Source <<< $source >>> does not match expected ast of <<< $expected >>>, got <<< $result >>> instead",
        s"Source <<< $source >>> matches expected ast of <<< $expected >>>",
      )
