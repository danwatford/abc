package com.foomoo.abc.notation.processing

import com.foomoo.abc.UnitSpec
import com.foomoo.abc.notation.{AbcNotationHeader, AbcNotationHeaderInformationField, AbcNotationHeaderLineComment, AbcNotationTestSupport}

class AbcNotationStringBuilderSpec extends UnitSpec {

  val TEST_HEADER_LINES = List(
    AbcNotationHeaderInformationField("X", "1"),
    AbcNotationHeaderLineComment("Test Comment 1"),
    AbcNotationHeaderInformationField("T", "Test Tune Title"),
    AbcNotationHeaderLineComment("Test Comment 2"),
    AbcNotationHeaderInformationField("+", "Part2"),
    AbcNotationHeaderInformationField("K", "C")
  )

  val TEST_HEADER = AbcNotationHeader(TEST_HEADER_LINES)

  val TEST_HEADER_STRING =
    """X: 1
      |% Test Comment 1
      |T: Test Tune Title
      |% Test Comment 2
      |+: Part2
      |K: C""".stripMargin


  val TEST_BODY_STRING =
    """AB% Inline comment 1
      |[K: D]% Inline comment 2
      ||
      |[M: 1]% Line comment 1
      |C\
      |% Line comment 2
      |D
      |""".stripMargin


  "The AbcNotationStringBuilder" should "construct strings from notation headers" in {
    assertResult(TEST_HEADER_STRING) {
      AbcNotationStringBuilder.headerToString(TEST_HEADER)
    }
  }

  it should "construct strings from notation bodys" in {
    assertResult(TEST_BODY_STRING) {
      AbcNotationStringBuilder.bodyToString(AbcNotationTestSupport.TEST_BODY)
    }
  }

}
