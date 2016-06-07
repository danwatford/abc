package com.foomoo.abc.notation.serializing

import com.foomoo.abc.UnitSpec
import com.foomoo.abc.notation.AbcNotationTestSupport
import com.foomoo.abc.notation.AbcNotationTestSupport._

class AbcNotationSerializerSpec extends UnitSpec {

  "The AbcNotationSerializer" should "output json" in {

    val json: String = AbcNotationSerializer.write(TEST_TUNE)

    json should include(""""key":"M","value":"1"""")
  }

  it should "serialize and deserialize to the same object" in {

    val testTuneNotation = AbcNotationTestSupport.TEST_TUNE

    val json: String = AbcNotationSerializer.writePretty(testTuneNotation)

    println(json)

    val readTuneNotation = AbcNotationSerializer.read(json)

    readTuneNotation should equal(testTuneNotation)

  }
}
