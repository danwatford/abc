package com.foomoo.abc.notation.serializing

import com.foomoo.abc.UnitSpec
import com.foomoo.abc.notation.AbcNotationTestSupport._

class AbcNotationSerializerSpec extends UnitSpec {

  "The AbcNotationSerializer" should "output json" in {

    val json: String = AbcNotationSerializer.write(TEST_TUNE)

    json should include (""""key":"M","value":"1"""")
  }

}
