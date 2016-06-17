package com.foomoo.abc.tune.serializing

import com.foomoo.abc.UnitSpec
import com.foomoo.abc.tune.AbcTuneTestSupport

/**
  */
class AbcTuneSerializerSpec extends UnitSpec {

  "The AbcTuneSerializer" should "serializer and deserialize to the same object" in {

    val testTune = AbcTuneTestSupport.getTestTune()

    val json: String = AbcTuneSerializer.writePretty(testTune)

    println(json)

    val readTune = AbcTuneSerializer.read(json)

    readTune should equal(testTune)
  }

}
