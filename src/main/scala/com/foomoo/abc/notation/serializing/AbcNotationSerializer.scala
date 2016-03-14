package com.foomoo.abc.notation.serializing

import com.foomoo.abc.notation.{AbcNotationBodyElement, AbcNotationHeaderLine, AbcTuneNotation}

object AbcNotationSerializer {
  import org.json4s._
  import org.json4s.native.Serialization
  import org.json4s.native.Serialization.{read => serializationRead, write => serializationWrite, writePretty => serializationWritePretty}

  implicit val AbcNotationFormats = Serialization.formats(FullTypeHints(List(classOf[AbcNotationHeaderLine], classOf[AbcNotationBodyElement])))

  def writePretty(tuneNotation: AbcTuneNotation): String = serializationWritePretty(tuneNotation)

  def write(tuneNotation: AbcTuneNotation): String = serializationWrite(tuneNotation)

  def read(serializedTune: String): AbcTuneNotation = serializationRead(serializedTune)

}
