package com.foomoo.abc.notation.serializing

import com.foomoo.abc.notation.{AbcNotationBodyElement, AbcNotationHeaderLine, AbcNotationTune}

import scala.reflect.Manifest

object AbcNotationSerializer {
  import org.json4s._
  import org.json4s.native.Serialization
  import org.json4s.native.Serialization.{read => serializationRead, write => serializationWrite, writePretty => serializationWritePretty}

  implicit val AbcNotationFormats = Serialization.formats(FullTypeHints(List(classOf[AbcNotationHeaderLine], classOf[AbcNotationBodyElement])))

  def writePretty(tuneNotation: AbcNotationTune): String = serializationWritePretty(tuneNotation)

  def write(tuneNotation: AbcNotationTune): String = serializationWrite(tuneNotation)

  def read(serializedTune: String)(implicit mf: Manifest[AbcNotationTune]): AbcNotationTune = serializationRead(serializedTune)

}
