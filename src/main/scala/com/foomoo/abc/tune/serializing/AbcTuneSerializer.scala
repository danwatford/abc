package com.foomoo.abc.tune.serializing

import com.foomoo.abc.tune._
import org.json4s._

import scala.reflect.Manifest

class AbcTuneCustomSerializer extends CustomSerializer[AbcTune](format => ( {
  case JObject(fields) => {
    val builder = new AbcTuneBuilder()

    fields.foreach {
      case JField("reference", JString(reference)) => builder.setReference(reference)
      case JField("meter", JString(meter)) => builder.setMeter(meter)
      case JField("key", JString(key)) => builder.setKey(key)
      case JField("composer", JString(composer)) => builder.setComposer(composer)
      case JField("titles", JArray(titlesJsonValues)) => titlesJsonValues.foreach {
        case JString(title) => builder.addTitle(title)
        case _ => ()
      }
      case JField("bodyElements", bodyElementsJsonValues) =>
        implicit val AbcBodyElementFormats = DefaultFormats.withHints(FullTypeHints(List(classOf[AbcStructuralElement], classOf[AbcNoteElement])))

        val bodyElements = bodyElementsJsonValues.extract[List[AbcStructuralElement]]
        builder.setBodyElements(bodyElements)

    }

    builder.build()
  }
},
  PartialFunction.empty
))


object AbcTuneSerializer {

  import org.json4s._
  import org.json4s.native.Serialization.{read => serializationRead, write => serializationWrite, writePretty => serializationWritePretty}

  implicit val AbcTuneFormats = DefaultFormats.withHints(FullTypeHints(List(classOf[AbcStructuralElement], classOf[AbcNoteElement]))) +
    new AbcTuneCustomSerializer() + FieldSerializer[AbcTune]()

  def writePretty(tune: AbcTune): String = serializationWritePretty(tune)

  def write(tune: AbcTune): String = serializationWrite(tune)

  def read(serializedTune: String)(implicit mf: Manifest[AbcTune]): AbcTune = serializationRead(serializedTune)

}

