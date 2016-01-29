package com.foomoo.abc.parsing

import com.foomoo.abc.{AbcTune, AbcTuneBuilder}

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.CharSequenceReader

trait AbcTuneParser extends RegexParsers {

  case class AbcHeaderItem(headerChar: Char, headerValue: String)

  def nonLinebreakString: Parser[String] = """[^\r\n]*""".r

  def keyItem: Parser[AbcHeaderItem] = "K:" ~> nonLinebreakString ^^ { case headerValue => AbcHeaderItem('K', headerValue) }

  def headerItem: Parser[AbcHeaderItem] =
    """^[^K]""".r ~ (":" ~> nonLinebreakString) ^^ { case headerKey ~ headerValue => AbcHeaderItem(headerKey.charAt(0), headerValue) }

  def header: Parser[List[AbcHeaderItem]] = rep(headerItem) ~ keyItem ^^ { case headerItemList ~ keyHeaderItem => keyHeaderItem :: headerItemList }

  def notes: Parser[String] = nonLinebreakString

  def tune: Parser[AbcTune] = header ~ notes ^^ {
    case headerItemList ~ notes => {
      val builder = new AbcTuneBuilder

      headerItemList.foreach {
        case AbcHeaderItem('X', reference) => builder.setReference(reference)
        case AbcHeaderItem('T', title) => builder.setTitle(title)
        case AbcHeaderItem('M', meter) => builder.setMeter(meter)
        case AbcHeaderItem('C', composer) => builder.setComposer(composer)
        case AbcHeaderItem('K', key) => builder.setKey(key)
        case _ =>
      }

      builder.build()
    }
  }

}

object AbcTuneParser extends AbcTuneParser {

  def parseAbcTune(input: CharSequenceReader): AbcTune = {
    tune(input) match {
      case Success(t, _) => t
      case NoSuccess(msg, next) => throw new IllegalArgumentException(s"Could not parse '$input' near '${next.pos.longString}': $msg")
    }
  }

  def parseAbcTune(s: CharSequence): AbcTune = parseAbcTune(new CharSequenceReader(s))

}
