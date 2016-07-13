package com.foomoo.abc.app

import java.nio.file.{Path, Paths}

import com.foomoo.abc.notation._
import com.foomoo.abc.notation.parsing.AbcNotationParser
import com.foomoo.abc.service.SubsequenceMatchService
import com.foomoo.abc.service.SubsequenceMatchService.NoteSequence
import com.foomoo.abc.tune.conversion.AbcNotationConverter
import com.foomoo.abc.tune.{AbcNote, AbcTune}

import scala.io.Source
import scala.util.parsing.input.CharSequenceReader

/**
  * An example application to parse a collection of ABC tunes and search for matching note sequences between them.
  */
object TuneMatcher extends App {

  // Arguments are paths to tune files.
  println("Args: " + args.toSeq)
  private val paths: Seq[Path] = args.map(Paths.get(_))
  println("Paths: " + paths)

  private val tuneFileStrings: Seq[String] = paths.map(p => Source.fromFile(p.toFile, "UTF-8").mkString)

  // Convert each string to tunes.
  println(s"Reading ${tuneFileStrings.length} tune files")
  val tuneFiles: Seq[AbcNotationFile] = tuneFileStrings.map { tuneFileString =>
    val input = new CharSequenceReader(tuneFileString)
    AbcNotationParser.file(input) match {
      case AbcNotationParser.Success(ts, _) => ts
      case AbcNotationParser.NoSuccess(msg, next) => throw new IllegalArgumentException(
        msg + "\nNext is: " + next.pos)
    }
  }

  val tuneNotations: Seq[AbcNotationTune] = tuneFiles.flatMap(tuneFile => tuneFile.tunes)

  // Convert notation to tunes and then deduplicate according to the tunes' titles.
  private val tunesWithDuplicates: Seq[AbcTune] = tuneNotations.map(AbcNotationConverter.convertTune)
  private val tunes: Seq[AbcTune] = tunesWithDuplicates.groupBy(tuneToTitle).map(_._2.head).toSeq

  println(s"Read ${tunes.length} tunes")

  private val subsequenceTunesMap: Map[NoteSequence, Set[AbcTune]] = SubsequenceMatchService.getSubSequenceTunes(16, tunes)

  // Filter out any sequences with only one matching tune.
  private val filteredSubsequenceTunesMap: Map[NoteSequence, Set[AbcTune]] = subsequenceTunesMap.filter(_._2.size > 1)

  println(s"Have ${filteredSubsequenceTunesMap.size} filtered sub-sequences")

  filteredSubsequenceTunesMap.foreach {case (subSequence, tuneSet) =>

      val tuneTitles: Set[String] = tuneSet.map(tuneToTitle)

      val tuneTitlesFormatted = tuneTitles.mkString("--- ", "\n--- ", "")

      println(subSequence.map { case AbcNote(note) => note })
      println("appears in")
      println(tuneTitlesFormatted)
  }

  def tuneToTitle(tune: AbcTune): String = tune.titles.head

}
