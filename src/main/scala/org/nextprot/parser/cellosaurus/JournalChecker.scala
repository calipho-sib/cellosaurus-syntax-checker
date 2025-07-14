package org.nextprot.parser.cellosaurus

import scala.io.Source
import scala.collection.mutable.Map

class JournalVolume(val journal: String, val volume: String) {

    override def toString() : String = {
        s"JournalVolume('$journal', '$volume')"
    }

}

object JournalChecker {

    var journal_set: Set[String] = Set.empty

    def init(filename: String) : Unit = {
        println(s"INFO, loading ${filename}")
        val lines = Source.fromFile(filename).getLines()
        var lineNo = 0
        for (line <- lines) {
            lineNo += 1
            val elems = line.strip().split("\\|")
            if (line.trim()=="") {
                // do nothing
            } else if (elems.size != 2) {
                println(s"ERROR, unexpected number of fields, skipping line ${lineNo}: ${line}")
            } else {
                val journal = elems(0).strip()
                journal_set += journal
            }
        }
        println(s"INFO, loaded ${journal_set.size} journals from ${filename}")
    }

    def extract_journal_volume(data: String): JournalVolume = {
        var words = data.strip().split(" ")
        while (words.nonEmpty) {
            val journal = words.mkString(" ")
            if (journal_set.contains(journal)) {
                val volume = data.substring(journal.size + 1)
                return new JournalVolume(journal, volume)
            }
            words = words.dropRight(1)
        }
        return null
    }


    def main(args: Array[String]): Unit = {
  
        val datadir = "/home/pmichel/work/cellosaurus-api/data_in/"
        JournalChecker.init(datadir + "cellosaurus_journals.cv")
        println(JournalChecker.extract_journal_volume("ACS Omega 36"))
        println(JournalChecker.extract_journal_volume("ACS Omega Suppl. 36"))           // Suppl. is not part of the journal name
        println(JournalChecker.extract_journal_volume("Acta Histochem. Suppl. 33"))     // Suppl. is part of the journal name
        println(JournalChecker.extract_journal_volume("Schtrumpf 36"))                  // => null because no journal 'Schtrumpf' or 'Schtrumpf 36' exist
        println("end")




    }

}