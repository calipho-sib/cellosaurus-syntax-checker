package org.nextprot.parser.cellosaurus

import scala.io.Source
import scala.collection.mutable.Map

object ResistanceParser {

  def parseLine(rawline: String): Map[String,String] = {

    // remove final '.' if necessary
    var line = rawline
    if (rawline.endsWith(".")) line = rawline.substring(0,rawline.length-1) 

    // if [Note=...] needs to be handled, see TransformantParser

    // split line elements on ";"
    var db: String = null
    var ac: String = null
    var name: String = null
    val elems = line.split("; ")
    if (elems.length >= 3) {
      // first element should be one of ChEBI, DrugBank, NCIt, UniProtKB
      db = elems(0)
      if (db != "NCIt" && db != "UniProtKB" && db != "ChEBI" && db != "DrugBank" && db != "PubChem") {
        throw new Exception("Invalid database " + db + " in " + line)
      }
      // second element is the accession
      ac = elems(1)
      // third elements and next ones are all part of the name
      val name_elems = elems.slice(2,elems.length)
      name = name_elems.mkString("; ")
    } else {
      name = elems(0).trim()
    }

    // build result to be returned
    val result = Map[String,String]()
    result("db") = db
    result("ac") = ac
    result("name") = name
    return result
  }

  def main(args: Array[String]): Unit = {

    val filename = args(0)
    val lines = Source.fromFile(filename).getLines()
    var lineNo = 0
    for (line <- lines) {
      if (line.startsWith("CC   Selected for resistance to: ")) {
        val linevalue = line.substring(33).trim()
        println("----  : " + linevalue)
        lineNo += 1
        try {
          val map = parseLine(linevalue)
          for (k <- map.keys) {
            println(k + " : " + map(k))
          }

        } catch {
          case e: Exception => println(s"ERROR at line $lineNo: ${e.getMessage}")
        }
      }
    }
    println("End")
  }
}