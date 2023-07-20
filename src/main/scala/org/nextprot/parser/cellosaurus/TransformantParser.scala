package org.nextprot.parser.cellosaurus

import scala.io.Source
import scala.collection.mutable.Map

object TransformantParser {

  def parseLine(rawline: String): Map[String,String] = {

    // remove final '.' if necessary
    var line = rawline
    if (rawline.endsWith(".")) line = rawline.substring(0,rawline.length-1) 

    // extract optional final note [Note=...]
    var note: String = null
    val idx = line.indexOf("[Note=")
    if (idx != -1) {
      note = line.substring(idx+6)
      if (note.endsWith("]")) {
        note = note.substring(0, note.length-1)
        line = line.substring(0, idx).trim()
      } else {
        throw new Exception("Note not terminated with ] in " + line)
      }
    }

    // split line elements on ";"
    var db: String = null
    var ac: String = null
    var name: String = null
    val elems = line.split("; ")
    if (elems.length >= 3) {
      // first element should be one of NCIt, NCBI_TaxID, ChEBI
      db = elems(0)
      if (db != "NCIt" && db != "NCBI_TaxID" && db != "ChEBI" && db != "DrugBank") {
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
    result("note") = note
    return result
  }

  def main(args: Array[String]): Unit = {

    val filename = args(0)
    val lines = Source.fromFile(filename).getLines()
    var lineNo = 0
    for (line <- lines) {
      if (line.startsWith("CC   Transformant: ")) {
        val linevalue = line.substring(19).trim()
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