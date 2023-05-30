package org.nextprot.parser.cellosaurus

import scala.io.Source
import scala.collection.mutable.Map

object DerivedFromSiteParser {

  def parseLine(rawline: String): Map[String,String] = {

    // remove final '.' if necessary
    var line = rawline
    if (rawline.endsWith(".")) line = rawline.substring(0,rawline.length-1) 
    
    // split elements over ';' and check we have 2, 3 or 4 elements
    val elems = line.split("; ")
    if (elems.length< 2 || elems.length>4) {
      throw new Exception("Invalid number of elements: " + elems.length)
    }

    // extract first element; site
    val site : String = elems(0)
    if  ( ! ((site=="In situ") || (site=="Metastatic"))) {
      throw new Exception("Invalid site: " + site )
    }

    // extract second element : name
    val name : String = elems(1)
    if (name.startsWith("Note=") || name.startsWith("UBERON=")) {
      throw new Exception("Missing anatomy term name: " + name)
    }

    // now extract oprional fields
    var note: String = null
    var uber: String = null

    // if we have 3 elements, the 3rd  must be either a note element or a uberon element
    if (elems.length>2) {
      if (elems(2).startsWith("UBERON=")) {
        uber = elems(2).substring(7)
      } else if (elems(2).startsWith("Note=")) {
        note = elems(2).substring(5)
      } else {
        throw new Exception("Unexpected element : " + elems(2))
      }
    }

    // if we have 4 elements, the 4th must be a uberon element    
    if (elems.length==4) {
      if (elems(3).startsWith("UBERON=")) {
        uber = elems(3).substring(7)
      } else {
        throw new Exception("Unexpected element : " + elems(3))
      }
    }

    // build result to be returned
    val result = Map[String,String]()
    result("site") = site
    result("name") = name
    result("note") = note
    result("uber") = uber
    return result
  }

  def main(args: Array[String]): Unit = {

    val filename = args(0)
    val lines = Source.fromFile(filename).getLines()
    var lineNo = 0
    for (line <- lines) {
      if (line.startsWith("CC   Derived from site: ")) {
        val linevalue = line.substring(24).trim()
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