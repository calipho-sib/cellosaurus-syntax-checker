package org.nextprot.parser.cellosaurus

import scala.io.Source
import scala.collection.mutable.Map

object SiteMapping {

  var filename: String = null
  val ac2label = Map[String, String]()

  def load(name: String) = {
    filename = name
    println("Loading " + filename)
    val lines = Source.fromFile(filename).getLines()
    var lineNo = 0
    var prefix: String = null
    for (line <- lines) {
      lineNo += 1
      if (line.startsWith("CL_") || line.startsWith("UBERON_")) {
        val idx = line.indexOf(" ") // first <space> separates accession from label
        val ac = line.substring(0,idx)
        val label = line.substring(idx+1)
        //println("<" + ac + ">=<" + label + ">")
        ac2label(ac) = label
      }
    }
    println("Loaded " + ac2label.size + " ac / label pairs")
  }

  def getLabel(ac: String) : String = {
    if (ac2label.size==0) return "--Term not loaded--"
    try {
      val label = ac2label(ac)
      return label
    } catch {
      case e : Exception => println("ERROR, Missing " + ac + " term in " + filename)
      return "--Missing term--"
    }
  }

// for test purpose
  def main(args: Array[String]): Unit = {

    val filename = args(0)
    load(filename)
    println("Loaded")
    var myacs = List("xx", "CL_0002603", "UBERON_0001708", "UBERON_0000955", "UBERON_0008339")
    for (ac <- myacs) {
      println(ac + " : " + getLabel(ac))
    }
    println("End")

  }
}