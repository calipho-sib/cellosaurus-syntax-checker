package org.nextprot.parser.cellosaurus

import scala.io.Source
import scala.collection.mutable.Map

object SiteMapping {

  // separator can be overriden by adding a line like:
  // separator=<char> in the file read by load method.
  var separator : String = ";"
  var filename: String = null
  val ac2label = Map[String, String]()

  def load(name: String) = {
    filename = name
    println("Loading " + filename)
    println("Default field separator is <" + separator + ">")
    val lines = Source.fromFile(filename).getLines()
    var lineNo = 0
    var prefix: String = null
    for (line <- lines) {
      lineNo += 1
      if (! line.isEmpty() && ! line.startsWith("--")) {
        if (line.startsWith("1) Cell types")) {
          prefix = "CL_"
        } else if (line.startsWith("2) Organs")) { 
          prefix = "UBERON_"
        } else if (line.startsWith("separator=")) {
          separator = line.substring(10)
          println("Field separator reset to <" + separator + ">")
        } else {
          val pos = line.indexOf(prefix)
          if (pos == -1) {
            println("WARNING: No " + prefix + " at line " + lineNo)
          } else {
            val data = line.substring(pos)
            val items = data.split('+')
            for (item <- items) {
              val aclabel = item.split(separator)
              if (aclabel.length!=2) throw new Exception("Invalid <ac>" + separator + "<label> pair at line " + lineNo)
              val ac = aclabel(0)
              val label = aclabel(1)
              ac2label(ac) = label
            }        
          }
        }
      }
    }
    println("Loaded " + ac2label.size + " ac / label pairs")
  }

  def getLabel(ac: String) : String = {
    try {
      val label = ac2label(ac)
      return label
    } catch {
      case e : Exception => println("ERROR, found no label for " + ac + " in " + filename)
      return "-"
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