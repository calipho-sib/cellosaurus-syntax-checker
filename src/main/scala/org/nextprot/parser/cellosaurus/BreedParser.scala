package org.nextprot.parser.cellosaurus

import scala.io.Source
import scala.collection.mutable.Map
import scala.xml._


class Breed(val name: String, val vbo: String) {

  var xrefs = List[DbXref]()
  init

  def init = {
    if (vbo != null && vbo.length > 0) {
      // use split on char rather on string (would be double escaped "\\+")
      val acs = vbo.split('+') 
      val labels = name.split(" x ") 
      var idx = 0
      acs.foreach(ac => {
        val pos = labels(idx).indexOf("(")
        val label = 
          if (pos == -1) labels(idx) else labels(idx).substring(0,pos).trim()
        val xref = new DbXref("VBO", ac, label = label)
        xrefs = xref :: xrefs
        idx += 1
      })
      xrefs = xrefs.reverse
    }
  }

  def toXML =
    <breed>
      {name}
      {
      if (xrefs.size > 0) 
        <xref-list>
        {xrefs.map(_.toXML)}
        </xref-list>
      else
        Null
      }
    </breed>
}

object BreedParser {

  def parseLine(rawline: String): Breed = {

    // remove final '.' if necessary
    var line = rawline
    if (rawline.endsWith(".")) line = rawline.substring(0,rawline.length-1) 

    // split line elements on ";"
    val elems = line.split("; ")
    val name: String = elems(0).trim()
    var vbo : String = null
    if (elems.size ==2) {
      vbo = elems(1)
      if (!vbo.startsWith("VBO=")) throw new Exception("Invalid breed xref database")
    } else if (elems.size>2) {
      throw new Exception("Too many fields separated with ';'")
    }
    return new Breed(name, vbo)
  }

  def main(args: Array[String]): Unit = {

    DbXrefInfo.load(args(0)) // read cellosaurus_xrefs.txt
    val filename = args(1)   // read input file

    val lines = Source.fromFile(filename).getLines()
    var lineNo = 0
    for (line <- lines) {
      if (line.startsWith("CC   Breed/subspecies: ")) {
        val linevalue = line.substring(23).trim()
        println("----  : " + linevalue)
        lineNo += 1
        try {
          val breed = parseLine(linevalue)
          println(breed.toXML)
        } catch {
          case e: Exception => println(s"ERROR at line $lineNo: ${e.getMessage}")
        }
      }
    }
    println("End")
  }
}