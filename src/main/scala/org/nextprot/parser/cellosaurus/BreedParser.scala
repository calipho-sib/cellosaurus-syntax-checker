package org.nextprot.parser.cellosaurus

import scala.io.Source
import scala.collection.mutable.Map
import scala.xml._


class Breed(val name: String, val xref_str: String) {

  var xref_list = List[DbXref]()
  init

  def init = {
    if (xref_str != null && xref_str.length > 0) {
      // split db and acs
      val xref_split = xref_str.split("=")
      val db : String = xref_split(0)
      val acs_str : String = xref_split(1)
      // use split on char rather on string (would be double escaped "\\+")
      val acs = acs_str.split('+')
      val labels = name.split(" x ") 
      var idx = 0
      acs.foreach(ac => {
        val pos = labels(idx).indexOf("(")
        val label = 
          if (pos == -1) labels(idx) else labels(idx).substring(0,pos).trim()
        val xref = new DbXref(db, ac, label = label)
        xref_list = xref :: xref_list
        idx += 1
      })
      xref_list = xref_list.reverse
    }
  }

  def toXML =
    <breed>
      {name}
      {
      if (xref_list.size > 0) 
        <xref-list>
        {xref_list.map(_.toXML)}
        </xref-list>
      else
        Null
      }
    </breed>
}

object BreedParser {

  def parseLine(rawline: String): Breed = {

    // Data examples: 
    // Eker x Long-Evans; RS=RS:0000385+RS:0000347.
    // Hereford x Limousin; VBO=VBO_0000232+VBO_0000274.
    // Oregon-R; FlyBase_Strain=FBsn0000276.

    val validDbSet = Set("VBO", "RS", "FlyBase_Strain")

    // remove final '.' if necessary
    var line = rawline
    if (rawline.endsWith(".")) line = rawline.substring(0,rawline.length-1) 

    // split line elements on ";" 
    val elems = line.split("; ")
    val name: String = elems(0).trim()
    var xref_str : String = null
    if (elems.size ==2) {
      xref_str = elems(1)
      val xref_split = xref_str.split("=")
      val db = xref_split(0)
      if ( ! validDbSet.contains(db)) throw new Exception("Invalid breed xref database")
    } else if (elems.size>2) {
      throw new Exception("Too many fields separated with ';'")
    }
    return new Breed(name, xref_str)
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