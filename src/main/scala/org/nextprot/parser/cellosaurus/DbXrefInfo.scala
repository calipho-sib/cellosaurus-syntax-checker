package org.nextprot.parser.cellosaurus

import scala.io.Source
import scala.collection.mutable.Map

object DbXrefInfo {

  var mymap = Map[String, (String, String, String)]()

  def load(xref_file: String) : Unit = {

    var xcat = ""
    var xdb = ""
    var xurl = ""
    var xiri = ""
    var xserver = ""
    for (line <- Source.fromFile(xref_file).getLines()) {
      if (line.startsWith("Abbrev")) {
        xdb = line.substring(8).trim()
        xcat = ""
        xurl = ""
        xiri = ""
        xserver = ""
      }
      else if (line.startsWith("Server")) xserver = line.substring(8).trim()
      else if (line.startsWith("Db_URL")) xurl = line.substring(8).trim()
      else if (line.startsWith("Db_IRI")) xiri = line.substring(8).trim()
      else if (line.startsWith("Cat")) xcat = line.substring(8).trim()
      else if (xdb != "" && xurl != "") {
        if (xurl == "None") xurl = xserver  // use the server base url instead of db url 
        mymap(xdb) = (xurl, xcat, xiri)
      }
    }
  }

  def getDbSet() : Set[String] = { mymap.keys.toSet }
 
  def getUrl(db: String) : String = { mymap(db)._1 }

  def getCat(db: String) : String = { mymap(db)._2 }

  def getIri(db: String) : String = { mymap(db)._3 }

  def contains(db: String) : Boolean = { mymap.contains(db) }

  def main(args: Array[String]): Unit = {

    val filename = args(0)
    DbXrefInfo.load(filename)
    mymap.keys.foreach(k => { 
      val c = getCat(k)
      val u = getUrl(k)
      val i = getIri(k)
      println(s"$k : $c - $u - $i") 
    })

  }


}