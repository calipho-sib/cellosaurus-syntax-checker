package org.nextprot.parser.cellosaurus

import scala.io.Source
import scala.collection.mutable.Map

object DbXrefInfo {

  var mymap = Map[String, (String, String)]()

  def load(xref_file: String) : Unit = {

    var xdb = ""
    var xcat = ""
    var xurl = ""
    var xserver = ""
    for (line <- Source.fromFile(xref_file).getLines()) {
      if (line.startsWith("Abbrev")) xdb = line.substring(8).trim()
      else if (line.startsWith("Server")) xserver = line.substring(8).trim()
      else if (line.startsWith("Db_URL")) xurl = line.substring(8).trim()
      else if (line.startsWith("Cat")) xcat = line.substring(8).trim()
      else if (xdb != "" && xurl != "") {
        if (xurl == "None") // Use the server base url instead
          xurl = xserver
        mymap(xdb) = (xurl, xcat)
      }
    }
  }

  def getUrl(db: String) : String = { mymap(db)._1 }

  def getCat(db: String) : String = { mymap(db)._2 }

  def contains(db: String) : Boolean = { mymap.contains(db) }

  def main(args: Array[String]): Unit = {

    val filename = args(0)
    DbXrefInfo.load(filename)
    mymap.keys.foreach(k => { 
      val c = getCat(k)
      val u = getUrl(k)
      println(s"$k : $c - $u") 
    })

  }


}