package org.nextprot.parser.cellosaurus

import scala.io.Source
import scala.xml._

class Knockout(method: String, xref: DbXref, note: String) {
        
    override def toString() : String = {
        s"Knockout(method=$method, xref:$xref, note:${if (note==null) "(null)" else note})"
    }

    def toXML = 
        <knockout-cell>
            <knockout-method>{method}</knockout-method>
            {xref.toXML}
            {
            if (note != null) <knockout-cell-note>{note}</knockout-cell-note> else Null
            }
        </knockout-cell>

}

object KnockoutParser {

    def parseLine(rawtext: String) : Knockout = {
        val text = if (rawtext.endsWith(".")) rawtext.substring(0,rawtext.length-1) else rawtext
        val toklist = text.split("; ")
        val method = toklist(0).split("=")(1)
        val db = toklist(1)
        val ac = toklist(2)
        val dbac = db + "=" + ac
        if (! SourceChecker.isInDbSet(dbac, Set("HGNC", "MGI", "UniProtKB", "FlyBase_Gene", "VGNC", "RGD")) ) 
          throw new Exception(s"found invalid xref in Knockout cell comment '${rawtext}'")
        val geneAndNote = toklist(3).split(" \\(Note=")
        val geneName = geneAndNote(0)
        val note = if (geneAndNote.length>1) geneAndNote(1).split("\\)")(0) else null
        val xref = new DbXref(db, ac, geneName, "")
        return new Knockout(method, xref, note)
    }


    def main(args: Array[String]): Unit = {

        // sbt "run ../cellosaurus-api/data_in/cellosaurus_xrefs.txt knockout.txt"

        DbXrefInfo.load(args(0)) // load allowed db name with cat & url properties
        SourceChecker.init(DbXrefInfo.getDbSet())

        val filename = args(1)
        var lineNo = 0
        for (line <- Source.fromFile(filename).getLines()) {
            val topic = "CC   Knockout cell: "
            if (line.startsWith(topic)) {
                val linevalue = line.substring(topic.length).trim()
                lineNo += 1
                try {
                    println("--------------")
                    println(line)
                    val ko = parseLine(linevalue)
                    println(ko)
                    println(ko.toXML)
               } catch {
//                 case e: Exception => {throw e}
                   case e: Exception => {println(s"ERROR at line ${lineNo}")}
               }
            }
        }
    }

}