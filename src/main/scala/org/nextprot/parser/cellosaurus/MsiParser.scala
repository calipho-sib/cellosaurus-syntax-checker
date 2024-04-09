package org.nextprot.parser.cellosaurus

import scala.io.Source
import scala.xml._

class Msi(value: String, note: String, sc: SimpleSourcedComment) {
    
    val xreflist = sc.xreflist
    val publist = sc.publist
    val srclist = sc.orglist
    val hasSources = (sc.getSourceCount() > 0)

    override def toString() : String = {
        s"Msi(value=$value, note:${if (note==null) "(null)" else note}, sources:$sc.sources)"
    }

    def toXML = 
        <microsatellite-instability msi-value={value}>
            {
            if (note != null) <microsatellite-instability-note>{note}</microsatellite-instability-note> else Null
            }
            {
            if (hasSources)
                <source-list>
                { if (xreflist.size>0) xreflist.map(_.toXML) else Null }
                { if (publist.size>0) publist.map(_.toXML) else Null }
                { if (srclist.size>0) srclist.map(_.toXML) else Null }
                </source-list>
            else
                Null
            }
        </microsatellite-instability>

}

object MsiParser {

    def parseLine(rawtext: String, cellLineId: String, verbose: Boolean) : Msi = {
        val text = if (rawtext.endsWith(".")) rawtext.substring(0,rawtext.length-1) else rawtext
        val possibleValues = Set("Instable (MSI)", "Instable (MSI-high)", "Instable (MSI-low)", "Stable (MSS)")
        val pv = possibleValues.find(v => text.startsWith(v + " "))
        if (pv == None) throw new Exception("Unknown Microsatellite Instability value")
        val value = pv.get
        var tail = text.substring(value.length + 1)
        var note: String = null
        if (tail.startsWith("(Note=")) {
            val closeNotePos = tail.indexOf(")")
            note = tail.substring(6, closeNotePos)
            tail = tail.substring(closeNotePos + 1)
        }
        val sc = SimpleSourcedCommentParser.parse(rawtext, clId = cellLineId, verbose = verbose)
        if (verbose) {
            sc.sources.split("; ").foreach(src => {
                if (! SourceChecker.isKnown(src) ) 
                    println(s"ERROR: Unknown Microsatellite instability comment source '${src}' at cell line ${cellLineId}")
            })
        }
        return new Msi(value, note, sc)
    }


    def main(args: Array[String]): Unit = {

        // sbt "run ../cellosaurus-api/data_in/cellosaurus_xrefs.txt mabtar.txt"

        val datadir = "/home/pmichel/work/cellosaurus-api/data_in/"
        DbXrefInfo.load(args(0)) // load allowed db name with cat & url properties
        val instMap = SourceChecker.loadInstitutionFile(datadir + "institution_list")
        val childParentMap = SourceChecker.loadHierarchy(datadir + "cellosaurus.txt")
        SourceChecker.init(DbXrefInfo.getDbSet(), instMap, childParentMap)

        val filename = args(1)
        var lineNo = 0
        for (line <- Source.fromFile(filename).getLines()) {
            if (line.startsWith("CC   Microsatellite instability: ")) {
                val linevalue = line.substring(33).trim()
                lineNo += 1
                try {
                    println("--------------")
                    println(line)
                    val msi = parseLine(linevalue, cellLineId = "some cell line id", verbose=true)
                    println(msi)
                    println(msi.toXML)
               } catch {
                   case e: Exception => {println(s"ERROR at line ${lineNo}: ${e.getMessage()}")}
               }
            }
        }
    }

}