package org.nextprot.parser.cellosaurus

import scala.io.Source
import scala.xml._

class Msi(value: String, note: String, sources: List[String]) {
    
    var xrefs = List[DbXref]()
    var publiRefs = List[String]()
    var orgRefs = List[String]()
    
    init()

    def init() : Unit = {
        sources.foreach(s => {
            if (SourceChecker.isKnownPubliRef(s)) { 
                publiRefs = s :: publiRefs }
            else if (SourceChecker.isKnownXref(s)) { 
                val dbac = s.split("=")
                xrefs = new DbXref(dbac(0), dbac(1)) :: xrefs 
            } else if (SourceChecker.isKnownOrgRef(s) || SourceChecker.isKnownMiscRef(s)) {
                orgRefs = s :: orgRefs 
            } else {
                Console.err.println(s"WARN. unknown source '${s}'")
            }
        })
    }

    override def toString() : String = {
        s"Msi(value=$value, note:${if (note==null) "(null)" else note}, sources:$sources)"
    }

//          <microsatellite-instability-value>{value}</microsatellite-instability-value>


    def toXML = 
        <microsatellite-instability msi-value={value}>
            {
            if (note != null) <microsatellite-instability-note>{note}</microsatellite-instability-note> else Null
            }
            <microsatellite-instability-sources>
            {
            if (xrefs.size > 0)
                <xref-list>{xrefs.map(_.toXML)}</xref-list>
            else
                Null
            }
            {
            if (publiRefs.size > 0)
                <reference-list>{publiRefs.map(id => <reference resource-internal-ref={id}/>)}</reference-list>
            else  
                Null
            }
            {
            if (orgRefs.size > 0) 
                <source-list>{orgRefs.map(src => <source>{src}</source>)}</source-list>
            else
                Null
            }
            </microsatellite-instability-sources>
        </microsatellite-instability>

}

object MsiParser {

    def parseLine(rawtext: String) : Msi = {
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
        val openPos = tail.indexOf("(")
        val closePos = tail.indexOf(")")
        tail = tail.substring(openPos + 1,closePos)
        var sources = tail.split("; ")
        sources.foreach(src => {
            if (! SourceChecker.isKnown(src) ) throw new Exception(s"Unknown Microsatellite instability comment source '${src}'")
        })
        return new Msi(value, note, sources.toList)
    }


    def main(args: Array[String]): Unit = {

        // sbt "run ../cellosaurus-api/data_in/cellosaurus_xrefs.txt mabtar.txt"

        DbXrefInfo.load(args(0)) // load allowed db name with cat & url properties
        SourceChecker.init(DbXrefInfo.getDbSet(), null)

        val filename = args(1)
        var lineNo = 0
        for (line <- Source.fromFile(filename).getLines()) {
            if (line.startsWith("CC   Microsatellite instability: ")) {
                val linevalue = line.substring(33).trim()
                lineNo += 1
                try {
                    println("--------------")
                    println(line)
                    val msi = parseLine(linevalue)
                    println(msi)
                    println(msi.toXML)
               } catch {
                   case e: Exception => {println(s"ERROR at line ${lineNo}: ${e.getMessage()}")}
               }
            }
        }
    }

}