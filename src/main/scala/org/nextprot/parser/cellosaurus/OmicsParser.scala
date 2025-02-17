package org.nextprot.parser.cellosaurus

import scala.io.Source
import scala.xml._

class Omics(branch: String, rest: String) {
    
    override def toString() : String = {
        s"Omics(branch=$branch, rest:${if (rest==null) "(null)" else rest})"
    }

    def toXML = 
        <omics branch={branch}>
            {
            if (rest != null) {rest} else Null
            }
        </omics>

}

object OmicsParser {

    def parseLine(rawtext: String) : Omics = {
        val text = if (rawtext.endsWith(".")) rawtext.substring(0,rawtext.length-1) else rawtext
        
        val elems = text.split("; ")
        val possibleValues = Set("Genomics", "Glycomics", "Lipidomics", "Metabolomics", "Phenotyping", "Proteomics", "Transcriptomics", "Variations")
        val branch = elems.head
        if (! possibleValues.contains(branch)) throw new Exception("Invalid omics branch " + branch)
        val rest = elems.tail.mkString("; ")
        return new Omics(branch, rest)
    }



    def main(args: Array[String]): Unit = {

        var omics: Omics = null
        try {
            omics = OmicsParser.parseLine("Genomics; hello; boy")
            println(omics.toXML)
        } catch {
            case e: Exception => { println(e) }
        }

    }
}


