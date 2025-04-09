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

    var possibleValues = scala.collection.mutable.Set[String]() 

    def load(filename: String): Unit = {
        println("Loading " + filename)
        val lines = Source.fromFile(filename).getLines()
        var lineNo = 0
        for (line <- lines) {
            lineNo += 1
            possibleValues.add(line.strip())
        }
    }

    def parseLine(rawtext: String) : Omics = {
        // make sure data ends with a "." because possibleValues all ends with a "."
        val text = if (! rawtext.endsWith(".")) rawtext + "." else rawtext
        val elems = text.split("; ")
        val elem1 = elems.head
        val branch = if (elem1.endsWith(".")) elem1.substring(0,elem1.size-1) else elem1
        if (! possibleValues.contains(text)) throw new Exception("Invalid omics value: " + text)
        val rest = elems.tail.mkString("; ")
        return new Omics(branch, rest)
    }



    def main(args: Array[String]): Unit = {

        OmicsParser.load("../cellosaurus-api/data_in/cellosaurus_omics.cv")
        var omics: Omics = null
        try {
            omics = OmicsParser.parseLine("Genomics; Whole genome sequencing; Low read coverage.") // OK
            println(omics.toXML)
            omics = OmicsParser.parseLine("Proteomics.") // OK
            println(omics.toXML)
            omics = OmicsParser.parseLine("Genomics; hello; boy") // raises error cos not in cellosaurus_omics.cv
            println(omics.toXML)
        } catch {
            case e: Exception => { println(e) }
        }

    }
}


