package org.nextprot.parser.cellosaurus

import scala.io.Source
import scala.collection.mutable.Map


object SpeciesChecker {

    // we store the possible OX lines in oxlineMap
    // based on the content of the file used in init() 
    var oxlineMap = Map[String, Integer]()

    // OK line examples
    //
    // OX   NCBI_TaxID=10090; ! Mus musculus (Mouse)
    // OX   NCBI_TaxID=10116; ! Rattus norvegicus (Rat)

    // MUST be called before calling other methods
    def init(filename: String) : Unit = {
        println(s"INFO, loading ${filename}")
        val lines = Source.fromFile(filename).getLines()
        var lineNo = 0
        for (line <- lines) {
            lineNo += 1
            val elems = line.strip().split(",")
            if (line.trim()=="") {
                // do nothing
            } else if (elems.size != 4) {
                println(s"ERROR, unexpected number of fields, skipping line ${lineNo}: ${line}")
            } else {
                val id = elems(0).trim()
                val sciname = elems(1).trim()
                val comname = elems(2).trim()
                val oxline = s"OX   NCBI_TaxID=${id}; ! ${sciname} (${comname})"
                oxlineMap(oxline) = 0
            }
        }


    }

    def checkAndCountOxLine(oxline: String): Boolean = {
        val count = oxlineMap.get(oxline)
        if (count != None) oxlineMap(oxline) += 1
        return oxlineMap.contains(oxline)
    }

    def getNumberOfDifferentSpecies() : Integer = {
        var count = 0
        for (num <- oxlineMap.values) {
            if (num > 0) count += 1
        }
        return count
    }

    def getNumberOfLinesWithSpecies(id: String) : Integer = {
        var result : Integer = 0
        val pattern = "=" + id + ";"
        for (key <- oxlineMap.keys) {
            if (key.indexOf(pattern) != -1) {
                result = oxlineMap(key) 
            }
        }
        return result
    }

    def main(args: Array[String]): Unit = {
  
        val datadir = "/home/pmichel/work/cellosaurus-api/data_in/"
        SpeciesChecker.init(datadir + "cellosaurus_species.cv")

        println(SpeciesChecker.checkAndCountOxLine("OX   NCBI_TaxID=10090; ! Mus musculus (Mouse)"))
        println(SpeciesChecker.checkAndCountOxLine("OX   NCBI_TaxID=10090; ! Mus musculus (Mouse)"))
        println(SpeciesChecker.checkAndCountOxLine("OX   NCBI_TaxID=10116; ! Rattus norvegicus (Rat)"))
        println(SpeciesChecker.checkAndCountOxLine("OX   NCBI_TaxID=10090; ! Mus musculus (Mouse)"))
        println(SpeciesChecker.checkAndCountOxLine("OX   NCBI_TaxID=9606; ! Homo sapiens (Human)"))
        println(SpeciesChecker.checkAndCountOxLine("OX   NCBI_TaxID=9902; ! Bison bonasus (European bison)"))
        println("diff species:" + SpeciesChecker.getNumberOfDifferentSpecies())
        println("cnt    human:" + SpeciesChecker.getNumberOfLinesWithSpecies("9606"))
        println("cnt    mouse:" + SpeciesChecker.getNumberOfLinesWithSpecies("10090"))
        println("cnt      rat:" + SpeciesChecker.getNumberOfLinesWithSpecies("10116"))



    }

}