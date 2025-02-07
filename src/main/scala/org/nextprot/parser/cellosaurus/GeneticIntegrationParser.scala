package org.nextprot.parser.cellosaurus

import scala.io.Source
import scala.util.matching.Regex
import scala.xml._


class GenInt(method:String, name: String, db: String, ac: String, note:String) {

  val validDbs = Set("CGNC", "FlyBase_Gene", "FPbase", "HGNC", "MGI", "RGD", "UniProtKB", "VGNC")
  val trgXref : DbXref = if (validDbs.contains(db)) new DbXref(db, ac, label = name) else null
  
  override def toString() : String = {
    s"GenInt(method=$method, name=$name, db:$db, ac:$ac, note:$note)"
  }

  def toXML =
    <genetic-integration method={method}>
      {
      if (trgXref == null) name else Null
      }
      {
      if (trgXref != null) trgXref.toXML else Null
      }
      {
      if (note != null) <genetic-integration-note>{note}</genetic-integration-note> else Null
      }
    </genetic-integration>    
  
} // end class



object GeneticIntegrationParser {

  /*
  Format: CC   Genetic integration: Method=Method_Name; Gene=Resource_abbrev; ac; Gene/protein_name[ (Note=Free_text)].

  Examples:
  CC   Genetic integration: Method=CRISPR/Cas9; Gene=HGNC; 5467; IGF2R.
  CC   Genetic integration: Method=CRISPR/Cas9; Gene=HGNC; 7059; MGMT (Note=With p.Gly180_Asn_207del and 19 mutations = SNAP-tag).
  CC   Genetic integration: Method=Transfection; Gene=CBG, click beetle green luciferase.
  */

  val validDbs = Set("CGNC", "FlyBase_Gene", "FPbase", "HGNC", "MGI", "RGD", "UniProtKB", "VGNC")
  val validMethods = Set("CRISPR/Cas9", "Homologous recombination", "PiggyBac transposition", "Recombinase-mediated cassette exchange", "Sleeping Beauty transposition", "TALEN", "Transduction", 
    "Transfection/transduction", "Transfection", "Transgenic mouse", "Transgenic rat", "Transgenic fish", "ZFN")
  
  def parseLine(rawline: String): GenInt = {
    // remove final '.' if necessary
    val line : String = if (rawline.endsWith(".")) rawline.substring(0,rawline.length-1) else rawline

    // extract note if exists
    val idxNote = line.indexOf("(Note=")
    val note : String = if (idxNote == -1) null else {
      if ( ! line.endsWith(")")) throw Exception("Missing ')' at the end of note at line: " + line)
      line.substring(idxNote+6, line.size-1)
    }

    // extract genetic info
    val methodAndGenetic  : String = if (idxNote == -1) line else line.substring(0, idxNote).trim()
    val idxGen = methodAndGenetic.indexOf("Gene=")
    if (idxGen == -1) throw Exception("Missing Gene= at line: " + line) 
    val geneticraw = methodAndGenetic.substring(idxGen + 5).trim()
    val genetic = if (geneticraw.endsWith(";")) geneticraw.substring(0, geneticraw.length-1) else geneticraw
    val elems = genetic.split("; ")
    val dbIsValid = validDbs.contains(elems(0))
    val db : String = if (dbIsValid) elems(0) else null
    if (dbIsValid && elems.size<3) throw Exception("Missing accession and / or gene name after database at line: " + line) 
    val ac : String = if (dbIsValid) elems(1) else null
    val name : String = if (dbIsValid) elems(2) else genetic

    // extract method info
    val methodic  : String = line.substring(0, idxGen).trim()
    val idxMet = methodic.indexOf("Method=")
    if (idxMet == -1) throw Exception("Missing Method= at line: " + line) 
    val methodraw = methodic.substring(idxMet + 7).trim()
    val method = if (methodraw.endsWith(";")) methodraw.substring(0, methodraw.length-1) else methodraw
    if (! validMethods.contains(method)) throw Exception("Invalid method name at line: " + line)
    return new GenInt(method, name, db, ac, note)
  }

  def main(args: Array[String]): Unit = {

    // sbt "run ../cellosaurus-api/data_in/cellosaurus_xrefs.txt ../cellosaurus-api/data_in/genetic-integration.txt"

    DbXrefInfo.load(args(0)) // load allowed db name with cat & url properties

    val filename = args(1)
    var lineNo = 0
    for (line <- Source.fromFile(filename).getLines()) {
      if (line.startsWith("CC   Genetic integration: ")) {
        val linevalue = line.substring(26).trim()
        lineNo += 1
        try {
            println("--------------")
            println(line)
            val mabtar = parseLine(linevalue)
            println(mabtar)
            println(mabtar.toXML)
        } catch {
           case e: Exception => {}
        }
      }
    }

    println("\nError(s) only now:\n")

    lineNo = 0
    for (line <- Source.fromFile(filename).getLines()) {
      if (line.startsWith("CC   Genetic integration: ")) {
        val linevalue = line.substring(26).trim()
        lineNo += 1
        try {
          parseLine(linevalue)
        } catch {
           case e: Exception => {
            println("--------------")
            println(line)
            println(s"ERROR at line $lineNo: ${e.getMessage}")
           }
        }
      }
    }

    println("End")
  }
}