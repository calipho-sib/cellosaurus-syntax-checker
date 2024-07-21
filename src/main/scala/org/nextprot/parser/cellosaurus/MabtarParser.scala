package org.nextprot.parser.cellosaurus

import scala.io.Source
import scala.util.matching.Regex
import scala.xml._

class Mabtar(name: String, db: String, ac: String, note:String) {

  val trgXref : DbXref = if (db=="ChEBI" || db=="UniProtKB" || db=="FPbase") new DbXref(db, ac, label = name) else null
  
  override def toString() : String = {
    s"Mabtar(name=$name, db:$db, ac:$ac, note:$note)"
  }

  def toXML =
    <monoclonal-antibody-target>
      {
      if (trgXref == null) name else Null
      }
      {
      if (trgXref != null) trgXref.toXML else Null
      }
      {
      if (note != null) <monoclonal-antibody-target-note>{note}</monoclonal-antibody-target-note> else Null
      }
    </monoclonal-antibody-target>    
  
} // end class


// MonoclonalAntibodyTargetParser
object MabtarParser {

  val validDbs = Set("ChEBI", "UniProtKB", "FPbase")

  def parseLine(rawline: String): Mabtar = {

    // multi line comment
    // data line format: [ChEBI¦UniProtKB; Db_AC;] Target_name [(Note=Target_Note)].
    // examples:
    // CC   Monoclonal antibody target: ChEBI; CHEBI:34692; Dicofol (Kelthane).
    // CC   Monoclonal antibody target: UniProtKB; P16087; Feline immunodeficiency virus (isolate Petaluma) (FIV) gag (AA 136-357 = p24).
    // CC   Monoclonal antibody target: Human cancer cells.
    // CC   Monoclonal antibody target: UniProtKB; P18341; Bovine TGFB1 (Note=Also reacts with human, mouse, rat and many other species).
    // CC   Monoclonal antibody target: ChEBI; CHEBI:82022; Imazethapyr (Pursuit) (Note=Also recognizes imazaquin, imazamethabenz-methyl and imazapic).
    // CC   Monoclonal antibody target: Human lung cancer cells (Note=Also recognizes many other type of cells)

    // remove final '.' if necessary
    val line : String = if (rawline.endsWith(".")) rawline.substring(0,rawline.length-1) else rawline

    // extract note if exists
    val idxNote = line.indexOf("(Note=")
    val note : String = if (idxNote == -1) null else {
      if ( ! line.endsWith(")")) throw Exception("Missing ')' at the end of note at line: " + line)
      line.substring(idxNote+6, line.size-1)
    }

    // extract target and split target elements (optional db,ac + mandatory target name)
    val target : String = if (idxNote == -1) line else line.substring(0, idxNote).trim()
    val elems = target.split("; ")
    val db : String = if (validDbs.contains(elems(0))) elems(0) else null
    if (db != null && elems.size<3) throw Exception("Missing accession and / or target name after database at line: " + line) 
    val ac : String = if (validDbs.contains(elems(0))) elems(1) else null
    val name : String = if (! validDbs.contains(elems(0))) target else {
      // reconcat elements after db and ac (except note already extracted)
      elems.zipWithIndex.filter((cnt, idx) => {idx >= 2}).map((cnt,idx) => {cnt}).mkString("; ")
    }
    return new Mabtar(name, db, ac, note)
  }

  def main(args: Array[String]): Unit = {

    // sbt "run ../cellosaurus-api/data_in/cellosaurus_xrefs.txt mabtar.txt"

    DbXrefInfo.load(args(0)) // load allowed db name with cat & url properties

    val filename = args(1)
    var lineNo = 0
    for (line <- Source.fromFile(filename).getLines()) {
      if (line.startsWith("CC   Monoclonal antibody target: ")) {
        val linevalue = line.substring(33).trim()
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
      if (line.startsWith("CC   Monoclonal antibody target: ")) {
        val linevalue = line.substring(33).trim()
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