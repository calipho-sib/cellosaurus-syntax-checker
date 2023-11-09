package org.nextprot.parser.cellosaurus

import scala.io.Source
import scala.util.matching.Regex
import scala.xml._

class Mabiso(heavyChain: String, lightChain: String, publiRefs: List[PubliRef], xrefs: List[DbXref], sources: List[STsource]) {
  override def toString() : String = {
    s"Mabiso(heavyChain=$heavyChain, lightChain:$lightChain, publiRefs:$publiRefs, xrefs:$xrefs, sources:$sources)"
  }
}

//case class Mabiso(heavyChain: String, lightChain: String, sources: List[String]) 

// MonoclonalAntibodyIsotypeParser
object MabisoParser {

  val validLightChains = Set("kappa", "kappa+lambda", "lambda")

  val validHeavyChains = Set("IgA", "IgA+IgM", "IgA2", "IgE", "IgE Iga haplotype", "IgE Igb haplotype", "IgG", 
    "IgG+IgM", "IgG1", "IgG1+IgG2", "IgG1+IgG2a", "IgG1+IgG2b", "IgG1+IgG3", "IgG1+IgM", "IgG2", "IgG2a", 
    "IgG2a+IgG2b", "IgG2b", "IgG2c", "IgG3", "IgG3+IgM", "IgG4", "IgM", "IgM+IgY", "IgY", "Not determined", "Not specified")

  val validPubliDb = Set("PubMed", "DOI", "CelloPub", "Patent")

  def splitStringOutsideBrackets(input: String): Array[String] = {
    val pattern = "\\s*;\\s*(?![^()]*\\))".r
    pattern.split(input)
  }

  def getHeavyChain(item: String, lightIdx: Int, srcIdx: Int) : String = {
    if (lightIdx > -1) return item.substring(0, lightIdx).trim()
    if (srcIdx > -1) return item.substring(0, srcIdx).trim()
    return item.trim()
  }

  def getLightChain(item: String, lightIdx: Int, srcIdx: Int) : String = {
    if (lightIdx == -1) return null
    if (srcIdx == -1) return item.substring(lightIdx+1).trim()
    return item.substring(lightIdx+1, srcIdx).trim()
  }

  def splitSources(item: String, lightIdx: Int, srcIdx: Int) : List[String] = {
    if (srcIdx == -1) {
       List.empty[String]
    } else {
      item.substring(srcIdx+1).split(";").map(s => { 
        val ts = s.trim()
        if (ts.endsWith(")")) ts.substring(0,ts.size-1) else ts
      }).toList
    }
  }

  def getRawSources(srcList: List[String]) : List[STsource] = {
    return srcList.filterNot( _.contains("=") ).map(STsource(_)).toList
  }

  def getXrefSources(srcList: List[String]) : List[DbXref] = {
    return srcList
      .filter( _.contains("=") )
      .filterNot( s => {validPubliDb.contains(s.split("=")(0)) }) // not a publication ref
      .map(src => {
        val dbac=src.split("=")
        val db = dbac(0)
        if (! DbXrefInfo.contains(db)) throw Exception("Invalid db in xref: " + db)
        val ac = dbac(1)
        new DbXref(db, ac, "", "")
      }).toList
  }

  def getPubliRefSources(srcList: List[String]) : List[PubliRef] = {
    return srcList.filter( _.contains("=") )
      .filter( s => {validPubliDb.contains(s.split("=")(0)) }) //  IS a publication ref
      .map(new PubliRef(_))
      .toList
  }


  def parseLine(rawline: String): List[Mabiso] = {

    // CC   Monoclonal antibody isotype: IgG1 (PubMed=2646376); IgM (PubMed=6466869; PubMed=6863545).

    // remove final '.' if necessary
    var line = rawline
    if (rawline.endsWith(".")) line = rawline.substring(0,rawline.length-1) 

    // split items separated by ";" but only  outside parentheses
    val items = splitStringOutsideBrackets(line)
    return items.map(item => {
      val lightIdx = item.indexOf(",")
      val srcIdx = item.indexOf("(")
      val hc = getHeavyChain(item, lightIdx, srcIdx)
      if (hc == null || ! validHeavyChains.contains(hc)) throw Exception("Invalid heavy chain: " + hc)
      val lc = getLightChain(item, lightIdx, srcIdx)
      if (lc != null && ! validLightChains.contains(lc)) throw Exception("Invalid light chain: " + lc)
      val srcList = splitSources(item, lightIdx, srcIdx)
      val raw = getRawSources(srcList)             
      val xrf = getXrefSources(srcList)            
      val pub = getPubliRefSources(srcList)
      Mabiso(hc, lc, pub, xrf, raw)
    }).toList
  }

  def main(args: Array[String]): Unit = {

    // Example of arguments:
    
    // sbt "run ../cellosaurus-api/data_in/cellosaurus_xrefs.txt isotype.txt"

    DbXrefInfo.load(args(0)) // load allowed db name with cat & url properties

    val filename = args(1)
    var lineNo = 0
    for (line <- Source.fromFile(filename).getLines()) {
      if (line.startsWith("CC   Monoclonal antibody isotype: ")) {
        val linevalue = line.substring(34).trim()
        lineNo += 1
        try {
            println("--------------")
            println(line)
            val mabisoList = parseLine(linevalue)
            mabisoList.foreach(m => { println(m) })
        } catch {
           case e: Exception => {}
        }
      }
    }

    println("\nError(s) only now:\n")

    lineNo = 0
    for (line <- Source.fromFile(filename).getLines()) {
      if (line.startsWith("CC   Monoclonal antibody isotype: ")) {
        val linevalue = line.substring(34).trim()
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