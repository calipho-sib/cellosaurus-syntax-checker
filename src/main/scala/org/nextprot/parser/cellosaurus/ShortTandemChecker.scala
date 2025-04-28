package org.nextprot.parser.cellosaurus

import scala.io.Source
import scala.collection.mutable.Map
import scala.collection.mutable.ArrayBuffer

object ShortTandemChecker {
  

  def check(ac: String, st_lines: List[String]) = {

    var all_sources = Set[String]()
    var markers = ArrayBuffer[String]()
    st_lines.foreach(line => {
      if (line.startsWith("ST   Source(s): ")) {
        all_sources = line.substring(16).split("; ").toSet
      } else {
        markers += line
      }
    })

    var prev_marker = ""
    var prev_allele = ""
    var marker_wo_src = false
    if (st_lines.size > 0 && markers.size == 0) {
        println("ERROR, no ST marker in " + ac)
    }

    markers.sorted.foreach(m => {
      val elems = m.split(":")
      if (elems.size != 2) {
        println("ERROR, invalid ST marker line in " + ac + ", line: " + m)
      } else {
        val marker = elems(0).substring(5)
        val allele = elems(1).split("\\(")(0).strip()
        val srclst = get_marker_src(ac, m)

        srclst.foreach( s => {
          var ok = false
          all_sources.foreach( ds => {
            // source s must be same as one of all_sources element
            if (s == ds) {  
              ok = true
            // or s must start with the same letters as a an element of all_sources + "_"
            } else if (s.startsWith(ds + "_")) {
              ok = true
            }
          })
          if (!ok) println("ERROR, undeclared source " + s + " in " + ac + ", line: " + m)
        })

        if (marker != prev_marker) {
          prev_marker = marker
          prev_allele = allele
          marker_wo_src = srclst.isEmpty
        } else {
          if (allele == prev_allele) {
            println("ERROR, duplicate alleles for marker " + marker + " in " + ac + ", line: " + m)
          }
          if (marker_wo_src || srclst.isEmpty) {
            println("ERROR, some alleles with no source for marker " + marker + " in " + ac + ", at or near line: " + m)
          }
        }
      }
    })

  }

  def get_marker_src(ac: String, line: String) : List[String] = {
    val p1 = line.indexOf("(")
    val p2 = line.indexOf(")")
    if (p1 >= 0 && p2 < p1) {
      println("ERROR, ST line format error, misplaced or missing closing parenthesis in " + ac + ", line: " + line)
    }
    if (p1 >= 0 && p2 > p1) {
      return line.substring(p1+1,p2).split("; ").toList
    } else {
      return List.empty[String]
    }

  }


  def main(args: Array[String]): Unit = {

    val filename = args(0)
    val lines = Source.fromFile(filename).getLines().toList
    check("CVCL_1234", lines)
//    try {
//      println("...")
//    } catch {
//      case e: Exception => println(s"ERROR at line $lineNo: ${e.getMessage}")
//    }
    println("End")
  }
}