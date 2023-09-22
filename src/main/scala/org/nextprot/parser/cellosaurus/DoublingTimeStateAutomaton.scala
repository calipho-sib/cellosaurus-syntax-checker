package org.nextprot.parser.cellosaurus

import scala.io.Source
import scala.collection.mutable.Map

class DtElement(val value: String) {
  var note: String = null;
  var refs: String = null;
  override def toString() : String = {
    val v = "value=" + value
    val n = if (note==null) "(null)" else note
    val r = if (refs==null) "(null)" else refs
    return v + " note=" + n + " refs=" + r
  }
}

object DoublingTimeStateAutomaton {

  val stateTran = Map(
    "value" -> List(
      Map("pattern" -> ", ", "next_state" -> "value"),
      Map("pattern" -> " (Note=", "next_state" -> "note"),
      Map("pattern" -> " (", "next_state" -> "refs")),
    "note" -> List(
      Map("pattern" -> "), ", "next_state" -> "value"),
      Map("pattern" -> ") (", "next_state" -> "refs")),
    "refs" -> List(
      Map("pattern" -> "); ", "next_state" -> "value"),
      Map("pattern" -> ").", "next_state" -> "END")))

  def findFirstMatch(line: String, pos: Int, transitions: List[Map[String, String]]): Map[String, Any] = {
    
    // store transition pattern matches
    val pmList = transitions.flatMap { trans =>
      val pattern = trans("pattern")
      val matchPos = line.indexOf(pattern, pos + 1)
      if (matchPos >= 0) {
        val next_pos = matchPos + pattern.length
        Some(Map[String,Any]("next_state" -> trans("next_state"), "pattern" -> pattern, "match_pos" -> matchPos, "next_pos" -> next_pos))
      } else {
        None
      }
    }

    if (pmList.isEmpty) {
      throw new Exception(s"No transition found: ${line.substring(0, pos)} <<< after here >>> ${line.substring(pos)}")
    }

    // sort pattern matches found: nearest match_pos first / longest match first
    pmList.sortWith { (pm1, pm2) =>
      val pos1 = 99999 - pm1("match_pos").asInstanceOf[Int]
      val pos2 = 99999 - pm2("match_pos").asInstanceOf[Int]
      val pm1Key = s"${(pos1).formatted("%05d")}/${pm1("pattern")}"
      val pm2Key = s"${(pos2).formatted("%05d")}/${pm2("pattern")}"
      pm1Key > pm2Key
    }

    // return first match (the only one which is relevant)
    return pmList(0)
  }

  def parseLine(line: String): List[Map[String,String]] = {
    //println(s"\nline  : $line")
    var elems = List[Map[String,String]]()
    var pos = 0
    var state = "value"
    var done = false
    while (! done) {
      val matchObj = findFirstMatch(line, pos, stateTran(state))
      val endPos = matchObj("match_pos").asInstanceOf[Int]
      val pattern = matchObj("pattern").asInstanceOf[String]
      val text = line.substring(pos, endPos)
      val elem = Map("state" -> state, "text" -> text, "pattern" -> pattern)
      //println(s"$state : ${line.substring(pos, endPos)}")
      pos = matchObj("next_pos").asInstanceOf[Int]
      state = matchObj("next_state").asInstanceOf[String]
      elems = elems :+ elem
      if (state == "END") {
        done = true
      }
    }
    return elems
  }

/*
  Builds a list of DoublingTime elements from a list of states
  input  : list of (state, text, pattern) 
  output : list of (value, note, refs)
*/
  def buildDtList(elems: List[Map[String,String]]): List[Map[String,String]] = {

    var dtlist =  List[Map[String,String]]()
    for (el <- elems) {
      val state = el("state")
      val text = el("text")
      // each time we encounter a value, we init a new doubling time element
      if (state == "value") {
        var dt = Map[String,String]()
        dt("value") = text
        dt("note") = null
        dt("refs") = null
        // we insert the new element at the head of the list
        dtlist = dt :: dtlist
      } else  {
        dtlist.head(state) = text
      } 
    }
    // at this stage the list of DoublingTime elements is in reverted order
    // we assign missing refs by copying refs found in previous element
    var previousRefs : String = null
    for (dt <- dtlist) {
      if (dt("refs") == null) {
        dt("refs") = previousRefs
      } else {
        previousRefs = dt("refs")
      }
    } 
    // we return the list in the natural order reflecting how the data is found in source file
    return dtlist.reverse
  }

  def main(args: Array[String]): Unit = {

    val filename = args(0)
    val lines = Source.fromFile(filename).getLines()
    var lineNo = 0
    for (line <- lines) {
      if (line.startsWith("CC   Doubling time")) {
        val parsedLine = line.substring(20).trim()        
        lineNo += 1
        try {
          val elems = parseLine(parsedLine)
          //for (el <- elems) { println(el) }
          val dtlist = buildDtList(elems)
          for (dt <- dtlist) {
            println(dt)
          }

        } catch {
          case e: Exception => println(s"ERROR at line $lineNo: ${e.getMessage}")
        }
      }
    }
    println("End")
  }
}