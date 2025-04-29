package org.nextprot.parser.cellosaurus

import scala.io.Source
import scala.collection.mutable.Map


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
    val sortedPmList = pmList.sortWith { (pm1, pm2) =>
      val pos1 = 99999 - pm1("match_pos").asInstanceOf[Int]
      val pos2 = 99999 - pm2("match_pos").asInstanceOf[Int]
      val pm1Key = f"${pos1}%05d/${pm1("pattern")}"
      val pm2Key = f"${pos2}%05d/${pm2("pattern")}"
      pm1Key > pm2Key
    }

    // return first match (the only one which is relevant)
    return sortedPmList.head
  }

  def parseLine(line: String): List[Map[String,String]] = {
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

  def getConsolidatedDoublingTimeRangeInHours(dtList: List[String]): Option[(Int, Int)] = {
    var min : Int = 1000000000
    var max : Int = 0
    for (value <- dtList) {
      val opt_range = getDoublingTimeRangeInHours(value)
      if (opt_range != None) {
        val range = opt_range.get
        if (range._1 < min) min = range._1
        if (range._2 > max) max = range._2
      }
    }
    if (max == 0) return None
    return Some((min, max))
  }


  def getDoublingTimeRangeInHours(raw_value: String): Option[(Int, Int)] = {

    try {

      val value = raw_value
        .replace("<", "").replace("~","")                           // filter out any ~ or < (we ignore them)
        .replace("hours","hour").replace("days", "day")             // standardize unit
        .replace("weeks","week").replace("months", "month")         // standardize unit
      if (! value.contains("hour") && ! value.contains("day") &&    // ignore values without a time unit
          ! value.contains("week") && ! value.contains("month")) {  // ignore values without a time unit
        return None
      } 

      val parts = value.split(" ")
      var unit : String = "";
      var q1 : Float = 0.0f;
      var q2 : Float = 0.0f;
      if (parts.size==2) {
        unit = parts(1)
        val expr = parts(0)
        if (expr.charAt(0) == '>') {
          val q = expr.substring(1).toFloat
          q1 = q + 1.0f
          q2 = q * 1.1f
          if (q2 < q1) {
            val tmp = q2
            q2 = q1
            q1 = tmp
          }
        } else if (expr.contains("-")) {
          val subparts = expr.split("-")
          q1 = subparts(0).toFloat
          q2 = subparts(1).toFloat
        } else {
          q1 = expr.toFloat
          q2 = q1
        }
      } else if (parts.size==4 && parts(1) == "+-") {
        unit = parts(3)
        val center : Float = parts(0).toFloat
        val shift : Float = parts(2).toFloat
        q1 = center - shift
        q2 = center + shift
      } else {
        println("WARNING, cannot parse doubling time expression: " + raw_value)
        return None
      }
      // now convert quantities into hours
      if (unit == "day") {
        q1 = q1 * 24.0f
        q2 = q2 * 24.0f
      } else if (unit == "week") {
        q1 = q1 * 168.0f
        q2 = q2 * 168.0f
      } else if (unit == "month") {
        q1 = q1 * 30.0f * 24.0f
        q2 = q2 * 31.0f * 24.0f
      }
      // now round to integer values
      return Some((q1.toInt, math.ceil(q2).toInt))
    } catch {
      case _: Exception =>
        println("WARNING, cannot parse doubling time expression: " + raw_value)
        return None
    }    
  }


  def main(args: Array[String]): Unit = {

    val filename = args(0)
    val lines = Source.fromFile(filename).getLines()
    var lineNo = 0
    for (line <- lines) {
      if (line.startsWith("CC   Doubling time")) {
        val parsedLine = line.substring(20).trim()        
        lineNo += 1
        println("----------------------------------------")
        println(line)
        try {
          val elems = parseLine(parsedLine)
          val dtlist = buildDtList(elems)
          var values = List[String]()
          for (dt <- dtlist) {
            val value = dt("value")
            values = values :+ value
            println(value)
            println("--> " + getDoublingTimeRangeInHours(dt("value")))
          }

          println("==> " + getConsolidatedDoublingTimeRangeInHours(values))
        } catch {
          case e: Exception => println(s"ERROR at line $lineNo: ${e.getMessage}")
        }
      }
    }
    println("End")
  }
}