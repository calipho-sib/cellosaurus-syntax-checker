package org.nextprot.parser.cellosaurus

import scala.io.Source
import scala.collection.mutable.Map

class SourcedComment(comment: String, sources: List[String]) {

//  val trgTerm : CvTerm = if (db=="ChEBI") new CvTerm(db, ac, name) else null
//  val trgXref : DbXref = if (db=="UniProtKB") new DbXref(db, ac, name, "") else null
  
  override def toString() : String = {
    val src_str = if (sources.length==0) "(none)" else sources.mkString("; ")
    s"SourcedComment(\n  comment: $comment\n  sources: $src_str\n)"
  }
}

trait BasicParserTrait {

  // abstract functions to be implemented in concrete parsers
  def sourceIsValid(db_ac: String): Boolean
  def getPatternActionList() : List[PatternAction]

  def findListOfBracketContentWithNestedLevelAndPosition(text: String): List[(String, Int, Int)] = {
    var result: List[(String, Int, Int)] = List()
    var stack: List[(Int, Int)] = List()
    for ((char, index) <- text.zipWithIndex) {
      char match {
        case '(' =>
          val depth = stack.length
          stack = (index, depth) :: stack
        case ')' =>
          stack match {
            case Nil =>
            // Ignore unmatched closing brackets
            case (start, depth) :: tail =>
              val end = index + 1
              val matchedText = text.slice(start + 1, end - 1)
              result = (matchedText, depth, start) :: result
              stack = tail
          }
        case _ =>
      }
    }
    result.reverse
  }

  def parseLine(rawline: String): List[SourcedComment] = {
    // remove final "." if necessary
    val linevalue = if (rawline.endsWith(".")) rawline.substring(0,rawline.length-1) else rawline
    val bracketContents = findListOfBracketContentWithNestedLevelAndPosition(linevalue)    
    //bracketContents.foreach({ case(text, level, pos) => println(s"RAW\n  text:$text\n  level:$level\n  pos:$pos")})
    val bracketSources = bracketContents.filter({case(text, level, pos) => text.split("; ").forall(el => sourceIsValid(el)) })
    //bracketSources.foreach({ case(text, level, pos) => println(s"SRC\n  text:$text\n  level:$level\n  pos:$pos")})
    var currPos = 0
    var currComment = ""
    var currSources : List[String] = List.empty
    var result: List[SourcedComment] = List.empty
    bracketSources.foreach({ case(sources, level, pos) => 
      // WARNING: the order of patterns is important !!!
      val paList = getPatternActionList().map(pa => new PatternAction(pa.pattern.replace("$sources", sources), pa.action))
      // choose the first matching pattern. 
      // pattern with a COMPLETE action should occur ONLY at the end of line
      val matched = paList.find(pa => {
        //println(s"$pa, ${linevalue.indexOf(pa.pattern, currPos)}, pos: $pos, currPos:$currPos, linelng:${linevalue.length}, pos+patt:${pos + pa.pattern.length}" )
        linevalue.indexOf(pa.pattern, currPos) == pos &&
        (pa.action != Action.COMPLETE || pos + pa.pattern.length == linevalue.length)
      })
      if (matched == None) throw new Exception(s"Pattern for source(s) not found: $sources")
      val pa = matched.get
      currComment += linevalue.substring(currPos, pos).trim()
      if (pa.action == Action.KEEP) currComment += " " + pa.pattern
      currSources = currSources ++ sources.split("; ").toList
      //println(s"action: ${pa.action}")
      if (pa.action == Action.SPLIT || pa.action == Action.COMPLETE) {
        val sc = new SourcedComment(currComment, List(currSources: _*))
        result = sc :: result
        currComment = ""
        currSources = List.empty
      }
      //println(s"currPos: $currPos linevalue.length: ${linevalue.length}" )
      currPos = pos + pa.pattern.length
    })
    if (currPos < linevalue.length) 
      currComment += linevalue.substring(currPos, linevalue.length)
      result = new SourcedComment(currComment, List(currSources: _*)) :: result
    result.reverse
  }

}

enum Action { case SPLIT, KEEP, COMPLETE }
case class PatternAction(pattern: String, action: Action)

object AnecdotalParser extends BasicParserTrait {
  val name = "AnecdotalParser"
  val topic = "CC   Anecdotal: "
  def sourceIsValid(db_ac: String): Boolean = {
      SourceChecker.isKnownPubliRef(db_ac) ||
      (SourceChecker.isKnownXref(db_ac) && ! SourceChecker.isInDbSet(db_ac, Set("Cellosaurus")))
  }
  def getPatternActionList() : List[PatternAction] = {
    return List(
      PatternAction( "($sources). ", Action.SPLIT),    // pattern triggering a split (occurs in the middle of the line)
      PatternAction( "($sources) " , Action.KEEP),     // pattern kept in the comment but added to sources (occurs in the middle of the line)
      PatternAction( "($sources)"  , Action.COMPLETE)  // pattern added to sources (COMPLETE should occur at the end of the line)
    )
  }
}

object BiotechnologyParser extends BasicParserTrait {
  val name = "BiotechnologyParser"
  val topic = "CC   Biotechnology: "
  def sourceIsValid(db_ac: String): Boolean = {
      SourceChecker.isKnownPubliRef(db_ac) ||
      (SourceChecker.isKnownXref(db_ac) && ! SourceChecker.isInDbSet(db_ac, Set("Cellosaurus", "DrugBank")))
  }
  def getPatternActionList() : List[PatternAction] = {
    return List(
      PatternAction( "($sources). ", Action.SPLIT),    // pattern triggering a split (occurs in the middle of the line)
      PatternAction( "($sources) " , Action.KEEP),     // pattern kept in the comment but added to sources (occurs in the middle of the line)
      PatternAction( "($sources)"  , Action.COMPLETE)  // pattern added to sources (COMPLETE should occur at the end of the line)
    )
  }
}

/*
No split necessary, sources always appear at the end in brackets.
*/
object CharacteristicsParser extends BasicParserTrait {
  val name = "CharacteristicsParser"
  val topic = "CC   Characteristics: "
  def sourceIsValid(db_ac: String): Boolean = {
      SourceChecker.isKnownPubliRef(db_ac) ||
      (SourceChecker.isKnownXref(db_ac) && ! SourceChecker.isInDbSet(db_ac, Set("Cellosaurus")))
  }
  def getPatternActionList() : List[PatternAction] = {
    return List(
      PatternAction( "($sources)"  , Action.COMPLETE) // COMPLETE should occur at the end of line
    )
  }
}


object DonorInfoParser extends BasicParserTrait {
  val name = "DonorInfoParser"
  val topic = "CC   Donor information: "
  var validXrefDbSet : Set[String] = null // set at run time
  def sourceIsValid(db_ac: String): Boolean = {
      SourceChecker.isKnownPubliRef(db_ac) ||
      (SourceChecker.isKnownXref(db_ac) && ! SourceChecker.isInDbSet(db_ac, Set("Cellosaurus")))
  }
  def getPatternActionList() : List[PatternAction] = {
    return List(
      PatternAction( "($sources)"  , Action.COMPLETE) // COMPLETE should occur at the end of line
    )
  }
}

object KaryotypeParser extends BasicParserTrait {
  val name = "KaryotypeParser"
  val topic = "CC   Karyotypic information: "
  def sourceIsValid(db_ac: String): Boolean = {
      SourceChecker.isKnownPubliRef(db_ac) ||
      (SourceChecker.isKnownXref(db_ac) && ! SourceChecker.isInDbSet(db_ac, Set("Cellosaurus"))) ||
      SourceChecker.isKnownOrgRef(db_ac)
  }
  def getPatternActionList() : List[PatternAction] = {
    return List(
      PatternAction( "($sources)"  , Action.COMPLETE) // COMPLETE should occur at the end of line
    )
  }
}

object TestParser extends BasicParserTrait {
  val name = "TestParser"
  val topic = null
  def sourceIsValid(src: String): Boolean = { SourceChecker.isKnown(src) }
  def getPatternActionList() : List[PatternAction] = {
    return List(
      PatternAction( "($sources)"  , Action.COMPLETE) // COMPLETE should occur at the end of line
    )
  }
}

case class SourceInfo(var topics: Set[String], var typ: String, var count: Int)

object ParserPlayground {

  val interestingTopics: Set[String] = Set(
    "CC   Anecdotal: ","CC   Biotechnology: ","CC   Caution: ","CC   Characteristics: ",
    "CC   Donor information: ","CC   Karyotypic information: ","CC   Microsatellite instability: ",
    "CC   Problematic cell line: ", "CC   Miscellaneous: ", "CC   Senescence: ", "CC   Virology: ")


  def analyzeSources(filename: String) : Unit = {

    val parser = TestParser

    val source = Source.fromFile(filename)
    val lineiter = source.getLines()
    var lineNo = 0
    var sourceMap: Map[String, SourceInfo] = Map()
    lineiter.foreach { line =>
      lineNo+=1
      val someTopic = interestingTopics.find(el => line.startsWith(el))
      if (someTopic != None) {
        val topic = someTopic.get
        var rawline = line.substring(topic.length).trim()
        val linevalue = if (rawline.endsWith(".")) rawline.substring(0,rawline.length-1) else rawline
        //println(s"topic : $topic")
        //println(s"value : $linevalue")
        val bracketContents = parser.findListOfBracketContentWithNestedLevelAndPosition(linevalue)
        bracketContents.foreach( {case(text, level, pos) => 
          val sources = text.split("; ")
          sources.foreach(src => 
            val parts = src.split("=")
            val key = if (parts.length > 1) parts(0) + "=<ac>" else parts(0)
            if (!sourceMap.contains(key)) sourceMap(key) = new SourceInfo(Set.empty,null,0)
            val si = sourceMap(key)
            si.topics = si.topics + topic
            si.count += 1
          )
        })
      }
    }
    source.close()
    sourceMap.keys.toList.sorted.foreach(k => {
      sourceMap(k).typ = 
        if (SourceChecker.isKnownPubliRef(k)) "knwon publiref" 
        else if (SourceChecker.isKnownXref(k)) "known xref"
        else if (SourceChecker.isKnownOrgRef(k)) "known organization"
        else "unknown"
      println(s"$k\t${sourceMap(k).typ}\t${sourceMap(k).count}\t${sourceMap(k).topics}")
    })
  }

// -------------------------------------------------
  def main(args: Array[String]): Unit = {
// -------------------------------------------------

    val filename = args(0)

    // init source checker with default values
    SourceChecker.init(null, null)

    if (1==1) {
      analyzeSources(filename)
      println("End")
      sys.exit(0)      
    }

    //val parser = AnecdotalParser
    //val parser = BiotechnologyParser
    //val parser = CharacteristicsParser
    //val parser = DonorInfoParser
    val parser = KaryotypeParser

    val lines = Source.fromFile(filename).getLines()
    var lineNo = 0
    println(s"---------- ${parser.name} - parsing result -----------")
    for (line <- lines) {
      if (line.startsWith(parser.topic)) {
        var linevalue = line.substring(parser.topic.length).trim()
        lineNo += 1
        println("\n---- : " + linevalue)
        try {
          val sc = parser.parseLine(linevalue)
          println(s"elements : ${sc.length}")
          println(sc) 
        } catch {
          case e: Exception => println(s"ERROR at line $lineNo: ${e.getMessage}")
        }
      }
    }
    println("End")
  }
}