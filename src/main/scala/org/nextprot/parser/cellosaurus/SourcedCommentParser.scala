package org.nextprot.parser.cellosaurus

import scala.io.Source
import scala.collection.mutable.Map

case class SourcedComment(comment: String, sources: List[String], patternAction: Action) {
  
  override def toString() : String = {
    val src_str = if (sources.length==0) "(none)" else sources.mkString("; ")
    s"SourcedComment(\n  comment: $comment\n  sources: $src_str\naction: $patternAction\n)"
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
      if (pa.action == Action.KEEP || pa.action == Action.KEEP_AND_SPLIT || pa.action == Action.IGNORE) {
        currComment += " " + pa.pattern
      }
      if (pa.action != Action.IGNORE) {
        currSources = currSources ++ sources.split("; ").toList
      }
      //println(s"action: ${pa.action}")
      if (pa.action == Action.SPLIT  || pa.action == Action.KEEP_AND_SPLIT || pa.action == Action.COMPLETE) {
        val sc = new SourcedComment(currComment, List(currSources: _*), pa.action)
        result = sc :: result
        currComment = ""
        currSources = List.empty
      }
      //println(s"currPos: $currPos linevalue.length: ${linevalue.length}" )
      currPos = pos + pa.pattern.length
    })
    if (currPos < linevalue.length) {
      currComment += linevalue.substring(currPos, linevalue.length)
    }
    if (currComment.length > 0 ) {
      result = new SourcedComment(currComment, List(currSources: _*), Action.PICKUP) :: result
    }
    result.reverse
  }

}

enum Action { case 
  IGNORE,         // pattern kept in text but not added to source list
  SPLIT,          // pattern removed from text and added to source list, text split after pattern 
  KEEP,           // pattern kept in text and added to source list
  KEEP_AND_SPLIT, // pattern kept in text, added to source list, text split after pattern
  COMPLETE,       // pattern removed from text and added to sources, match MUST occur at the end of line
  PICKUP          // action set by parseLine() to pick up data to handle after the last pattern match
}

case class PatternAction(pattern: String, action: Action)

/* 
  Generic parser for 
 */
object SourcedCommentParser extends BasicParserTrait {
  val name = "SourcedCommentParser"
  val topic = "CC"
  val categories = Set(
    "Anecdotal", "Biotechnology", "Caution", "Characteristics", "Donor information", 
    "Karyotypic information", "Miscellaneous", "Senescence", "Virology")
  def sourceIsValid(src: String): Boolean = { 
    SourceChecker.isKnown(src) && ! SourceChecker.isInDbSet(src, Set("Cellosaurus")) }
  def getPatternActionList() : List[PatternAction] = {
    return List(
      PatternAction( "($sources). " , Action.IGNORE),     // 
      PatternAction( "($sources) "  , Action.IGNORE),     // 
      PatternAction( "($sources), " , Action.IGNORE),     // 
      PatternAction( "($sources); " , Action.IGNORE),     // 
      PatternAction( "($sources)"   , Action.COMPLETE) // COMPLETE should occur at the end of line
    )
  }
}


case class SourceInfo(var topics: Set[String], var typ: String, var count: Int)

object ParserPlayground {

  val interestingTopics: Set[String] = Set(
    "CC   Anecdotal: ","CC   Biotechnology: ","CC   Caution: ","CC   Characteristics: ",
    "CC   Donor information: ","CC   Karyotypic information: ","CC   Microsatellite instability: ",
    "CC   Problematic cell line: ", "CC   Miscellaneous: ", "CC   Senescence: ", "CC   Virology: ")


  def analyzeSources(filename: String, parser: BasicParserTrait) : Unit = {

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
        if (SourceChecker.isKnownPubliRef(k)) "known publiref" 
        else if (SourceChecker.isKnownXref(k)) "known xref"
        else if (SourceChecker.isKnownOrgRef(k)) "known organization"
        else if (SourceChecker.isKnownMiscRef(k)) "known  misc.source"
        else "UNKNOWN"
      val topics = sourceMap(k).topics.toList.mkString(", ")
      //println(s"$k\t${sourceMap(k).typ}\t${sourceMap(k).count}\t${topics}")
      println(s"$k\t${sourceMap(k).typ}\t${sourceMap(k).count}")
    })
  }

// -------------------------------------------------
  def main(args: Array[String]): Unit = {
// -------------------------------------------------

    val data_dir = args(0)
    val filename = args(1)

    // init source checker with real data
    DbXrefInfo.load(data_dir + "/cellosaurus_xrefs.txt")
    val instDict = SourceChecker.loadInstitutionFile(data_dir + "/institution_list")
    val cpDict = SourceChecker.loadHierarchy(data_dir + "/cellosaurus.txt")
    SourceChecker.init(DbXrefInfo.getDbSet(), instDict, cpDict)

    //val parser = AnecdotalParser
    //val parser = BiotechnologyParser
    //val parser = CautionParser
    //val parser = CharacteristicsParser
    //val parser = DonorInfoParser
    //val parser = KaryotypeParser
    //val parser = MiscellaneousParser
    //val parser = SenescenceParser
    //val parser = VirologyParser
    val parser = SourcedCommentParser

    if (1==2) {
      println(s"---------- ${parser.name} - Analysis -----------")
      analyzeSources(filename, parser)
      //sys.exit(0)      
    }

    val lines = Source.fromFile(filename).getLines()
    var lineNo = 0

    if (1==2) {
      println(s"---------- ${parser.name} - parsing result -----------")
      for (line <- lines) {
        if (line.startsWith(parser.topic)) {  // topic property is not used in prod, we use categories instead
          var linevalue = line.substring(parser.topic.length).trim()
          lineNo += 1
          println("\n---- : <<" + linevalue + ">>")
          try {
            val sc_list = parser.parseLine(linevalue)
            println(s"elements : ${sc_list.length}")
            sc_list.foreach(sc => {
              println(s"COMMENT=${sc.comment}\nSOURCES=${sc.sources}\nPATTERN=${sc.patternAction}")
              sc.sources.foreach( source => {
                if (SourceChecker.isKnownMiscRef(source)) {
                  println(s"MISC-SRC=${source}")
                } else {
                  println(s"OTHER-SRC=${source}")
                }
              })
            })
          } catch {
            case e: Exception => println(s"ERROR at line $lineNo: ${e.getMessage}")
          }
        }
      }
    }
    

    println("End")


  }


}


