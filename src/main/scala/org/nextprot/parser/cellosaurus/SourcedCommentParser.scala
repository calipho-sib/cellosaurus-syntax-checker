package org.nextprot.parser.cellosaurus

import scala.io.Source
import scala.collection.mutable.Map

case class SourcedComment(comment: String, sources: List[String], patternAction: Action) {

//  val trgTerm : CvTerm = if (db=="ChEBI") new CvTerm(db, ac, name) else null
//  val trgXref : DbXref = if (db=="UniProtKB") new DbXref(db, ac, name, "") else null
  
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
  COMPLETE,       // pattern removed from text and added to sources, MUST match at the end of line
  PICKUP          // action set by parseLine() to pick up data to handle after the last pattern match
}
case class PatternAction(pattern: String, action: Action)

object AnecdotalParser extends BasicParserTrait {
  val name = "AnecdotalParser"
  val topic = "CC   Anecdotal: "
  def sourceIsValid(db_ac: String): Boolean = {
      SourceChecker.isKnown(db_ac) && ! SourceChecker.isInDbSet(db_ac, Set("Cellosaurus"))
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
      SourceChecker.isKnown(db_ac) && ! SourceChecker.isInDbSet(db_ac, Set("Cellosaurus", "DrugBank"))
  }
  def getPatternActionList() : List[PatternAction] = {
    return List(
      PatternAction( "($sources). ", Action.SPLIT),    // pattern never matches except with Cellosaurus & DrubgBank which are discarded
      PatternAction( "($sources) " , Action.KEEP),     // pattern never matches
      PatternAction( "($sources)"  , Action.COMPLETE)  // pattern matches with DOI, PubMed
    )
  }
}

object CautionParser extends BasicParserTrait {

// Was indicated to have been established in the absence of EBV (PubMed=7522246), but contains EBV (CLS=300204)

  val name = "CautionParser"
  val topic = "CC   Caution: "
  def sourceIsValid(db_ac: String): Boolean = {
      SourceChecker.isKnown(db_ac) // && ! SourceChecker.isInDbSet(db_ac, Set("Cellosaurus", "DrugBank"))
  }
  def getPatternActionList() : List[PatternAction] = {
    return List(
      PatternAction( "($sources). ", Action.SPLIT),    // 
      PatternAction( "($sources) " , Action.KEEP),     // 
      PatternAction( "($sources), " , Action.KEEP),     // matches PubMed, AV3, FL, WISH
      PatternAction( "($sources)"  , Action.COMPLETE)  // 
    )
  }
}


/*
No split necessary, sources always appear at the end in brackets.
*/

// Control cell line for the CHOP-KO-DR (Cellosaurus=CVCL_B046), GCN2-KO-DR (Cellosaurus=CVCL_B049) and PERK-KO-DR (Cellosaurus=CVCL_B067) cell lines.

object CharacteristicsParser extends BasicParserTrait {
  val name = "CharacteristicsParser"
  val topic = "CC   Characteristics: "
  def sourceIsValid(db_ac: String): Boolean = {
      SourceChecker.isKnown(db_ac) // && ! SourceChecker.isInDbSet(db_ac, Set("Cellosaurus"))
  }
  def getPatternActionList() : List[PatternAction] = {
    return List(
      PatternAction( "($sources). ", Action.SPLIT),    // 
      PatternAction( "($sources) " , Action.KEEP),     // Cellosaurus
      PatternAction( "($sources), " , Action.KEEP),     // Cellosaurus
      PatternAction( "($sources); " , Action.KEEP),     // Cellosaurus
      PatternAction( "($sources)"  , Action.COMPLETE) //
    )
  }
}


object DonorInfoParser extends BasicParserTrait {
  val name = "DonorInfoParser"
  val topic = "CC   Donor information: "
  var validXrefDbSet : Set[String] = null // set at run time
  def sourceIsValid(db_ac: String): Boolean = {
      SourceChecker.isKnown(db_ac) // && ! SourceChecker.isInDbSet(db_ac, Set("Cellosaurus"))
  }
  def getPatternActionList() : List[PatternAction] = {
    return List(
      PatternAction( "($sources). " ,  Action.KEEP),     // Cellosaurus
      PatternAction( "($sources) " , Action.KEEP),     // Cellosaurus
      PatternAction( "($sources), " , Action.KEEP),     // Cellosaurus
      PatternAction( "($sources)"  , Action.COMPLETE) // COMPLETE should occur at the end of line
    )
  }
}

object KaryotypeParser extends BasicParserTrait {
  val name = "KaryotypeParser"
  val topic = "CC   Karyotypic information: "
  def sourceIsValid(db_ac: String): Boolean = {
      SourceChecker.isKnown(db_ac) // && ! SourceChecker.isInDbSet(db_ac, Set("Cellosaurus"))
  }
  def getPatternActionList() : List[PatternAction] = {
    return List(
      PatternAction( "($sources) " , Action.KEEP),     // Cellosaurus
      PatternAction( "($sources)"  , Action.COMPLETE) // COMPLETE should occur at the end of line
    )
  }
}

object MiscellaneousParser extends BasicParserTrait {
  val name = "MiscellaneousParser"
  val topic = "CC   Miscellaneous: "
  def sourceIsValid(db_ac: String): Boolean = {
      SourceChecker.isKnown(db_ac) // && ! SourceChecker.isInDbSet(db_ac, Set("Cellosaurus"))
  }
  def getPatternActionList() : List[PatternAction] = {
    return List(
   //   PatternAction( "($sources). " , Action.KEEP_AND_SPLIT),     // PubMed...
   //   PatternAction( "($sources); " , Action.KEEP),               // Coriell...
      PatternAction( "($sources) " , Action.KEEP),                  // Cellosaurus, CelloPub, DOI
      PatternAction( "($sources), " , Action.KEEP),                 // PubMed, Cellosaurus
      PatternAction( "($sources)"  , Action.COMPLETE)               // many
    )
  }
}


object SenescenceParser extends BasicParserTrait {
  val name = "SenescenceParser"
  val topic = "CC   Senescence: "
  def sourceIsValid(db_ac: String): Boolean = {
      SourceChecker.isKnown(db_ac) // && ! SourceChecker.isInDbSet(db_ac, Set("Cellosaurus"))
  }
  def getPatternActionList() : List[PatternAction] = {
    return List(
      PatternAction( "($sources). " , Action.KEEP_AND_SPLIT),     // PubMed...
      PatternAction( "($sources); " , Action.KEEP),               // Coriell...
      PatternAction( "($sources), " , Action.KEEP),               // ATCC...
      PatternAction( "($sources)"  , Action.KEEP)                 // many
    )
  }
}

object VirologyParser extends BasicParserTrait {
  val name = "VirologyParser"
  val topic = "CC   Virology: "
  def sourceIsValid(db_ac: String): Boolean = {
      SourceChecker.isKnown(db_ac) // && ! SourceChecker.isInDbSet(db_ac, Set("Cellosaurus"))
  }
  def getPatternActionList() : List[PatternAction] = {
    return List(
      PatternAction( "($sources). " , Action.KEEP_AND_SPLIT),     // PubMed...
  //    PatternAction( "($sources); " , Action.KEEP),               // none
      PatternAction( "($sources), " , Action.KEEP),               // PubMed...
      PatternAction( "($sources)"  , Action.KEEP)                 // many
    )
  }
}

object SourcedCommentParser extends BasicParserTrait {
  val name = "SourcedCommentParser"
  val topic = "CC   Test: "
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


  def analyzeSources(filename: String, parser: BasicParserTrait) : Unit = {

    //val parser = TestParser

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
    SourceChecker.init(DbXrefInfo.getDbSet(), instDict)

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

    if (1==1) {
      println(s"---------- ${parser.name} - Analysis -----------")
      analyzeSources(filename, parser)
      //sys.exit(0)      
    }


    val lines = Source.fromFile(filename).getLines()
    var lineNo = 0
    println(s"---------- ${parser.name} - parsing result -----------")
    for (line <- lines) {
      if (line.startsWith(parser.topic)) {
        var linevalue = line.substring(parser.topic.length).trim()
        lineNo += 1
        println("\n---- : <<" + linevalue + ">>")
        try {
          val sc_list = parser.parseLine(linevalue)
          println(s"elements : ${sc_list.length}")
          sc_list.foreach(sc => { println(s"${sc.comment}\n${sc.sources}\npattern action: ${sc.patternAction}") })
          //sc_list.foreach(sc => { println(sc) })
        } catch {
          case e: Exception => println(s"ERROR at line $lineNo: ${e.getMessage}")
        }
      }
    }
    println("End")
  }
}