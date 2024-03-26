package org.nextprot.parser.cellosaurus

import scala.io.Source
import scala.collection.mutable.Map

enum SOURCES_STATUS { case NONE_VALID, SOME_VALID, ALL_VALID, IGNORED, NONE }

class SimpleSourcedComment(val rawline: String) {

  val value = if (rawline.endsWith(".")) rawline.substring(0,rawline.length-1) else rawline
  var body: String = null
  var sources: String = null
  var status: SOURCES_STATUS = null
  var xreflist = List[DbXref]()
  var publist = List[PubliRef]()
  var orglist = List[STsource]()
 
  // to be used after parsing
  def getFinalValue() : String = {
    if (sources == "") return body
    return s"$body ($sources)"
  }

  def getSourceCount() : Integer = {
    return xreflist.size + publist.size + orglist.size
  }

  override def toString() : String = {
    s"\nSimpleSourcedComment(\n  value: $value\n  body: '$body'\n  sources: '$sources'\n  status: $status\n  xrefs: $xreflist\n  pubs: $publist\n  orgs: $orglist\n)"
  }
}


object SimpleSourcedCommentParser {

  var verbose: Boolean = true
  var logLevel: Integer = 0 // 0=INFO+WARNING+ERROR, 1=WARNING+ERROR, 2=ERROR

  val name = "SimpleSourcedCommentParser"

  val categories = Set(
    "Anecdotal", "Biotechnology", "Caution", "Characteristics", "Donor information", 
    "Karyotypic information", "Miscellaneous", "Senescence", "Virology")

  def sourceIsValid(src: String): Boolean = { 
    SourceChecker.isKnown(src) && ! SourceChecker.isInDbSet(src, Set("Cellosaurus")) }

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
              if (verbose && logLevel < 2) println(s"WARNING: unbalanced closing parenthese in ${text}")
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

  def parse(line: String, clId: String) : SimpleSourcedComment = {

    var sc = new SimpleSourcedComment(line)
    splitBodySources(sc)
    parseSources(sc, cellLineId = clId)
    return sc
  }

  def splitBodySources(sc: SimpleSourcedComment): Unit = {

    val cnt_list = findListOfBracketContentWithNestedLevelAndPosition(sc.value)
    val last_clp = cnt_list.find({ case(cnt, level, pos) => level == 0 && pos + cnt.size + 2 == sc.value.size })
    if (last_clp == None) {
      sc.body = sc.value.trim()
      sc.sources = ""
    } else {
      val (sources, level, pos) = last_clp.get
      sc.body = sc.value.substring(0, pos).trim()
      sc.sources = sources.trim()
    }
  }

  def parseSources(sc: SimpleSourcedComment, cellLineId: String = ""): Unit = {
    
    if (sc.sources == "") {
      sc.status = SOURCES_STATUS.NONE
      return
    }

    sc.status = SOURCES_STATUS.NONE_VALID
    
    var validCount: Integer = 0

    if (SourceChecker.isKnownMiscRef(sc.sources)) {

      val prefix = SourceChecker.getMiscRefPrefix(sc.sources)

      // special case of parent cell line: we need to get parent id and update source
      if (prefix == "from inference of" || prefix == "Direct_author_submission") {
          sc.status = SOURCES_STATUS.IGNORED
          if (verbose && logLevel < 1) println(s"INFO: ignoring source in comment '${sc.value}'")

      } else if (prefix == "from parent cell line") {
        val parentId = SourceChecker.getCellosaurusParentIdFromId(cellLineId)
        if (parentId != null) {
          sc.sources = prefix + " " + parentId
          sc.xreflist = new DbXref("Cellosaurus", SourceChecker.getCellosaurusAcFromId(parentId), label=parentId) :: sc.xreflist
          sc.status = SOURCES_STATUS.ALL_VALID
        } else {
          sc.status = SOURCES_STATUS.NONE_VALID
          if (verbose) println(s"ERROR: cell line '$cellLineId' has no parent but found in comment '${sc.value}'")
        }
    
      } else {
 
        // create one xref for each cell line id found after the prefix
        val items = sc.sources.substring(prefix.length).split("; ")
        items.foreach(item => {
          val id = item.trim()
          if (SourceChecker.isKnownCellosaurusId(id)) {
            sc.xreflist = new DbXref("Cellosaurus", SourceChecker.getCellosaurusAcFromId(id), label=id) :: sc.xreflist
            validCount+=1
            sc.status = SOURCES_STATUS.SOME_VALID
          } else  {
            if (verbose) println(s"ERROR: Unknown cell line identifier '$id' in sources of '(${sc.value})'")
          }
        })
        if (items.size > 0 && validCount == items.size) sc.status = SOURCES_STATUS.ALL_VALID       
      }

    } else {

      // here we can get a mixture of xrefs, refs, orgs
      var src_count : Integer = 0
      sc.sources.split("; ").foreach(src => {
        
        src_count += 1

        if (SourceChecker.isKnownPubliRef(src)) {
          sc.publist = new PubliRef(src) :: sc.publist
          validCount+=1
          sc.status = SOURCES_STATUS.SOME_VALID

        // cellosaurus xrefs appearing at the end of comments are not consdered sources
        } else if (SourceChecker.isKnownXref(src)  && ! SourceChecker.isInDbSet(src, Set("Cellosaurus"))) {
          val parts = src.split("=")
          sc.xreflist = new DbXref(db=parts(0), ac=parts(1)) :: sc.xreflist
          validCount+=1
          sc.status = SOURCES_STATUS.SOME_VALID

        } else if (SourceChecker.isKnownOrgRef(src)) {
          sc.orglist = new STsource(src) :: sc.orglist
          validCount+=1
          sc.status = SOURCES_STATUS.SOME_VALID

        } else {
          if (verbose && logLevel < 2) println(s"WARNING: Unknown source '$src' in '${sc.value}'")
        }
      })
      if (src_count > 0 && validCount == src_count) sc.status = SOURCES_STATUS.ALL_VALID       

    }
    
  } 

}

object SimpleParserPlayground {

  val interestingTopics: Set[String] = Set(
    "CC   Anecdotal: ","CC   Biotechnology: ","CC   Caution: ","CC   Characteristics: ",
    "CC   Donor information: ","CC   Karyotypic information: ","CC   Microsatellite instability: ",
    "CC   Problematic cell line: ", "CC   Miscellaneous: ", "CC   Senescence: ", "CC   Virology: ")


  // -------------------------------------------------
  def main(args: Array[String]): Unit = {
  // -------------------------------------------------

    // How to run it:
    // sbt "run ../cellosaurus-api/data_in seqvar.txt"

    val data_dir = args(0)
    val filename = args(1)

    // init source checker with real data
    DbXrefInfo.load(data_dir + "/cellosaurus_xrefs.txt")
    val instDict = SourceChecker.loadInstitutionFile(data_dir + "/institution_list")
    val cpDict = SourceChecker.loadHierarchy(data_dir + "/cellosaurus.txt")
    SourceChecker.init(DbXrefInfo.getDbSet(), instDict, cpDict)

    val parser = SimpleSourcedCommentParser
    parser.verbose = false
    

    // Tests
    var sc: SimpleSourcedComment = null

    println("\n--- Test splitting body and sources ---\n")
    
    sc = new SimpleSourcedComment("Some comment.")
    parser.splitBodySources(sc)
    assertEquals( sc.body, "Some comment", "Body extraction from value with no (...)")
    assertEquals( sc.sources, "", "Sources extraction from value with no (...)")

    sc = new SimpleSourcedComment("Some comment")
    parser.splitBodySources(sc)
    assertEquals( sc.body, "Some comment", "Body extraction from value with no (...) and without final .")

    sc = new SimpleSourcedComment("Some comment (PubMed=1234)")
    parser.splitBodySources(sc)
    assertEquals( sc.body, "Some comment", "Body extraction from value with (...) at the end of value")
    assertEquals( sc.sources, "PubMed=1234", "SrcList extraction from value with (...) at the end of value")

    sc = new SimpleSourcedComment("Some (PubMed=0000) comment (PubMed=1234)")
    parser.splitBodySources(sc)
    assertEquals( sc.body, "Some (PubMed=0000) comment", "Body extraction from value with (...) in the middle of the value")
    assertEquals( sc.sources, "PubMed=1234", "SrcList extraction from value with (...) in the middle of the value")

    sc = new SimpleSourcedComment("Some comment (with some embedded (PubMed=1234))")
    parser.splitBodySources(sc)
    assertEquals( sc.body, "Some comment", "Body extraction from value with embedded (...) at the end of the value")
    assertEquals( sc.sources, "with some embedded (PubMed=1234)", "Sources extraction from value with embedded (...) at the end of the value")

    println("\n--- Test parsing sources ---\n")

    sc = new SimpleSourcedComment("Some comment (from autologous cell lines 6CFSMEo-; CFSMEo-)")
    parser.splitBodySources(sc)
    parser.parseSources(sc)
    assertEquals(sc.status, SOURCES_STATUS.ALL_VALID, "Comment with misc sources ALL valid")
    //println(sc)

    sc = new SimpleSourcedComment("Some comment (from autologous cell lines 6CFSMEo-; turlu)")
    parser.splitBodySources(sc)
    parser.parseSources(sc)
    assertEquals(sc.status, SOURCES_STATUS.SOME_VALID, "Comment with misc SOME sources valid")
    
    sc = new SimpleSourcedComment("Some comment (from autologous cell lines tutu; turlu)")
    parser.splitBodySources(sc)
    parser.parseSources(sc)
    assertEquals(sc.status, SOURCES_STATUS.NONE_VALID, "Comment with NO misc sources valid")
    
    sc = new SimpleSourcedComment("Some comment (from autologous cell line tutu)")
    parser.splitBodySources(sc)
    parser.parseSources(sc)
    assertEquals(sc.status, SOURCES_STATUS.NONE_VALID, "Comment with NO misc sources valid")
    
    sc = new SimpleSourcedComment("Some comment (from autologous cell line 6CFSMEo-)")
    parser.splitBodySources(sc)
    parser.parseSources(sc)
    assertEquals(sc.status, SOURCES_STATUS.ALL_VALID, "Comment with ALL misc sources valid")
    
    sc = new SimpleSourcedComment("Some comment (from parent cell line)")
    parser.splitBodySources(sc)
    parser.parseSources(sc, cellLineId = "1.2B4")
    assertEquals(sc.status, SOURCES_STATUS.ALL_VALID, "Status of a value with a valid parent cell line source")
    assertEquals(sc.sources, "from parent cell line HuP-T3", "Modified sources of a value with a valid parent cell line")
    assertEquals(sc.xreflist.size, 1,  "xref created from a value with a valid parent cell line source")

    sc = new SimpleSourcedComment("Some comment (PubMed=1234; ATCC=2345; ECACC; Codex BioSolutions)")
    parser.splitBodySources(sc)
    parser.parseSources(sc)
    assertEquals(sc.status, SOURCES_STATUS.ALL_VALID, "Status of comment with 4 / 4 valid sources")
    assertEquals(sc.orglist.size, 2, "Organization list of comment with 4 valid sources")
    assertEquals(sc.xreflist.size, 1, "Xref list of comment with 4 / 4 valid sources")
    assertEquals(sc.publist.size, 1, "Publication list of comment with 4 / 4 valid sources")
    //println(sc)

    sc = new SimpleSourcedComment("Some comment (PubMed=1234; ATCC=2345; dkjsdkjdksdj; Codex BioSolutions)")
    parser.splitBodySources(sc)
    parser.parseSources(sc)
    assertEquals(sc.status, SOURCES_STATUS.SOME_VALID, "Status of comment with 3 / 4 valid sources")

    sc = new SimpleSourcedComment("Some comment (XXXPubMed=1234; XXXATCC=2345)")
    parser.splitBodySources(sc)
    parser.parseSources(sc)
    assertEquals(sc.status, SOURCES_STATUS.NONE_VALID, "Status of comment with 0 / 2 valid sources")
    assertEquals(sc.getSourceCount(), 0, "Count our sources in value with 0 / 2 valid sources")
    //println(sc)

    println("\n--- Test everything together ---\n")

    sc = parser.parse("My (detailed) comment (PubMed=12345; ECACC)", "someId")
    assertEquals(sc.status, SOURCES_STATUS.ALL_VALID, "Status of comment with 2 / 2 valid sources")
    assertEquals(sc.getSourceCount(), 2, "Count our sources in value with 2 / 2 valid sources")

    sc = parser.parse("My comment (Direct_author_submission)", "someId")
    assertEquals(sc.status, SOURCES_STATUS.IGNORED, "Status of comment with direct author submission as source")
    assertEquals(sc.getSourceCount(), 0, "Count of sources in value when direct author submission is the source")
    //println(sc)

    sc = parser.parse("My comment (from inference of genome of donor)", "someId")
    assertEquals(sc.status, SOURCES_STATUS.IGNORED, "Status of comment with 'from inference of ...' as source")
    assertEquals(sc.getSourceCount(), 0, "Count of sources in value when 'from inference of ...' is the source")
    //println(sc)
    
    sc = parser.parse("Some comment.", "someId")
    assertEquals( sc.body, "Some comment", "Body extraction from value with no (...)")
    assertEquals( sc.sources, "", "Sources extraction from value with no (...)")
    assertEquals(sc.status, SOURCES_STATUS.NONE, "Status of comment with no (...)")
    //println(sc)


    println("\nEnd")



  }

  def assertTrue(expression: Boolean, label:String): Unit = {
    val status = if (expression) "OK   " else "ERROR"
    println(s"${status} : ${label}")
  }

  def assertEquals(actual: Any, expected: Any, label:String): Unit = {
    if (actual == expected) {
      println(s"OK    : ${label}")
    } else  {
      println(s"ERROR : ${label}")
      println(s"=>    : expected '${expected}' but got '${actual}'")
    }
  }

}


