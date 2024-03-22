package org.nextprot.parser.cellosaurus

import scala.io.Source
import scala.collection.mutable.Map


/*
  The following parsers are unused but their config (topic, pattern action list and source validation) 
  gives interseting information about the data in each topic they were conceived to parse
*/

// unused
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

// unused
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

// unused
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

// unused
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


// unused
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

// unused
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

// unused
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


// unused
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

// unused
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
