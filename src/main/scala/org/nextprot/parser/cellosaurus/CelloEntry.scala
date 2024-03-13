package org.nextprot.parser.cellosaurus

import scala.xml._
import org.nextprot.parser.cellosaurus._
import org.nextprot.parser.cellosaurus.Comment

class CelloEntry(
    val ac: String,
    val oldacs: List[OldAc],
    val id: String,
    val synonyms: List[Synonym],
    val credat: String,
    val upddat: String,
    val eversion: String,
    val category: String,
    val sex: String,
    val age: String,
    val dbrefs: List[DbXref],
    var comments: List[Comment],
    val webpages: List[WebPage],
    val diseases: List[DbXref],
    val species: List[DbXref],
    val origin: List[DbXref],
    val derived: List[DbXref],
    val publis: List[PubliRef],
    val strSources: List[STsource],
    val strSourceRefs: List[PubliRef],
    val strmarkers: List[Strmarker],
    val reglist: List[Registration],
    val hlalists: List[HLAlistwithSource],
    val seqvarlist: List[SequenceVariation],
    val genomeAncestry: PopulistwithSource,
    val misspellinglist: List[Misspelling],
    val doublingTimeList: List[DoublingTime],
    val knockoutList: List[Knockout],
    val msiList: List[Msi],
    val mabisoList: List[Mabiso],
    val mabtarList: List[Mabtar],
    val derivedFromSiteList: List[DerivedFromSite],
    val cellType: CellType,
    val transformantList: List[Transformant],
    val resistanceList: List[Resistance]
) {

  // pam
  def getOiGroup(): String = {
    var result = List[String]()
    if (origin.size > 0) {
      origin.foreach(el => { result = el.ac :: result })
      if (result.size > 0) result = ac :: result
    }
    val sorted = result.sortWith((s: String, t: String) => { s < t })
    return sorted.mkString(",")
  }

  def toOiEntry(): OiEntry = {
    var species = List[String]()
    var populations = List[String]()
    var subspecies = List[String]()
    this.species.foreach(el => { species = el.ac :: species })
    this.comments.foreach(el => {
      if (el.category == "Population") { populations = el.text :: populations }
    })
    this.comments.foreach(el => {
      if (el.category == "Breed/subspecies") {
        subspecies = el.text :: subspecies
      }
    })
    val spc =
      species.sortWith((s: String, t: String) => { s < t }).mkString(",")
    val pop =
      populations.sortWith((s: String, t: String) => { s < t }).mkString(",")
    val sub =
      subspecies.sortWith((s: String, t: String) => { s < t }).mkString(",")
    val oiEntry = new OiEntry(
      ac = this.ac,
      sex = this.sex,
      species = spc,
      population = pop,
      subspecies = sub
    )
    return oiEntry
  }

  

  def hasNormalCCTopics(): Boolean = {
    return comments
      .filterNot(cc => { CelloParser.specialCCTopics.contains(cc.category) })
      .size > 0
  }


  def aclist_toXML(ac:String, oldacs: List[OldAc]) : xml.Elem = {
    val buf = new xml.NodeBuffer
    buf += <accession type="primary">{ac}</accession>
    oldacs.foreach( x => { buf += x.toXML })
    val result = <accession-list>{buf}</accession-list>
    return result
  }


  def namelist_toXML(id: String, syns: List[Synonym]): xml.Elem = {
    val buf = new xml.NodeBuffer
    buf += <name type="identifier">{id}</name>
    syns.foreach( s => {buf += s.toXML })
    var list = <name-list>{buf}</name-list>
    return list
  }

  def toXML =

    <cell-line 
      category={category} 
      created={credat} 
      last-updated={upddat} 
      entry-version={eversion} 
      sex={if (sex != "") sex else null} 
      age={if (age != "") age else null} >

      {aclist_toXML(ac, oldacs)}

      {namelist_toXML(id, synonyms)}

      {
      if (hasNormalCCTopics())
         <comment-list>{comments.map(_.toXML)}</comment-list>
       else 
        Null
      }

      {
      if (strSources.size > 0 || strSourceRefs.size > 0)
        <str-list>
          <str-sources>
            {
            if (strSourceRefs.size > 0)
              <reference-list>{strSourceRefs.map(_.toXML)}</reference-list>
            else 
              Null
            }
            {
            if (strSources.size > 0)
                <source-list>{strSources.map(_.toXML)}</source-list>
            else 
              Null
            }
          </str-sources>
          <marker-list>{strmarkers.map(_.toXML)}</marker-list>
        </str-list>
      else
        Null
      }


      {
      if (diseases.size > 0)
        <disease-list>{diseases.map(_.toXML)}</disease-list>
      else
        Null
      }

      <species-list>{species.map(_.toXML)}</species-list>

      {
      if (derived.size > 0)
        <derived-from>{derived.map(_.toXML)}</derived-from>
      else
        Null
      }
      
      {
      if (origin.size > 0)
        <same-origin-as>{origin.map(_.toXML)}</same-origin-as>
      else
        Null
      }

      {
      if (webpages.size > 0)
        <web-page-list>{webpages.map(_.toXML)}</web-page-list>
      else
        Null
      }
      
      {
      if (publis.size > 0)
        <reference-list>{publis.map(_.toXML)}</reference-list>
      else
        Null
      }

      {
      if (hlalists.size > 0)
        <hla-typing-list>{hlalists.map(_.toXML)}</hla-typing-list>
      else
        Null
      }

      {
      if (doublingTimeList.size > 0)
        <doubling-time-list>{doublingTimeList.map(_.toXML)}</doubling-time-list>
      else
        Null
      }

      {
      if (knockoutList.size > 0)
        <knockout-cell-list>{knockoutList.map(_.toXML)}</knockout-cell-list>
      else
        Null
      }

      {
      if (msiList.size > 0)
        <microsatellite-instability-list>{msiList.map(_.toXML)}</microsatellite-instability-list>
      else
        Null
      }

      {
      if (mabisoList.size > 0)
        <monoclonal-antibody-isotype-list>{mabisoList.map(_.toXML)}</monoclonal-antibody-isotype-list>
      else
        Null
      }

      {
      if (mabtarList.size > 0)
        <monoclonal-antibody-target-list>{mabtarList.map(_.toXML)}</monoclonal-antibody-target-list>
      else
        Null
      }

      {
      if (derivedFromSiteList.size > 0)
        <derived-from-site-list>{derivedFromSiteList.map(_.toXML)}</derived-from-site-list>
      else
        Null
      }
  
      {
      if (cellType != null) 
        cellType.toXML
      else
        Null
      }

      {
      if (genomeAncestry != null) 
        genomeAncestry.toXML
      else
        Null
      }

      {
      if (transformantList.size > 0)
        <transformant-list>{transformantList.map(_.toXML)}</transformant-list>
      else
        Null
      }

      {
      if (resistanceList.size > 0)
        <resistance-list>{resistanceList.map(_.toXML)}</resistance-list>
      else
        Null
      }

      {
      if (misspellinglist.size > 0)
        <misspelling-list>{misspellinglist.map(_.toXML)}</misspelling-list>
      else
        Null
      }
  
      {
      if (reglist.size > 0)
        <registration-list>{reglist.map(_.toXML)}</registration-list>
      else
        Null
      }

      {
      if (seqvarlist.size > 0)
        <sequence-variation-list>{seqvarlist.map(_.toXML)}</sequence-variation-list>
      else
        Null
      }
  
      {
      if (dbrefs.size > 0)
        <xref-list>{dbrefs.map(_.toXML)}</xref-list>
      else 
        Null
      }
    </cell-line>


  def toOBO = {
    var oboEntryString = "\n[Term]\n"
    var currcomment = ""

    oboEntryString += "id: " + ac + "\n"
    // oboEntryString += "name: " + id.replace('{','(').replace('}',')') + "\n" // alain: obo doesn't like curly brackets
    oboEntryString += "name:" + CelloParser.escape_chars_for_obo(
      id
    ) + "\n" // pam:   escape chars for obo
    synonyms.foreach(syno => { oboEntryString += syno.toOBO })
    oboEntryString += "subset: " + category.replace(' ', '_') + "\n"
    if (sex != "") oboEntryString += "subset: " + sex.replace(' ', '_') + "\n"
    dbrefs.foreach(dbref => { oboEntryString += dbref.toOBO })
    publis.foreach(publi => { oboEntryString += publi.toOBO })
    webpages.foreach(webpage => { oboEntryString += webpage.toOBO })
    diseases.foreach(disease => { oboEntryString += disease.toOBO })
    species.foreach(specie => { oboEntryString += specie.toOBO })
    comments.foreach(comment => { // stick them all in one line
      if (currcomment == "") currcomment = "comment: \""
      else currcomment += " "
      currcomment += comment.toOBO
    })
    if (currcomment != "")
      oboEntryString += currcomment.replace("..", ".") + "\"\n"
    origin.foreach(origin => {
      oboEntryString += "relationship: originate_from_same_individual_as " + origin.ac + " ! " + origin.label + "\n"
    })
    derived.foreach(derivedfrom => {
      oboEntryString += "relationship: derived_from " + derivedfrom.ac + " ! " + derivedfrom.label + "\n"
    })
    // oboEntryString += "creation_date: \"" + credat +  "T00:00:00Z\"\n" // OBO format requires useless hours-minutes
    oboEntryString += "creation_date: " + credat + "T00:00:00Z\n" // OBO format requires useless hours-minutes
    oboEntryString
  }

  def updatDBrefs = { // find dbrefs associated with 'Discontinued' comments, set their property flags and remove urls
    var newCommentlist = List[Comment]()
    comments.foreach(comment => {
      if (comment.category.equals("Discontinued") && comment.text.split("; ").size > 2) { // Console.err.println(comment.text)
        val comment_parts = comment.text.split("; ")
        val discDB = comment_parts(0)
        val discAC = comment_parts(1)
        val property = comment_parts(2)
        dbrefs.foreach(dbref => {
          val db = dbref.db
          // special rule for AddexBio, see cellosaurus_xrefs.txt
          val ac = if (db.equals("AddexBio")) dbref.ac.split("/")(0) else dbref.ac
          if (ac.equals(discAC) && db.equals(discDB)) {
            dbref.discontinued = property
            dbref.final_url = "" // No url
          }
        })
      } else newCommentlist = comment :: newCommentlist
    })
    comments = newCommentlist
  }
}
