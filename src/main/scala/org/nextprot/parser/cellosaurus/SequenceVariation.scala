package org.nextprot.parser.cellosaurus

import scala.io.Source
import scala.collection.mutable.Map
import scala.xml._


class SequenceVariation(
    val cl_ac: String,
    val vartyp: String,
    val mutyp: String,
    val zygosity: String,
    var text: String,
    val sourcedComment: SimpleSourcedComment
) {
  var varXreflist = List[DbXref]()
  var srcOrglist = List[STsource]()
  var srcPublist = List[PBsource]()
  var srcXreflist = List[XRsource]()
  var ac2 = ""
  var db2 = ""
  var geneName2 = ""
  var mutdesc = ""
  var varnote = ""

  init
  def init = {
    val toklist = text.split("; ")
    val db = toklist(1)
    val ac = toklist(2)
    var geneName = toklist(3)
    var srctok = ""
    if (
      toklist.size > 5 && vartyp != "Gene amplification" && vartyp != "Gene deletion"
    ) mutdesc = toklist(5)
    if (text.contains(" + ")) { // Gene fusion, there is a second dbref: prepare it
      // like  CC   Sequence variation: Gene fusion; HGNC; 3446; ERG + HGNC; 3508; EWSR1; Name(s)=EWSR1-ERG, EWS-ERG; Note=In frame (PubMed=8162068).
      geneName = geneName.split(" ")(0)
      ac2 = toklist(4)
      geneName2 = toklist(5).split("\\.")(0)
      mutdesc = toklist(6).split("=")(1)
      if (mutdesc.contains(" (")) mutdesc = mutdesc.split(" \\(")(0)
    }
  
    toklist.foreach(token => {
      if (token.startsWith("Note=")) {
        varnote = token.substring(5).trim()
        if (varnote.contains(" (")) varnote = varnote.split(" \\(")(0)
      }
      // CC   Sequence variation: Mutation; HGNC; 11730; TERT; Simple; p.Arg631Gln (c.1892G>A); ClinVar=VCV000029899; Zygosity=Unspecified (Direct_author_submission).
      else if (token.startsWith("ClinVar=") || token.startsWith("dbSNP=")) {
        db2 = token.split("=")(0)
        varXreflist = new DbXref(
          db = db2,
          ac = token.split("=")(1).split(" \\(")(0)
        ) :: varXreflist
      }
    })

    // 
    if (! CelloParser.ok_seqvardblist.contains(db)) {
      Console.err.println("FATAL error, Invalid db in Sequence variation: '" + db + "' , see details above.")
      System.exit(1)
    }

    varXreflist = new DbXref(db, ac, label = geneName) :: varXreflist
    if (ac2 != "") {
      varXreflist = new DbXref(db, ac2, geneName2) :: varXreflist
      varXreflist = varXreflist.reverse
    }

    srcPublist = sourcedComment.publist
    srcXreflist = sourcedComment.xreflist
    srcOrglist = sourcedComment.orglist

  }

  def toXML =
    <sequence-variation 
      variation-type={vartyp}
  	  zygosity-type={ if (zygosity != "") { zygosity } else null }
    	mutation-type={ if (mutyp != "") { mutyp } else null } >

      {
      if (mutdesc != "")
        <mutation-description>{mutdesc}</mutation-description>
      else
        Null
      }
      {
      if (varnote != "")
        <variation-note>{varnote}</variation-note>
      else 
        Null
      }
      <xref-list>
        {varXreflist.map(_.toXML)}
      </xref-list>
      {
      if (srcOrglist.size > 0 || srcPublist.size > 0 || srcXreflist.size > 0) 
        <source-list>        
        { if (srcXreflist.size > 0) srcXreflist.map(_.toXML) else Null }
        { if (srcPublist.size > 0) srcPublist.map(_.toXML) else Null }
        { if (srcOrglist.size > 0) srcOrglist.map(_.toXML) else Null }
        </source-list>        
      else 
        Null
      }
</sequence-variation>
}

