package org.nextprot.parser.cellosaurus

import scala.xml._


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
class STsource(val src: String) {
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  override def toString() :String = {
    return s"STsource($src)"
  }
  def toXML =
    <source>{src}</source>
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
class XRsource(val text: String, val xref: DbXref) {
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  def toXML = {
    <source>
      {if (text != null && text.size > 0) {text} else Null }
      {xref.toXML}
    </source>
  }
  override def toString() :String = {
    return s"XRsource('$text', $xref)"
  }

}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
class PBsource(val db_ac: String) {
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  val parts = db_ac.split("=")
  val db : String = parts(0)
  // case for a few ST allele sources (pmid concat with some sub identifier)
  var ac : String = parts(1)
  var subid : String = null
  if (db=="PubMed" && ac.contains("_")) {
    ac = ac.split("_")(0)
    val pos = parts(1).indexOf("_") 
    subid = parts(1).substring(pos + 1)
  }
  val publiRef = new PubliRef(db + "=" + ac)

  def toXML = {
    <source>
      {if (subid != null && subid.size > 0) {subid} else Null }
      {publiRef.toXML}
    </source>
  }
  override def toString() :String = {
    return s"PBsource($publiRef)"
  }

}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
class PubliRef(val db_ac: String) {
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  def toXML =
    <reference resource-internal-ref={db_ac}/>

  def toOBO = {
    var drline = "xref: "
    if (db_ac.contains("Patent")) // replace with url
      drline += "http://www.google.com/patents/" + db_ac.split("=")(1) + "\n"
    else
      drline += db_ac + "\n"
    drline.replace('=', ':')
  }

  override def toString() :String = {
    return s"PubliRef($db_ac)"
  }
}


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
class DbXref(
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    val db: String,
    val ac: String,
    var label: String = "",
    var discontinued: String = "",
) {

  val category = DbXrefInfo.getCat(db)
  val url = DbXrefInfo.getUrl(db)
  var final_url = url

  init
  def init = { // prepare final url from template and accession
    if (url.contains("%s")) {
      // Deal with a few exceptions...
      // if (db.equals("BTO")) {
      //   // BTO:  %s is the numerical part of the BTO:nnnnnnn identifier
      //   final_url = url.replace("%s", ac.substring(4)) 
      if (db.equals("CGH-DB")) {
        // CGH-DB: Note: %s and %t are respectively the values before and after the dash in the DR line.
        final_url = url.replace("%s", ac.split("-")(0)).replace("%t", ac.split("-")(1)) 
      } else if (db.equals("AddexBio")) {
        // AddexBio: Note: %s is the value after the slash "/" in the DR line.
        final_url = url.replace("%s", ac.split("/")(1)) 
      } else if (db.equals("TKG")) { 
        // TKG: Note: n% is the second digit of the cell line AC and %s is the cell line AC without the 'TKG'
        val digits = ac.split(" ")(1)
        final_url = url.replace("%n", digits.substring(1, 2)).replace("%s", digits); // wtf! digits.substring(1,1) is empty!!!
      } else {
        // General form
        final_url = url.replace("%s", ac)
      }
    } else {
      final_url = "" // for xrefs like ICLC
    }
  }

  override def toString() :String = {
    return s"DbXref($db=$ac)"
  }

  def toXML =
    <xref database={db} category={category} accession={ac}>
      {
      if (label != "")
        <label>{label}</label>
      else
        Null
      }
      {
      if (discontinued != "")
        <discontinued>{discontinued}</discontinued>
      else
        Null
      }
      {
      if (final_url != "")
        <url>{scala.xml.PCData(final_url)}</url>
      else
        Null
      }
    </xref>


  def toOBO = {
    var drline = ""
    if (db.equals("Cellosaurus")) {
      drline = "relationship: originate_from_same_individual_as " + ac + " ! " + label + "\n"
    } else {
      drline = "xref: " + db + ":" + ac + "\n"
      if (label != null && label.length>0) {
        drline = "xref: " + db + ":" + ac + " ! " + label + "\n"
      }
    }
    drline
  }

}


