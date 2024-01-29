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
    val _db: String,
    val _ac: String,
    var _property: String,
    val _entryCategory: String
) {

  val _category = DbXrefInfo.getCat(_db)
  val _url = DbXrefInfo.getUrl(_db)
  var final_url = _url
  var propname = "gene/protein designation" // default value, overriden in comments.updatDBrefs for discontinued cell lines

  init
  def init = { // prepare final url from template and accession
    if (_url.contains("%s")) {
      // Deal with a few exceptions...
      if (_db.equals("BTO"))
        final_url = _url.replace(
          "%s",
          _ac.substring(4)
        ) // BTO:  %s is the numerical part of the BTO:nnnnnnn identifier
      else if (_db.equals("CGH-DB"))
        final_url = _url
          .replace("%s", _ac.split("-")(0))
          .replace(
            "%t",
            _ac.split("-")(1)
          ) // CGH-DB: Note: %s and %t are respectively the values before and after the dash in the DR line.
      else if (_db.equals("AddexBio"))
        final_url = _url.replace(
          "%s",
          _ac.split("/")(1)
        ) // AddexBio: Note: %s is the value after the slash "/" in the DR line.
      else if (_db.equals("ECACC")) {
        var urlCategory = ""
        var modifiedUrl = ""
        var collection = ""
        if (_entryCategory.equals("Hybridoma")) {
          urlCategory = "hybridoma"; collection = "ecacc_gc";
        } else if (_entryCategory.equals("Induced pluripotent stem cell")) {
          urlCategory = "ipsc"; collection = "ecacc_ipsc";
        } else if (_entryCategory.startsWith("chromosomal")) {
          urlCategory = "humangeneticca"; collection = "ecacc_hgc";
        } else if (_entryCategory.startsWith("Neurone Disease (MND)")) {
          urlCategory = "diseaseandnormalcohortcollections";
          collection = "ecacc_mnd";
        } else if (_entryCategory.startsWith("randomly")) {
          urlCategory = "humanrandomcontrol"; collection = "ecacc_hrc";
        }
        if (urlCategory != "")
          modifiedUrl = _url
            .replace("generalcell", urlCategory)
            .split("&")(0) + "&collection=" + collection
        else modifiedUrl = _url
        final_url = modifiedUrl.replace("%s", _ac)
      } else if (_db.equals("TKG")) { // TKG: Note: n% is the second digit of the cell line AC and %s is the cell line AC without the 'TKG'
        val digits = _ac.split(" ")(1)
        final_url = _url
          .replace("%n", digits.substring(1, 2))
          .replace("%s", digits); // wtf! digits.substring(1,1) is empty!!!
      } else // General form
        final_url = _url.replace("%s", _ac)
    } else
      final_url = "" // for xrefs like ICLC
  }

  override def toString() :String = {
    return s"DbXref($_db=$_ac)"
  }

  def toXML =
    <xref database={_db} category={_category} accession={_ac}>
      {
      if (_property != "")
        <property-list>
            <property name={propname} value={_property}/>
          </property-list>
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
    val drline = "xref: " + _db + ":" + _ac + "\n"
    drline
  }
}


