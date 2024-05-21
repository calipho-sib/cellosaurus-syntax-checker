package org.nextprot.parser.cellosaurus

import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex
import scala.xml._
import org.nextprot.parser.cellosaurus._

class Author(val name: String) {
  def toXML =
    <person name={name}/>
}

class CelloPublication(
    val year: String,
    val journal: String,
    val pubtype: String,
    val volume: String,
    val issn13: String,
    val firstpage: String,
    val lastpage: String,
    val publisher: String,
    val institute: String,
    val city: String,
    val country: String,
    val internal_id: String,
    val title: String,
    val booktitle: String,
    val conftitle: String,
    val serietitle: String,
    val doctitle: String,
    val authors: List[Author],
    val editors: List[Author],
    val dbrefs: List[DbXref]
) {

  def toXML =
    <publication 
      date={year} 
      type={pubtype} 
      journal-name={ if (journal != "") journal else null } 
      book-title={ if (booktitle != "") booktitle else null } 
      conference-title={ if (conftitle != "") conftitle else null } 
      serie-title={ if (serietitle != "") serietitle else null } 
      document-title={ if (doctitle != "") doctitle else null } 
      volume={ if (volume != "") volume else null } 
      issn-13={ if (issn13 != "") issn13 else null } 
      first-page={ if (firstpage != "") firstpage else null } 
      last-page={ if (lastpage != "") lastpage else null } 
      publisher={ if (publisher != "") publisher else null } 
      institution={ if (institute != "") institute else null } 
      city={ if (city != "") city else null } 
      country={ if (country != "") country else null } 
      internal-id={internal_id} >

      <title>{scala.xml.PCData(title)}</title>
      {
      if (editors.size > 0)
        <editor-list>{editors.map(_.toXML)}</editor-list>
      else
        Null
      }
      <author-list>{authors.map(_.toXML)}</author-list>
      {
      if (dbrefs.size > 0)
        <xref-list>{dbrefs.map(_.toXML)}</xref-list>
      else
        Null
      }
    </publication>
}


object PublicationBuilder {

  var authorlist = ArrayBuffer[String]()
  var editorlist = ArrayBuffer[String]()
  var xreflist = ArrayBuffer[String]()
  var pubXreflist = List[DbXref]()
  var celloPubAuthorlist = List[Author]()
  var celloPubEditorlist = List[Author]()
  var linedata = ""
  var journal = ""
  var volume = ""
  var issn13 = ""
  var year = ""
  var firstpage = ""
  var lastpage = ""
  var title = ""
  var booktitle = ""
  var conftitle = ""
  var serietitle = ""
  var doctitle = ""
  var publisher = ""
  var city = ""
  var country = ""
  var institute = ""
  var pubtype = ""
  var internalId = ""

  val editorregexp =new Regex("[A-Z][a-z]+.* .*[A-Z]\\.$") // eg: Saunders S.J., Gruev B., Park J.-G.

  def reinit() = {
    authorlist = ArrayBuffer[String]()
    editorlist = ArrayBuffer[String]()
    xreflist = ArrayBuffer[String]()
    pubXreflist = List[DbXref]()
    celloPubAuthorlist = List[Author]()
    celloPubEditorlist = List[Author]()
    linedata = ""
    journal = ""
    volume = ""
    issn13 = ""
    year = ""
    firstpage = ""
    lastpage = ""
    title = ""
    booktitle = ""
    conftitle = ""
    serietitle = ""
    doctitle = ""
    publisher = ""
    city = ""
    country = ""
    institute = ""
    pubtype = ""
    internalId = ""
  }

  def parseCommonFields(elems: Array[String]) = {
    val range = elems(1).substring(3).split("-") // pp.1-234 => Array(1,234) 
    firstpage = range(0)
    lastpage = range(1)
    institute = elems(2)
    city = elems(3)
    val country_year = elems(4).split(""" \(""") // USA (1977)
    country = country_year(0)
    year = country_year(1).split("""\)""")(0)
  }

  def toCellopublication(publiFlatentry: ArrayBuffer[String]): CelloPublication = {
    reinit()
    // Parse cellosaurus ref file to build the Cellopublication class instances
    for (line <- publiFlatentry) {
      if (line.size > 5) linedata = line.substring(5).trim()
      if (line.startsWith("RX   ")) { // The line occurs only once per entry in cellosaurus refs
        val xref_arr : Array[String] = linedata.substring(0, linedata.size - 1).split("; ") 
        for (xref <- xref_arr) xreflist.append(xref)
        internalId = xreflist(0) // Pubmed comes first when PubMed + DOI
      } else if (line.startsWith("RA   "))
        val auth_arr = linedata.substring(0, linedata.size - 1).split(", ")
        for (auth <- auth_arr) authorlist.append(auth)
      else if (line.startsWith("RG   "))
        val rg_arr = linedata.split(";")
        for (rg <-rg_arr) authorlist.append(rg)
      else if (line.startsWith("RT   ")) {
        if (title == "") title = linedata.trim()
        else title += " " + linedata.trim() // Titles can span several lines
      } else if (line.startsWith("RL   ")) {
        try {
            if (linedata.startsWith("Patent")) { // RL   Patent number CN1061093C, 24-Jan-2001.
            year = linedata.substring(
                linedata.size - 12,
                linedata.size - 1
            ) // keep day and month
            pubtype = "patent"
            } else if (linedata.startsWith("Thesis")) { // RL   Thesis PhD (1971), Erasmus University Rotterdam, Netherlands.
            year = linedata.split("[\\(||\\)]")(1)
            val rltokens = linedata.split(", ")
            institute = rltokens(rltokens.size - 2)
            country = rltokens(rltokens.size - 1).split("\\.")(0)
            pubtype =
                "t" + linedata
                .split(" \\(")(0)
                .substring(1) // t + hesis + level (phd, ms, ..)

            } else if (linedata.startsWith("(In book) ")) {
                pubtype = "book"
                val elems = linedata.substring(10).split("; ")
                issn13 = elems(0)
                parseCommonFields(elems)

            } else if (linedata.startsWith("(In book chapter) ")) {
                pubtype = "book chapter"
                val elems = linedata.substring(18).split("; ")
                booktitle = elems(0)
                if (elems(1).endsWith("(eds.)")) {
                    val eds = elems(1).dropRight(7).split(", ")
                    for (ed <- eds) if (ed != "None") editorlist.append(ed)
                }
                val range = elems(2).substring(3).split("-") // pp.1-234 => Array(1,234) 
                firstpage = range(0)
                lastpage = range(1)
                publisher = elems(3)
                city = elems(4)
                val country_year = elems(5).split(""" \(""") // USA (1977)
                country = country_year(0)
                year = country_year(1).split("""\)""")(0)

            } else if (linedata.startsWith("(In conference) ")) {
                pubtype = "conference"
                val elems = linedata.substring(16).split("; ")
                conftitle = elems(0)
                parseCommonFields(elems)

            } else if (linedata.startsWith("(In technical document) ")) {
                pubtype = "technical document"
                val elems = linedata.substring(24).split("; ")
                serietitle = elems(0)
                parseCommonFields(elems)

            } else if (linedata.startsWith("(In misc. document) ")) {
                pubtype = "miscellaneous document"
                val elems = linedata.substring(20).split("; ")
                doctitle = elems(0)
                parseCommonFields(elems)

            } else { // General journal RL eg: RL   Naunyn Schmiedebergs Arch. Pharmacol. 352:662-669(1995).
            year = linedata.split(":")(1).split("[\\(||\\)]")(1)
            val rltokens = linedata.split(":")(0).split(" ")
            pubtype = "article"
            journal = rltokens.dropRight(1).mkString(" ")
            if (journal.contains("Suppl")) { // add the Suppl part to volume, eg: J. Physiol. Pharmacol. 60 Suppl.
                var digitpos = 0
                val matchlist =
                new Regex("[0-9]").findAllIn(journal).matchData.toList
                if (matchlist.size != 0)
                digitpos = matchlist(0).start
                else
                digitpos = journal.indexOf("Suppl")
                volume = journal.substring(digitpos)
                journal = journal.substring(0, digitpos - 1)
            }

            if (volume == "")
                volume = rltokens(rltokens.size - 1)
            else // Add to suppl part
                volume = volume + " " + rltokens(rltokens.size - 1)

            val pages = linedata.split(":")(1).split("\\(")(0).split("-")
            firstpage = pages(0)
            if (pages.size > 1)
                lastpage = pages(1)
            else // like RL   Cancer Genet. Cytogenet. 84:142 Abs. A10(1995).
                lastpage = firstpage
            }
        } catch {
            case e: Exception => {throw new Exception(s"ERROR while parsing: ${line}") } // handled earlier
        }


      } else if (line.startsWith("//")) { // Record complete
        authorlist.foreach(author => {
          celloPubAuthorlist = new Author(name = author) :: celloPubAuthorlist
        })
        editorlist.foreach(editor => {
          celloPubEditorlist = new Author(name = editor) :: celloPubEditorlist
        })
        xreflist.foreach(xref => {
          val db = xref.split("=")(0)
          pubXreflist = new DbXref(db = db, ac = xref.split("=")(1))  :: pubXreflist
        })
      }
    }
    // .reverse in author lists to recover original order
    val publiEntry = new CelloPublication(
      year = year,
      journal = journal,
      pubtype = pubtype,
      volume = volume,
      issn13 = issn13,
      firstpage = firstpage,
      lastpage = lastpage,
      publisher = publisher,
      institute = institute,
      city = city,
      country = country,
      internal_id = internalId,
      title = title.split("\"")(1),
      booktitle = booktitle,
      conftitle = conftitle,
      serietitle = serietitle,
      doctitle = doctitle,
      authors = celloPubAuthorlist.reverse,
      editors = celloPubEditorlist.reverse,
      dbrefs = pubXreflist
    )
    publiEntry
  }


  def main(args: Array[String]): Unit = {

    DbXrefInfo.load(args(0))

    val prettyXMLprinter = new scala.xml.PrettyPrinter(512, 2)
    val expected_results = expected_out.split("//\n")
    val records_in = data_in.split("//\n")
    var index = 0
    // =======================
    val check = true
    // =======================
    var err_cnt = 0
    for (rec <- records_in) {
        val fullrec = rec + "//"
        val lines = ArrayBuffer(fullrec.split("\n"):_*)
        //for (line <- lines) println("line: " + line)
        val publi = toCellopublication(lines)
        val result = prettyXMLprinter.format(publi.toXML).strip()
        if (check) {
          val expect = expected_results(index).strip()
          if (result != expect) {
            err_cnt += 1
            var idx = 0
            val explines = expect.split("\n")
            val actlines = result.split("\n")
            println("----------")
            for (exp <- explines) {
                val act = actlines(idx)
                if (exp!=act) {
                println(s"ERROR at this line, index ${index}")
                    println("expect:" + exp)
                    println("actual:" + act)
                }
                idx += 1
            }
            println("record expected:")
            println(expect)
            println("record computed:")
            println(result)
            println("----------")
          }
        } else {
            println(result)
        }
        println("//")
        index += 1
    }
    if (check) {
        if (err_cnt==0) {
            println(s"run ${records_in.size} tests: All OK")
        } else {
            println(s"run ${records_in.size} tests: ${err_cnt} ERROR(s)")
        }
    }



  }

  val data_in = """
RX   PubMed=2868604; DOI=10.1530/acta.0.1110054;
RA   Brandi M.L., Rotella C.M., Zonefrati R., Toccafondi R., Aloj S.M.;
RT   "Loss of adrenergic regulation of cAMP production in the FRTL-5 cell
RT   line.";
RL   Acta Endocrinol. 111:54-61(1986).
//
RX   CelloPub=CLPUB00069;
RA   Kishi K., Hanano M., Hirosawa H., Koike T., Sakai C., Aoyagi Y.,
RA   Sanada M., Moiyama Y., Shibata A.;
RT   "Characterization of a new human granulo-monocytic leukemic cell
RT   line.";
RL   Acta Haematol. Jpn. 46:122-122(1983).
//
RX   CelloPub=CLPUB00156;
RA   Leung J.W.;
RT   "The study of a human rhabdomyosarcoma cell line (HUS-2).";
RL   Thesis PhD (1974), University of Kansas, United States.
//
RX   CelloPub=CLPUB00262;
RA   Gold M.;
RT   "A conspiracy of cells. One woman's immortal legacy and the medical
RT   scandal it caused.";
RL   (In book) ISBN 9780887060991; pp.1-171; State University of New York Press; Albany; USA (1986).
//
RX   DOI=10.1007/978-2-8178-0765-2_34;
RA   King W.W.-K., Lam P.K., Li A.K.-C.;
RT   "A model for chemosentivity testing using in vitro MTT assay on a
RT   human squamous carcinoma tongue cell line.";
RL   (In book chapter) Cancer treatment an update; Banzet P., Holland J.F., Khayat D., Weil M. (eds.); pp.179-184; Springer; Paris; France (1994).
**   PDF=Yes
//
RX   CelloPub=CLPUB00680;
RA   Mikhailova I.N., Baryshnikov A.Y., Morozova L.F., Burova O.S.,
RA   Shubina I.Z., Kovalevsky D.A., Voronina E.S., Treshalina H.M.,
RA   Lushnikova A.A., Tsyganova I.V., Mazurenko N.N.;
RT   "Human skin melanoma cell lines collection.";
RL   (In book chapter) Management of malignant melanoma; None (eds.); pp.6.1-6.6; SM Group; Dover; USA (2016).
**   PDF=Yes
//
RX   CelloPub=CLPUB00758;
RA   Sykes G.R.;
RT   "Mitochondria-targeting PCR and CO1 barcode sequence analyses as
RT   alternatives to isoenzymology.";
RL   (In conference) Proceedings of the International Society for BioProcess Technology (ISBioTech) 4th fall meeting; pp.1-27; International Society for BioProcess Technology; Virginia Beach; USA (2016).
//
RX   DOI=10.13140/RG.2.2.17700.88960;
RA   Briley A., Shapiro B.A.;
RT   "hTERT-immortalized and primary keratinocytes differentiate into
RT   epidermal structures in 3D organotypic culture.";
RL   (In technical document) ATCC application note 17; pp.1-4; ATCC; Manassas; USA (2016).
//
RX   CelloPub=CLPUB00597;
RG   National Institute on Aging;
RT   "1994 catalog of cell lines. NIA Aging Cell Repository.";
RL   (In misc. document) Institute for Medical Research (Camden, N.J.); pp.1-351; National Institutes of Health; Bethesda; USA (1994).
//
"""

  val expected_out = """
<publication date="1986" type="article" journal-name="Acta Endocrinol." volume="111" first-page="54" last-page="61" internal-id="PubMed=2868604">
  <title><![CDATA[Loss of adrenergic regulation of cAMP production in the FRTL-5 cell line.]]></title>
  <author-list>
    <person name="Brandi M.L."/>
    <person name="Rotella C.M."/>
    <person name="Zonefrati R."/>
    <person name="Toccafondi R."/>
    <person name="Aloj S.M."/>
  </author-list>
  <xref-list>
    <xref database="DOI" category="Reference resources" accession="10.1530/acta.0.1110054">
      <url><![CDATA[https://dx.doi.org/10.1530/acta.0.1110054]]></url>
    </xref>
    <xref database="PubMed" category="Reference resources" accession="2868604">
      <url><![CDATA[https://www.ncbi.nlm.nih.gov/pubmed/2868604]]></url>
    </xref>
  </xref-list>
</publication>
//
<publication date="1983" type="article" journal-name="Acta Haematol. Jpn." volume="46" first-page="122" last-page="122" internal-id="CelloPub=CLPUB00069">
  <title><![CDATA[Characterization of a new human granulo-monocytic leukemic cell line.]]></title>
  <author-list>
    <person name="Kishi K."/>
    <person name="Hanano M."/>
    <person name="Hirosawa H."/>
    <person name="Koike T."/>
    <person name="Sakai C."/>
    <person name="Aoyagi Y."/>
    <person name="Sanada M."/>
    <person name="Moiyama Y."/>
    <person name="Shibata A."/>
  </author-list>
  <xref-list>
    <xref database="CelloPub" category="Reference resources" accession="CLPUB00069">
      <url><![CDATA[https://www.cellosaurus.org/cellopub/CLPUB00069]]></url>
    </xref>
  </xref-list>
</publication>
//
<publication date="1974" type="thesis PhD" institution="University of Kansas" country="United States" internal-id="CelloPub=CLPUB00156">
  <title><![CDATA[The study of a human rhabdomyosarcoma cell line (HUS-2).]]></title>
  <author-list>
    <person name="Leung J.W."/>
  </author-list>
  <xref-list>
    <xref database="CelloPub" category="Reference resources" accession="CLPUB00156">
      <url><![CDATA[https://www.cellosaurus.org/cellopub/CLPUB00156]]></url>
    </xref>
  </xref-list>
</publication>
//
<publication date="1986" type="book" issn-13="ISBN 9780887060991" first-page="1" last-page="171" institution="State University of New York Press" city="Albany" country="USA" internal-id="CelloPub=CLPUB00262">
  <title><![CDATA[A conspiracy of cells. One woman's immortal legacy and the medical scandal it caused.]]></title>
  <author-list>
    <person name="Gold M."/>
  </author-list>
  <xref-list>
    <xref database="CelloPub" category="Reference resources" accession="CLPUB00262">
      <url><![CDATA[https://www.cellosaurus.org/cellopub/CLPUB00262]]></url>
    </xref>
  </xref-list>
</publication>
//
<publication date="1994" type="book chapter" book-title="Cancer treatment an update" first-page="179" last-page="184" publisher="Springer" city="Paris" country="France" internal-id="DOI=10.1007/978-2-8178-0765-2_34">
  <title><![CDATA[A model for chemosentivity testing using in vitro MTT assay on a human squamous carcinoma tongue cell line.]]></title>
  <editor-list>
    <person name="Banzet P."/>
    <person name="Holland J.F."/>
    <person name="Khayat D."/>
    <person name="Weil M."/>
  </editor-list>
  <author-list>
    <person name="King W.W.-K."/>
    <person name="Lam P.K."/>
    <person name="Li A.K.-C."/>
  </author-list>
  <xref-list>
    <xref database="DOI" category="Reference resources" accession="10.1007/978-2-8178-0765-2_34">
      <url><![CDATA[https://dx.doi.org/10.1007/978-2-8178-0765-2_34]]></url>
    </xref>
  </xref-list>
</publication>
//
<publication date="2016" type="book chapter" book-title="Management of malignant melanoma" first-page="6.1" last-page="6.6" publisher="SM Group" city="Dover" country="USA" internal-id="CelloPub=CLPUB00680">
  <title><![CDATA[Human skin melanoma cell lines collection.]]></title>
  <author-list>
    <person name="Mikhailova I.N."/>
    <person name="Baryshnikov A.Y."/>
    <person name="Morozova L.F."/>
    <person name="Burova O.S."/>
    <person name="Shubina I.Z."/>
    <person name="Kovalevsky D.A."/>
    <person name="Voronina E.S."/>
    <person name="Treshalina H.M."/>
    <person name="Lushnikova A.A."/>
    <person name="Tsyganova I.V."/>
    <person name="Mazurenko N.N."/>
  </author-list>
  <xref-list>
    <xref database="CelloPub" category="Reference resources" accession="CLPUB00680">
      <url><![CDATA[https://www.cellosaurus.org/cellopub/CLPUB00680]]></url>
    </xref>
  </xref-list>
</publication>
//
<publication date="2016" type="conference" conference-title="Proceedings of the International Society for BioProcess Technology (ISBioTech) 4th fall meeting" first-page="1" last-page="27" institution="International Society for BioProcess Technology" city="Virginia Beach" country="USA" internal-id="CelloPub=CLPUB00758">
  <title><![CDATA[Mitochondria-targeting PCR and CO1 barcode sequence analyses as alternatives to isoenzymology.]]></title>
  <author-list>
    <person name="Sykes G.R."/>
  </author-list>
  <xref-list>
    <xref database="CelloPub" category="Reference resources" accession="CLPUB00758">
      <url><![CDATA[https://www.cellosaurus.org/cellopub/CLPUB00758]]></url>
    </xref>
  </xref-list>
</publication>
//
<publication date="2016" type="technical document" serie-title="ATCC application note 17" first-page="1" last-page="4" institution="ATCC" city="Manassas" country="USA" internal-id="DOI=10.13140/RG.2.2.17700.88960">
  <title><![CDATA[hTERT-immortalized and primary keratinocytes differentiate into epidermal structures in 3D organotypic culture.]]></title>
  <author-list>
    <person name="Briley A."/>
    <person name="Shapiro B.A."/>
  </author-list>
  <xref-list>
    <xref database="DOI" category="Reference resources" accession="10.13140/RG.2.2.17700.88960">
      <url><![CDATA[https://dx.doi.org/10.13140/RG.2.2.17700.88960]]></url>
    </xref>
  </xref-list>
</publication>
//
<publication date="1994" type="misc. document" doc-title="Institute for Medical Research (Camden, N.J.)" first-page="1" last-page="351" institution="National Institutes of Health" city="Bethesda" country="USA" internal-id="CelloPub=CLPUB00597">
  <title><![CDATA[1994 catalog of cell lines. NIA Aging Cell Repository.]]></title>
  <author-list>
    <person name="National Institute on Aging"/>
  </author-list>
  <xref-list>
    <xref database="CelloPub" category="Reference resources" accession="CLPUB00597">
      <url><![CDATA[https://www.cellosaurus.org/cellopub/CLPUB00597]]></url>
    </xref>
  </xref-list>
</publication>
"""


}


