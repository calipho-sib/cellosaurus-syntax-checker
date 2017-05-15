package org.nextprot.parser.cellosaurus

import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map
import collection.immutable.HashSet
import scala.util.matching.Regex
import java.nio.charset.CodingErrorAction
import scala.io.Codec
import scala.util.control.Breaks._
import java.io.PrintWriter
import java.io.File
import java.util.Arrays
import java.util.Calendar
import java.text.SimpleDateFormat
import scala.xml._
//import org.nextprot.parser.cellosaurus.datamodel
//import org.nextprot.parser.cellosaurus.datamodel.publication
//import org.nextprot.parser.cellosaurus.utils._

object CelloParser {
  implicit val codec = Codec("UTF-8")
  codec.onMalformedInput(CodingErrorAction.REPLACE)
  // grep -n --color='auto' -P "[\x80-\xFF]" cellosaurus.txt

  def main(args: Array[String]) = {
    val today = Calendar.getInstance().getTime()
    var todaystring: String = new SimpleDateFormat("yyyy-MM-dd").format(today)
    var started: Boolean = false
    var obostarted: Boolean = false
    var toOBO: Boolean = false
    var toxml: Boolean = false
    var stats: Boolean = false
    var celloversion: String = ""
    var drmapname: String = ""
    var oboheadercomment: String = ""
    var obotypedef: String = "\n[Typedef]\nid: derived_from\nname: derived from\nis_transitive: true\n\n" +
                              "\n[Typedef]\nid: originate_from_same_individual_as\nname: originate from same individual as\nis_symmetric: true\n"
    var currEntry = ArrayBuffer[String]()
    var aclist = ArrayBuffer[String]()
    var aslist = ArrayBuffer[String]()
    var idlist = ArrayBuffer[String]()
    var hilist = ArrayBuffer[String]()
    var duplist = ArrayBuffer[String]()
    var drmap = ArrayBuffer[String]()
    val emap = Map.empty[String, Int]
    val oxmap = Map.empty[String, Int]
    val line_occmap = Map("ID" -> (1, 1), "AC" -> (1, 1), "AS" -> (0, 1), "SY" -> (0, 1), "DR" -> (0, 1999), "RX" -> (0, 999), "WW" -> (0, 999), "CC" -> (0, 999), "ST" -> (0, 999), "DI" -> (0, 99), "OX" -> (1, 999), "HI" -> (0, 999),
      "OI" -> (0, 999), "SX" -> (0, 1), "CA" -> (1, 1)) // min and max occurences
    val line_ordmap = Map("ID" -> 1, "AC" -> 2, "AS" -> 3, "SY" -> 4, "DR" -> 5, "RX" -> 6, "WW" -> 7, "CC" -> 8, "ST" -> 9, "DI" -> 10, "OX" -> 11, "HI" -> 12, "OI" -> 14,
      "SX" -> 14, "CA" -> 15)
    val idacmap = Map.empty[String, String]
    var pmids = Set.empty[String]
    var uniquerefs = Set.empty[String]
    var cellorefs = Set.empty[String]
    var Entries = ArrayBuffer[ArrayBuffer[String]]()
    val acregexp = new Regex("CVCL_[A-Z0-9][A-Z0-9][A-Z0-9][A-Z0-9]$")
    val stdataregexp = new Regex("[0-9][0-9]?(\\.[1-9])?(,[1-9][0-9]?(\\.[1-9]?)?){0,4}( \\([A-Zs][A-Za-z0-9_;=:/\\.\\- ]+\\))?$") // s is for some_subclones
    val ameloregexp = new Regex("X|X,Y|Y|Not_detected( \\([A-Z][A-Za-z0-9_;=:\\- ]+\\))?$")
    val repcntregexp = new Regex("^[0-9\\.,]{1,30}+$");
    // Just a reminder, the actual CV is stored in celloparser.cv file
    val ok_dblist1 = List("ATCC", "BCRC", "BCRJ", "BTO", "BioSample", "CBA", "CCLE", "CCLV", "CCRID", "CGH-DB", "ChEMBL-Cells", "ChEMBL-Targets", "CLDB",
      "CLO", "Coriell", "Cosmic", "Cosmic-CLP", "dbMHC", "DGRC", "DSMZ", "ECACC", "EFO", "ENCODE", "ESTDAB", "GDSC", "hPSCreg", "ICLC",
      "IFO", "IGRhCellID", "IHW", "IMGT/HLA", "ISCR", "IZSLER", "JCRB", "KCLB", "LINCS", "Lonza", "MCCL", "MeSH",
      "NISES", "NIH-ARP", "RCB", "RSCB", "SKIP", "SKY/M-FISH/CGH", "TKG", "Ximbio")
    val ok_rxdblist = List("PubMed", "Patent", "DOI", "CelloPub")
    val ok_sxlist = List("Female", "Male", "Mixed sex", "Sex ambiguous", "Sex undetermined")
    // Just a reminder, the actual CV is stored in celloparser.cv file
    val ok_cclist1 = List("Breed/subspecies", "Caution", "Derived from metastatic site", "Discontinued", "From", "Group", "Knockout cell", "Miscellaneous", "Misspelling",
      "Monoclonal antibody target", "Omics", "Part of", "Population", "Problematic cell line", "Registration", "Selected for resistance to", "Transfected with")
    // Just a reminder, the actual CV is stored in celloparser.cv file
    val ok_catlist1 = List("Cancer cell line", "Hybrid cell line", "Hybridoma", "Induced pluripotent stem cell", "Adult stem cell",
      "Spontaneously immortalized cell line", "Stromal cell line",
      "Telomerase immortalized cell line", "Transformed cell line", "Undefined cell line type", "Embryonic stem cell",
      "Factor-dependent cell line", "Finite cell line")
    var errcnt = 0
    var drcnt = 0
    var rxcnt = 0
    var wwcnt = 0
    var syncnt = 0
    var nonUTF8cnt = 0
    var blankcnt = 0
    var curr_line_nb = 0
    var celloentry : CelloEntry = null
    var xmlfile : PrintWriter = null
    var obofile : PrintWriter = null
    val prettyXMLprinter = new scala.xml.PrettyPrinter(512, 2)
    val xmlcopyright = 
  <copyright>
        Copyrighted by the SIB Swiss Institute of Bioinformatics.
        Distributed under the Creative Commons Attribution-NoDerivs License - see http://creativecommons.org/licenses/by-nd/3.0/
  </copyright>

    // Parse command line arguments
    if (args.length == 0) {
      Console.err.println("Usage: celloparser.jar cellosaurus.txt [DRmap=DRMapfilename] [-obo] [-xml] [-stats]")
      Console.err.println("-obo will generate cellosaurus.obo, -xml will generate cellosaurus.xml")
      sys.exit(1)
      }
    if (!new File(args(0)).exists) { Console.err.println(args(0) + " not found"); sys.exit(1) }
    args.foreach(arg => {
      if (arg.contains("obo")) {toOBO = true;  obofile = new PrintWriter(new File("cellosaurus.obo"))}
      else if (arg.contains("xml")) {toxml = true;  xmlfile = new PrintWriter(new File("cellosaurus.xml"))} //xmlfile.write("<?xml version=\\"1.0\\" encoding=\\"UTF-8\\"?>")
      else if (arg.contains("stats")) stats = true
      else if (arg.contains("DRmap=")) { drmapname = arg.split("=")(1) }
    })

    // Load CVs for cc, ca, st, and xref dbs
    var jarpath = new File(System.getProperty("java.class.path"));
    //Console.err.println("jar path is: " + jarpath)
    var celloCVpath = jarpath.getAbsoluteFile().getParentFile().toString() + System.getProperty("file.separator") + "celloparser.cv"
    var celloXrefpath = jarpath.getAbsoluteFile().getParentFile().toString() + System.getProperty("file.separator") + "cellosaurus_xrefs.txt"
    var celloRefpath = jarpath.getAbsoluteFile().getParentFile().toString() + System.getProperty("file.separator") + "cellosaurus_refs.txt"
    //var celloCVpath = "/home/agateau/workspace/cellosaurus-syntax-checker/celloparser.cv"
    if (!new File(celloCVpath).exists) { Console.err.println("celloparser.cv not found at: " + celloCVpath); sys.exit(1) }
    //var celloXrefpath = "/home/agateau/workspace/cellosaurus-syntax-checker/cellosaurus_xrefs.txt"
    if (!new File(celloXrefpath).exists) { Console.err.println("cellosaurus_xrefs.txt not found at: " + celloXrefpath); sys.exit(1) }
    //var celloRefpath = "/home/agateau/workspace/cellosaurus-syntax-checker/cellosaurus_refs.txt"
    if (!new File(celloRefpath).exists) { Console.err.println("cellosaurus_refs.txt not found at: " + celloRefpath); sys.exit(1) }
    var ca = ArrayBuffer[String]()
    var cc = ArrayBuffer[String]()
    var dr = ArrayBuffer[String]()
    var st = ArrayBuffer[String]()
    var valid_element = ""
    for (line <- Source.fromFile(celloCVpath).getLines()) {
      if (line.matches("[CDS][ACRT]   .*")) {
        valid_element = line.substring(5).trim()
        if (valid_element.contains("#")) valid_element = valid_element.split("#")(0).trim()
      }
      if (line.startsWith("DR   ")) dr += valid_element
      else if (line.startsWith("CA   ")) ca += valid_element
      else if (line.startsWith("CC   ")) cc += valid_element
      else if (line.startsWith("ST   ")) st += valid_element
    }
    val ok_dblist = dr.toList
    val ok_cclist = cc.toList
    val ok_catlist = ca.toList
    val ok_stlist = st.toList
    // Prepare subsetdef list from categories (for OBO generation)
    var subsetdefs = ""
    ok_catlist.foreach(cat => { // categories
      subsetdefs += "subsetdef: " + cat.replace(" ","_") + " \"" + cat + "\"\n"
      })
    // Add possible Sexes
    subsetdefs += "subsetdef: Female \"Female\"\n"
    subsetdefs += "subsetdef: Male \"Male\"\n"
    subsetdefs += "subsetdef: Mixed_sex \"Mixed sex\"\n"
    subsetdefs += "subsetdef: Sex_ambiguous \"Sex ambiguous\"\n"
    subsetdefs += "subsetdef: Sex_undetermined \"Sex undetermined\"\n"
    subsetdefs = subsetdefs.split("\n").sortWith(_ < _).mkString("\n")

    // Parse cellosaurus xref file to get databases categories and urls, and put in a map
    val xmap = scala.collection.mutable.Map[String, (String, String)]()
    var xdb = ""
    var xcat = ""
    var xurl = ""
    var xserver = ""
    for (line <- Source.fromFile(celloXrefpath).getLines()) {
      if (line.startsWith("Abbrev")) xdb = line.substring(8).trim()
      else if (line.startsWith("Server")) xserver = line.substring(8).trim()
      else if (line.startsWith("Db_URL")) xurl = line.substring(8).trim()
      else if (line.startsWith("Cat")) xcat = line.substring(8).trim()
      else if (xdb != "" && xurl != "") {
        if (xurl == "None") // Use the server base url instead
          xurl = xserver
        xmap(xdb) = (xurl, xcat)
      }
    }

    // Parse cellosaurus txt file, build headers for obo file, and split in flat entries 
    for (line <- Source.fromFile(args(0)).getLines()) {
      curr_line_nb += 1
      if (line.map(_.toInt).contains(65533)) { println("Warning: " + line); nonUTF8cnt += 1 } // code for special '�' replacement character from coded
      if (obostarted && !line.contains("-----")) oboheadercomment += "!" + line + "\n"
      if (line.startsWith("____")) started = true
      else if (line.startsWith(" Description:")) {oboheadercomment += "!" + line + "\n"; obostarted = true}
      else if (line.endsWith("permission first.")) obostarted = false
      else if (started) {
        if (line.length() < 2) { println("Warning: blank line at line number " + curr_line_nb); blankcnt += 1 }
        else currEntry += line
        if (line == "//") { Entries += currEntry; currEntry = new ArrayBuffer[String] }
      }
      else if(line.startsWith(" Version:")) celloversion = line.split(" ")(2)
    }

    val xmlheader = // date and version are dynamic
          <header>
            <terminology-name>Cellosaurus</terminology-name>
            <description>Cellosaurus: a controlled vocabulary of cell lines</description>
            <release version={ celloversion } updated={ todaystring }/>    
           <terminology-list>
               <terminology name="NCBI-Taxonomy" source="National Center for Biotechnology Information" description="Taxonomy database of organisms">
                    <url><![CDATA[https://www.ncbi.nlm.nih.gov/taxonomy]]></url>
                </terminology>
                <terminology name="NCIt" source="National Cancer Institute" description="Terminology of biomedical concepts">
                    <url><![CDATA[https://ncit.nci.nih.gov]]></url>
                </terminology>
                <terminology name="ChEBI" source="European Molecular Biology Laboratory" description="Chemical Entities of Biological Interest">
                    <url><![CDATA[https://www.ebi.ac.uk/chebi/]]></url>
                </terminology>
                <terminology name="PubChem" source="PubChem compound database" description="Public repository for information on chemical substances and their biological activities">
                    <url><![CDATA[https://pubchem.ncbi.nlm.nih.gov/]]></url>
                </terminology>
           </terminology-list>
         </header>   
           
    if(toOBO) {
      val obodate = new SimpleDateFormat("MM:dd:yyyy").format(today)
      val oboheader: String = "format-version: 1.2\ndata-version: " + celloversion + "\ndate: " + obodate + " 12:00\ndefault-namespace: cellosaurus\n\n"
      obofile.write(oboheader)
      obofile.write(subsetdefs)
      obofile.write("\nontology: Cellosaurus\n\n")
      obofile.write(oboheadercomment)
    }  
    
    // All entries text-loaded
    var entrynb = 0
    Console.err.println("Error report (" + Entries.size + " entries) :\n")
    Entries.foreach(entry => {
      var id = ""
      var coreid = ""
      var ac = ""
      var drlist = ArrayBuffer[String]()
      var curr_rank = 0
      var last_rank = 0
      var curr_ccrank = 0
      var last_ccrank = 0
      var strsrcCnt = 0 // count of ST Sources lines in an entry, never > 1
      var hasSTR = false
      val linecntmap = Map("ID" -> 0, "AC" -> 0, "AS" -> 0, "SY" -> 0, "DR" -> 0, "RX" -> 0, "WW" -> 0, "CC" -> 0, "ST" -> 0, "DI" -> 0, "OX" -> 0, "HI" -> 0,
        "OI" -> 0, "SX" -> 0, "CA" -> 0) // Initialize to 0 the line count for each possible field
      if (!(entry(0).startsWith("ID   ") && entry(1).startsWith("AC   "))) { Console.err.println("Severe error: Missing ID/AC line at " + entry(0) + " Please correct before re-check"); sys.exit(2) }
      entry.foreach(entryline => { //println(entryline)
        var entrylinedata = ""
        var header = entryline.substring(0, 2)
        if (entryline.length() > 5) {
          entrylinedata = entryline.substring(5)
          if (!line_ordmap.contains(header)) { Console.err.println("Unknown line type: " + entryline); errcnt += 1 }
          else {
            linecntmap(header) += 1 // Increment count for line type
            curr_rank = line_ordmap(header)
            if (curr_rank < last_rank) { Console.err.println("Misordered line type: " + entryline + " in entry " + id); errcnt += 1 }
            last_rank = curr_rank
          }
        }
        if (entryline.contains("\t")) Console.err.println("Tab found at: " + entryline)
        if (entrylinedata.contains("  ")) Console.err.println("Multiple spaces found at: " + entryline)
        if (entrylinedata.contains(";;") || entrylinedata.contains("; ;")) Console.err.println("Multiple ; found at: " + entryline)
        if (entryline.endsWith(" ")) Console.err.println("Trailing space found at: " + entryline)
        else if (entryline.endsWith("..")) Console.err.println("Double dot found at: " + entryline)
        if (entryline.startsWith("ID   ")) { id = entrylinedata; idlist += id }
        else if (entryline.startsWith("AC   ")) {
          ac = entrylinedata
          if (acregexp.findFirstIn(ac) == None) { Console.err.println("Incorrect AC format at: " + entryline); errcnt += 1 }
          aclist += ac
          // Map AC to entry
          emap(ac) = entrynb
          // Map AC to ID
          idacmap(ac) = id
          entrynb += 1;
          }
        else if (entryline.startsWith("AS   ")) { // AC Secundary
          val locaslist = entrylinedata.split("; ")
          locaslist.foreach(as => {
            aslist += as
            if (acregexp.findFirstIn(as) == None) { Console.err.println("Incorrect AC format at: " + entryline); errcnt += 1 }
            })
          }
        else if (entryline.startsWith("SY   ")) { // Synonyms
          if (entryline.endsWith(";")) { Console.err.println("SY trailing ; found at: " + entryline); errcnt += 1 }
          val locsynlist = entrylinedata.split("; ")
          val synduplist = locsynlist.diff(locsynlist.distinct)
          if (synduplist.size != 0) synduplist.foreach(syn => { Console.err.println("Locally duplicated SY: " + syn); errcnt += 1 })
            locsynlist.foreach(synonym => {
            syncnt += 1
            if (synonym == id) { Console.err.println("Synonym exists as main ID: " + id); errcnt += 1 }
            })
          }
        else if (entryline.startsWith("DR   ")) { // X-refs
          if (entryline.endsWith(";") || entryline.endsWith(".")) { Console.err.println("DR trailing ; found at: " + entryline); errcnt += 1 }
          val dbname = entrylinedata.split("; ")(0)
          if (!ok_dblist.contains(dbname)) { Console.err.println("Illegal db:" + dbname + " found at: " + entryline); errcnt += 1 }
          drcnt += 1
          drlist += entrylinedata.split("/")(0)
          if (drmapname != "") { // Add DR to DRmap
            if (id.contains(" [")) coreid = id.split("\\[")(0).trim() else coreid = id
            drmap += dbname + "\t" + entrylinedata.split(";")(1).trim() + "\t" + ac + "\t" + coreid + "\n"
            }
          }
        else if (entryline.startsWith("RX   ")) { // PubMed/DOIs
          if (!entryline.endsWith(";")) { Console.err.println("RX unterminated line found at: " + entryline); errcnt += 1 }
          val rxdbname = entrylinedata.split("=")(0)
          if (!ok_rxdblist.contains(rxdbname)) { Console.err.println("Illegal db:" + rxdbname + " found at: " + entryline); errcnt += 1 }
          if (entrylinedata.split("[=;]").length < 2) { Console.err.println("No " + rxdbname + " identifier found at: " + entryline + "(" + id + ")"); errcnt += 1 }
          else {
            // DOI=10.1577/1548-8667(1998)010<0075:DOLTCL>2.0.CO;2;
            val identifier = if(rxdbname=="DOI") entrylinedata.split("=")(1).dropRight(1) else entrylinedata.split("[=;]")(1)
            uniquerefs += rxdbname + "=" + identifier // for subsequent consistency check with cellosaurus_refs.txt
            if (rxdbname == "PubMed") {
              if ("^[1-9][0-9]{0,7}".r.findFirstIn(identifier) == None) { Console.err.println("Wrong format for " + rxdbname + " identifier: " + identifier + " found at: " + entryline); errcnt += 1 }
              else {
                pmids += identifier
                if (drmapname != "") { // Add DR to DRmap
                  if (id.contains(" [")) coreid = id.split("\\[")(0).trim() else coreid = id
                  drmap += "PubMed\t" + identifier + "\t" + ac + "\t" + coreid + "\n"
                }
              }
            }
            else if ((rxdbname == "DOI") && !identifier.startsWith("10.")) { Console.err.println("Wrong format for " + rxdbname + " identifier: " + identifier + " found at: " + entryline); errcnt += 1 }
            else if ((rxdbname == "CelloPub") && ("^CLPUB[0-9]{5}".r.findFirstIn(identifier)) == None) { Console.err.println("Wrong format for " + rxdbname + " identifier: " + identifier + " found at: " + entryline); errcnt += 1 }
            else if ((rxdbname == "Patent") && ("^[A-Z]{2}[0-9]{7,11}[A-Z]{0,1}[1-9]{0,1}".r.findFirstIn(identifier)) == None) { Console.err.println("Wrong format for " + rxdbname + " identifier: " + identifier + " found at: " + entryline); errcnt += 1 }
            rxcnt += 1
            }
          }
        else if (entryline.startsWith("WW   ")) { // Web links
          if (!(entrylinedata.startsWith("http://") || entrylinedata.startsWith("https://") || entrylinedata.startsWith("ftp://"))) { Console.err.println("Invalid url found at: " + entryline); errcnt += 1 }
          wwcnt += 1
          }
        else if (entryline.startsWith("CC   ")) { // Comments
          val cctopic = entrylinedata.split(":")(0)
          if (!ok_cclist.contains(cctopic)) { Console.err.println("Unknown CC topic found at: " + entryline); errcnt += 1 }
          else curr_ccrank = ok_cclist.indexOf(cctopic)
          if (!entryline.endsWith(".")) { Console.err.println("Unterminated CC found at: " + entryline); errcnt += 1 }
          if (curr_ccrank < last_ccrank) { Console.err.println("Misordered CC topic line: " + entryline + " in entry " + id); errcnt += 1 }
          last_ccrank = curr_ccrank
          // check database separator tokens
          val cctext = entrylinedata.split(": ")(1)
          val cctoks = cctext.split("; ")
          if(cctoks.size > 1 ) {
          	val maybedb = cctoks(0)
           	if (((maybedb == "UniProtKB") || (maybedb == "HGNC") || (ok_dblist.contains(maybedb))) && cctoks.size < 3) { Console.err.println("Missing separator at : " + entryline); errcnt += 1 }
         	 }
          if(cctopic == "Discontinued" && !entrylinedata.contains("Catalog number")) {
            // These discontinued CCs must also exist as DR lines
            var discontinued = entrylinedata.split(": ")(1) // just keep db reference
            discontinued = (discontinued.substring(0,discontinued.lastIndexOf(';')).trim())
            if(!drlist.contains(discontinued))
              { Console.err.println("No match for discontinued: '" + discontinued + "' found among DR lines of " + ac); errcnt += 1 }
            }
          }
        else if (entryline.startsWith("ST   ")) { // Short tandem repeats
          hasSTR = true
          if (!entrylinedata.contains(": ")) { Console.err.println("Incorrect ST data format at: " + entryline); errcnt += 1 }
          else {
            val sttopic = entrylinedata.split(":")(0)
            val stdata = entrylinedata.split(": ")(1).trim()
            if (!sttopic.contains("Source(s)")) {
              if (!ok_stlist.contains(sttopic)) { Console.err.println("Unknown ST site found at: " + entryline); errcnt += 1 }
              else { // check ST data format
                val chrdata = stdata.split("\\(")(0).trim()
                if (sttopic.contains("Amelogenin")) {
                  if (ameloregexp.findFirstIn(stdata) == None) { Console.err.println("Incorrect ST data format (Amel) at: " + entryline); errcnt += 1 }
                }
                else if(!stdata.contains("Not_detected")) {
                  // Todo: maybe split at parenthesis and apply a simpler stdataregexp only to the reference part if any
                  if (repcntregexp.findFirstIn(chrdata) == None) { Console.err.println("Incorrect ST data format (regex) at: " + entryline); errcnt += 1 } // repeat counts
                  else if (stdataregexp.findFirstIn(stdata) == None) { Console.err.println("Incorrect ST data format (regex) at: " + entryline); errcnt += 1 } // repeats plus references
                  else { 
                    // check for order and unicity token by token
                    val toklist = chrdata.split(",")
                    toklist.foreach(token => {
                      //Console.err.println("token: " + token)
                      val dottoken = token.split("\\.")
                      if (dottoken(0).length() > 2) { if(!entrylinedata.contains("Dog ")) {Console.err.println("Incorrect ST data format at: " + entryline); errcnt += 1 }} // no more than 99 except for dog
                      else if (dottoken.length > 1 && dottoken(1).length() > 1) { Console.err.println("Incorrect ST data format at: " + entryline); errcnt += 1 }
                    })
                    // check for order and unicity
                    if (toklist.length != toklist.distinct.length) { Console.err.println("Duplicate in ST data at: " + entryline); errcnt += 1 }
                    else if (toklist.length > 1) {
                      val doubles = toklist.map { x => x.toDouble }
                      if (doubles.toList != doubles.sortWith(_ < _).toList) { Console.err.println("Wrong order in ST data at: " + entryline); errcnt += 1 }
                    }
                  }
                }
              }
            }
            else { // ST   Source(s) line
              strsrcCnt += 1
              if (stdata.contains(",")) { Console.err.println("No comma allowed in ST source format at: " + entryline); errcnt += 1 }
              if (stdata.contains("PubMed")) { // PubMed in sources
              val srcrefs = stdata.split("; ").filter(_.startsWith("PubMed"))
              srcrefs.foreach(ref => { // for subsequent consistency check with cellosaurus_refs.txt
                if (ref.contains(";"))
                  uniquerefs += ref.split(";")(0)
                else if (ref.contains(","))
                  uniquerefs += ref.split(",")(0)
                else
                  uniquerefs += ref
              })
            }
           }
          }
        }
        else if (entryline.startsWith("DI   ")) { // Diseases
            if (entryline.endsWith(";") || entryline.endsWith(".")) { Console.err.println("DI trailing ; found at: " + entryline); errcnt += 1 }
            val dilist = entrylinedata.split("; ")
            if (dilist.length != 3) { Console.err.println("Illegal disease format found at: " + entryline); errcnt += 1 }
            if (dilist(0) != "NCIt") { Console.err.println("Illegal disease db:" + dilist(0) + " found at: " + entryline); errcnt += 1 }
            }
        else if (entryline.startsWith("OX   ")) { // Organisms
            if (entryline.endsWith(";")) { Console.err.println("OX trailing ; found at: " + entryline); errcnt += 1 }
            val oxlist = entrylinedata.split(";")
            if (oxlist.length != 2) { Console.err.println("Illegal taxonomy format found at: " + entryline); errcnt += 1 }
            val dbname = oxlist(0).split("=")(0)
            val taxid = oxlist(0).split("=")(1)
            if (dbname != "NCBI_TaxID") { Console.err.println("Illegal taxonomy db:" + dbname + " found at: " + entryline); errcnt += 1 }
            if (!oxlist(1).contains(" ! ")) { Console.err.println("Illegal taxonomy format found at: " + entryline); errcnt += 1 }
            if ((oxlist(1) == " ! Homo sapiens" && (taxid != "9606")) || (oxlist(1) == " ! Mus musculus" && (taxid != "10090"))) Console.err.println("Wrong taxid at: " + entryline)
            if (!oxmap.contains(taxid)) oxmap(taxid) = 1
            else oxmap(taxid) += 1
            }
        else if (entryline.startsWith("HI   ") || entryline.startsWith("OI   ")) { // Hierarchy
            var hitoken = "";
            if (entryline.endsWith(";") || entryline.endsWith(".")) { Console.err.println("HI/OI trailing ;/. found at: " + entryline); errcnt += 1 }
            if (!entrylinedata.contains(" ! ")) { Console.err.println("Illegal HI/OI format found at: " + entryline); errcnt += 1 }
            val toklist = entrylinedata.split(" ")
            if (toklist.length < 3) { Console.err.println("Illegal HI/OI format found at: " + entryline); errcnt += 1 }
            hitoken = toklist(0)
            if (acregexp.findFirstIn(toklist(0)) == None) { Console.err.println("Incorrect HI/OI AC format at: " + entryline); errcnt += 1 }
            else hilist += hitoken;
            }
        else if (entryline.startsWith("SX   ")) { // Sex
            if (!ok_sxlist.contains(entrylinedata)) { Console.err.println("Illegal sex found at: " + entryline); errcnt += 1 }
            }
        else if (entryline.startsWith("CA   ")) { // Category
            if (!ok_catlist.contains(entrylinedata)) { Console.err.println("Illegal category found at: " + entryline); errcnt += 1 }
            }
        else if (entryline.startsWith("//")) { // Entry terminator, check line occurences in collected entry
            linecntmap.keys.foreach { key =>
            if ((linecntmap(key) < line_occmap(key)._1) || (linecntmap(key) > line_occmap(key)._2))
              if (key == "AC" || key == "ID") { Console.err.println("Severe error: " + key + " in entry " + id + " Please correct before re-check"); sys.exit(3) }
              else { Console.err.println("Illegal line count for: " + key + " in entry " + id); errcnt += 1 }
            }
            if(hasSTR && strsrcCnt != 1) { Console.err.println("Illegal ST   Source(s) line count in " + ac); errcnt += 1 }
        }
      })
    })

    // Entry level checks
    duplist = aclist.diff(aclist.distinct)
    if (duplist.length != 0) { duplist.foreach(ac => { Console.err.println("duplicated AC: " + ac); errcnt += 1 }) }

    duplist = idlist.diff(idlist.distinct)
    if (duplist.length != 0) { duplist.foreach(id => { Console.err.println("duplicated ID: " + id); errcnt += 1 }) }

    // Check that no AS exists as a living AC
    aslist.foreach { as => if (aclist.contains(as)) { Console.err.println("living AC in AS: " + as); errcnt += 1 } }

    // Check for orphans hierarchies (inexistant ACs)
    val misslist = hilist.filter(s => !aclist.contains(s)).toSet
    if (misslist.size != 0) { misslist.foreach(ac => { Console.err.println("Inexistent HI/OI AC: " + ac); errcnt += 1 }) }
    
    if (toxml) {
      xmlfile.write("<Cellosaurus>\n")
      xmlfile.write(prettyXMLprinter.format(xmlheader) + "\n")
      xmlfile.write("<cell-line-list>\n")
    }

    // Check OI/HI lines consistency (inter-entry checks)
    var ac = ""
    var oiac = ""
    var oiid = ""
    var ox = ""
    var parentSex = ""
    var parentSpecies = ""
    var category = ""
    var disease = ""
    var dislist = ArrayBuffer[String]()
    var disErrorlist = ArrayBuffer[String]()

    Entries.foreach(entry => {
      category = ""
      parentSex = ""
      parentSpecies = ""
      disease = ""
      dislist.clear
      disErrorlist.clear
                                 
      entry.foreach(entryline => {
       if (entryline.startsWith("AC   ")) { ac = entryline.substring(5); }
       else if (entryline.startsWith("OX   ")) { ox = entryline.split("=")(1) }
       else if (entryline.startsWith("DI   ")) { disease = entryline.split("; ")(2); dislist += disease }
       else if (entryline.startsWith("OI   ")) {
               oiac = entryline.substring(5).split(" ")(0)
               oiid = (entryline.substring(5).split("!")(1)).substring(1)
               if (oiac.equals(ac)) { Console.err.println("Self-referencing OI: " + oiac + "/" + ac); errcnt += 1 }
               else {
                  var ok = false
                  if (!misslist.contains(oiac)) { // otherwise previously reported
                     currEntry = Entries(emap(oiac))
                     currEntry.foreach(line => { 
                                       if (line.startsWith("OI   ") && line.contains(ac)) ok = true
                                         })
                     if (!ok) { Console.err.println("Inexistent reciproque OI/AC: " + oiac + "/" + ac); errcnt += 1 }
                     else if (idacmap(oiac) != oiid) { Console.err.println("Incorrect OI AC/ID pair: " + oiac + "/" + oiid); errcnt += 1 }
                       }
                     }
                }
       else if (entryline.startsWith("HI   ")) {
               oiac = entryline.substring(5).split(" ")(0)
               oiid = (entryline.substring(5).split("!")(1)).substring(1)
               if (oiac.equals(ac)) { Console.err.println("Self-referencing HI: " + oiac + "/" + ac); errcnt += 1 }
               if (!misslist.contains(oiac)) { // otherwise previously reported
                   if (idacmap(oiac) != oiid) { Console.err.println("Incorrect HI AC/ID pair: " + oiac + "/" + oiid); errcnt += 1 }
                   currEntry = Entries(emap(oiac))
                   // Parse parent entry
                   currEntry.foreach(line => { //if(ac=="CVCL_A121") println("scanning parent of CVCL_A121: " + line)
                             if (line.startsWith("OX   ")) { parentSpecies = line.split("=")(1) }
                             else if (line.startsWith("DI   ")) { if (!dislist.contains(line.split("; ")(2))) { disErrorlist += "Missing parent disease in: " + oiac + "(parent)=" + line.split("; ")(2) + " " + ac + "=" + disease } }
                             else if (line.startsWith("SX   ")) { parentSex = line.split("   ")(1) }
                             else if (line.startsWith("HI   ")) { if (line.substring(5).split(" ")(0).equals(ac)) { Console.err.println("Reciprocal HI: " + oiac + "/" + ac); errcnt += 1 } }
                             })
                   }
               }
       else if (entryline.startsWith("SX   ") && parentSex != "") {
                if (entryline.split("   ")(1) != parentSex) { Console.err.println("Wrong parent sex match: " + oiac + ":" + ac); errcnt += 1 }
                }
       else if (entryline.startsWith("CA   ")) { category = entryline.split("   ")(1) }
      })
      
    if (toxml || toOBO) celloentry = toCelloEntry(entry, xmap) // generate model representation
    if (toxml) {//if(ac.startsWith ("CVCL_G193")) 
      celloentry.updatDBrefs // Add a property flag to discontinued cell line dbrefs
      xmlfile.write(prettyXMLprinter.format(celloentry.toXML) + "\n") 
      }
    if(toOBO) //if(ac.startsWith ("CVCL_9728")) Console.err.println(celloentry.toOBO) 
      obofile.write(celloentry.toOBO)
    if (!category.contains("Hybrid")) { // outside line loop, needs all entry parsed
       if ((parentSpecies != "") && !ox.contains("hybrid") && (parentSpecies != ox)) { Console.err.println("Wrong parent species: " + oiac + "(parent)=" + parentSpecies + " " + ac + "=" + ox) }
       if (disErrorlist.length != 0) { Console.err.println(disErrorlist(0)); errcnt += 1 }
       }
   })


      if (toxml) {
      xmlfile.write("</cell-line-list>\n") // close the cell lines
      xmlfile.write("<publication-list>\n")
      // Parse cellosaurus ref file to build the Cellopublication class instances 
      var publiFlat = ArrayBuffer[String]()
      var pubcnt = 0
      for (line <- Source.fromFile(celloRefpath).getLines()) {
        publiFlat += line
        if (line.startsWith("//")) { // Record complete
          val Cellopub = toCellopublication(publiFlat, xmap)
          cellorefs += Cellopub.internal_id // For a subsequent sanity check with uniquerefs as collected in the cellosaurus.txt parsing loop
          xmlfile.write(prettyXMLprinter.format(Cellopub.toXML) + "\n")
          publiFlat.clear
          pubcnt += 1
        }
      }
      //Console.err.println(pubcnt + " refs in cellosaurus_refs.txt")
      xmlfile.write("</publication-list>\n") // finnish xml file
      xmlfile.write(prettyXMLprinter.format(xmlcopyright))
      xmlfile.write("\n</Cellosaurus>")
      xmlfile.close
      }
    
    if (toOBO) { // finnish OBO file
      obofile.write(obotypedef)
      obofile.close
    }

    println(Entries.length + " entries: " + errcnt + " error(s)")
    println(nonUTF8cnt + " non-UTF8 character containing line(s), " + blankcnt + " blank line(s)")


    // Sanity check of cellosaurus.txt and cellosaurus_refs.txt content
    if (toxml && (uniquerefs -- cellorefs).size > 0) {
      Console.err.println("\nFatal error: missing references in cellosaurus_refs.txt:")
      (uniquerefs -- cellorefs).foreach(ref => { Console.err.println(ref) })
    }

    if (drmapname != "") { // Write DRmap to file
      val drmapfile = new PrintWriter(new File(drmapname))
      drmap.sortWith(_ < _).foreach { line => drmapfile.write(line) }
      drmapfile.close()
    }
    
    if (stats) {
      println("\n ===== Statistics =====\n")
      println(drcnt + " Xrefs")
      println(rxcnt + " RX refs (" + pmids.size + " unique PMIDs)")
      println(wwcnt + " Web links")
      println(syncnt + " synonyms")
      println(oxmap.size + " different species")
      oxmap.keys.foreach { i =>
        if (i == "9606") println("Human: " + oxmap(i))
        else if (i == "10090") println("Mouse: " + oxmap(i))
        else if (i == "10116") println("Rat: " + oxmap(i))
      }
    }
  }

  //------------------ Utility methods, TODO: move in a separate package and import------------------------------
  
  def toCellopublication(publiFlatentry: ArrayBuffer[String], xmap: scala.collection.mutable.Map[String, (String, String)]): CelloPublication = {
    var authorlist = ArrayBuffer[String]()
    var editorlist = ArrayBuffer[String]()
    var xreflist = ArrayBuffer[String]()
    var pubXreflist = List[DbXref]()
    var celloPubAuthorlist = List[Author]()
    var celloPubEditorlist = List[Author]()
    var linedata = ""
    var journal = ""
    var volume = ""
    var year = ""
    var firstpage = ""
    var lastpage = ""
    var title = ""
    var publisher = ""
    var city = ""
    var country = ""
    var institute = ""
    //var location = ""
    var pubtype = ""
    var internalId = ""
    val editorregexp = new Regex("[A-Z][a-z]+.* .*[A-Z]\\.$") // eg: Saunders S.J., Gruev B., Park J.-G.
    
    // Parse cellosaurus ref file to build the Cellopublication class instances
    for (line <- publiFlatentry) {
      if (line.size > 5) linedata = line.substring(5).trim()
      if (line.startsWith("RX   ")) { // The line occurs only once per entry in cellosaurus refs
        xreflist = linedata.substring(0, linedata.size - 1).split("; ").to[ArrayBuffer]
        internalId = xreflist(0) // Pubmed comes first when PubMed + DOI
       }
      else if (line.startsWith("RA   ")) authorlist ++= linedata.substring(0, linedata.size - 1).split(", ").to[ArrayBuffer]
      else if (line.startsWith("RG   ")) authorlist = linedata.split(";").to[ArrayBuffer]
      else if (line.startsWith("RT   ")) {
        if (title == "") title = linedata.trim()
        else title += " " + linedata.trim() // Titles can span several lines
      }
      else if (line.startsWith("RL   ")) {
        if (linedata.startsWith("Patent")) { // RL   Patent number CN1061093C, 24-Jan-2001.
          year = linedata.substring(linedata.size - 12, linedata.size - 1) // keep day and month
          pubtype = "patent"
        }
        else if (linedata.startsWith("Thesis")) { // RL   Thesis PhD (1971), Erasmus University Rotterdam, Netherlands.
          year = linedata.split("[\\(||\\)]")(1)
          val rltokens = linedata.split(", ")
          institute =  rltokens(rltokens.size-2)
          country =  rltokens(rltokens.size-1).split("\\.")(0)
          pubtype = "t" + linedata.split(" \\(")(0).substring(1) // t + hesis + level (phd, ms, ..)
        }
        else if (linedata.startsWith("(In)")) { // RL   (In) Abstracts of the 91st meeting of the Japanese Society of Veterinary Science, pp.149-149, Tokyo (1981).
          year = linedata.substring(linedata.size - 10).split("[\\(||\\)]")(1)
          val rltokens = linedata.split("; ")
          for (rltoken <- rltokens) {
            if(rltoken.startsWith("pp.")) {
            firstpage = rltoken.substring(3).split("-")(0)
            lastpage = rltoken.substring(3).split("-")(1)
            }
            else if(rltoken.contains("(eds.)")) { // Collect editors (Hiddemann W., Haferlach T., Unterhalt M., Buechner T., Ritter J. (eds.))
             editorlist =  rltoken.dropRight(7).split(", ").to[ArrayBuffer] 
            }
          }
          publisher =  rltokens(rltokens.size-2)
          if(publisher.startsWith("pp") || publisher.startsWith("Abs")|| publisher.startsWith("Abs")) publisher = ""
          city =  rltokens(rltokens.size-1).split(" \\(")(0)
          journal = rltokens(0).substring(5) // Actually the 'book' title ((In) is skipped)
          pubtype = "book chapter"
        }
        else { // General journal RL eg: RL   Naunyn Schmiedebergs Arch. Pharmacol. 352:662-669(1995).
          year = linedata.split(":")(1).split("[\\(||\\)]")(1)
          val rltokens = linedata.split(":")(0).split(" ")
          pubtype = "article"
          journal = rltokens.dropRight(1).mkString(" ")
          if(journal.contains("Suppl")) { // add the Suppl part to volume, eg: J. Physiol. Pharmacol. 60 Suppl.
            var digitpos = 0
            val matchlist = new Regex("[0-9]").findAllIn(journal).matchData.toList
            if(matchlist.size != 0)
               digitpos = matchlist(0).start
            else  
               digitpos = journal.indexOf("Suppl")
            volume = journal.substring(digitpos)
            journal = journal.substring(0,digitpos-1)
          }
          
          if(volume == "")
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
      } else if (line.startsWith("//")) { // Record complete
        authorlist.foreach(author => { celloPubAuthorlist = new Author(name = author) :: celloPubAuthorlist })
        editorlist.foreach(editor => { celloPubEditorlist = new Author(name = editor) :: celloPubEditorlist })
        xreflist.foreach(xref => {
          val db = xref.split("=")(0)
          pubXreflist = new DbXref(_db = db, _ac = xref.split("=")(1), _category = xmap(db)._2, _url = xmap(db)._1, _property = "", _entryCategory = "") :: pubXreflist
        })
      }
    }
    // .reverse in author lists to recover original order
    val publiEntry = new CelloPublication(year = year, name = journal, pubtype = pubtype, volume = volume, firstpage = firstpage, lastpage = lastpage, publisher = publisher, institute = institute, city = city, country = country, internal_id = internalId,
      title = title.split("\"")(1), authors = celloPubAuthorlist.reverse, editors = celloPubEditorlist.reverse, dbrefs = pubXreflist)
    publiEntry
  }
  
   def toCelloEntry(flatEntry: ArrayBuffer[String], xmap: scala.collection.mutable.Map[String, (String, String)]): CelloEntry = {
    var entrylinedata = ""
    var ac = ""
    var id = ""
    var category = ""
    var entrycategory = ""
    var sex = ""
    var source_pmid = ""
    var alleles = ""

    var celloStrmarkerlist = List[Strmarker]()
    var celloSourcelist = List[Source]()
    var celloSourcereflist = List[PubliRef]()
    var celloOldaclist = List[OldAc]()
    var celloSynlist = List[Synonym]()
    var celloPublilist = List[PubliRef]()
    var celloDislist = List[CvTerm]()
    var celloSpeclist = List[CvTerm]()
    var celloOriglist = List[CvTerm]()
    var celloDerivedlist = List[CvTerm]()
    var celloXreflist = List[DbXref]()
    var celloCommentlist = List[Comment]()
    var celloWebPagelist = List[WebPage]()

    flatEntry.foreach(entryline => { // First pass just to get the cell line category, it can influence the urls in DbXrefs
    if (entryline.startsWith("CC   Part of: ECACC")) entrycategory = entryline.substring(20) // Category    
    else if (entryline.startsWith("CA   ") && entrycategory == "") entrycategory = entryline.substring(5) // Category
    })
    
    flatEntry.foreach(entryline => {
      if (entryline.length() > 5) entrylinedata = entryline.substring(5)
      if (entryline.startsWith("AC   ")) ac = entrylinedata // primary accession  
      else if (entryline.startsWith("AS   ")) { // secundary accession  
        val asList = entrylinedata.split("; ")
        asList.foreach(as => {
          celloOldaclist = new OldAc(oldac = as) :: celloOldaclist
        })
      }
      else if (entryline.startsWith("ID   ")) id = entrylinedata // primary name  
      else if (entryline.startsWith("SY   ")) { // synonyms  
        val synList = entrylinedata.split("; ")
        synList.foreach(synonym => {
          celloSynlist = new Synonym(syno = synonym) :: celloSynlist
        })
      }
      else if (entryline.startsWith("CA   ")) category = entrylinedata // Category  
      else if (entryline.startsWith("SX   ")) sex = entrylinedata // Sex  
      else if (entryline.startsWith("DR   ")) { // xref 
        val db = entrylinedata.split("; ")(0)
        if(!xmap.contains(db)) Console.err.println("Error: no entry for \"" + db + "\" in cellosaurus_xrefs.txt")
        else celloXreflist = new DbXref(_db = db, _ac = entrylinedata.split(";")(1).trim(), _category = xmap(db)._2, _url = xmap(db)._1, _property = "", _entryCategory = entrycategory) :: celloXreflist
      }
      else if (entryline.startsWith("CC   ")) { // comment
        val linetokens = entrylinedata.split(": ")
        val category = linetokens(0)
        var textdata = linetokens(1).trim()
        if(linetokens.size > 2) // The text token may contain a colon which is not a separator
          textdata += ": " + linetokens(2).trim()
        if(!(category.equals("Miscellaneous") || category.equals("Caution") || category.equals("Problematic cell line")) )
          textdata = textdata.dropRight(1) // Drop final dot
          celloCommentlist = new Comment(category = category, text = textdata, xmap) :: celloCommentlist
        //else Console.err.println(category + " " + textdata)
      }
      else if (entryline.startsWith("WW   ")) { // web pages  
        celloWebPagelist = new WebPage(url = entrylinedata.trim()) :: celloWebPagelist
      }
      else if (entryline.startsWith("DI   ")) { // diseases
        celloDislist = new CvTerm(_terminology = entrylinedata.split("; ")(0), _ac = entrylinedata.split("; ")(1), _name = entrylinedata.split("; ")(2)) :: celloDislist
      }
      else if (entryline.startsWith("OX   ")) { // species 
        celloSpeclist = new CvTerm(_terminology = "NCBI-Taxonomy", _ac = entrylinedata.split("=")(1).split("; ")(0), _name = entrylinedata.split("! ")(1)) :: celloSpeclist
      }
      else if (entryline.startsWith("OI   ")) { // same origin 
        celloOriglist = new CvTerm(_terminology = "Cellosaurus", _ac = entrylinedata.split(" ! ")(0), _name = entrylinedata.split(" ! ")(1)) :: celloOriglist
      }
      else if (entryline.startsWith("HI   ")) { // derived from 
        celloDerivedlist = new CvTerm(_terminology = "Cellosaurus", _ac = entrylinedata.split(" ! ")(0), _name = entrylinedata.split(" ! ")(1)) :: celloDerivedlist
      }
      else if (entryline.startsWith("RX   ")) { // publications 
        celloPublilist = new PubliRef(db_ac = entrylinedata.split(";")(0)) :: celloPublilist
      }
      else if (entryline.startsWith("ST   Source")) { // short dandem repeats source
        val srcList = entrylinedata.substring(11).split("; ") // skip the 'Source(s):' comment
        srcList.foreach(src => {
          if(src.contains("="))
            celloSourcereflist = new PubliRef(db_ac = src) :: celloSourcereflist
          else  
            celloSourcelist = new Source(src = src) :: celloSourcelist
        })
      }
      else if (entryline.startsWith("ST   ")) { // short tandem repeats 
        var celloStrMarkerSourcelist = List[Source]()
        var celloStrMarkerSourcereflist = List[PubliRef]()
        val id = entrylinedata.split(": ")(0)
        val rawdata = entrylinedata.split(": ")(1)
        if(rawdata.contains("(")) { // ST   D21S11: 27,32.2 (PubMed=25877200)
          // Separate alleles from references
        alleles = rawdata.split(" \\(")(0)
        val allelerefs = rawdata.split("[\\(||\\)]")(1)
        allelerefs.split("; ").foreach(alleleref => {
          if(alleleref.contains("="))
            celloStrMarkerSourcereflist = new PubliRef(db_ac = alleleref) :: celloStrMarkerSourcereflist
          else  
            celloStrMarkerSourcelist = new Source(src = alleleref) :: celloStrMarkerSourcelist
            })
        }
        else alleles = rawdata
        val strMarkerdata = new StrmarkerData(alleles=alleles, strmarkersources=celloStrMarkerSourcelist, strmarkersourcerefs=celloStrMarkerSourcereflist)
        celloStrmarkerlist = new Strmarker(id = id, conflict = "false", markerdatalist = List(strMarkerdata)) :: celloStrmarkerlist
      }
    })
        // conflict example
        // ST   D7S820: 9,10 (Cosmic-CLP; ECACC; PubMed=18713817)
        //ST   D7S820: 10 (PubMed=25877200)

    val markerIdList = celloStrmarkerlist.map(_.id)
    val duplIdlist = markerIdList.diff(markerIdList.distinct).toSet
    var finalmarkerList = celloStrmarkerlist.filter(marker => !duplIdlist.contains(marker.id))
    if(duplIdlist.size > 0 ) {
         duplIdlist.foreach(markerid => { // merge strmarkers with same id
         var newmarkdatalist  = List[StrmarkerData]() 
         val tomergemarkersList = celloStrmarkerlist.filter(marker => markerid == marker.id)
         tomergemarkersList.foreach(strmarker => {
         newmarkdatalist =  strmarker.markerdatalist(0) :: newmarkdatalist 
          })
          finalmarkerList = new Strmarker(id = markerid, conflict = "true", markerdatalist = newmarkdatalist) :: finalmarkerList
       })
      }

    // Instanciate full entry, .reverse in lists to recover original order
    val entry = new CelloEntry(ac = ac, oldacs = celloOldaclist, id = id, synonyms = celloSynlist.reverse, category = category, sex = sex, dbrefs = celloXreflist.reverse,
      comments = celloCommentlist.reverse, webpages = celloWebPagelist.reverse, diseases = celloDislist.reverse, species = celloSpeclist.reverse,
      origin = celloOriglist.reverse, derived = celloDerivedlist.reverse, publis = celloPublilist.reverse, sources = celloSourcelist,
      sourcerefs = celloSourcereflist, strmarkers = finalmarkerList)
 
    entry
  }
  
}
  
//----------------- Modeling classes, TODO: move in a separate package and import---------------

class Author(val name: String) {

  def toXML =
    <person name={ name }/>
}

class CelloEntry(val ac: String, val oldacs: List[OldAc], val id: String, val synonyms: List[Synonym], val category: String, val sex: String,
                 val dbrefs: List[DbXref], var comments: List[Comment], val webpages: List[WebPage], val diseases: List[CvTerm],
                 val species: List[CvTerm], val origin: List[CvTerm], val derived: List[CvTerm], val publis: List[PubliRef],
                 val sources: List[Source], val sourcerefs: List[PubliRef], val strmarkers: List[Strmarker]) {

  def toXML =
    <cell-line category={ category } sex={ if (sex != "") sex else null }>
      <accession-list>
        <accession type="primary">{ ac }</accession>
        {
          if (oldacs.size > 0) { oldacs.map(_.toXML) }
        }
      </accession-list>
      <name-list>
        <name type="identifier">{ id }</name>
        {
          if (synonyms.size > 0) { synonyms.map(_.toXML) }
        }
      </name-list>
      {
        if (comments.size > 0)
          <comment-list>
            { comments.map(_.toXML) }
          </comment-list>
      }
      {
        if (sources.size > 0 )
          <str-list>
            <source-list>
              { sources.map(_.toXML) }
          {
          if (sourcerefs.size > 0 )
            <reference-list>
        	    { sourcerefs.map(_.toXML) }
						</reference-list>
          }
            </source-list>
            <marker-list>
              { strmarkers.map(_.toXML) }
            </marker-list>
          </str-list>
      }
      {
        if (diseases.size > 0)
          <disease-list>
            { diseases.map(_.toXML) }
          </disease-list>
      }
      <species-list>
        { species.map(_.toXML) }
      </species-list>
      {
        if (derived.size > 0)
          <derived-from>
            { derived.map(_.toXML) }
          </derived-from>
      }
      {
        if (origin.size > 0)
          <same-origin-as>
            { origin.map(_.toXML) }
          </same-origin-as>
      }
      {
        if (webpages.size > 0)
          <web-page-list>
            { webpages.map(_.toXML) }
          </web-page-list>
      }
      {
        if (publis.size > 0)
          <reference-list>
            { publis.map(_.toXML) }
          </reference-list>
      }
      {
        if (dbrefs.size > 0)
          <xref-list>
            { dbrefs.map(_.toXML) }
          </xref-list>
      }
    </cell-line>
      
  def toOBO =  {
  var oboEntryString = "\n[Term]\n"
  var currcomment = ""
  
  oboEntryString += "id: " + ac + "\n"
  oboEntryString += "name: " + id.replace('{','(').replace('}',')') + "\n" // obo doesn't like curly brackets
  synonyms.foreach(syno => {oboEntryString += syno.toOBO})
  oboEntryString += "subset: " + category.replace(' ','_') + "\n"
  if(sex != "") oboEntryString += "subset: " + sex.replace(' ','_') + "\n"
  dbrefs.foreach(dbref => {oboEntryString += dbref.toOBO})
  publis.foreach(publi => {oboEntryString += publi.toOBO})
  webpages.foreach(webpage => {oboEntryString += webpage.toOBO})
  diseases.foreach(disease => {oboEntryString += disease.toOBO})
  species.foreach(specie => {oboEntryString += specie.toOBO})
  comments.foreach(comment => { // stick them all in one line
    if(currcomment == "") currcomment = "comment: \""
    else currcomment += " "
    currcomment += comment.toOBO})
  if(currcomment != "") oboEntryString += currcomment.replace("..",".") + "\"\n"
  origin.foreach(origin => {oboEntryString += "relationship: originate_from_same_individual_as " + origin._ac + " ! " + origin._name + "\n"})
  derived.foreach(derivedfrom => {oboEntryString += "relationship: derived_from " + derivedfrom._ac + " ! " + derivedfrom._name + "\n"})
  oboEntryString
  }
  
  def updatDBrefs =  { // find dbrefs associated with 'Discontinued' comments, set their property flags and remove urls
    var discAC = ""
    var property = ""
    var newCommentlist = List[Comment]()
    comments.foreach(comment => {
      if(comment.category.equals("Discontinued") && comment.text.split("; ").size > 2) { //Console.err.println(comment.text)
        discAC = comment.text.split("; ")(1)
        property = comment.text.split("; ")(2)
        dbrefs.foreach(dbref => {
          if(dbref._ac.equals(discAC)) { //Console.err.println(discAC + ": " + property)
            dbref.propname = "Discontinued"
            dbref._property = property
            dbref.final_url = "" // No url
          }
        })
      }
      else newCommentlist = comment :: newCommentlist
    })
    comments = newCommentlist
  }  
}

class StrmarkerData(val alleles: String, val strmarkersources: List[Source], val strmarkersourcerefs: List[PubliRef]) {

  def toXML =
    <marker-data>
			<alleles>{alleles}</alleles>
      {
        if (strmarkersources.size > 0 || strmarkersourcerefs.size > 0)
             <source-list>
              { strmarkersources.map(_.toXML) }
           {
          if (strmarkersourcerefs.size > 0 )
            <reference-list>
        	    { strmarkersourcerefs.map(_.toXML) }
						</reference-list>
          }
           </source-list>
      }
    </marker-data>
}

class Strmarker(val id: String, val conflict: String, val markerdatalist: List[StrmarkerData]) {

  def toXML =
    <marker id={ id } conflict={ conflict }>
			<marker-data-list>
      { markerdatalist.map(_.toXML) }
			</marker-data-list>
    </marker>
}

class Source(val src: String) {

  def toXML =
    <source>{ src }</source>
}

class OldAc(val oldac: String) {

  def toXML =
    <accession type="secondary">{ oldac }</accession>
}

class Synonym(val syno: String) {

  def toXML =
    <name type="synonym">{ syno }</name>
  
  def toOBO =  {
    val synoline = "synonym: \"" + syno.replace('\\','/') + "\" RELATED []\n"
    synoline
  }
}

class PubliRef(val db_ac: String) {

  def toXML =
    <reference resource-internal-ref={ db_ac }/>

  def toOBO =  {
    var drline = "xref: " 
    if(db_ac.contains("Patent")) // replace with url
      drline += "http://www.google.com/patents/" + db_ac.split("=")(1) + "\n"
    else
      drline += db_ac + "\n"
    drline.replace('=',':')
  }
}

class CvTerm(val _terminology: String, val _ac: String, val _name: String) {

  def toXML =
    <cv-term terminology={ _terminology } accession={ _ac }>{ _name }</cv-term>

  def toOBO =  {
    var db = _terminology
    var drline = ""
    if(db.equals("NCBI-Taxonomy")) db = "NCBI_TaxID"
    if(_terminology.equals("Cellosaurus"))
      drline = "relationship: originate_from_same_individual_as " + _ac + " ! " + _name + "\n"
    else  
      drline = "xref: " + db + ":" + _ac + " ! " + _name + "\n"
    drline
  }
}

class WebPage(val url: String) {

  def toXML =
    <url>{ scala.xml.PCData(url) }</url>

  def toOBO =  {
    val drline = "xref: " + url + "\n"
    drline
  }
}

class DbXref(val _db: String, val _ac: String, val _category: String, val _url: String, var _property: String, val _entryCategory: String) {
  var final_url = _url
  var propname = "gene/protein designation" // default value, overriden in comments.updatDBrefs for discontinued cell lines
  
  init
  def init = { // prepare final url from template and accession
    if (_url.contains("%s")) {
      // Deal with a few exceptions...
      if(_db.equals("BTO"))
        final_url = _url.replace("%s", _ac.substring(4)) // BTO:  %s is the numerical part of the BTO:nnnnnnn identifier
      else if(_db.equals("CGH-DB"))
        final_url = _url.replace("%s", _ac.split("-")(0)).replace("%t", _ac.split("-")(1)) // CGH-DB: Note: %s and %t are respectively the values before and after the dash in the DR line.
      else if(_db.equals("CLS"))
        final_url = _url.replace("%s", _ac.split("/")(1)) // CLS: Note: %s is the value after the slash "/" in the DR line.
      else if(_db.equals("ECACC")) {
        var urlCategory = ""
        var modifiedUrl = ""
        var collection = ""
        if(_entryCategory.equals("Hybridoma")) {urlCategory = "hybridoma"; collection="ecacc_gc";}
        else if(_entryCategory.equals("Induced pluripotent stem cell")) {urlCategory = "ipsc"; collection="ecacc_ipsc";}
        else if(_entryCategory.startsWith("chromosomal")) {urlCategory = "humangeneticca"; collection="ecacc_hgc";}
        else if(_entryCategory.startsWith("randomly")) {urlCategory = "humanrandomcontrol"; collection="ecacc_hrc";}
        if(urlCategory != "") modifiedUrl = _url.replace("generalcell", urlCategory).split("&")(0) + "&collection=" + collection
        else modifiedUrl = _url
        final_url = modifiedUrl.replace("%s", _ac)
        }
      else if(_db.equals("NIH-ARP"))
        final_url = _url.replace("%s", _ac.split("-")(1)) // NIH-ARP: Note: %s is the value after the dash in the DR line.
      else if(_db.equals("TKG")) {// TKG: Note: n% is the second digit of the cell line AC and %s is the cell line AC without the 'TKG'
        val digits = _ac.split(" ")(1)
        final_url = _url.replace("%n", digits.substring(1,2)).replace("%s", digits); // wtf! digits.substring(1,1) is empty!!!
        } 
      else // General form
        final_url = _url.replace("%s", _ac)
    }
    else
     final_url = "" // for xrefs like ICLC
  }

  def toXML =
    <xref database={ _db } category={ _category } accession={ _ac }>
      {
        if (_property != "")
          <property-list>
            <property name={ propname } value={ _property }/>
          </property-list>
      }
      {
        if (final_url != "")
      <url>{ scala.xml.PCData(final_url) }</url>
      }
    </xref>

  def toOBO =  {
    val drline = "xref: " + _db + ":" + _ac + "\n"
    drline
  }
}

class Comment(val category: String, var text: String, xmap: scala.collection.mutable.Map[String, (String, String)]) {
  var dbref: DbXref = null
  var ccXreflist = List[DbXref]()
  var cvterm: CvTerm = null
  var method = ""
  var geneName = ""
  var ac2 = ""
  var geneName2 = ""
  init

  def init = { // prepare data for complex comments (methods, xrefs...)
    if (text.contains("Method=")) {
      method = text.split("=")(1).split("; ")(0)
      val toklist = text.split("; ")
      if (toklist.size > 3) { // some data are not xrefs, eg: CVCL_5A48 -> CC   Knockout cell: Method=Homologous recombination; Rad51d. (cricetulus)
        val db = toklist(1)
        val ac = toklist(2)
        geneName = toklist(3)
        if (geneName.size > 9) {
          // Console.err.println("gene: " + geneName)
          if (text.contains(" + ")) { // There is a second dbref: prepare it
            // like  CC   Knockout cell: Method=ZFN; HGNC; 24338; C1GALT1C1 + HGNC; 19139; POMGNT1.
            geneName = geneName.split(" ")(0)
            ac2 = toklist(4)
            geneName2 = toklist(5).split("\\.")(0)
          }
          else // eg: firefly luciferase (with optimized codon usage for mammalian expression)
            geneName = ""
        }
        text = ""
        ccXreflist = new DbXref(_db = db, _ac = ac, _category = xmap(db)._2, _url = xmap(db)._1, _property = geneName,  _entryCategory = "") :: ccXreflist
        if (ac2 != "") {
          ccXreflist = new DbXref(_db = db, _ac = ac2, _category = xmap(db)._2, _url = xmap(db)._1, _property = geneName2,  _entryCategory = "") :: ccXreflist
        }
      }
    }
    else if (category.equals("Monoclonal antibody target") && text.contains(";")) {
      val linetoks = text.split("; ")
      val db = linetoks(0)
      val ac = linetoks(1)
      var newtext = linetoks(2)
      var i = 0
      if(linetoks.size > 3) // like "Monoclonal antibody target: UniProtKB; P02724; Human GYPA/CD235a (with L-20 and E-24; recognizes N blood group antigen)."
        for(i <- 3 to linetoks.size-1)
          newtext += "; " + linetoks(i)
      if(db != "ChEBI")
        ccXreflist = new DbXref(_db = db, _ac = ac, _category = xmap(db)._2, _url = xmap(db)._1, _property = newtext,  _entryCategory = "") :: ccXreflist
      else
        cvterm = new CvTerm(_terminology = "ChEBI", _ac = ac.split("\\)")(0), _name = newtext)
      text = "" // Since the text is copied either as a property or name there is no need to display as comment's text
    }
    else if (category.equals("Transfected with") && text.contains(";")) {
      val linetoks = text.split("; ")
      val db = linetoks(0);
      if (xmap.contains(db)) {
        var property = linetoks(2)
        var i = 0
        if(linetoks.size > 3) // like "Transfected with: UniProtKB; P42212; GFP (with L-64, T-65 and L-231 = EGFP). Transfected with: UniProtKB; Q99ZW2; Streptomyces pyogenes Cas9 (nuclear version; nCas9n)."
          for(i <- 3 to linetoks.size-1)
            property += "; " + linetoks(i)
        ccXreflist = new DbXref(_db = db, _ac = linetoks(1), _category = xmap(db)._2, _url = xmap(db)._1, _property = property,  _entryCategory = "") :: ccXreflist
      }
    }
    else if ((category.equals("Transformant") || category.equals("Selected for resistance to")) && text.contains(";") ) {
      val linetoks = text.split("; ")
      var terminology = linetoks(0);
      val ac = linetoks(1)
      val name = linetoks(2)

      text = ""
      if(terminology == "NCBI_TaxID") terminology = "NCBI-Taxonomy"
      if(terminology == "UniProtKB")
        ccXreflist = new DbXref(_db = terminology, _ac = ac, _category = xmap(terminology)._2, _url = xmap(terminology)._1, _property = name,  _entryCategory = "") :: ccXreflist
      else  
        cvterm = new CvTerm(_terminology = terminology, _ac = ac, _name = name)
    }
  }

  def toXML =
    <comment category={ category }> { if(text != "") {text} } { if (method != "") <method>{ method }</method> } { if (cvterm != null) {cvterm.toXML}} { if (ccXreflist.size > 0) <xref-list> { ccXreflist.map(_.toXML) } </xref-list> }</comment>

  def toOBO =  {
    var commtext = category + ": " 
    if(ccXreflist.size > 0) {
       if(method != "")
        commtext += "Method=" + method + "; "
      commtext += ccXreflist(0)._db + "; " + ccXreflist(0)._ac + "; " + ccXreflist(0)._property + "."
      }
    else if(cvterm != null) {
      if(category.equals("Transformant")) // xref in a different format, may change in next releases
        commtext += cvterm._name + "(" + cvterm._terminology + "; " + cvterm._ac + ")."
      else  
        commtext += cvterm._terminology + "; " + cvterm._ac + "; " + cvterm._name + "."
      }
    else    
      commtext += text + "."
    commtext
  }
}

class CelloPublication(val year: String, val name: String, val pubtype: String, val volume: String, val firstpage: String, val lastpage: String,
                       val publisher: String, val institute: String, val city: String, val country: String, val internal_id: String, val title: String, val authors: List[Author], val editors: List[Author], val dbrefs: List[DbXref]) {

  def toXML =
    <publication date={ year } type={ pubtype } name={ if (name != "") name else null } volume={ if (volume != "") volume else null } firstpage={ if (firstpage != "") firstpage else null } lastpage={ if (lastpage != "") lastpage else null } publisher={ if (publisher != "") publisher else null } institute={ if (institute != "") institute else null } city={ if (city != "") city else null } country={ if (country != "") country else null } internal-id={ internal_id }>
      <title>{ scala.xml.PCData(title) }</title>
      {
        if (editors.size > 0)
          <editor-list>
            { editors.map(_.toXML) }
          </editor-list>
      }
      <author-list>
        { authors.map(_.toXML) }
      </author-list>
      {
        if (dbrefs.size > 0)
          <xref-list>
            { dbrefs.map(_.toXML) }
          </xref-list>
      }
    </publication>
}

