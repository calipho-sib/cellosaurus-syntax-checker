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
import java.util.Date;
import scala.xml._
import org.nextprot.parser.cellosaurus._

/*

See also

https://www.javadoc.io/doc/org.scala-lang/scala-library/latest/index.html
https://docs.scala-lang.org/scala3/book/string-interpolation.html
https://www.scala-sbt.org/1.x/docs/Compiler-Plugins.html
https://docs.scala-lang.org/overviews/compiler-options/index.html
https://mvnrepository.com/search?q=sbt-assembly

*/

object CelloParser {

  var doesntDie: Boolean = false

  val ok_seqvardblist = List("HGNC", "MGI", "RGD", "UniProtKB", "VGNC")

  val specialCCTopics = List(
    "HLA typing",
    "Genome ancestry",
    "Registration",
    "Sequence variation",
    "Misspelling",
    "Doubling time",
    "Microsatellite instability",
    "Knockout cell",
    "Omics",
    "Monoclonal antibody isotype",
    "Monoclonal antibody target",
    "Derived from site",
    "Cell type",
    "Transformant",
    "Genetic integration",
    "Selected for resistance to",
    "Breed/subspecies"
  )
  val ok_rxdblist = List("PubMed", "Patent", "DOI", "CelloPub")

  def escape_chars_for_obo(s: String): String = {
    // prefix these characters with a backslash
    // \ -> \\
    // { -> \{
    // } -> \}
    // " -> \"
    // see also https://owlcollab.github.io/oboformat/doc/GO.format.obo-1_4.html#S.1.5
    return s
      .replace("\\", "\\\\")
      .replace("{", "\\{")
      .replace("}", "\\}")
      .replace("\"", "\\\"")
  }

  def main(args: Array[String]) = {

    val today = Calendar.getInstance().getTime()
    var todaystring: String = new SimpleDateFormat("yyyy-MM-dd").format(today)
    var started: Boolean = false
    // var debug: Boolean = false
    var obostarted: Boolean = false
    var toOBO: Boolean = false
    var toxml: Boolean = false
    var stats: Boolean = false
    var celloversion: String = ""
    var conflictfilename: String = ""
    var drmapname: String = ""
    var oboheadercomment: String = ""
    var obotypedef: String =
      "\n[Typedef]\nid: derived_from\nname: derived from\nis_transitive: true\n\n" +
        "\n[Typedef]\nid: originate_from_same_individual_as\nname: originate from same individual as\nis_symmetric: true\n"
    var currEntry = ArrayBuffer[String]()
    var aclist = ArrayBuffer[String]()
    var aslist = ArrayBuffer[String]()
    var idlist = ArrayBuffer[String]()
    var hilist = ArrayBuffer[String]()
    var duplist = ArrayBuffer[String]()
    var oigroupmap = Map[String, List[OiEntry]]()
    var drmap = ArrayBuffer[String]()
    val emap = Map.empty[String, Int]
    val line_occmap = Map(
      "ID" -> (1, 1),
      "AC" -> (1, 1),
      "AS" -> (0, 1),
      "SY" -> (0, 1),
      "DR" -> (0, 1999),
      "RX" -> (0, 999),
      "WW" -> (0, 999),
      "CC" -> (0, 999),
      "ST" -> (0, 999),
      "DI" -> (0, 99),
      "OX" -> (1, 999),
      "HI" -> (0, 999),
      "OI" -> (0, 999),
      "SX" -> (0, 1),
      "AG" -> (0, 1),
      "CA" -> (1, 1),
      "DT" -> (0, 1)
    ) // min and max occurences
    val line_ordmap = Map(
      "ID" -> 1,
      "AC" -> 2,
      "AS" -> 3,
      "SY" -> 4,
      "DR" -> 5,
      "RX" -> 6,
      "WW" -> 7,
      "CC" -> 8,
      "ST" -> 9,
      "DI" -> 10,
      "OX" -> 11,
      "HI" -> 12,
      "OI" -> 14,
      "SX" -> 14,
      "AG" -> 15,
      "CA" -> 16,
      "DT" -> 17
    )
    val idacmap = Map.empty[String, String]
    var synoaclist = List.empty[(String, String)]
    var synoidlist = List.empty[(String, String)]
    var stripiddups = List.empty[(String, String)]
    var caseiddups = List.empty[(List[String], List[String])]
    var synsyndups = List.empty[(List[String], List[String])]
    // var namesynodups = List.empty[(String,String)]
    var pmids = Set.empty[String]
    var uniquerefs = Set.empty[String]
    var cellorefs = Set.empty[String]
    var Entries = ArrayBuffer[ArrayBuffer[String]]()
    val acregexp = new Regex("CVCL_[A-Z0-9][A-Z0-9][A-Z0-9][A-Z0-9]$")
    val stdataregexp = new Regex(
      "[0-9][0-9]?(\\.[1-9])?(,[1-9][0-9]?(\\.[1-9]?)?){0,4}( \\([A-Zs][A-Za-z0-9_;=:/\\.\\- ]+\\))?$"
    ) // s is for some_subclones
    val ameloregexp = new Regex(
      "X|X,Y|Y|Not_detected( \\([A-Z][A-Za-z0-9_;=:\\- ]+\\))?$"
    )
    val repcntregexp = new Regex("^[0-9\\.,]{1,30}+$")
    val entrydateregexp = new Regex("^[0-3][0-9]-[0-1][0-9]-[0-2][0-9]$")

    // Just a reminder, the actual CV is stored in celloparser.cv file
    val ok_dblist1 = List(
      "ATCC",
      "BCRC",
      "BCRJ",
      "BTO",
      "BioSample",
      "CBA",
      "CCLE",
      "CCLV",
      "CCRID",
      "CGH-DB",
      "ChEMBL-Cells",
      "ChEMBL-Targets",
      "CLDB",
      "CLO",
      "Coriell",
      "Cosmic",
      "Cosmic-CLP",
      "dbMHC",
      "DGRC",
      "DSMZ",
      "ECACC",
      "EFO",
      "ENCODE",
      "ESTDAB",
      "GDSC",
      "hPSCreg",
      "ICLC",
      "IFO",
      "IGRhCellID",
      "IHW",
      "IMGT/HLA",
      "ISCR",
      "IZSLER",
      "JCRB",
      "KCLB",
      "LINCS",
      "Lonza",
      "MCCL",
      "MeSH",
      "NISES",
      "NIH-ARP",
      "RCB",
      "RSCB",
      "SKIP",
      "SKY/M-FISH/CGH",
      "TKG",
      "Ximbio"
    )

    val ok_seqvarlist =
      List("Gene amplification", "Gene deletion", "Gene fusion", "Mutation")
    val ok_zygositylist = List(
      "-",
      "Hemizygous",
      "Hemizygous or homozygous",
      "Homoplasmic",
      "Homozygous",
      "Mosaic",
      "Unspecified",
      "Heteroplasmic",
      "Heterozygous",
      "Heterozygous or homozygous"                   
    )
    val ok_vartyplist = List(
      "Simple",
      "Simple_corrected",
      "Simple_edited",
      "Repeat_expansion",
      "Repeat_expansion_corrected",
      "Repeat_expansion_edited",
      "Unexplicit",
      "Unexplicit_corrected",
      "Unexplicit_edited",
      "None_reported"
    )
    val ok_amplityplist =
      List("Duplication", "Triplication", "Quadruplication", "Extensive")
    val ok_sxlist =
      List("Female", "Male", "Mixed sex", "Sex ambiguous", "Sex unspecified")
    // Just a reminder, the actual CV is stored in celloparser.cv file
    val ok_cclist1 = List(
      "Anecdotal",
      "Breed/subspecies",
      "Caution",
      "Derived from metastatic site",
      "Derived from sampling site",
      "Discontinued",
      "From",
      "Genome ancestry",
      "Group",
      "HLA typing",
      "Knockout cell",
      "Microsatellite instability",
      "Miscellaneous",
      "Misspelling",
      "Monoclonal antibody isotype",
      "Monoclonal antibody target",
      "Omics",
      "Part of",
      "Population",
      "Problematic cell line",
      "Registration",
      "Selected for resistance to",
      "Genetic integration",
      "Doubling time",
      "Derived from site",
      "Cell type",
      "Transformant",
      "Genetic integration"
    )
    // Just a reminder, the actual CV is stored in celloparser.cv file
    val ok_catlist1 = List(
      "Cancer cell line",
      "Hybrid cell line",
      "Hybridoma",
      "Induced pluripotent stem cell",
      "Adult stem cell",
      "Spontaneously immortalized cell line",
      "Stromal cell line",
      "Conditionally immortalized cell line",
      "Telomerase immortalized cell line",
      "Transformed cell line",
      "Undefined cell line type",
      "Embryonic stem cell",
      "Factor-dependent cell line",
      "Finite cell line"
    )
    var errcnt = 0
    var drcnt = 0
    var rxcnt = 0
    var wwcnt = 0
    var syncnt = 0
    var ageunitcnt = 0
    var agendrange = 0
    var blankcnt = 0
    var curr_line_nb = 0
    var celloentry: CelloEntry = null
    var xmlfile: PrintWriter = null
    var obofile: PrintWriter = null
    val prettyXMLprinter = new scala.xml.PrettyPrinter(512, 2)
    val xmlcopyright =
      <copyright>
        Copyrighted by the SIB Swiss Institute of Bioinformatics.
        Distributed under the Creative Commons Attribution 4.0 International (CC BY 4.0) license - see http://creativecommons.org/licenses/by/4.0/
      </copyright>

    // Parse command line arguments
    if (args.length == 0) {
      Console.err.println(
        "Usage: celloparser.jar cellosaurus.txt [DRmap=DRMapfilename] [conflicts=conflictfilename] [-obo] [-xml] [-stats]"
      )
      Console.err.println(
        "-obo will generate cellosaurus.obo, -xml will generate cellosaurus.xml"
      )
      Console.err.println(
        "conflicts= will generate the five xplicate type tables in given filename"
      )
      sys.exit(1)
    }
    if (!new File(args(0)).exists) {
      Console.err.println(args(0) + " not found"); sys.exit(1)
    }
    args.foreach(arg => {
      if (arg.contains("obo")) {
        toOBO = true; obofile = new PrintWriter(new File("cellosaurus.obo"))
      } else if (arg.toLowerCase().contains("dontdie")) {
        doesntDie = true
      } else if (arg.contains("xml")) {
        toxml = true; xmlfile = new PrintWriter(new File("cellosaurus.xml"))
      } // xmlfile.write("<?xml version=\\"1.0\\" encoding=\\"UTF-8\\"?>")
      else if (arg.contains("stats")) stats = true
      else if (arg.contains("conflicts=")) conflictfilename = arg.split("=")(1)
      else if (arg.contains("DRmap=")) drmapname = arg.split("=")(1)
    })

    // Load CVs for cc, ca, st, xref dbs , site mappings and institution lists
    var jarpath = new File(System.getProperty("java.class.path"));
    Console.err.println("jar path is: " + jarpath)
    var celloCVpath =
      jarpath.getAbsoluteFile().getParentFile().toString() + System.getProperty(
        "file.separator"
      ) + "celloparser.cv"
    var celloXrefpath =
      jarpath.getAbsoluteFile().getParentFile().toString() + System.getProperty(
        "file.separator"
      ) + "cellosaurus_xrefs.txt"
    var celloRefpath =
      jarpath.getAbsoluteFile().getParentFile().toString() + System.getProperty(
        "file.separator"
      ) + "cellosaurus_refs.txt"
    var celloSiteMappingPath =
      jarpath.getAbsoluteFile().getParentFile().toString() + System.getProperty(
        "file.separator"
      ) + "cellosaurus_anatomy.cv"
    var celloInstitutionListPath =
      jarpath.getAbsoluteFile().getParentFile().toString() + System.getProperty(
        "file.separator"
      ) + "cellosaurus_institutions.cv"
    var celloOmicsListPath =
      jarpath.getAbsoluteFile().getParentFile().toString() + System.getProperty(
        "file.separator"
      ) + "cellosaurus_omics.cv"
    var speciesListPath =
      jarpath.getAbsoluteFile().getParentFile().toString() + System.getProperty(
        "file.separator"
      ) + "cellosaurus_species.cv"

    // var celloCVpath = "/home/agateau/workspace/cellosaurus-syntax-checker/celloparser.cv"
    if (!new File(celloSiteMappingPath).exists) {
      Console.err.println(
        "cellosaurus_anatomy.cv not found at: " + celloSiteMappingPath
      ); sys.exit(1)
    }
    if (!new File(celloCVpath).exists) {
      Console.err.println("celloparser.cv not found at: " + celloCVpath);
      sys.exit(1)
    }
    // var celloXrefpath = "/home/agateau/workspace/cellosaurus-syntax-checker/cellosaurus_xrefs.txt"
    if (!new File(celloXrefpath).exists) {
      Console.err.println(
        "cellosaurus_xrefs.txt not found at: " + celloXrefpath
      ); sys.exit(1)
    }
    // var celloRefpath = "/home/agateau/workspace/cellosaurus-syntax-checker/cellosaurus_refs.txt"
    if (!new File(celloRefpath).exists) {
      Console.err.println("cellosaurus_refs.txt not found at: " + celloRefpath);
      sys.exit(1)
    }
    if (!new File(celloInstitutionListPath).exists) {
      Console.err.println("cellosaurus_institutions.cv not found at: " + celloInstitutionListPath);
      sys.exit(1)
    }
    if (!new File(celloOmicsListPath).exists) {
      Console.err.println("cellosaurus_omics.cv not found at: " + celloOmicsListPath);
      sys.exit(1)
    }
    if (!new File(speciesListPath).exists) {
      Console.err.println("cellosaurus_species.cv not found at: " + speciesListPath);
      sys.exit(1)
    }
    var ca = ArrayBuffer[String]()
    var cc = ArrayBuffer[String]()
    var dr = ArrayBuffer[String]()
    var st = ArrayBuffer[String]()
    var hlatypes = ArrayBuffer[String]()
    var hlatype2hgnc = Map[String, String]()
    var poptypes = ArrayBuffer[String]()
    var valid_element = ""

    if (toxml) SiteMapping.load(celloSiteMappingPath)

    for (line <- Source.fromFile(celloCVpath).getLines()) {
      if (line.matches("[CDSHP][ACRTLO]   .*")) {
        valid_element = line.substring(5).trim()
        if (valid_element.contains("#"))
          valid_element = valid_element.split("#")(0).trim()
      }
      if (line.startsWith("DR   ")) {
        dr += valid_element
      } else if (line.startsWith("CA   ")) {
        ca += valid_element
      } else if (line.startsWith("CC   ")) {
        cc += valid_element
      } else if (line.startsWith("ST   ")) {
        st += valid_element
      } else if (line.startsWith("HL   ")) {
        val items = valid_element.split("\t")
        val key = items(0).trim()
        val value = "HGNC=" + items(1).trim()
        hlatypes += key
        hlatype2hgnc.put(key, value)
      } else if (line.startsWith("PO   ")) {
        poptypes += valid_element
      }
    }
    val ok_dblist = dr.toList
    val ok_cclist = cc.toList
    val ok_catlist = ca.toList
    val ok_stlist = st.toList
    val ok_hlatypeslist = hlatypes.toList
    val ok_poptypeslist = poptypes.toList

    // Prepare subsetdef list from categories (for OBO generation)
    var subsetdefs = ""
    ok_catlist.foreach(cat => { // categories
      subsetdefs += "subsetdef: " + cat.replace(" ", "_") + " \"" + cat + "\"\n"
    })
    // Add possible Sexes
    subsetdefs += "subsetdef: Female \"Female\"\n"
    subsetdefs += "subsetdef: Male \"Male\"\n"
    subsetdefs += "subsetdef: Mixed_sex \"Mixed sex\"\n"
    subsetdefs += "subsetdef: Sex_ambiguous \"Sex ambiguous\"\n"
    subsetdefs += "subsetdef: Sex_unspecified \"Sex unspecified\"\n"
    subsetdefs = subsetdefs.split("\n").sortWith(_ < _).mkString("\n")

    // Parse cellosaurus xref file to get databases categories and urls
    DbXrefInfo.load(celloXrefpath)
    // Initialize the source checker (recognizes PubliRefs, Xrefs and OrgRefs)
    val instDict = SourceChecker.loadInstitutionFile(celloInstitutionListPath)
    println("args(0):" + args(0))
    val cpDict = SourceChecker.loadHierarchy(args(0))
    SourceChecker.init(DbXrefInfo.getDbSet(), instDict, cpDict)
    SpeciesChecker.init(speciesListPath)
    OmicsParser.load(celloOmicsListPath)

    /*
    println("Direct_author_submission: " + SourceChecker.isKnownMiscRef("Direct_author_submission"))
    println("Direct_author_submission: " + SourceChecker.isKnown("Direct_author_submission"))
    println("DepMap:" + SourceChecker.isKnownOrgRef("DepMap"))
    println("Cosmic-CLP:" + SourceChecker.isKnownOrgRef("Cosmic-CLP"))
    sys.exit()
    */

    println(s"Checking ${args(0)} for non UTF-8 characters...")
    if (Utf8Checker.check(args(0))) { println("ok") } else { println("Exiting"); sys.exit() }

    println(s"Checking ${celloRefpath} for non UTF-8 characters...")
    if (Utf8Checker.check(celloRefpath)) { println("ok") } else { println("Exiting"); sys.exit() }


    // Parse cellosaurus txt file, build headers for obo file, and split in flat entries
    // and detect line doublons within cell line records
    var cl_line_map = Map[String,String]()
    for (line <- Source.fromFile(args(0)).getLines()) {
      curr_line_nb += 1
      if (obostarted && !line.contains("-----"))
        oboheadercomment += "!" + line + "\n"
      if (line.startsWith("____")) started = true
      else if (line.startsWith(" Description:")) {
        oboheadercomment += "!" + line + "\n"; obostarted = true
      } else if (line.endsWith("cellosaurus@sib.swiss")) obostarted = false
      else if (started) {
        if (cl_line_map.contains(line)) {
          Console.err.println("Warning, line doublon '" + line + "' at line " + curr_line_nb)
        } else {
          cl_line_map(line)=null
        }
        if (line.length() < 2) {
          println("Warning: blank line at line number " + curr_line_nb);
          blankcnt += 1
        } else currEntry += line
        if (line == "//") {
          cl_line_map.clear()
          Entries += currEntry; currEntry = new ArrayBuffer[String]
        }
      } else if (line.startsWith(" Version:")) celloversion = line.split(" ")(2)
    }

    if (toOBO) {
      val obodate = new SimpleDateFormat("MM:dd:yyyy").format(today)
      val oboheader: String =
        "format-version: 1.2\ndata-version: " + celloversion + "\ndate: " + obodate + " 12:00\ndefault-namespace: cellosaurus\n\n"
      obofile.write(oboheader)
      obofile.write(subsetdefs)
      obofile.write("\nontology: Cellosaurus\n\n")
      obofile.write(oboheadercomment)
    }

    // All entries text-loaded
    var stripid = ""
    var lastid = ""
    var entrynb = 0
    Console.err.println("Error report (" + Entries.size + " entries) :\n")
    Entries.foreach(entry => {
      var id = ""
      var coreid = ""
      var ac = ""
      var drlist = ArrayBuffer[String]()
      var st_lines = ArrayBuffer[String]()
      var localsynlist = List.empty[String] // synonym's list to check against Misspelling comments
      var curr_rank = 0
      var last_rank = 0
      var cell_type_count = 0
      var curr_ccrank = 0
      var last_ccrank = 0
      var strsrcCnt = 0 // count of ST Sources lines in an entry, never > 1
      var hasSTR = false
      val linecntmap = Map(
        "ID" -> 0,
        "AC" -> 0,
        "AS" -> 0,
        "SY" -> 0,
        "DR" -> 0,
        "RX" -> 0,
        "WW" -> 0,
        "CC" -> 0,
        "ST" -> 0,
        "DI" -> 0,
        "OX" -> 0,
        "HI" -> 0,
        "OI" -> 0,
        "SX" -> 0,
        "AG" -> 0,
        "CA" -> 0,
        "DT" -> 0
      ) // Initialize to 0 the line count for each possible field
      if (!(entry(0).startsWith("ID   ") && entry(1).startsWith("AC   "))) {
        Console.err.println(
          "Severe error: Missing ID/AC line at " + entry(
            0
          ) + " Please correct before re-check"
        ); sys.exit(2)
      }

      val singleTopicMap = scala.collection.mutable.Map(
        "CC   Problematic cell line" -> 0,
        "CC   Population" -> 0, 
        "CC   Doubling time" -> 0, 
        "CC   Monoclonal antibody isotype" -> 0, 
        "CC   Genome ancestry" -> 0, 
        "CC   Donor information" -> 0, 
        "CC   Breed/subspecies" -> 0)
        ac = "???"
      entry.foreach(line => {
        if (line.startsWith("AC   ")) ac = line.substring(5).strip()
        val topic = line.split(":")(0)
        if (singleTopicMap.contains(topic)) singleTopicMap(topic) += 1
      })
      for ((topic, count) <- singleTopicMap) {
        if (count > 1) println(s"ERROR in $ac : unexpected multiple $topic topic lines" )
      }

      entry.foreach(entryline => { // println(entryline)
        var entrylinedata = ""
        var header = entryline.substring(0, 2)
        if (entryline.length() > 5) {
          entrylinedata = entryline.substring(5)
          if (!line_ordmap.contains(header)) {
            Console.err.println("Unknown line type: " + entryline); errcnt += 1
          } else {
            linecntmap(header) += 1 // Increment count for line type
            curr_rank = line_ordmap(header)
            if (curr_rank < last_rank) {
              Console.err.println(
                "Misordered line type: " + entryline + " in entry " + id
              ); errcnt += 1
            }
            last_rank = curr_rank
          }
        }
        if (entryline.contains("\t"))
          Console.err.println("Tab found at: " + entryline)
        if (entrylinedata.contains("  "))
          Console.err.println("Multiple spaces found at: " + entryline)
        if (entrylinedata.contains(";;") || entrylinedata.contains("; ;"))
          Console.err.println("Multiple ; found at: " + entryline)
        if (entryline.endsWith(" "))
          Console.err.println("Trailing space found at: " + entryline)
        else if (entryline.endsWith(".."))
          Console.err.println("Double dot found at: " + entryline)
        
        if (entryline.startsWith("ID   ")) { 
          id = entrylinedata; idlist += id 
        
        } else if (entryline.startsWith("AC   ")) {
          ac = entrylinedata
          if (acregexp.findFirstIn(ac) == None) {
            Console.err.println("Incorrect AC format at: " + entryline);
            errcnt += 1
          }
          aclist += ac
          // Map AC to entry
          emap(ac) = entrynb
          // Map AC to ID
          idacmap(ac) = id
          if (conflictfilename != "") {
            if (id.contains(" [")) {
              stripid = id.split(" \\[")(0)
              val default = (-1, "")
              if (idacmap.values.exists(_ == stripid)) {
                // get the pure (no bracket-followed) version of the id
                val mappedac =
                  idacmap.find(_._2 == stripid).getOrElse(default)._1
                stripiddups =
                  stripiddups :+ ((stripid + " ", mappedac.toString()))
              }
              stripiddups = stripiddups :+ ((id, ac))
            } else if (id.toLowerCase == lastid.toLowerCase) {
              val default = (-1, "")
              val mappedac = idacmap.find(_._2 == lastid).getOrElse(default)._1
              val newcasedup = (List(lastid, id), List(mappedac.toString(), ac))
              caseiddups = caseiddups :+ newcasedup
            }
          }
          entrynb += 1;
          lastid = id

        } else if (entryline.startsWith("AS   ")) { // AC Secundary
          val locaslist = entrylinedata.split("; ")
          locaslist.foreach(as => {
            aslist += as
            if (acregexp.findFirstIn(as) == None) {
              Console.err.println("Incorrect AC format at: " + entryline);
              errcnt += 1
            }
          })

        } else if (entryline.startsWith("SY   ")) { // Synonyms
          if (entryline.endsWith(";")) {
            Console.err.println("SY trailing ; found at: " + entryline);
            errcnt += 1
          }
          val locsynlist = entrylinedata.split("; ")
          val synduplist = locsynlist.diff(locsynlist.distinct)
          if (synduplist.size != 0) synduplist.foreach(syn => {
            Console.err.println("Locally duplicated SY: " + syn); errcnt += 1
          })
          locsynlist.foreach(synonym => {
            syncnt += 1
            localsynlist = localsynlist :+ (synonym)
            if (synonym == id) {
              Console.err.println("Synonym exists as main ID: " + id);
              errcnt += 1
            } else if (conflictfilename != "") {
              synoidlist = synoidlist :+ ((synonym.toLowerCase, synonym))
              synoaclist = synoaclist :+ ((synonym.toLowerCase, ac))
            } // add to map
          })

        } else if (entryline.startsWith("DR   ")) { // X-refs
          if (entryline.endsWith(";") || entryline.endsWith(".")) {
            Console.err.println("DR trailing ; found at: " + entryline);
            errcnt += 1
          }
          val DRtokens = entrylinedata.split("; ")
          if (DRtokens.length != 2) {
            Console.err.println(
              "Missing or extra DR argument at: " + entryline
            ); errcnt += 1
          }
          val dbname = DRtokens(0)
          if (!ok_dblist.contains(dbname)) {
            Console.err.println(
              "Illegal db:" + dbname + " found at: " + entryline
            ); errcnt += 1
          }
          drcnt += 1
          // drlist += entrylinedata.split("/")(0) // not necessary anymore (was for AddexBio)
          drlist += entrylinedata
          if (drmapname != "") { // Add DR to DRmap
            if (id.contains(" [")) coreid = id.split("\\[")(0).trim()
            else coreid = id
            drmap += dbname + "\t" + entrylinedata
              .split(";")(1)
              .trim() + "\t" + ac + "\t" + coreid + "\n"
          }

        } else if (entryline.startsWith("RX   ")) { // PubMed/DOIs
          if (!entryline.endsWith(";")) {
            Console.err.println("RX unterminated line found at: " + entryline);
            errcnt += 1
          }
          if (entrylinedata.split("=").length > 2) {
            Console.err.println("Too many ⁼'=' found at: " + entryline); 
            errcnt += 1
          }
          val rxdbname = entrylinedata.split("=")(0)
          if (!ok_rxdblist.contains(rxdbname)) {
            Console.err.println("Illegal db:" + rxdbname + " found at: " + entryline); 
            errcnt += 1
          }
          if (entrylinedata.split("[=;]").length < 2) {
            Console.err.println(
              "No " + rxdbname + " identifier found at: " + entryline + "(" + id + ")"
            ); errcnt += 1
          } else {
            // DOI=10.1577/1548-8667(1998)010<0075:DOLTCL>2.0.CO;2;
            val identifier =
              if (rxdbname == "DOI") entrylinedata.split("=")(1).dropRight(1)
              else entrylinedata.split("[=;]")(1)
            uniquerefs += rxdbname + "=" + identifier // for subsequent consistency check with cellosaurus_refs.txt
            if (rxdbname == "PubMed") {
              if ("^[1-9][0-9]{0,7}".r.findFirstIn(identifier) == None) {
                Console.err.println(
                  "Wrong format for " + rxdbname + " identifier: " + identifier + " found at: " + entryline
                ); errcnt += 1
              } else {
                pmids += identifier
                if (drmapname != "") { // Add DR to DRmap
                  if (id.contains(" [")) coreid = id.split("\\[")(0).trim()
                  else coreid = id
                  drmap += "PubMed\t" + identifier + "\t" + ac + "\t" + coreid + "\n"
                }
              }
            } else if ((rxdbname == "DOI") && !identifier.startsWith("10.")) {
              Console.err.println(
                "Wrong format for " + rxdbname + " identifier: " + identifier + " found at: " + entryline
              ); errcnt += 1
            } else if (
              (rxdbname == "CelloPub") && ("^CLPUB[0-9]{5}".r
                .findFirstIn(identifier)) == None
            ) {
              Console.err.println(
                "Wrong format for " + rxdbname + " identifier: " + identifier + " found at: " + entryline
              ); errcnt += 1
            } else if (
              (rxdbname == "Patent") && ("^[A-Z]{2}(RE)?(H)?[0-9]{5,11}[A-Z]{0,1}[1-9]{0,1}".r
                .findFirstIn(identifier)) == None
            ) {
              Console.err.println(
                "Wrong format for " + rxdbname + " identifier: " + identifier + " found at: " + entryline
              ); errcnt += 1
            }
            rxcnt += 1
          }

        } else if (entryline.startsWith("WW   ")) { // Web links
          try { 
            new WebPage(entrylinedata)
          } catch {
            case e: Exception => {
              errcnt += 1
              Console.err.println(s"ERROR while parsing WW line for ${ac}: ${e.getMessage}")
            }
          }
          wwcnt += 1

        } else if (entryline.startsWith("CC   ")) { // Comments
          if (entryline.contains("==")) {
            Console.err.println("ERROR Found '==' string at: " + entryline);
            errcnt += 1
          }
          val cctopic = entrylinedata.split(":")(0)
          if (!ok_cclist.contains(cctopic)) {
            Console.err.println("Unknown CC topic found at: " + entryline);
            errcnt += 1
          } else curr_ccrank = ok_cclist.indexOf(cctopic)
          if (!entryline.endsWith(".")) {
            Console.err.println("Unterminated CC found at: " + entryline);
            errcnt += 1
          }
          if (curr_ccrank < last_ccrank) {
            Console.err.println(
              "Misordered CC topic line: " + entryline + " in entry " + id
            ); errcnt += 1
          }
          last_ccrank = curr_ccrank
          // check database separator tokens
          val cctext = entrylinedata.split(": ")(1)
          val cctoks = cctext.split("; ")
          if (cctoks.size > 1) {
            val maybedb = cctoks(0)
            if (
              ((maybedb.startsWith("UniProtKB")) || 
              (maybedb.startsWith("HGNC")) || 
              (maybedb.startsWith("NCBI_TaxID")) || 
              (ok_dblist.contains(maybedb))) && cctoks.size < 3
            ) {
              Console.err.println("Missing separator at : " + entryline);
              errcnt += 1
            } else if (maybedb.startsWith("CHEBI")) {
              Console.err.println("Missing 'ChEBI;' database token at: " + entryline); 
              errcnt += 1
            } else if (
              cctopic.contains("target") && 
              !maybedb.startsWith("UniProtKB") && 
              !maybedb.startsWith("ChEBI") && 
              !maybedb.startsWith("FPbase") && 
              !maybedb.startsWith("PubChem")
            ) {
              Console.err.println("Missing database token at: " + entryline);
              errcnt += 1
            } else if (cctext.contains("=ZFN")) {
              val somedb = if (cctoks(1).startsWith("Gene=")) cctoks(1).substring(5) else cctoks(1)
              if (! DbXrefInfo.contains(somedb)) {
                Console.err.println("Wrong database token at: " + entryline + " : <" + somedb + ">" ); 
                errcnt += 1
              }
            }
          }

          if (SimpleSourcedCommentParser.categories.contains(cctopic)) {
            val raw_sc = entryline.substring(5 + cctopic.length + 2)
            // next line can yield some INFO, WARNING, ERROR prints
            val sc = SimpleSourcedCommentParser.parse(raw_sc, id, verbose=true)

          } else if (cctopic == "Discontinued" && !entrylinedata.contains("Catalog number")) {
            // These discontinued CCs must also exist as DR lines
            var discontinued = entrylinedata.split(": ")(1) // just keep db reference
            discontinued = (discontinued.substring(0, discontinued.lastIndexOf(';')).trim())
            if (!drlist.contains(discontinued)) {
              Console.err.println(
                "No match for discontinued: '" + discontinued + "' found among DR lines of " + ac
              ); errcnt += 1
            }


          } else if (cctopic == "From") {
            if (cctoks.size < 3 || cctoks.size > 4) {
              Console.err.println(
                "Invalid number of elements in 'From' comment at: " + entryline
              ); errcnt += 1
            }

          } else if (cctopic == "Doubling time") {
            try {
              val elems = DoublingTimeStateAutomaton.parseLine(cctext.trim)
              val dtlist = DoublingTimeStateAutomaton.buildDtList(elems)
              dtlist.foreach(dt => { 
                val note = if (dt("note") != null) " (Note=" + dt("note") + ")" else ""
                val refs = if (dt("refs") != null) " (" + dt("refs") + ")" else ""
                var text = dt("value") + note + refs
                val sc = SimpleSourcedCommentParser.parse(text, clId = id, verbose = true)
                if (sc.getSourceCount() == 0) {
                  println(s"ERROR: No valid source at cell line '${id}' in ${entryline}")
                }
                val range = DoublingTimeStateAutomaton.getDoublingTimeRangeInHours(dt("value")) // may send WARNINGs
                new DoublingTime(value = dt("value"), note = dt("note"), sc = sc)               // output not used here
              })
            } catch {
              case e: Exception => {
                errcnt += 1
                Console.err.println(s"ERROR while parsing Doubling time comment of ${ac}: ${e.getMessage}")
              }
            }

          } else if (cctopic == "Knockout cell") {
            try {
              KnockoutParser.parseLine(cctext.trim)
            } catch {
              case e: Exception => {
                errcnt += 1
                Console.err.println(s"ERROR while parsing Knockout cell comment of ${ac}: ${e.getMessage}")
              }
            }

          } else if (cctopic == "Omics") {
            try {
              OmicsParser.parseLine(cctext.trim)
            } catch {
              case e: Exception => {
                errcnt += 1
                Console.err.println(s"ERROR while parsing Omics comment of ${ac}: ${e.getMessage}")
              }
            }


          } else if (cctopic == "Microsatellite instability") {
            try {
              val msi = MsiParser.parseLine(cctext.trim, cellLineId = id, verbose = true)
              if (! msi.hasSources ) {
                println(s"ERROR: No valid source at cell line '${id}' in ${entryline}.")
              }
            } catch {
              case e: Exception => {
                errcnt += 1
                Console.err.println(s"ERROR while parsing Microsatellite instability comment of ${ac}: ${e.getMessage}")
              }
            }

          } else if (cctopic == "Monoclonal antibody isotype") {
            try {
              MabisoParser.parseLine(cctext.trim, cellLineId = id, verbose = true)

            } catch {
              case e: Exception => {
                errcnt += 1
                println(s"ERROR while parsing Monoclonal antibody isotype comment of ${ac}: ${e.getMessage}")
              }
            }

          } else if (cctopic == "Monoclonal antibody target") {
            try {
              MabtarParser.parseLine(cctext.trim)
            } catch {
              case e: Exception => {
                errcnt += 1
                println(s"ERROR while parsing Monoclonal antibody target comment of ${ac}: ${e.getMessage}")
              }
            }

            
          } else if (cctopic == "Derived from site") {
            try {
              val result = DerivedFromSiteParser.parseLine(cctext.trim)
            } catch {
              case e: Exception => {
                errcnt += 1
                println(
                  s"ERROR while parsing Derived from site comment of ${ac}: ${e.getMessage}"
                )
              }
            }
          } else if (cctopic == "Transformant") {
            try {
              val result = TransformantParser.parseLine(cctext.trim)
            } catch {
              case e: Exception => {
                errcnt += 1
                println(
                  s"ERROR while parsing Transformant comment of ${ac}: ${e.getMessage}"
                )
              }
            }

          } else if (cctopic == "Genetic integration") {
            try {
              GeneticIntegrationParser.parseLine(cctext.trim)
            } catch {
              case e: Exception => {
                errcnt += 1
                println(s"ERROR while parsing Genetic integration comment of ${ac}: ${e.getMessage}")
              }
            }


          } else if (cctopic == "Selected for resistance to") {
            try {
              val result = ResistanceParser.parseLine(cctext.trim)
              if (result != null && result("db") != null) {
                // try to build a Xref... which may throw a useful error for user
                val xref = new DbXref(result("db"), result("ac"))
              }
            } catch {
              case e: Exception => {
                errcnt += 1
                println(
                  s"ERROR while parsing Selected for resistance to comment of ${ac}: ${e.getMessage}"
                )
              }
            }

          } else if (cctopic == "Breed/subspecies") {
            try {
              val result = BreedParser.parseLine(cctext.trim)
            } catch {
              case e: Exception => {
                errcnt += 1
                println(
                  s"ERROR while parsing Breed/subspecies comment: ${e.getMessage} at line: ${cctext.trim}"
                )
              }
            }

          } else if (cctopic == "Cell type") {
            try {
              new CellType(cctext.trim) // just try to create the element
              cell_type_count += 1
              if (cell_type_count > 1) {
                throw new Exception("More than one cell type for " + ac)
              }
            } catch {
              case e: Exception => {
                errcnt += 1
                println(
                  s"ERROR while parsing Cell type comment: ${e.getMessage} at line: ${cctext.trim}"
                )
              }
            }
          } else if (cctopic == "Misspelling") {
            val ms = new Misspelling(cctext)
            val ms_err = ms.errors
            ms_err.foreach(err => {
              errcnt += 1
              Console.err.println(err)
            })
            // if (cctoks.size < 2) { Console.err.println("Wrong format for " + entryline); errcnt += 1 }
            val misspelledname = cctoks(0)
            if (id == misspelledname) {
              Console.err.println(
                "Misspelled name is in current ID at " + entryline
              ); errcnt += 1
            }
            if (localsynlist.contains(misspelledname)) {
              Console.err.println(
                "Misspelled name is in current SY at " + entryline
              ); errcnt += 1
            }

          } else if (cctopic == "Registration") { // format like an x-ref, registry then registry number
            if (cctoks.size != 2) {
              Console.err.println("Wrong format for " + entryline); errcnt += 1
            }

          // =============================================================================================
          } else if (cctopic == "Sequence variation") { // one of 4 categorie listed in ok_seqvarlist
            val allseqvartoks = cctext.split("; ")
            if (!ok_seqvarlist.contains(allseqvartoks(0))) {
              Console.err.println("Illegal sequence variation category found at: " + entryline) 
              errcnt += 1
            }
            if (allseqvartoks(0) == "Mutation" && !ok_vartyplist.contains(allseqvartoks(4))) {
              Console.err.println("Illegal or missing Mutation type found at: " + entryline); 
              errcnt += 1
            } else if (allseqvartoks(0) == "Gene fusion" && !allseqvartoks(3).contains("+")) {
              Console.err.println("Illegal Gene fusion found at: " + entryline)
              errcnt += 1
            } // implement check for names= ?
            else if (allseqvartoks(0) == "Gene amplification" && !ok_amplityplist.contains(allseqvartoks(4))) {
              Console.err.println("Illegal Gene amplification found at: " + entryline) 
              errcnt += 1
            }
            if (allseqvartoks(0) == "Mutation") { // Zygozity mandatory for Mutation
              var zygofound = false
              allseqvartoks.foreach(token => {
                if (token.startsWith("Zygosity=")) { zygofound = true }
              })
              if (!zygofound) {
                Console.err.println("Zygosity not found at: " + entryline);
                errcnt += 1
              }
            }
            // check gene db
            val db = allseqvartoks(1)
            if (! CelloParser.ok_seqvardblist.contains(db)) {
              Console.err.println("Invalid db in Sequence variation '" + db + "' : " + entryline)
              errcnt += 1
            }

            // check source(s) at the end of line
            val sc = SimpleSourcedCommentParser.parse(entrylinedata, clId = id, verbose=true)
            //println("sc body = <" + sc.body + ">")

            // check zygosity allowed values
            allseqvartoks.foreach(token => {
              if (token.startsWith("Zygosity=")) {
                val tokfields = token.split("=")
                var zygotype = tokfields(1).split("\\(")(0).strip()
                //println("token=<" + token + ">, zygosity=<" + zygotype + ">")
                if (!ok_zygositylist.contains(zygotype)) {
                  Console.err.println("Illegal zygosity found at: " + entryline)
                  errcnt += 1
                }
              }
            })

            // Check optional note just before the sources actually starts with "Note="
            val tokens = sc.body.split("; ")
            val lastToken = tokens(tokens.size -1)
            //println("line = <" + cctext + ">")
            //println("last token = <" + lastToken + ">, line = <" + cctext + ">")
            val expectedPrefixes = List("Note=", "Zygosity=", "Name(s)=", "Clinvar=", "dbSNP=")
            var found = false
            expectedPrefixes.foreach(pfx => { if (lastToken.startsWith(pfx)) found = true })
            if (! found) {
                Console.err.println("Illegal element before source(s) at: " + entryline)
                errcnt += 1
            }

            var textdata = entryline.substring(25).strip()
            var seqvartoks = textdata.split("; ")
            var seqvartype = seqvartoks(0)
            var zygotype = ""
            var mutyp = ""
            if (seqvartype == "Mutation" || seqvartype == "Gene amplification") mutyp = seqvartoks(4)
            seqvartoks.foreach(token => {
              if (token.contains("Zygosity=")) {
                zygotype = token.split("=")(1)
                if (zygotype.contains(" (")) { zygotype = zygotype.split(" \\(")(0) }
                if (zygotype.contains("."))  { zygotype = zygotype.split("\\.")(0) }
              }
            })
            try {
              new SequenceVariation(
                cl_ac = ac, vartyp = seqvartype, mutyp = mutyp, 
                zygosity = zygotype, text = textdata, sourcedComment = sc)
            } catch {
              case e: Exception => {
                Console.err.println(s"ERROR while parsing Sequence variation comment at $ac : ${e.getMessage}")
                errcnt += 1
              } 
            }


          // ================================================================================

          } else if (cctopic == "Genome ancestry") { // check populations cv
            val allpoptoks = cctext.split(" \\(")
            if (allpoptoks.size != 2) {
              Console.err.println("Wrong format for Genome ancestry source in " + ac)
              errcnt += 1
            } else {
              val popsrctok = allpoptoks(1).split("\\)")(0)
              val poptoks = allpoptoks(0).split("; ")
              if (poptoks.size != 7) {
                Console.err.println("Wrong population count for Genome ancestry source in " + ac)
                errcnt += 1
              }
              var totalpercent = 0.0
              poptoks.foreach(token => {
                val onepoptoklist = token.split("=")
                if (onepoptoklist.size != 2) {
                  Console.err.println("Unknown Genome ancestry (" + token + ") found in: " + ac)
                  errcnt += 1
                } else {
                  if (new Regex("^[0-9.]+%$").findFirstIn(onepoptoklist(1)) == None) {
                    Console.err.println("Wrong population frequency format: " + entryline)
                    errcnt += 1
                  } else {
                    totalpercent += onepoptoklist(1).dropRight(1).toFloat
                  }
                  if (!ok_poptypeslist.contains(onepoptoklist(0))) {
                    Console.err.println("Unknown Genome ancestry population (" + token + ") found in: " + ac)
                    errcnt += 1
                  }
                }
              })
              // check total percent is 100
              if (totalpercent < 99.8 || totalpercent > 100.2) {
                Console.err.println("Total count is not 100% at " + entryline);
                errcnt += 1
              }
              if (!allpoptoks(1).endsWith(").")) {
                Console.err.println("Wrong format for Genome ancestry source at " + entryline)
                errcnt += 1
              } else if (popsrctok.contains("PubMed=")) {
                uniquerefs += popsrctok // add pubmeds to refs list
              }
              val sc = SimpleSourcedCommentParser.parse(cctext, clId = id, verbose = true)
              if (sc.getSourceCount() == 0) {
                println(s"ERROR: No valid source at cell line '${id}' in HLA typing: ${cctext}")
              }
            }

          } else if (cctopic == "HLA typing") { // sample HLA typing: A*02:01,02:06; B*40:01:02:01,67:01:01; C*03,07; DPB1*04:01,05:01 (IMGT/HLA).
            // Retokenize to separate items from source Check types
            val alltoks = cctext.split(" \\(")
            if (alltoks.size != 2) {
              Console.err.println("Wrong format for HLA typing source in " + ac)
              errcnt += 1
            } else {
              val srctok = alltoks(1).split("\\)")(0) // content of parentheses at the end of the line (source / xref)
              val hlatoks = alltoks(0).split("; ") // first part of the line
              hlatoks.foreach(token => {
                val onetoklist = token.split("\\*")
                if (onetoklist.size != 2) {
                  Console.err.println("Unknown HLA type (" + token + ") found in: " + ac)
                  errcnt += 1
                } else {
                  val tokstart = onetoklist(0) + "*"
                  if (!ok_hlatypeslist.contains(tokstart)) {
                    Console.err.println("Unknown HLA type (" + token + ") found in: " + ac)
                    errcnt += 1
                  }
                }
              })
              if (!alltoks(1).endsWith(").")) {
                Console.err.println("Wrong format for HLA typing source at " + entryline)
                errcnt += 1
              } else if (srctok.contains("PubMed=") || srctok.contains("Patent=") || 
                         srctok.contains("DOI=") || srctok.contains("CelloPub=")) { 
                uniquerefs += srctok // add references
              }
              // perform source checking
              val sc = SimpleSourcedCommentParser.parse(cctext, clId = id, verbose = true)
              if (sc.getSourceCount() == 0) {
                println(s"ERROR: No valid source at cell line '${id}' in HLA typing: ${cctext}")
              }
            }
          }
        } else if (entryline.startsWith("ST   ")) { // Short tandem repeats
          st_lines += entryline
          hasSTR = true
          // if (!entrylinedata.contains(": ")) { Console.err.println("Incorrect ST data format at: " + entryline); errcnt += 1 }
          if ("[A-Za-z0-9)]: ".r.findFirstIn(entrylinedata) == None) {
            Console.err.println("Incorrect ST data format at: " + entryline);
            errcnt += 1
          } else {
            val sttopic = entrylinedata.split(":")(0)
            val stdata = entrylinedata.split(": ")(1).trim()
            if (!sttopic.contains("Source(s)")) {
              if (!ok_stlist.contains(sttopic)) {
                Console.err.println("Unknown ST site found at: " + entryline);
                errcnt += 1
              } else {
                val data_src = stdata.split("\\(")
                // check ST data sources if any
                if (data_src.size > 1) {
                  val srclist = "(" + data_src(1).trim()
                  SimpleSourcedCommentParser.parse("ST   alleles ... "+ srclist, id, verbose=true)
                }
                // check ST data format
                val chrdata = data_src(0).trim()
                if (sttopic.contains("Amelogenin")) {
                  if (ameloregexp.findFirstIn(stdata) == None) {
                    Console.err.println(
                      "Incorrect ST data format (Amel) at: " + entryline
                    ); errcnt += 1
                  }
                } else if (!stdata.contains("Not_detected")) {
                  // Todo: maybe split at parenthesis and apply a simpler stdataregexp only to the reference part if any
                  if (repcntregexp.findFirstIn(chrdata) == None) {
                    Console.err.println(
                      "Incorrect ST data format (regex) at: " + entryline
                    ); errcnt += 1
                  } // repeat counts
                  else if (stdataregexp.findFirstIn(stdata) == None) {
                    Console.err.println(
                      "Incorrect ST data format (regex) at: " + entryline
                    ); errcnt += 1
                  } // repeats plus references
                  else {
                    // check for order and unicity token by token
                    val toklist = chrdata.split(",")
                    toklist.foreach(token => {
                      // Console.err.println("token: " + token)
                      val dottoken = token.split("\\.")
                      if (dottoken.size > 2) {
                        Console.err.println(
                          "Incorrect ST data format at: " + entryline
                        ); errcnt += 1
                      } // no more one dot oer token
                      else if (dottoken(0).length() > 2) {
                        if (!entrylinedata.contains("Dog ")) {
                          Console.err.println(
                            "Incorrect ST data format at: " + entryline
                          ); errcnt += 1
                        }
                      } // no more than 99 except for dog
                      else if (
                        dottoken.length > 1 && dottoken(1).length() > 1
                      ) {
                        Console.err.println(
                          "Incorrect ST data format at: " + entryline
                        ); errcnt += 1
                      }
                    })
                    // check for order and unicity
                    if (toklist.length != toklist.distinct.length) {
                      Console.err.println(
                        "Duplicate in ST data at: " + entryline
                      ); errcnt += 1
                    } else if (toklist.length > 1) {
                      val doubles = toklist.map { x => x.toDouble }
                      if (doubles.toList != doubles.sortWith(_ < _).toList) {
                        Console.err.println(
                          "Wrong order in ST data at: " + entryline
                        ); errcnt += 1
                      }
                    }
                  }
                }
              }
            } else { // ST   Source(s) line

              // pam check sources
              SimpleSourcedCommentParser.parse("ST   Source(s) ... (" + stdata + ")", id, verbose=true)

              // check sources syntax
              strsrcCnt += 1
              if (stdata.contains(",")) {
                Console.err.println(
                  "No comma allowed in ST source format at: " + entryline
                ); errcnt += 1
              }
              if (stdata.endsWith(";") || stdata.endsWith(".")) {
                Console.err.println("ST trailing ; found at: " + entryline);
                errcnt += 1
              }
              if (stdata.contains("PubMed")) { // PubMed in sources
                val srcrefs = stdata.split("; ").filter(_.startsWith("PubMed"))
                srcrefs.foreach(
                  ref =>
                    { // for subsequent consistency check with cellosaurus_refs.txt
                      if (ref.contains(";"))
                        uniquerefs += ref.split(";")(0)
                      else if (ref.contains(","))
                        uniquerefs += ref.split(",")(0)
                      else
                        uniquerefs += ref
                    }
                )
              }
            }
          }
        } else if (entryline.startsWith("DI   ")) { // Diseases
          if (entryline.endsWith(";") || entryline.endsWith(".")) {
            Console.err.println("DI trailing ; found at: " + entryline);
            errcnt += 1
          }
          val dilist = entrylinedata.split("; ")
          if (dilist.length != 3) {
            Console.err.println(
              "Illegal disease format found at: " + entryline
            ); errcnt += 1
          }
          if (dilist(0) != "NCIt" && dilist(0) != "ORDO") {
            Console.err.println(
              "Illegal disease db:" + dilist(0) + " found at: " + entryline
            ); errcnt += 1
          }
        } else if (entryline.startsWith("OX   ")) { // Organisms

          if ( ! SpeciesChecker.checkAndCountOxLine(entryline)) {
            Console.err.println("ERROR, invalid OX line: " + entryline)
            errcnt += 1
          }

        } else if (entryline.startsWith("HI   ") || entryline.startsWith("OI   ")) { // Hierarchy
          var hitoken = "";
          if (entryline.endsWith(";") || entryline.endsWith(".")) {
            Console.err.println("HI/OI trailing ;/. found at: " + entryline);
            errcnt += 1
          }
          if (!entrylinedata.contains(" ! ")) {
            Console.err.println("Illegal HI/OI format found at: " + entryline);
            errcnt += 1
          }
          val toklist = entrylinedata.split(" ")
          if (toklist.length < 3) {
            Console.err.println("Illegal HI/OI format found at: " + entryline);
            errcnt += 1
          }
          hitoken = toklist(0)
          if (acregexp.findFirstIn(toklist(0)) == None) {
            Console.err.println("Incorrect HI/OI AC format at: " + entryline);
            errcnt += 1
          } else hilist += hitoken;
        } else if (entryline.startsWith("SX   ")) { // Sex
          if (!ok_sxlist.contains(entrylinedata)) {
            Console.err.println("Illegal sex found at: " + entryline);
            errcnt += 1
          }
        } else if (entryline.startsWith("AG   ")) { // Age
          val AGregexp = "AG   [A-Z0-9<>]".r
          val AGregexp2 = "(^[0-9-]+Y)([0-9]+M)?".r
          val AGregexp3 = "^[0-9-]+(F)?M([0-9]+[WD])?".r
          val AGregexp4 = "^[0-9-]+(F)?[WD]$".r
          val firstchar = entryline.substring(5, 6)
          if (AGregexp.findFirstIn(entryline) == None) {
            Console.err.println("Illegal age (case 1) found at: " + entryline);
            errcnt += 1
          } else if (
            (firstchar == "<" || firstchar == ">") && new Regex(
              "AG   [><][1-9]"
            ).findFirstIn(entryline) == None
          ) {
            Console.err.println("Illegal age (case 2) found at: " + entryline);
            errcnt += 1
          } else if ("^[1-9]".r.findFirstIn(entrylinedata) != None) { // starts with digit
            if (
              AGregexp2.findFirstIn(entrylinedata) == None &&
              AGregexp3.findFirstIn(entrylinedata) == None &&
              AGregexp4.findFirstIn(entrylinedata) == None
            ) {
              Console.err.println(
                "Illegal age (case 3) found at: " + entryline
              ); errcnt += 1
            }
          }
          // Further checks for valid ranges/units
          if (AGregexp2.findFirstIn(entrylinedata) != None) {
            val parsepattern = "([0-9-]+)([A-Z]+)([0-9-]+)?([A-Z]+)?".r
            val parsepattern(value1, unit1, value2, unit2) = entrylinedata : @unchecked
            if (value1.contains("-")) {
              val rangelist = value1.split("-")
              ageunitcnt = rangelist(0).toInt
              agendrange = rangelist(1).toInt
            } else {
              ageunitcnt = value1.toInt
              agendrange = ageunitcnt + 1
            }
            if (
              (ageunitcnt >= agendrange) ||
              ageunitcnt > 114 ||
              (unit2 != null && unit2 != "M") ||
              (value2 != null && value2.toInt > 11)
            ) {
              Console.err.println(
                "Illegal age (case 4) found at: " + entryline
              ); errcnt += 1
            }
          } else if (AGregexp3.findFirstIn(entrylinedata) != None) {
            val parsepattern = "([0-9-]+)([A-Z]+)([0-9-]+)?([A-Z]+)?".r
            val parsepattern(value1, unit1, value2, unit2) = entrylinedata : @unchecked
            if (value1.contains("-")) {
              val rangelist = value1.split("-")
              ageunitcnt = rangelist(0).toInt
              agendrange = rangelist(1).toInt
            } else {
              ageunitcnt = value1.toInt
              agendrange = ageunitcnt + 1
            }
            if (
              (ageunitcnt >= agendrange) || ageunitcnt > 36 ||
              (value2 != null && value2.toInt > 50) ||
              (unit1 == "FM" && unit2 != null && !unit2.startsWith("F")) ||
              (unit1 == "M" && unit2 != null && unit2.startsWith("F")) ||
              (unit1 == "FM" && ageunitcnt > 9)
            ) {
              // Console.err.println("ageunitcnt:" + ageunitcnt); // 22
              // Console.err.println("agendrange:" + agendrange); // 24
              // Console.err.println("unit1:" + unit1);           // M
              // Console.err.println("unit2:" + unit2);           // null
              // Console.err.println("value1:" + value1);         // 22-24
              // Console.err.println("value2:" + value2);         // null
              Console.err.println(
                "Illegal age (case 5) found at: " + entryline
              );
              errcnt += 1
            }
          } else if (AGregexp4.findFirstIn(entrylinedata) != None) {
            val parsepattern = "([0-9-]+)([A-Z]+)".r
            val parsepattern(value, unit) = entrylinedata : @unchecked
            if (value.contains("-")) {
              val rangelist = value.split("-")
              ageunitcnt = rangelist(0).toInt
              agendrange = rangelist(1).toInt
            } else {
              ageunitcnt = value.toInt
              agendrange = ageunitcnt + 1
            }
            if (
              (ageunitcnt >= agendrange) ||
              (unit == "FW" && ageunitcnt > 42) ||
              (unit == "FD" && ageunitcnt > 300)
            ) {
              Console.err.println(
                "Illegal age (case 6) found at: " + entryline
              ); errcnt += 1
            }
          }
        } else if (entryline.startsWith("CA   ")) { // Category
          if (!ok_catlist.contains(entrylinedata)) {
            Console.err.println("Illegal category found at: " + entryline);
            errcnt += 1
          }
        } else if (entryline.startsWith("DT   ")) { // Dates and version
          val DTlist = entrylinedata.split("; ")
          if (DTlist.size != 3) {
            Console.err.println("Illegal subfield count at: " + entryline);
            errcnt += 1
          }
          val creatlist = DTlist(0).split(": ")
          if (creatlist(0) != "Created") {
            Console.err.println("Illegal subfield title at: " + entryline);
            errcnt += 1
          }
          val updatlist = DTlist(1).split(": ")
          if (updatlist(0) != "Last updated") {
            Console.err.println("Illegal subfield title at: " + entryline);
            errcnt += 1
          }
          if (
            entrydateregexp.findFirstIn(creatlist(1)) == None ||
            entrydateregexp.findFirstIn(updatlist(1)) == None
          ) {
            Console.err.println("Incorrect date format at: " + entryline);
            errcnt += 1
          }
          val verslist = DTlist(2).split(": ")
          if (verslist(0) != "Version") {
            Console.err.println("Illegal subfield title at: " + entryline);
            errcnt += 1
          } else if (new Regex("^[0-9]+$").findFirstIn(verslist(1)) == None) {
            Console.err.println("Illegal version number: " + entryline);
            errcnt += 1
          }
        } else if (entryline.startsWith("//")) { // Entry terminator, check line occurences in collected entry
          linecntmap.keys.foreach { key =>
            if (
              (linecntmap(key) < line_occmap(key)._1) || (linecntmap(
                key
              ) > line_occmap(key)._2)
            ) {
              if (key == "AC" || key == "ID") {
                Console.err.println(
                  "Severe error: " + key + " in entry " + id + " Please correct before re-check"
                );
                sys.exit(3)
              } else {
                Console.err.println(
                  "Illegal line count for: " + key + " in entry " + id
                ); errcnt += 1
              }
            }
          }
          if (hasSTR && strsrcCnt != 1) {
            Console.err.println("Illegal ST   Source(s) line count in " + ac);
            errcnt += 1
          }
        } else {
          Console.err.println("Invalid line at: " + entryline); errcnt += 1
        } // catches any line not conforming to above patterns
      })


      // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      // Current Entry level checks
      // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

      ShortTandemChecker.check(ac, st_lines.toList)

    })


    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    // All Entries level checks
    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

    println("Checking AC duplicates..." + new java.util.Date())
    duplist = aclist.diff(aclist.distinct)
    if (duplist.length != 0) {
      duplist.foreach(ac => {
        Console.err.println("duplicated AC: " + ac); errcnt += 1
      })
    }

    println("Checking ID duplicates..." + new java.util.Date())
    duplist = idlist.diff(idlist.distinct)
    if (duplist.length != 0) {
      duplist.foreach(id => {
        Console.err.println("duplicated ID: " + id); errcnt += 1
      })
    }

    // Check that no AS exists as a living AC
    println("Checking AC AS  collisions..." + new java.util.Date())
    aslist.foreach { as =>
      if (aclist.contains(as)) {
        Console.err.println("living AC in AS: " + as); errcnt += 1
      }
    }

    // Check for orphans hierarchies (inexistant ACs)
    println("Checking orphan cell lines..." + new java.util.Date())
    val acSet = aclist.toSet
    val misslist = hilist.filter(s => !acSet.contains(s)).toSet
    if (misslist.size != 0) {
      misslist.foreach(ac => {
        Console.err.println("Inexistent HI/OI AC: " + ac); errcnt += 1
      })
    }

    if (toxml) {
      println("Building XML..." + new java.util.Date())
      val xmlheader = // date, version, entry count and pub count are dynamic
        <header>
            <terminology-name>Cellosaurus</terminology-name>
            <description>Cellosaurus: a knowledge resource on cell lines</description>
            <release version={celloversion} updated={
          todaystring
        } nb-cell-lines={Entries.size.toString} nb-publications={
          uniquerefs.size.toString
        } />
         </header>

      xmlfile.write("<Cellosaurus>\n")
      Console.err.println(
        "XML: " + Entries.size + " entries, " + uniquerefs.size + " publications...\n"
      )
      xmlfile.write(prettyXMLprinter.format(xmlheader) + "\n")
      xmlfile.write("<cell-line-list>\n")
    }


    println("Checking OI/HI consistency..." + new java.util.Date())
    // Check OI/HI lines consistency (inter-entry checks)
    var ac = ""
    var oiac = ""
    var oiid = ""
    var parentac = ""
    var parentid = ""
    var ox = ""
    var cellAge = ""
    var cellPopulation = ""
    var cellSex = ""
    var parentAge = ""
    var parentSex = ""
    var parentPopulation = ""
    var parentSpecies = ""
    var category = ""
    var derivedfromcc = ""
    var parentderivedfromcc = ""
    var disease = ""
    var hiCnt = 0
    var dislist = ArrayBuffer[String]()
    var disErrorlist = ArrayBuffer[String]()

    Entries.foreach(entry => {
      category = ""
      cellAge = ""
      cellSex = ""
      cellPopulation = ""
      parentac = ""
      parentAge = ""
      parentPopulation = ""
      parentSex = ""
      parentSpecies = ""
      derivedfromcc = ""
      parentderivedfromcc = ""
      disease = ""
      hiCnt = 0
      dislist.clear
      disErrorlist.clear

      entry.foreach(entryline => {
        if (entryline.startsWith("AC   ")) { ac = entryline.substring(5) }
        else if (entryline.startsWith("OX   ")) { ox = entryline.split("=")(1) }
        else if (
          entryline.contains("Derived from sampling site") || entryline
            .contains("Derived from metastatic site") || entryline
            .contains("Derived from site")
        ) {
          derivedfromcc = entryline.substring(5).split(": ")(1)
        } else if (entryline.startsWith("DI   ")) {
          disease = entryline.split("; ")(2); dislist += disease
        } else if (entryline.startsWith("OI   ")) {
          oiac = entryline.substring(5).split(" ")(0)
          oiid = (entryline.substring(5).split("!")(1)).substring(1)
          if (oiac.equals(ac)) {
            Console.err.println("Self-referencing OI: " + oiac + "/" + ac);
            errcnt += 1
          } else {
            var ok = false
            if (!misslist.contains(oiac)) { // otherwise previously reported
              currEntry = Entries(emap(oiac))
              currEntry.foreach(line => {
                if (line.startsWith("OI   ") && line.contains(ac)) ok = true
              })
              if (!ok) {
                Console.err.println(
                  "Inexistent reciprocal OI/AC: " + oiac + "/" + ac
                ); errcnt += 1
              } else if (idacmap(oiac) != oiid) {
                Console.err.println(
                  "Incorrect OI AC/ID pair: " + oiac + "/" + oiid
                ); errcnt += 1
              }
            }
          }
        } else if (entryline.startsWith("HI   ")) { // Entry has a parent
          hiCnt += 1
          parentac = entryline.substring(5).split(" ")(0)
          parentid = (entryline.substring(5).split("!")(1)).substring(1)
          if (parentac.equals(ac)) {
            Console.err.println("Self-referencing HI: " + parentac + "/" + ac);
            errcnt += 1
          }
          if (!misslist.contains(parentac)) { // otherwise previously reported
            if (idacmap(parentac) != parentid) {
              Console.err.println(
                "Incorrect HI AC/ID pair: " + parentac + "/" + parentid
              ); errcnt += 1
            }
            currEntry = Entries(emap(parentac))
            // Parse parent entry
            currEntry.foreach(line => { // if(ac=="CVCL_A121") println("scanning parent of CVCL_A121: " + line)
              if (line.startsWith("OX   ")) {
                parentSpecies = line.split("=")(1)
              } else if (line.startsWith("DI   ")) {
                if (!dislist.contains(line.split("; ")(2))) {
                  disErrorlist += "Missing parent disease in: " + parentac + "(parent)=" + line
                    .split("; ")(2) + " " + ac + "=" + disease
                }
              } else if (
                line.contains("Derived from sampling site") || line.contains(
                  "Derived from metastatic site"
                ) || line.contains("Derived from site")
              ) {
                parentderivedfromcc = line.substring(5).split(": ")(1)
              } else if (line.startsWith("SX   ")) {
                parentSex = line.split("   ")(1)
              } else if (line.startsWith("AG   ")) {
                parentAge = line.split("   ")(1)
              } else if (line.startsWith("CC   Population")) {
                parentPopulation = line.split(":")(1).trim()
              } else if (line.startsWith("HI   ")) {
                if (line.substring(5).split(" ")(0).equals(ac)) {
                  Console.err.println("Reciprocal HI: " + parentac + "/" + ac);
                  errcnt += 1
                }
              }
            })
          }
        } else if (entryline.startsWith("SX   ")) {
          cellSex = entryline.split("   ")(1)
        } else if (entryline.startsWith("AG   ")) {
          cellAge = entryline.split("   ")(1)
        } else if (entryline.startsWith("CC   Population")) {
          cellPopulation = entryline.split(": ")(1).trim()
        } else if (entryline.startsWith("CA   ")) {
          category = entryline.split("   ")(1)
        }
      })

      if (toxml || toOBO) {
        celloentry = toCelloEntry(entry) // generate model representation

        // pam
        val oiGroup = celloentry.getOiGroup()
        if (oiGroup.length > 0) {
          if (!oigroupmap.contains(oiGroup)) {
            var newList = List[OiEntry]()
            oigroupmap(oiGroup) = newList
          }
          oigroupmap(oiGroup) = celloentry.toOiEntry() :: oigroupmap(oiGroup)
        }

      }
      if (toxml) { // if(ac.startsWith ("CVCL_G193"))
        celloentry.updatDBrefs // Add a property flag to discontinued cell line dbrefs
        xmlfile.write(prettyXMLprinter.format(celloentry.toXML) + "\n")
      }
      if (
        toOBO
      ) // if(ac.startsWith ("CVCL_")) Console.err.println(celloentry.toOBO)
        obofile.write(celloentry.toOBO)
      if (!category.contains("Hybrid")) { // check parent's features that must be transmitted in non-hybrids, outside line loop, needs all entry parsed
        if (hiCnt > 1) {
          Console.err.println("More than one parent for non hybrid in : " + ac);
          errcnt += 1
        }
        if (
          (parentSpecies != "") && !ox
            .contains("hybrid") && (parentSpecies != ox)
        ) {
          Console.err.println(
            "Wrong parent species: " + parentac + "(parent)=" + parentSpecies + " " + ac + "=" + ox
          )
        }
        if (disErrorlist.length != 0) {
          Console.err.println(disErrorlist(0)); errcnt += 1
        }
        if ((parentac != "") && cellSex != parentSex) {
          Console.err.println(
            "Wrong parent's  ( " + parentac + " ) sex match in: " + ac
          ); errcnt += 1
        }
        if ((parentac != "") && cellPopulation != parentPopulation) {
          Console.err.println(
            "Wrong or missing parent's  ( " + parentac + ":" + parentPopulation + " ) population match in: " + ac
          ); errcnt += 1
        }
        if ((parentac != "") && parentAge != "" && cellAge != parentAge) {
          Console.err.println(
            "Wrong parent's  ( " + parentac + ":" + parentAge + ") age match in: " + ac + ":" + cellAge
          ); errcnt += 1
        }
        // else if ((parentac != "") && parentAge == "" && cellAge != parentAge) { Console.err.println("Check sisters  ( " + parentac  + ") for age  in: " + ac); errcnt += 1 }
        if ((parentac != "") && derivedfromcc != parentderivedfromcc) {
          Console.err.println(
            "Missing or conflicting parent's (" + parentac + ") 'derived from' CC in: " + ac
          ); errcnt += 1
        }
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
        try {
          val Cellopub = PublicationBuilder.toCellopublication(publiFlat)
          cellorefs += Cellopub.internal_id // For a subsequent sanity check with uniquerefs as collected in the cellosaurus.txt parsing loop
          xmlfile.write(prettyXMLprinter.format(Cellopub.toXML) + "\n")
          pubcnt += 1
        } catch {
          case e: Exception => {
            println(e.getMessage())
            errcnt += 1          
          }
        }
        publiFlat.clear
        }
      }
      // Console.err.println(pubcnt + " refs in cellosaurus_refs.txt")
      xmlfile.write("</publication-list>\n") // finnish xml file
      xmlfile.write(prettyXMLprinter.format(xmlcopyright))
      xmlfile.write("\n</Cellosaurus>")
      xmlfile.close
      println("Closed XML..." + new java.util.Date())

    }

    if (toOBO) { // finnish OBO file
      obofile.write(obotypedef)
      obofile.close
    }

    println(s"${Entries.length} entries: $errcnt error(s)")
    println(s"${blankcnt} blank line(s)")

    if (conflictfilename != "") { // Output five tables in reference file
      val dupfile = new PrintWriter(new File(conflictfilename))
      var matchedsynos = Set.empty[String]
      val stripidmap =
        stripiddups.groupBy { case (k, v) => k.takeWhile { _ != ' ' } }.map {
          case (k, tups) => (k, tups.map { _._2 })
        }
      dupfile.write("-- Stripped id xplicates --\n\n") // First table
      stripidmap.toSeq.sortWith(_._1 < _._1).foreach { elem =>
        dupfile.write(elem._1 + ": ")
        elem._2.init.foreach { ac => dupfile.write(ac + ", ") }
        dupfile.write(elem._2.last + "\n") // ex: 15C6: CVCL_D147, CVCL_9142
      }
      dupfile.write("\n\n")
      dupfile.write("-- case id xplicates --\n\n")

      // second table: casing duplicates
      caseiddups.toSeq.sortWith(_._1(0) < _._1(0)).foreach {
        elem => // ex: Bob, BOB: CVCL_2317, CVCL_E491
          if (elem._1.size > 2) Console.err.println("watch " + elem._1(0))
          dupfile.write(
            elem._1(0) + ", " + elem._1(1) + ": " + elem._2(0) + ", " + elem
              ._2(1) + "\n"
          )
      }

      // synonym stuff
      val synogroupmap = synoaclist.groupBy { case (k, v) => k }.map {
        case (k, tups) => (k, tups.map { _._2 })
      }
      val synoidgroupmap = synoidlist.groupBy { case (k, v) => k }.map {
        case (k, tups) => (k, tups.map { _._2 })
      }
      dupfile.write("\n\n")
      dupfile.write("-- id/synonyms xplicates --\n\n") // Third table

      idacmap.toSeq.sortWith(_._2 < _._2).foreach { elem =>
        if (synogroupmap.contains(elem._2.toLowerCase)) { // this id exists as synonym elsewhere
          val currsyngroup = synogroupmap(elem._2.toLowerCase).toSet
          if (!currsyngroup.contains(elem._1) || currsyngroup.size > 1) {
            matchedsynos += elem._2.toUpperCase
            dupfile.write(
              elem._2 + ": Name: " + elem._1 + ", Synonym: "
            ) // ex: 143BTK-: Name: CVCL_9W36, Synonym: CVCL_2270
            currsyngroup.init.foreach { ac => dupfile.write(ac + ", ") }
            dupfile.write(currsyngroup.last + "\n")
          }
        }
      }
      dupfile.write("\n\n")
      dupfile.write("-- synonyms/synonyms xplicates --\n\n") // fourth table

      var intersynolineslist = ArrayBuffer[String]()
      synogroupmap.foreach { synogroup =>
        if (
          !matchedsynos.contains(synogroup._1) && synogroup._2.toSet.size > 1
        ) {
          var dupstring = ""
          val synidilst = synoidgroupmap(synogroup._1).toSet
          for (synid <- synidilst.init) dupstring += synid + ", "
          dupstring += synoidgroupmap(
            synogroup._1
          ).toSet.last + ": Synonym: " // ex: 1102: Synonym: CVCL_8030, CVCL_C574
          for (ac <- synogroup._2.init) { dupstring += ac + ", " }
          dupstring += synogroup._2.last
          intersynolineslist += dupstring
        }
      }
      intersynolineslist.sorted.foreach { line => dupfile.write(line + "\n") }

      // Strip all '-', '/', '.', and ' ' characters from Ids
      val punctmap = idacmap.filter((t) =>
        t._2.contains("-") || t._2.contains("/") || t._2.contains(".") || t._2
          .contains(" ")
      )
      var punctduplist = ArrayBuffer[String]()
      var punctduplinelist = ArrayBuffer[String]()
      punctmap.foreach { elem =>
        val stripped = elem._2.filter(!" -/.".contains(_))
        punctduplist += stripped // add the striped version
      }

      val dups = punctduplist.diff(punctduplist.distinct) // get duplicates
      dups.foreach { dup =>
        var dupidlist = Set.empty[String]
        var dupaclist = Set.empty[String]
        var line = ""
        punctmap.foreach { idac =>
          val stripped = idac._2.filter(!" -/.".contains(_))
          if (dup == stripped) { dupidlist += idac._2; dupaclist += idac._1 }
        }
        dupidlist.init foreach { id => line += id + ", " }
        line += dupidlist.last + ": "
        dupaclist.init foreach { ac =>
          line += ac + ", "
        } // ex: 171-4, 17/14: CVCL_G573, CVCL_J097
        line += dupaclist.last
        punctduplinelist += line
      }

      dupfile.write("\n\n")
      dupfile.write("-- punctuaded names xplicates --\n\n") // Fifth table
      punctduplinelist.sorted.foreach { line => dupfile.write(line + "\n") }
      dupfile.close()
    }

    // Sanity check of cellosaurus.txt and cellosaurus_refs.txt content
    if (toxml && (uniquerefs -- cellorefs).size > 0) {
      Console.err.println(
        "\nFatal error: missing references in cellosaurus_refs.txt:"
      )
      (uniquerefs -- cellorefs).foreach(ref => { Console.err.println(ref) })
    }

    if (drmapname != "") { // Write DRmap to file
      val drmapfile = new PrintWriter(new File(drmapname))
      drmap.sortWith(_ < _).foreach { line => drmapfile.write(line) }
      drmapfile.close()
    }

    // pam
    // check OI consistency
    var differCount = 0
    oigroupmap.keys.foreach(gr => {
      var expectedCellCount = 1
      gr.foreach(ch => { if (ch == ',') expectedCellCount += 1 })
      val cellList = oigroupmap(gr)
      val actualCellCount = cellList.size
      var actualCellList = List[String]()
      cellList.foreach(el => { actualCellList = el.ac :: actualCellList })
      if (expectedCellCount != actualCellCount) {
        Console.err.println(
          "Inconsistent OI group " + gr + ": contains these members: " + actualCellList
            .sortWith(_ < _)
            .mkString(",")
        )
      }
      val oiList = oigroupmap(gr)
      val max = oiList.size - 1
      var idx = 0
      while (idx < max) {
        if (!oiList(idx).similar(oiList(idx + 1), gr)) differCount += 1
        idx += 1
      }
    })
    if (differCount > 0)
      Console.err.println(
        "Sister cells with unexpected differences in Sex, Species, Population or Breed/subspecies: " + differCount
      )

    if (stats) {
      println("\n ===== Statistics =====\n")
      println(s"$drcnt Xrefs")
      println(s"$rxcnt RX refs (${pmids.size} unique PMIDs)")
      println(s"$wwcnt Web links")
      println(s"$syncnt synonyms")
      println(s"${SpeciesChecker.getNumberOfDifferentSpecies()} different species")
      println(s"Human: ${SpeciesChecker.getNumberOfLinesWithSpecies("9606")}")
      println(s"Mouse: ${SpeciesChecker.getNumberOfLinesWithSpecies("10090")}")
      println(s"Rat  : ${SpeciesChecker.getNumberOfLinesWithSpecies("10116")}")
      

    }

    println("Parsing End " + new java.util.Date())

  }

  // ------------------ Utility methods, TODO: move in a separate package and import------------------------------


  def toCelloEntry(flatEntry: ArrayBuffer[String]): CelloEntry = {
    var entrylinedata = ""
    var ac = ""
    var id = ""
    var category = ""
    var sex = ""
    var age = ""
    var source_pmid = ""
    var alleles = ""
    var celloCreatDat = "1970-01-01"
    var celloUpdatDat = "1970-01-01"
    var celloVersion = "0"

    var celloStrmarkerlist = List[Strmarker]()
    var celloSTsrclist = List[STsource]()
    var celloPBsrclist = List[PBsource]()
    var celloXRsrclist = List[XRsource]()
    var celloHLAlists = List[HLAlistwithSource]()
    var celloDoublingTimeList = List[DoublingTime]()
    var celloKnockoutList = List[Knockout]()
    var celloOmicsList = List[Omics]()
    var celloMsiList = List[Msi]()
    var celloMabisoList = List[Mabiso]()
    var celloMabtarList = List[Mabtar]()
    var celloDerivedFromSiteList = List[DerivedFromSite]()
    var celloTransformantList = List[Transformant]()
    var celloGenintList = List[GenInt]()
    var celloResistanceList = List[Resistance]()
    var celloBreed : Breed = null
    var celloCellType: CellType = null
    var celloOldaclist = List[OldAc]()
    var celloSynlist = List[Synonym]()
    var celloPublilist = List[PubliRef]()
    var celloDislist = List[DbXref]()
    var celloSpeclist = List[DbXref]()
    var celloOriglist = List[DbXref]()
    var celloDerivedlist = List[DbXref]()
    var celloXreflist = List[DbXref]()
    var celloCommentlist = List[Comment]()
    var celloWebPagelist = List[WebPage]()
    var celloSeqVarlist = List[SequenceVariation]()
    var celloReglist = List[Registration]()
    var celloMisspellingList = List[Misspelling]()
    var popDatawithSource: PopulistwithSource = null

    flatEntry.foreach(entryline => {
      if (entryline.length() > 5) entrylinedata = entryline.substring(5)

      if (entryline.startsWith("AC   ")) ac = entrylinedata // primary accession
      else if (entryline.startsWith("AS   ")) { // secundary accession
        val asList = entrylinedata.split("; ")
        asList.foreach(as => {
          celloOldaclist = new OldAc(oldac = as) :: celloOldaclist
        })
      } else if (entryline.startsWith("ID   "))
        id = entrylinedata // primary name
      else if (entryline.startsWith("SY   ")) { // synonyms
        val synList = entrylinedata.split("; ")
        synList.foreach(synonym => {
          celloSynlist = new Synonym(syno = synonym) :: celloSynlist
        })
      } else if (entryline.startsWith("CA   "))
        category = entrylinedata // Category
      else if (entryline.startsWith("SX   ")) sex = entrylinedata // Sex
      else if (entryline.startsWith("AG   ")) age = entrylinedata // Age
      else if (entryline.startsWith("DR   ")) { // xref
        val db = entrylinedata.split("; ")(0)
        if (!DbXrefInfo.contains(db))
          Console.err.println("Error: no entry for \"" + db + "\" in cellosaurus_xrefs.txt")
        else
          celloXreflist = new DbXref(db = db, ac = entrylinedata.split(";")(1).trim()) :: celloXreflist
      } else if (entryline.startsWith("CC   ")) { // comment
        val linetokens = entrylinedata.split(": ")
        val category = linetokens(0)
        var textdata = linetokens(1).trim()
        // The text token may contain a colon which is not a separator
        if (linetokens.size > 2) textdata += ": " + linetokens(2).trim()
        // Drop final dot
        if ( ! (category.equals("Miscellaneous") || category.equals("Doubling time") || 
                category.equals("Caution") || category.equals("Problematic cell line"))) {
          textdata = textdata.dropRight(1) 
        }

        if (category.equals("HLA typing")) { // prepare hla-lists of gene/alleles with sources
          var celloHLAlist = List[HLAData]()
          val hlatoks = textdata.split(" \\(")(0).split("; ")
          hlatoks.foreach(hlaItem => {
            val hlaItemParts = hlaItem.split("\\*")
            val geneSymbol = "HLA-" + hlaItemParts(0)
            if (hlaItemParts.length > 1) {
              var hlaAlleles = hlaItemParts(1)
              celloHLAlist = new HLAData(geneSymbol = geneSymbol, alleles = hlaAlleles) :: celloHLAlist
            } else {
              Console.err.println("Error: missing '*' in HLA typing line: " + textdata)
            }
          })
          val sc = SimpleSourcedCommentParser.parse(textdata, clId = id, verbose = false)
          if (sc.getSourceCount() == 0) {
            println(s"ERROR: No valid source at cell line '${id}' in HLA typing: ${textdata}, exiting.")
            if (CelloParser.doesntDie) sc.orglist = new STsource("(none)") :: sc.orglist else sys.exit(1) // exit on fatal error
          }
          val celloHLAlistwithSource = new HLAlistwithSource(glist = celloHLAlist.reverse, sc = sc)
          celloHLAlists = celloHLAlistwithSource :: celloHLAlists

        } else if (category.equals("Doubling time")) {
          try {
            val elems = DoublingTimeStateAutomaton.parseLine(textdata)
            val dtlist = DoublingTimeStateAutomaton.buildDtList(elems)
            dtlist.foreach(dt => {
              val note = if (dt("note") != null) " (Note=" + dt("note") + ")" else ""
              val refs = if (dt("refs") != null) " (" + dt("refs") + ")" else ""
              var text = dt("value") + note + refs
              val sc = SimpleSourcedCommentParser.parse(text, clId = id, verbose = false) // err msg handled earlier
              if (sc.getSourceCount() == 0) {
                println(s"ERROR: No valid source at cell line '${id}' in ${entryline}, exiting.")
                if (CelloParser.doesntDie) sc.orglist = new STsource("(none)") :: sc.orglist else sys.exit(1) // exit on fatal error
              }
              val doublingTime = new DoublingTime(value = dt("value"), note = dt("note"), sc = sc)
              celloDoublingTimeList = doublingTime :: celloDoublingTimeList
            })
          } catch {
            case e: Exception => {} // handled earlier
          }

        } else if (category.equals("Knockout cell")) {
          try {
            val koc = KnockoutParser.parseLine(textdata)
            celloKnockoutList = koc :: celloKnockoutList
          } catch {
            case e: Exception => {} // handled earlier
          }

        } else if (category.equals("Omics")) {
          try {
            val om = OmicsParser.parseLine(textdata)
            celloOmicsList = om :: celloOmicsList
          } catch {
            case e: Exception => { } // handled earlier
          }

        } else if (category.equals("Microsatellite instability")) {
          try {
            val msi = MsiParser.parseLine(textdata, cellLineId = id, verbose = false)
            if (! msi.hasSources ) {
              println(s"ERROR: No valid source at cell line '${id}' in ${entryline}, exiting.")
              if (CelloParser.doesntDie) msi.sc.orglist = new STsource("(none)") :: msi.sc.orglist else sys.exit(1) // exit on fatal error
            }
            celloMsiList = msi :: celloMsiList
          } catch {
            case e: Exception => {} // handled earlier
          }

        } else if (category.equals("Monoclonal antibody isotype")) {
          try {
            celloMabisoList = MabisoParser.parseLine(textdata, cellLineId = id, verbose = false)
          } catch {
            case e: Exception => {} // handled earlier
          }

        } else if (category.equals("Monoclonal antibody target")) {
          try {
            celloMabtarList = MabtarParser.parseLine(textdata) :: celloMabtarList
          } catch {
            case e: Exception => {} // handled earlier
          }


        } else if (category.equals("Derived from site")) {
          try {
            val el = DerivedFromSiteParser.parseLine(textdata)
            if (el != null) {
              val derivedFromSite = new DerivedFromSite(
                site = el("site"),
                name = el("name"),
                note = el("note"),
                uber = el("uber")
              )
              celloDerivedFromSiteList =
                derivedFromSite :: celloDerivedFromSiteList
            }
          } catch {
            case e: Exception => {} // handled earlier
          }
        } else if (category.equals("Transformant")) {
          try {
            val el = TransformantParser.parseLine(textdata)
            if (el != null) {
              val transformant = new Transformant(
                db = el("db"),
                ac = el("ac"),
                name = el("name"),
                note = el("note")
              )
              celloTransformantList = transformant :: celloTransformantList
            }
          } catch {
            case e: Exception => {} // handled earlier
          }

        } else if (category.equals("Genetic integration")) {
          try {
            celloGenintList = GeneticIntegrationParser.parseLine(textdata) :: celloGenintList
          } catch {
            case e: Exception => {} // handled earlier
          }
          
        } else if (category.equals("Selected for resistance to")) {
          try {
            val el = ResistanceParser.parseLine(textdata)
            if (el != null) {
              val resistance = new Resistance(db = el("db"), ac = el("ac"), name = el("name"))
              celloResistanceList = resistance :: celloResistanceList
            }
          } catch {
            case e: Exception => {} // handled earlier
          }

        } else if (category.equals("Breed/subspecies")) {
          try {
            celloBreed = BreedParser.parseLine(textdata)
          } catch {
            case e: Exception => {} // handled earlier
          }

        } else if (category.equals("Cell type")) {
          try {
            val ct = new CellType(textdata)
            celloCellType = ct
          } catch {
            case e: Exception => {} // handled earlier
          }
        } else if (category.equals("Genome ancestry")) { // prepare population lists with sources
          var popuSrc = textdata.split("\\(|\\)")(1)

          var cellopoplist = List[PopFreqData]()
          val poptoks = textdata.split(" \\(")(0).split("; ")
          poptoks.foreach(popItem => {
            val popName = popItem.split("=")(0)
            val popFreq = popItem.split("=")(1).dropRight(1) // Remove the % sign
            cellopoplist = new PopFreqData(popName = popName, popFreq = popFreq) :: cellopoplist
          })
          val sc = SimpleSourcedCommentParser.parse(textdata, clId = id, verbose = false) // err msg handled earlier
          if (sc.getSourceCount() == 0) {
            println(s"ERROR: No valid source at cell line '${id}' in ${entryline}, exiting.")
            if (CelloParser.doesntDie) sc.orglist = new STsource("(none)") :: sc.orglist else sys.exit(1) // exit on fatal error
          }
          popDatawithSource = new PopulistwithSource(poplist = cellopoplist.reverse, sc = sc)



        } else if (category.equals("Sequence variation")) { // prepare stuff

          var seqvartoks = textdata.split("; ")
          var seqvartype = seqvartoks(0)
          var zygotype = ""
          var mutyp = ""
          val sc = SimpleSourcedCommentParser.parse(textdata, clId = id, verbose=false)
          if (seqvartype == "Mutation" || seqvartype == "Gene amplification") mutyp = seqvartoks(4)
          seqvartoks.foreach(token => {
            if (token.contains("Zygosity=")) {
              zygotype = token.split("=")(1)
              if (zygotype.contains(" (")) { zygotype = zygotype.split(" \\(")(0) }
              if (zygotype.contains("."))  { zygotype = zygotype.split("\\.")(0) }
            }
          })
          try {
            val sv = new SequenceVariation(
              cl_ac = ac, vartyp = seqvartype, mutyp = mutyp, 
              zygosity = zygotype, text = textdata, sourcedComment = sc)
            celloSeqVarlist = sv  :: celloSeqVarlist
          } catch {
            case e: Exception => {} // handled earlier
          }

        } else if (category.equals("Registration")) { // prepare registration list
          var regtoks = textdata.split("; ")
          celloReglist = new Registration(
            registry = regtoks(0),
            regnumber = regtoks(1)
          ) :: celloReglist
        }

        // TODO misspell
        else if (category.equals("Misspelling")) {
          celloMisspellingList =
            new Misspelling(textdata) :: celloMisspellingList
        }

        // default handler for normal comments
        celloCommentlist = new Comment(category = category, text = textdata, cellId = id) :: celloCommentlist

      } else if (entryline.startsWith("WW   ")) { // web pages
        try {
          val wp = new WebPage(entrylinedata.trim())
          celloWebPagelist = wp :: celloWebPagelist
        } catch {
          case e: Exception => {} // handled earlier
        }

      } else if (entryline.startsWith("DI   ")) { // diseases
        celloDislist = new DbXref(
          db = entrylinedata.split("; ")(0),
          ac = entrylinedata.split("; ")(1),
          label = entrylinedata.split("; ")(2)
        ) :: celloDislist
      } else if (entryline.startsWith("OX   ")) { // species
        celloSpeclist = new DbXref(
          db = "NCBI_TaxID",
          ac = entrylinedata.split("=")(1).split("; ")(0),
          label = entrylinedata.split("! ")(1)
        ) :: celloSpeclist
      } else if (entryline.startsWith("OI   ")) { // same origin
        celloOriglist = new DbXref(
          db = "Cellosaurus",
          ac = entrylinedata.split(" ! ")(0),
          label = entrylinedata.split(" ! ")(1)
        ) :: celloOriglist
      } else if (entryline.startsWith("HI   ")) { // derived from
        celloDerivedlist = new DbXref(
          db = "Cellosaurus",
          ac = entrylinedata.split(" ! ")(0),
          label = entrylinedata.split(" ! ")(1)
        ) :: celloDerivedlist
      } else if (entryline.startsWith("RX   ")) { // publications
        val ref1 = entrylinedata.split("; ")(0)
        val ref2 = if (ref1.endsWith(";")) ref1.dropRight(1) else ref1
        // Console.err.println("RX parsing, building PubliRef: " + entryline + " ->" + ref2.trim() + "<-")
        celloPublilist = new PubliRef(db_ac = ref2.trim()) :: celloPublilist
      } else if (entryline.startsWith("DT   ")) { // dates and version
        var orgdatlist = entrylinedata.split("; ")(0).split(": ")(1).split("-")
        celloCreatDat =
          (orgdatlist(2).toInt + 2000).toString + "-" + orgdatlist(
            1
          ) + "-" + orgdatlist(0) // convert to YYYY-MM-DD, the official xs:date
        orgdatlist = entrylinedata.split("; ")(1).split(": ")(1).split("-")
        celloUpdatDat =
          (orgdatlist(2).toInt + 2000).toString + "-" + orgdatlist(
            1
          ) + "-" + orgdatlist(0) // convert to YYYY-MM-DD, the official xs:date
        celloVersion = entrylinedata.split("; ")(2).split(": ")(1)


      } else if (entryline.startsWith("ST   Source")) {    // short tandem repeats source

        val srcList = entrylinedata.substring(11).strip()  // skip the 'Source(s):' comment
        val sc = SimpleSourcedCommentParser.parse("ST   Source(s) ... (" + srcList + ")", id, verbose=false)
        if (sc.getSourceCount() == 0) {
          println(s"ERROR: No valid source at cell line '${id}' in ${entryline}, exiting.")
          if (CelloParser.doesntDie) sc.orglist = new STsource("(none)") :: sc.orglist else sys.exit(1) // exit on fatal error
        }
        celloXRsrclist = sc.xreflist
        celloPBsrclist = sc.publist  
        celloSTsrclist = sc.orglist

      } else if (entryline.startsWith("ST   ")) { // short tandem repeats

        var markerSTsrclist = List[STsource]()
        var markerPBsrclist = List[PBsource]()
        var markerXRsrclist = List[XRsource]()
        val splitdata = entrylinedata.split(": ") 
        if ( splitdata.size == 2) { // otherwise data is invalid and an ERROR was sent earlier in code
          val id = splitdata(0)
          val rawdata = splitdata(1)
          if (rawdata.contains("(")) { // ST   D21S11: 27,32.2 (PubMed=25877200)
            // Separate allele counts from references
            alleles = rawdata.split(" \\(")(0)
            val allelerefs = rawdata.split("[\\(||\\)]")(1)
            val sc = SimpleSourcedCommentParser.parse("ST   alleles ... (" + allelerefs + ")", id, verbose=false)
            markerSTsrclist = sc.orglist 
            markerPBsrclist = sc.publist     
            markerXRsrclist = sc.xreflist

          } else alleles = rawdata
          val strMarkerdata = new StrmarkerData(
            alleles = alleles,
            stSources = markerSTsrclist,
            pbSources = markerPBsrclist,
            xrSources = markerXRsrclist
          )
          celloStrmarkerlist = new Strmarker(
            id = id,
            conflict = "false",
            markerdatalist = List(strMarkerdata)
          ) :: celloStrmarkerlist
        }
      }
    })
    // conflict example
    // ST   D7S820: 9,10 (Cosmic-CLP; ECACC; PubMed=18713817)
    // ST   D7S820: 10 (PubMed=25877200)

    val markerIdList = celloStrmarkerlist.map(_.id)
    val duplIdlist = markerIdList.diff(markerIdList.distinct).toSet
    var finalmarkerList =
      celloStrmarkerlist.filter(marker => !duplIdlist.contains(marker.id))
    if (duplIdlist.size > 0) {
      duplIdlist.foreach(markerid => { // merge strmarkers with same id
        var newmarkdatalist = List[StrmarkerData]()
        val tomergemarkersList =
          celloStrmarkerlist.filter(marker => markerid == marker.id)
        tomergemarkersList.foreach(strmarker => {
          newmarkdatalist = strmarker.markerdatalist(0) :: newmarkdatalist
        })
        finalmarkerList = new Strmarker(
          id = markerid,
          conflict = "true",
          markerdatalist = newmarkdatalist
        ) :: finalmarkerList
      })
    }
    // issue CP-012: sort by marker id
    val sortedMarkerList = finalmarkerList.sortBy(x => x.id.toLowerCase)
    // Instanciate full entry, .reverse in lists to recover original order
    val entry = new CelloEntry(
      ac = ac,
      oldacs = celloOldaclist,
      id = id,
      synonyms = celloSynlist.reverse,
      credat = celloCreatDat,
      upddat = celloUpdatDat,
      eversion = celloVersion,
      category = category,
      sex = sex,
      age = age,
      dbrefs = celloXreflist.reverse,
      comments = celloCommentlist.reverse,
      webpages = celloWebPagelist.reverse,
      diseases = celloDislist.reverse,
      species = celloSpeclist.reverse,
      origin = celloOriglist.reverse,
      derived = celloDerivedlist.reverse,
      publis = celloPublilist.reverse,
      strSTsources = celloSTsrclist,
      strPBsources = celloPBsrclist,
      strXRsources = celloXRsrclist,
      strmarkers = sortedMarkerList,
      reglist = celloReglist,
      hlalists = celloHLAlists.reverse,
      seqvarlist = celloSeqVarlist.reverse,
      genomeAncestry = popDatawithSource,
      misspellinglist = celloMisspellingList,
      doublingTimeList = celloDoublingTimeList,
      knockoutList = celloKnockoutList,
      omicsList = celloOmicsList.reverse,
      msiList = celloMsiList,
      mabisoList = celloMabisoList,
      mabtarList = celloMabtarList,
      derivedFromSiteList = celloDerivedFromSiteList,
      cellType = celloCellType,
      transformantList = celloTransformantList,
      genintList = celloGenintList,
      resistanceList = celloResistanceList,
      breed = celloBreed
    )
    entry
  }

}

//----------------- Modeling classes, TODO: move in a separate package and import---------------

// pam
class OiEntry(
    val ac: String,
    val sex: String,
    val species: String,
    val population: String,
    val subspecies: String
) {
  override def toString: String = {
    return ac + ": sex=" + sex + ", species=" + species + ", population:" + population + ", breed/subspecies:" + subspecies
  }
  def similar(other: OiEntry, group: String): Boolean = {
    var ok = true
    if (this.sex != other.sex) ok = false
    if (this.species != other.species) ok = false
    if (this.population != other.population) ok = false
    if (this.subspecies != other.subspecies) ok = false
    if (!ok) {
      Console.err.println(
        "Sister cells in OI group " + group + " are not similar: " + this.ac + " and " + other.ac
      )
      Console.err.println("Sister cell " + this.toString)
      Console.err.println("Sister cell " + other.toString)
    }
    return ok
  }
}



class StrmarkerData(
    val alleles: String,
    val stSources: List[STsource],
    val pbSources: List[PBsource],
    val xrSources: List[XRsource]    
) {

  def toXML =
    <marker-data>
			<marker-alleles>{alleles}</marker-alleles>
      {
      if (stSources.size > 0 || pbSources.size > 0 || xrSources.size > 0 )
        <source-list>        
        { if (xrSources.size > 0) xrSources.map(_.toXML) else Null }
        { if (pbSources.size > 0) pbSources.map(_.toXML) else Null }
        { if (stSources.size > 0) stSources.map(_.toXML) else Null }
        </source-list>
      else
        Null
      }
    </marker-data>

}

class Strmarker(
    val id: String,
    val conflict: String,
    val markerdatalist: List[StrmarkerData]
) {

  def toXML =
    <marker id={id} conflict={conflict}>
			<marker-data-list>
      {markerdatalist.map(_.toXML)}
			</marker-data-list>
    </marker>
}



class OldAc(val oldac: String) {

  def toXML =
    <accession type="secondary">{oldac}</accession>
}

class Synonym(val syno: String) {

  def toXML =
    <name type="synonym">{syno}</name>

  def toOBO = {
    // val synoline = "synonym: \"" + syno.replace('\\','/') + "\" RELATED []\n"   // Alain
    val synoline = "synonym: \"" + CelloParser.escape_chars_for_obo(
      syno
    ) + "\" RELATED []\n" // Pam
    synoline
  }
}


class Registration(val registry: String, val regnumber: String) {
  def toXML =
    <registration registry={registry} registration-number={regnumber}/>
}

class HLAData(val geneSymbol: String, val alleles: String) {
  def toXML =
    <hla-gene-alleles gene={geneSymbol} alleles={alleles} />
}


class WebPage(val data: String) {

  val wp = parse(data)

  def parse(data: String): Map[String, String] = {
    val res = scala.collection.mutable.Map[String, String]()
    val elems = data.split("; ")
    if (elems.size != 4) throw new Exception("Expected 4 elements in: " + data)
    val cat = elems(0).trim()
    if (cat == "Provider" || cat == "Info") {
      res("category") = cat
    } else {
      throw new Exception("Invalid category " + cat + " in: " + data)
    }
    val org = elems(1).trim()
    if (org == "-" || SourceChecker.isKnownOrgRef(org)) {
      res("institution") = org
    } else {
      throw new Exception("Unknown institution " + org + " in: " + data)
    }
    res("specifier") = elems(2).trim() // no check for this one
    val url = elems(3).trim()
    if (checkURL(url)) {
      res("url") = url
    } else {
      throw new Exception("Invalid URL " + url + " in : " + data)
    }
    return res
  }

  def checkURL(url: String): Boolean = {
    if (! url.startsWith("http")) return false
    if (url.contains("<")) return false
    if (url.contains(">")) return false
    if (url.contains("\"")) return false
    return true
  }

  def toXML =
    val cat: String = wp("category")
    val org: String = wp("institution")
    val url: String = wp("url")
    val txt: String = wp("specifier")
    <web-page category={cat} institution={org} specifier={txt}>
      <url>{scala.xml.PCData(url)}</url>
    </web-page>

  def toOBO = {
    val drline = "xref: " + wp("url") + "\n"
    drline
  }
}

class CellType(val data: String) {

  val ct = parse(data)

  def parse(data: String): Map[String, Object] = {
    val res = scala.collection.mutable.Map[String, Object]()
    res("xref") = null
    val elems = data.split("; ")
    res("name") = elems(0)
    if (elems.size > 1) {
      val cl_xref = elems(1).split("=")
      if (
        cl_xref.size != 2 || cl_xref(0) != "CL" || !cl_xref(1).startsWith("CL_")
      ) {
        throw new Exception("Invalid CL xref")
      }
      var ac = cl_xref(1)
      if (ac.endsWith(".")) ac = ac.substring(0, ac.length - 1)
      res("xref") = new DbXref("CL", ac, label = SiteMapping.getLabel(ac))
    }
    return res
  }

  def toXML = {
    val xref: DbXref = ct("xref").asInstanceOf[DbXref]
    <cell-type>{ct("name")}
    {
    if (xref != null)
      xref.toXML
    else
      Null
    }
    </cell-type>
  }
}

class Misspelling(val data: String) {

  val result = parse(data)

  val line = data
  // mandatory misspelled label
  val label = result._1
  // optional note
  val note = result._2
  // optional DbXref list
  val xrefs = result._3
  // optional list of internal reference id (publi, patent,...)
  val ref_ids = result._4
  val errors = result._5

  def parse(data: String): (String, String, List[DbXref], List[String], List[String]) = {

    // parsing result variables
    var xlabel = ""
    var note = ""
    var xrefs = List[DbXref]()
    var refs = List[String]()
    var errors = List[String]()

    // we should have at least the name and either a note, a xref or a publi reference
    var parts = data.split("; ")
    if (parts.size < 2) {
      errors =
        ("Invalid Misspelling format, note, dr_ref or rx_ref is missing in : " + data) :: errors
      return (xlabel, note, xrefs, refs, errors)
    }

    var idx = 0
    while (idx < parts.size) {
      // first retrieve the misspelling name (xlabel)
      if (idx == 0) {
        xlabel = parts(0)
        // for parts after first part, check if we get a note, a xref or a reference
      } else {
        val token = parts(idx)
        val dbac = token.split("=")
        if (dbac.size < 2) {
          errors =
            ("Invalid Misspelling format of note, dr_ref or rx_ref in : " + data) :: errors
        } else {
          val db = dbac(0)
          // case for the note
          if (db == "Note") {
            note = dbac(1)
            if (!note.endsWith(".")) note += "."
            // case for a reference
          } else if (
            db == "PubMed" || db == "DOI" || db == "Patent" || db == "CelloPub"
          ) {
            val ref =
              if (token.endsWith(".")) token.substring(0, token.length - 1)
              else token
            refs = ref :: refs
            // case for a xref if check of db is known (contained in xmap)
          } else if (DbXrefInfo.contains(db)) {
            val ac =
              if (dbac(1).endsWith("."))
                dbac(1).substring(0, dbac(1).length - 1)
              else dbac(1)
            val xref = new DbXref(db, ac)
            xrefs = xref :: xrefs
            // case with something unexpected
          } else {
            errors =
              ("Invalid Misspelling, Unknown db resource name in : " + data) :: errors
          }
        }
      }
      idx += 1
    }
    return (xlabel, note, xrefs, refs, errors)
  }

  def toXML =
    <misspelling> 
      <misspelling-name>{label}</misspelling-name>
      {
      if (note.length > 0) 
        <misspelling-note>{note}</misspelling-note>
      else
        Null 
      }
      {
      if (xrefs.size > 0) 
        <xref-list>
          {xrefs.map(_.toXML)}
          </xref-list>
      else
        Null
      }
      {
      if (ref_ids.size > 0) 
        <reference-list>
            {ref_ids.map(id => <reference resource-internal-ref={id}/>)}
          </reference-list>
      else
        Null
      }
    </misspelling>
}

class Transformant(
    val db: String,
    val ac: String,
    val name: String,
    val note: String
) {

  var xref: DbXref = null
  init

  def init = {
    if (db != null && db.length > 0) {
      xref = new DbXref(db, ac, label = name)
    }
  }

  def toXML =
    <transformant>
    {
    if (xref != null)
      xref.toXML
    else 
      name
    }
    {
    if (note != null && note.length > 0) 
      <transformant-note>{note}</transformant-note>
    else
      Null      
    }
    </transformant>
}

class Resistance(val db: String, val ac: String, val name: String) {

  var xref: DbXref = null
  init

  def init = {
    if (db != null && db.length > 0) {
      xref = new DbXref(db, ac, label = name)
    }
  }

  def toXML =
    <resistance>
      {
      if (xref != null) {
        xref.toXML
      } else {
        name
      }
    }
    </resistance>
}

class DerivedFromSite(
    val site: String,
    val name: String,
    val note: String,
    val uber: String
) {

  var xrefs = List[DbXref]()
  init

  def init = {
    if (uber != null && uber.length > 0) {
      // use split on char rather on string (would be double escaped "\\+")
      val items = uber.split('+') 
      items.foreach(item => {
        val xref =
          if (item.startsWith("UBERON"))
            new DbXref("UBERON", item, label = SiteMapping.getLabel(item))
          else if (item.startsWith("PO"))
            new DbXref("PO", item, label = SiteMapping.getLabel(item))
          else 
            new DbXref("Unknown", item, label = SiteMapping.getLabel(item)) // should throw an error
        xrefs = xref :: xrefs
      })
    }
  }

  def toXML =
    <derived-from-site>
      <site site-type={site}>{name}
      {
      if (xrefs.size > 0) 
        xrefs.map(_.toXML)
      else
        Null
      }
      </site>
      {
      if (note != null && note.length > 0)
        <site-note>{note}</site-note>
      else
        Null
      }
    </derived-from-site>
}

class DoublingTime(val value: String, val note: String, val sc: SimpleSourcedComment) {

  val xreflist = sc.xreflist
  val publist = sc.publist
  val srclist = sc.orglist
  val hasSources = (sc.getSourceCount() > 0)

  def toXML =
    <doubling-time>
      <doubling-time-value>{value}</doubling-time-value>
      {
      if (note != null) 
        <doubling-time-note>{note}</doubling-time-note>
      else
        Null
      }
      {
      if (hasSources)
        <source-list>
        { if (xreflist.size>0) xreflist.map(_.toXML) else Null }
        { if (publist.size>0) publist.map(_.toXML) else Null }
        { if (srclist.size>0) srclist.map(_.toXML) else Null }
        </source-list>
      else
        Null
      }
    </doubling-time>
}

class HLAlistwithSource(val glist: List[HLAData], sc: SimpleSourcedComment) {

  val xreflist = sc.xreflist
  val publist = sc.publist
  val srclist = sc.orglist
  val hasSources = (sc.getSourceCount() > 0)

  def toXML =
    <hla-typing>
      <hla-gene-alleles-list>{glist.map(_.toXML)}</hla-gene-alleles-list>
      { 
      if (xreflist.size>0) xreflist.map(_.toXML)
      else if (publist.size>0) publist.map(_.toXML) 
      else if (srclist.size>0) srclist.map(_.toXML) 
      else Null 
      }
    </hla-typing>
}

class PopFreqData(val popName: String, val popFreq: String) {
  def toXML =
    <population population-name={popName} population-percentage={popFreq} />
}

class PopulistwithSource(val poplist: List[PopFreqData], val sc: SimpleSourcedComment) {
  val xreflist = sc.xreflist
  val publist = sc.publist
  val srclist = sc.orglist
  val hasSources = (sc.getSourceCount() > 0)

  def toXML =
    <genome-ancestry>
      <population-list>{poplist.map(_.toXML)}</population-list>
      { 
      if (xreflist.size>0) xreflist.map(_.toXML)
      else if (publist.size>0) publist.map(_.toXML) 
      else if (srclist.size>0) srclist.map(_.toXML) 
      else Null 
      }
    </genome-ancestry>
}


class Comment(val category: String, var text: String, val cellId: String) {

  var xreflist = List[XRsource]()
  var publist = List[PBsource]()
  var srclist = List[STsource]()
  var hasSources: Boolean = false

  init

  def init = { // prepare data for complex comments ...)

    if (SimpleSourcedCommentParser.categories.contains(category) ) {
      val sc = SimpleSourcedCommentParser.parse(text, cellId, verbose=false)
      text = sc.getFinalValue()
      xreflist = sc.xreflist
      publist = sc.publist
      srclist = sc.orglist
      hasSources = (sc.getSourceCount() > 0)
    }
  }

  def toXML =
    // skip comments belonging to special topics since they appear later in a structured form
    if (!CelloParser.specialCCTopics.contains(category))
      <comment category={category}> 
      {if (text != "") text else Null} 
      {if (hasSources)
        <source-list>
        { if (xreflist.size>0) xreflist.map(_.toXML) else Null }
        { if (publist.size>0) publist.map(_.toXML) else Null }
        { if (srclist.size>0) srclist.map(_.toXML) else Null }
        </source-list>
      else
        Null
      }
      </comment>    
    else
      Null


  def toOBO = {
    var commtext = category + ": "
    // the condition below is for obsolete topi "Transfected with"
    // it should never be true
    if (xreflist.size > 0 && ! hasSources) {  // we want xref related to transfected comment, not the sources of the comment
      val xrsource = xreflist(0)
      val xref = xrsource.xref
      commtext += xref.db + "; " + xref.ac + "; " + xref.label + "."
    } else
      commtext += text + "."
    CelloParser.escape_chars_for_obo(commtext)
  }
}

