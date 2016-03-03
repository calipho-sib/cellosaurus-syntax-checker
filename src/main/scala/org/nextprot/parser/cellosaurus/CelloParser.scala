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

object CelloParser {
implicit val codec = Codec("UTF-8")
codec.onMalformedInput(CodingErrorAction.REPLACE)
// grep -n --color='auto' -P "[\x80-\xFF]" cellosaurus.txt


    def main(args: Array[String]) {
      var started : Boolean = false
      var toOBO : Boolean = false
      var stats : Boolean = false
      var drmapname : String = ""
      var bigHeader = ArrayBuffer[String]()
      var currEntry = ArrayBuffer[String]()
      var aclist = ArrayBuffer[String]()
      var idlist = ArrayBuffer[String]()
      var hilist = ArrayBuffer[String]()
      var duplist = ArrayBuffer[String]()
      var drmap = ArrayBuffer[String]()
      val emap    = Map.empty[String, Int]
      val oxmap   = Map.empty[String, Int]
      val line_occmap = Map("ID" -> (1,1), "AC" -> (1,1), "SY" -> (0,1),"DR" -> (0,999),"RX" -> (0,999),"WW" -> (0,999),"CC" -> (0,999),"ST" -> (0,999), "DI" -> (0,99),"OX" -> (1,999),"HI" -> (0,999),
                            "OI" -> (0,999),  "SX" -> (0,1), "CA" -> (1,1))
      val line_ordmap = Map("ID" -> 1, "AC" -> 2, "SY" -> 3, "DR" -> 4, "RX" -> 5, "WW" -> 6, "CC" -> 7, "ST" -> 8, "DI" -> 9, "OX" -> 10, "HI" -> 11, "OI" -> 12,
                              "SX" -> 13, "CA" -> 14)
      val idacmap = Map.empty[String, String]
      var pmids = Set.empty[String]
      var Entries = ArrayBuffer[ArrayBuffer[String]]()
      val acregexp = new Regex("CVCL_[A-Z0-9][A-Z0-9][A-Z0-9][A-Z0-9]$")
      val stdataregexp = new Regex("[1-9][0-9]?(\\.[1-9])?(,[1-9][0-9]?(\\.[1-9]?)?){0,3}( \\([A-Z][A-Za-z;= ]+\\))?$")
      val ameloregexp = new Regex("X(,Y)?( \\([A-Z][A-Za-z;= ]+\\))?$")
      val ok_dblist1 = List("ATCC", "BCRC", "BCRJ", "BTO","BioSample", "CBA", "CCLE", "CCLV", "CCRID", "CGH-DB", "ChEMBL-Cells", "ChEMBL-Targets", "CLDB",
          "CLO", "Coriell", "Cosmic", "Cosmic-CLP", "dbMHC", "DGRC", "DSMZ", "ECACC", "EFO", "ENCODE", "ESTDAB", "GDSC", "hPSCreg", "ICLC",
          "IFO", "IGRhCellID", "IHW", "IMGT/HLA", "ISCR", "IZSLER", "JCRB", "KCLB", "LINCS", "Lonza", "MCCL", "MeSH",
          "NISES", "NIH-ARP", "RCB", "RSCB", "SKIP", "SKY/M-FISH/CGH", "TKG", "Ximbio")
      val ok_rxdblist = List("PubMed", "Patent", "DOI","CelloPub")
      val ok_sxlist = List("Female", "Male", "Mixed sex","Sex ambiguous", "Sex undetermined")
      val ok_cclist1 = List("Breed/subspecies", "Caution", "Derived from metastatic site", "Discontinued", "From","Group", "Knockout cell","Miscellaneous", "Misspelling",
          "Monoclonal antibody target", "Omics", "Part of","Population", "Problematic cell line", "Registration", "Selected for resistance to", "Transfected with")
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
      
      if(args.length == 0) { Console.err.println("Please provide a filename"); sys.exit(1)}
      if(!new File(args(0)).exists) { Console.err.println(args(0) + " not found"); sys.exit(1)}
      args.foreach(arg => {
        if(arg.contains("OBO")) toOBO=true
        else if(arg.contains("stats")) stats=true
        else if(arg.contains("DRmap=")){drmapname = arg.split("=")(1)}
        })
      
      // Load CVs for cc, ca, and xref dbs
      var jarpath = new File(System.getProperty("java.class.path"));
      var celloCVpath = jarpath.getAbsoluteFile().getParentFile().toString() + System.getProperty("file.separator") + "celloparser.cv"
      //Console.err.println("lookin for celloparser.cv at: " + celloCVpath); sys.exit(1);
      if(!new File(celloCVpath).exists) { Console.err.println("celloparser.cv not found at: " + celloCVpath); sys.exit(1)}
      var ca = ArrayBuffer[String]()
      var cc = ArrayBuffer[String]()
      var dr = ArrayBuffer[String]()
      var st = ArrayBuffer[String]()
      var valid_element = ""
        for(line <- Source.fromFile(celloCVpath).getLines()) {
          if(line.matches("[CDS][ACRT]   .*")) {
            valid_element = line.substring(5).trim()
            if(valid_element.contains("#")) valid_element = valid_element.split("#")(0).trim()
          }
          if(line.startsWith("DR   ")) dr += valid_element
          else if(line.startsWith("CA   ")) ca += valid_element
          else if(line.startsWith("CC   ")) cc += valid_element
          else if(line.startsWith("ST   ")) st += valid_element
        }
      val ok_dblist = dr.toList
      val ok_cclist = cc.toList
      val ok_catlist = ca.toList
      val ok_stlist = st.toList
      
      // Parse cellosaurus txt file  
      for(line <- Source.fromFile(args(0)).getLines()) {
        curr_line_nb += 1
        if(line.map(_.toInt).contains(65533)) {println("Warning: " + line); nonUTF8cnt += 1} // code for special '�' replacement character from coded
        if(!started) bigHeader += line
        if(line.startsWith("____")) started=true
        else if (started) {
        if(line.length() < 2) {println("Warning: blank line at line number " + curr_line_nb); blankcnt+=1} 
        else currEntry += line
        if(line == "//") {Entries += currEntry; currEntry = new ArrayBuffer[String]}
        }
      }
    
    // All entries text-loaded
    var entrynb = 0
    Console.err.println("Error report (" + Entries.size + " entries) :\n")
    Entries.foreach(entry =>  {
       var id = ""
       var coreid = ""
       var ac = ""
       var curr_rank = 0
       var last_rank = 0
       val linecntmap = Map("ID" -> 0, "AC" -> 0, "SY" -> 0,"DR" -> 0,"RX" -> 0,"WW" -> 0,"CC" -> 0,"ST" -> 0,"DI" -> 0,"OX" -> 0,"HI" -> 0,
                            "OI" -> 0,  "SX" -> 0, "CA" -> 0) // Initialize to 0 the line count for each possible field
       
       //Console.err.println(entry(0) + " (" + entry.size + " lines)");
       if(!(entry(0).startsWith("ID   ") && entry(1).startsWith("AC   ")))
        {Console.err.println("Severe error: Missing ID/AC line at " + entry(0) + " Please correct before re-check"); sys.exit(2)}
       entry.foreach(entryline =>  {//println(entryline)
        var entrylinedata = ""
        var header = entryline.substring(0, 2)
       if(entryline.length() > 5) {
         entrylinedata = entryline.substring(5)
         if(!line_ordmap.contains(header))  {Console.err.println("Unknown line type: " + entryline); errcnt+=1}
         else {
         linecntmap(header) += 1 // Increment count for line type
         curr_rank = line_ordmap(header)
         if(curr_rank < last_rank) {Console.err.println("Misordered line type: " + entryline + " in entry " + id); errcnt+=1}
         last_rank = curr_rank  
         }
        }
        if(entryline.contains("\t")) Console.err.println("Tab found at: " + entryline)
        if(entrylinedata.contains("  ")) Console.err.println("Multiple spaces found at: " + entryline)
        if(entryline.endsWith(" ")) Console.err.println("Trailing space found at: " + entryline)
        if(entryline.startsWith("ID   ")) {id = entrylinedata; idlist += id}
        else if(entryline.startsWith("AC   ")) {
          ac = entrylinedata
          //println(ac);
          if(acregexp.findFirstIn(ac) == None) {Console.err.println("Incorrect AC format at: " + entryline); errcnt+=1}
          aclist += ac
          // Map AC to entry
          emap(ac) = entrynb
           // Map AC to ID
          idacmap(ac) = id
         entrynb += 1;  
        }
        else if(entryline.startsWith("SY   ")) {
          if(entryline.endsWith(";")) {Console.err.println("SY trailing ; found at: " + entryline); errcnt+=1}
          val locsynlist = entrylinedata.split("; ")
          val synduplist = locsynlist.diff(locsynlist.distinct)
          if(synduplist.size != 0) synduplist.foreach(syn =>  {Console.err.println("Locally duplicated SY: " + syn); errcnt+=1})
          locsynlist.foreach(synonym => {
            syncnt += 1
            if(synonym == id) {Console.err.println("Synonym exists as main ID: " + id); errcnt+=1}
            })
            
        }
        else if(entryline.startsWith("DR   "))  {
                    if(entryline.endsWith(";") || entryline.endsWith(".")) {Console.err.println("DR trailing ; found at: " + entryline); errcnt+=1}
                    val dbname = entrylinedata.split("; ")(0) 
                    if(!ok_dblist.contains(dbname)) {Console.err.println("Illegal db:" + dbname + " found at: " + entryline); errcnt+=1}
                    drcnt += 1
                    if(drmapname != "") { // Add DR to DRmap
                      if(id.contains(" [")) coreid = id.split("\\[")(0).trim() else coreid=id
                      drmap += dbname + "\t" + entrylinedata.split(";")(1).trim() + "\t" + ac + "\t" + coreid + "\n"
                    }
        			}
        else if(entryline.startsWith("RX   "))  {
                    if(!entryline.endsWith(";")) {Console.err.println("RX unterminated line found at: " + entryline); errcnt+=1}
                    val rxdbname = entrylinedata.split("=")(0) 
                    if(!ok_rxdblist.contains(rxdbname)) {Console.err.println("Illegal db:" + rxdbname + " found at: " + entryline); errcnt+=1}
                    if(entrylinedata.split("[=;]").length < 2) {Console.err.println("No " + rxdbname + " identifier found at: " + entryline + "(" + id + ")"); errcnt+=1}
                    else {
                      val identifier = entrylinedata.split("[=;]")(1);
                      if(rxdbname == "PubMed") {
                        if("^[1-9][0-9]{0,7}".r.findFirstIn(identifier) == None) {Console.err.println("Wrong format for " + rxdbname + " identifier: " + identifier + " found at: " + entryline); errcnt+=1}
                        else pmids += identifier
                       }
                      else if((rxdbname == "DOI") && !identifier.startsWith("10."))
                      {Console.err.println("Wrong format for " + rxdbname + " identifier: " + identifier + " found at: " + entryline); errcnt+=1}
                      else if((rxdbname == "CelloPub") && ("^CLPUB[0-9]{5}".r.findFirstIn(identifier)) == None)
                      {Console.err.println("Wrong format for " + rxdbname + " identifier: " + identifier + " found at: " + entryline); errcnt+=1}
                      else if((rxdbname == "Patent") && ("^[A-Z]{2}[0-9]{7,11}[A-Z]{0,1}[1-9]{0,1}".r.findFirstIn(identifier)) == None)
                      {Console.err.println("Wrong format for " + rxdbname + " identifier: " + identifier + " found at: " + entryline); errcnt+=1}
                      rxcnt += 1
                     }
        			}
        else if(entryline.startsWith("WW   ")) {
        			if(!(entrylinedata.startsWith("http://")  || entrylinedata.startsWith("https://") || entrylinedata.startsWith("ftp://")))
        				{Console.err.println("Invalid url found at: " + entryline); errcnt+=1}
        			wwcnt += 1
              }
        else if(entryline.startsWith("CC   ")) {
          val cctopic = entrylinedata.split(":")(0)
          if(!ok_cclist.contains(cctopic)) {Console.err.println("Unknown CC topic found at: " + entryline); errcnt+=1}
          if(!entryline.endsWith(".")) {Console.err.println("Unterminated CC found at: " + entryline); errcnt+=1}
              }
        else if(entryline.startsWith("ST   ")) { // Short tandem repeats
          if(!entrylinedata.contains(":")) {Console.err.println("Incorrect ST data format at: " + entryline); errcnt+=1}
          else if(entrylinedata.split(": ").size < 2) {Console.err.println("Incorrect ST data format at: " + entryline); errcnt+=1}
          else {
          val sttopic = entrylinedata.split(":")(0)
          val stdata = entrylinedata.split(": ")(1).trim()
          if(!sttopic.contains("Source(s)")) {
            if(!ok_stlist.contains(sttopic)) {Console.err.println("Unknown ST site found at: " + entryline); errcnt+=1}
            else { // check ST data format
              if(sttopic.contains("Amelogenin")) {
               if(ameloregexp.findFirstIn(stdata) == None) {Console.err.println("Incorrect ST data format at: " + entryline); errcnt+=1}
              }
              else if(stdataregexp.findFirstIn(stdata) == None) {Console.err.println("Incorrect ST data format at: " + entryline); errcnt+=1}
                 }
               }
              }}
        else if(entryline.startsWith("DI   ")) {
                    if(entryline.endsWith(";") || entryline.endsWith(".")) {Console.err.println("DI trailing ; found at: " + entryline); errcnt+=1 }
                    val dilist = entrylinedata.split("; ")
                    if(dilist.length != 3) {Console.err.println("Illegal disease format found at: " + entryline); errcnt+=1}
                    if(dilist(0) != "NCIt") {Console.err.println("Illegal disease db:" + dilist(0) + " found at: " + entryline); errcnt+=1}
        			}
        else if(entryline.startsWith("OX   ")) {
                    if(entryline.endsWith(";")) {Console.err.println("OX trailing ; found at: " + entryline); errcnt+=1}
                    val oxlist = entrylinedata.split(";")
                    if(oxlist.length != 2) {Console.err.println("Illegal taxonomy format found at: " + entryline); errcnt+=1}
                    val dbname = oxlist(0).split("=")(0)
                    val taxid = oxlist(0).split("=")(1)
                    if(dbname != "NCBI_TaxID") {Console.err.println("Illegal taxonomy db:" + dbname + " found at: " + entryline); errcnt+=1}
                    if(!oxlist(1).contains(" ! ")) {Console.err.println("Illegal taxonomy format found at: " + entryline); errcnt+=1}
                    if((oxlist(1) == " ! Homo sapiens" && (taxid != "9606")) || (oxlist(1) == " ! Mus musculus" && (taxid != "10090"))) Console.err.println("Wrong taxid at: " + entryline)
                    if(!oxmap.contains(taxid)) oxmap(taxid) = 1
                    else oxmap(taxid) += 1
        			}
        else if(entryline.startsWith("HI   ") || entryline.startsWith("OI   ")) {
                    var hitoken = "";
                    if(entryline.endsWith(";") || entryline.endsWith(".")) {Console.err.println("HI/OI trailing ;/. found at: " + entryline); errcnt+=1}
                    if(!entrylinedata.contains(" ! ")) {Console.err.println("Illegal HI/OI format found at: " + entryline); errcnt+=1}
                    val toklist = entrylinedata.split(" ")
                    if(toklist.length < 3) {Console.err.println("Illegal HI/OI format found at: " + entryline); errcnt+=1}
                    hitoken = toklist(0)
                    if(acregexp.findFirstIn(toklist(0)) == None) {Console.err.println("Incorrect HI/OI AC format at: " + entryline); errcnt+=1}
                    else hilist += hitoken;
        			      
            }
        else if(entryline.startsWith("SX   ")) {
         if(!ok_sxlist.contains(entrylinedata)) {Console.err.println("Illegal sex found at: " + entryline); errcnt+=1 } 
         }
        
        else if(entryline.startsWith("CA   ")) {
         if(!ok_catlist.contains(entrylinedata)) {Console.err.println("Illegal category found at: " + entryline); errcnt+=1} 

        }
        else if(entryline.startsWith("//")) { // check line occurences in collected entry
          linecntmap.keys.foreach{ key =>
                                 if((linecntmap(key) < line_occmap(key)._1) || (linecntmap(key) > line_occmap(key)._2))
                                   if(key == "AC" || key == "ID") {Console.err.println("Severe error: " + key + " in entry " + id + " Please correct before re-check"); sys.exit(3)}
                                   else {Console.err.println("Illegal line count for: " + key + " in entry " + id); errcnt+=1}}
        }
      })
     })
    
    // Entry level checks
    duplist = aclist.diff(aclist.distinct)
    if(duplist.length != 0)  {duplist.foreach(ac =>  {Console.err.println("duplicated AC: " + ac); errcnt+=1})}
 
    duplist = idlist.diff(idlist.distinct)
    if(duplist.length != 0)  {duplist.foreach(id =>  {Console.err.println("duplicated ID: " + id); errcnt+=1})}
    
    val misslist = hilist.filter(s => !aclist.contains(s)).toSet
    //if(misslist.length != 0)  { misslist.foreach(ac =>  {Console.err.println("Inexistent HI/OI AC: " + ac); errcnt+=1})}
    if(misslist.size != 0)  { misslist.foreach(ac =>  {Console.err.println("Inexistent HI/OI AC: " + ac); errcnt+=1})}
    
   // Check OI/HI lines consistency
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
    Entries.foreach(entry =>  {
      parentSex = ""
      parentSpecies = ""
      category = ""
      disease = ""
      dislist.clear
      disErrorlist.clear
      entry.foreach(entryline =>  {
      if(entryline.startsWith("AC   ")) { ac = entryline.substring(5); }  
      else if(entryline.startsWith("OX   ")) { ox= entryline.split("=")(1)}
      else if(entryline.startsWith("DI   ")) { disease= entryline.split("; ")(2); dislist += disease}
      else if(entryline.startsWith("OI   ")) {
             oiac = entryline.substring(5).split(" ")(0)
             oiid = (entryline.substring(5).split("!")(1)).substring(1)
             if(oiac.equals(ac)) {Console.err.println("Self-referencing OI: " + oiac + "/" + ac); errcnt+=1}
             else {
             var ok = false
             if(!misslist.contains(oiac))  { // otherwise previously reported
             currEntry = Entries(emap(oiac))
             currEntry.foreach(line =>  { //println(line)
             if(line.startsWith("OI   ") && line.contains(ac)) ok = true 
             })
             if(!ok) {Console.err.println("Inexistent reciproque OI/AC: " + oiac + "/" + ac); errcnt+=1}
             else if(idacmap(oiac) != oiid) {Console.err.println("Incorrect OI AC/ID pair: " + oiac + "/" + oiid); errcnt+=1}
             }}}  
      else if(entryline.startsWith("HI   ")) {
             oiac = entryline.substring(5).split(" ")(0)
             oiid = (entryline.substring(5).split("!")(1)).substring(1)
             if(oiac.equals(ac)) {Console.err.println("Self-referencing HI: " + oiac + "/" + ac); errcnt+=1}
             if(!misslist.contains(oiac))  {  // otherwise previously reported
             if(idacmap(oiac) != oiid) {Console.err.println("Incorrect HI AC/ID pair: " + oiac + "/" + oiid); errcnt+=1}
             currEntry = Entries(emap(oiac))
             // Parse parent entry
             currEntry.foreach(line =>  { //if(ac=="CVCL_A121") println("scanning parent of CVCL_A121: " + line)
             if(line.startsWith("OX   ")) {parentSpecies = line.split("=")(1)}
             else if(line.startsWith("DI   ")) {if (!dislist.contains(line.split("; ")(2))) {disErrorlist += "Missing parent disease in: " + oiac + "(parent)=" + line.split("; ")(2) + " " + ac + "=" + disease}}
             else if(line.startsWith("SX   ")) {parentSex = line.split("   ")(1)}
             else if(line.startsWith("HI   ")) {if(line.substring(5).split(" ")(0).equals(ac)) {Console.err.println("Reciprocal HI: " + oiac + "/" + ac); errcnt+=1}}
             })}}
             
      else if(entryline.startsWith("SX   ") && parentSex != "") {
             if(entryline.split("   ")(1) != parentSex) {Console.err.println("Wrong parent sex match: " + oiac + ":" + ac); errcnt+=1}
             }
      else if(entryline.startsWith("CA   ")) { category = entryline.split("   ")(1) }
    })
  if(!category.contains("Hybrid")) {
    if ((parentSpecies != "") && !ox.contains("hybrid") && (parentSpecies != ox)) {Console.err.println("Wrong parent species: " + oiac + "(parent)=" + parentSpecies + " " + ac + "=" + ox)}
    if(disErrorlist.length != 0) {Console.err.println(disErrorlist(0)); errcnt+=1}
    }
  })

      
   println(Entries.length + " entries: " + errcnt + " error(s)")    
   println(nonUTF8cnt + " non-UTF8 character containing line(s), " + blankcnt + " blank line(s)")    
   
   if(stats)  {
      println("\n ===== Statistics =====\n")
      println(drcnt + " Xrefs")
      println(rxcnt + " RX refs (" + pmids.size + " unique PMIDs)")
      println(wwcnt + " Web links")
      println(syncnt + " synonyms")
      println(oxmap.size + " different species")
      oxmap.keys.foreach{ i =>  
                        if(i == "9606")  println("Human: " + oxmap(i) )
                        else if(i == "10090")  println("Mouse: " + oxmap(i) )
                        else if(i == "10116")  println("Rat: " + oxmap(i) )
                        }
      }
    
    if(drmapname != "") { // Write DRmap to file
      val drmapfile = new PrintWriter(new File(drmapname))
      drmap.sortWith(_ < _).foreach { line => drmapfile.write(line) }
      drmapfile.close()
    }
   }
}