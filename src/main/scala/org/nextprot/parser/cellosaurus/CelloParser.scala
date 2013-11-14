package org.nextprot.parser.cellosaurus

import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map
import scala.util.matching.Regex
import java.nio.charset.CodingErrorAction
import scala.io.Codec

object CelloParser {
implicit val codec = Codec("UTF-8")
codec.onMalformedInput(CodingErrorAction.REPLACE)
// grep -n --color='auto' -P "[\x80-\xFF]" cellosaurus.txt


    def main(args: Array[String]) {
      var started : Boolean = false
      var toOBO : Boolean = false
      var stats : Boolean = false
      var bigHeader = ArrayBuffer[String]()
      var currEntry = ArrayBuffer[String]()
      var aclist = ArrayBuffer[String]()
      var idlist = ArrayBuffer[String]()
      var hilist = ArrayBuffer[String]()
      var duplist = ArrayBuffer[String]()
      val emap    = Map.empty[String, Int]
      val oxmap    = Map.empty[String, Int]
      val line_occmap = Map("ID" -> (1,1), "AC" -> (1,1), "SY" -> (0,1),"DR" -> (0,999),"WW" -> (0,999),"CC" -> (0,999),"DI" -> (0,1),"OX" -> (1,999),"HI" -> (0,999),
                            "OI" -> (1,999),  "SX" -> (0,1), "CA" -> (0,1))
      val line_ordmap = Map("ID" -> 1, "AC" -> 2, "SY" -> 3, "DR" -> 4, "WW" -> 5, "CC" -> 6, "DI" -> 7, "OX" -> 8, "HI" -> 9, "OI" -> 10,
                              "SX" -> 11, "CA" -> 12)
      val idacmap = Map.empty[String, String]
      var Entries = ArrayBuffer[ArrayBuffer[String]]()
      val acregexp = new Regex("CVCL_[A-Z0-9][A-Z0-9][A-Z0-9][A-Z0-9]$")
      val ok_dblist = List("ATCC", "BCRJ", "Brenda","CBA", "CCLE", "CCLV", "CCRID", "CHEMBL", "CLDB",
          "CLO", "Coriell", "Cosmic", "dbMHC","DOI", "DSMZ", "ECACC", "EFO", "ESTDAB", "hESCreg", "ICLC",
          "IFO", "IGRhCellID", "IHW", "IMGT/HLA", "ISCR", "IZSLER", "JCRB", "KCLB", "LINCS", "Lonza", "MCCL", "MeSH", "PubMed",
          "RCB", "RSCB", "TKG", "UKSCB")
      val ok_sxlist = List("Female", "Male", "Mixed sex","Sex ambiguous", "Sex undetermined")
      val ok_catlist = List("Cancer cell line", "Hybrid cell line", "Hybridoma", "Induced pluripotent stem cell", "Adult stem cell",
      		"Recombinant protein production insect cell line", "Spontaneously immortalized cell line", "Stromal cell line",
      		"Telomerase immortalized cell line", "Transformed cell line", "Undefined cell line type", "Embryonic cell line", "Embryonic stem cell",
      		"Factor-dependent cell line", "Finite cell line")
      var errcnt = 0
      var drcnt = 0
      var wwcnt = 0
      var syncnt = 0
      var nonUTF8cnt = 0
      var blankcnt = 0
      var curr_line_nb = 0
     if(args.length == 0) { Console.err.println("Please provide a filename"); exit(1)}
      args.foreach(arg => { if(arg.contains("OBO")) toOBO=true else if(arg.contains("stats")) stats=true })
      
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
    
    var entrynb = 0
    Console.err.println("Error report:\n")
    Entries.foreach(entry =>  {
       var id = ""
       var curr_rank = 0
       var last_rank = 0
       val linecntmap    = Map.empty[String, Int]
     if(!(entry(0).startsWith("ID   ") && entry(1).startsWith("AC   ")))
        {Console.err.println("Severe error: Missing ID/AC line at " + entry(0) + " Please correct before re-check"); exit}
       entry.foreach(entryline =>  {//println(entryline)
        var entrylinedata = ""
        var header = entryline.substring(0, 2)
       if(entryline.length() > 5) {
         entrylinedata = entryline.substring(5)
         if(!line_ordmap.contains(header))  {Console.err.println("Unknown line type: " + entryline); errcnt+=1}
         else {
           if(!linecntmap.contains(header)) linecntmap(header) = 1 else linecntmap(header) += 1
         curr_rank = line_ordmap(header)
         if(curr_rank < last_rank) {Console.err.println("Misordered line type: " + entryline + " in entry " + id); errcnt+=1}
         last_rank = curr_rank  
         }
        }
        if(entryline.contains("\t")) Console.err.println("Tab found at: " + entryline)
        if(entryline.contains("  ")) Console.err.println("Multiple spaces found at: " + entryline)
        if(entryline.endsWith(" ")) Console.err.println("Trailing space found at: " + entryline)
        if(entryline.startsWith("ID   ")) {id = entrylinedata; idlist += id}
        else if(entryline.startsWith("AC   ")) {
          if(acregexp.findFirstIn(entrylinedata) == None) {Console.err.println("Incorrect AC format at: " + entryline); errcnt+=1}
          aclist += entrylinedata
          // Map AC to entry
          emap(entrylinedata) = entrynb
           // Map AC to ID
          idacmap(entrylinedata) = id
         entrynb += 1;  
        }
        else if(entryline.startsWith("SY   ")) {
          if(entryline.endsWith(";")) {Console.err.println("SY trailing ; found at: " + entryline); errcnt+=1}
          val locsynlist = entrylinedata.split(";")
          val synduplist = locsynlist.diff(locsynlist.distinct)
          if(synduplist.size != 0) synduplist.foreach(syn =>  {Console.err.println("Locally duplicated SY: " + syn); errcnt+=1})
          var cursyn = ""
          locsynlist.foreach(synonym => {
            syncnt += 1
            if(synonym.startsWith(" ")) cursyn = synonym.substring(1)
            else cursyn = synonym
            if(cursyn == id) {Console.err.println("Synonym exists as main ID: " + cursyn); errcnt+=1}
            })
            
        }
        else if(entryline.startsWith("DR   "))  {
                    if(entryline.endsWith(";") || entryline.endsWith(".")) {Console.err.println("DR trailing ; found at: " + entryline); errcnt+=1}
                    val dbname = entrylinedata.split(";")(0) 
                    if(!ok_dblist.contains(dbname)) {Console.err.println("Illegal db:" + dbname + " found at: " + entryline); errcnt+=1}
                    drcnt += 1
        			}
        else if(entryline.startsWith("WW   ")) {
        			if(!(entrylinedata.startsWith("http://")  || entrylinedata.startsWith("https://") || entrylinedata.startsWith("ftp://")))
        				{Console.err.println("Invalid url found at: " + entryline); errcnt+=1}
        			wwcnt += 1
        }
        else if(entryline.startsWith("CC   ")) {
          if(!entryline.endsWith(".")) {Console.err.println("Unterminated CC found at: " + entryline); errcnt+=1}
        }
        else if(entryline.startsWith("DI   ")) {
                    if(entryline.endsWith(";") || entryline.endsWith(".")) {Console.err.println("DI trailing ; found at: " + entryline); errcnt+=1}
                    val dilist = entrylinedata.split(";")
                    if(dilist.length != 3) {Console.err.println("Illegal disease format found at: " + entryline); errcnt+=1}
                    if(dilist(0) != "NCI") {Console.err.println("Illegal disease db:" + dilist(0) + " found at: " + entryline); errcnt+=1}
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
                    if(entryline.endsWith(";") || entryline.endsWith(".")) {Console.err.println("HI/OI trailing ; found at: " + entryline); errcnt+=1}
                    if(!entrylinedata.contains(" ! ")) {Console.err.println("Illegal HI/OI format found at: " + entryline); errcnt+=1}
                    val toklist = entrylinedata.split(" ")
                    if(toklist.length < 3) {Console.err.println("Illegal HI/OI format found at: " + entryline); errcnt+=1}
                    if(acregexp.findFirstIn(toklist(0)) == None) {Console.err.println("Incorrect HI/OI AC format at: " + entryline); errcnt+=1}
                    else hilist += toklist(0)
        			}
        else if(entryline.startsWith("SX   ")) {
         if(!ok_sxlist.contains(entrylinedata)) {Console.err.println("Illegal sex found at: " + entryline); errcnt+=1} 
        }
        else if(entryline.startsWith("CA   ")) {
         if(!ok_catlist.contains(entrylinedata)) {Console.err.println("Illegal category found at: " + entryline); errcnt+=1} 

        }
        else if(entryline.startsWith("//")) { // check line occurences
          linecntmap.keys.foreach{ key =>
                                 if((linecntmap(key) < line_occmap(key)._1) || (linecntmap(key) > line_occmap(key)._2))
                                   if(key == "AC" || key == "ID") {Console.err.println("Severe error: " + key + " in entry " + id + " Please correct before re-check"); exit}
                                   else {Console.err.println("Illegal line count for: " + key + " in entry " + id); errcnt+=1}}
        }
      })
    })
    
    // Entry level checks
    duplist = aclist.diff(aclist.distinct)
    if(duplist.length != 0)  {duplist.foreach(ac =>  {Console.err.println("duplicated AC: " + ac); errcnt+=1})}
 
    duplist = idlist.diff(idlist.distinct)
    if(duplist.length != 0)  {duplist.foreach(id =>  {Console.err.println("duplicated ID: " + id); errcnt+=1})}
    
    val misslist = hilist.filter(s => !aclist.contains(s))
    if(misslist.length != 0)  { misslist.foreach(ac =>  {Console.err.println("Inexistant HI/OI AC: " + ac); errcnt+=1})}
    
   // Check OI/HI lines consistency
    var ac = ""
    var oiac = ""
    var oiid = ""
    Entries.foreach(entry =>  {
      entry.foreach(entryline =>  {
      if(entryline.startsWith("AC   ")) {ac = entryline.substring(5)}  
      else if(entryline.startsWith("OI   ")) {
             oiac = entryline.substring(5).split(" ")(0)
             oiid = (entryline.substring(5).split("!")(1)).substring(1)
             if(oiac == ac) {Console.err.println("Self-referencing OI: " + oiac + "/" + ac); errcnt+=1}
             else {
             var ok = false
             currEntry = Entries(emap(oiac))
             currEntry.foreach(line =>  { //println(line)
             if(line.startsWith("OI   ") && line.contains(ac)) ok = true 
             })
             if(!ok) {Console.err.println("Inexistant reciproque OI/AC: " + oiac + "/" + ac); errcnt+=1}
             }
             if(idacmap(oiac) != oiid) {Console.err.println("Incorrect OI AC/ID pair: " + oiac + "/" + oiid); errcnt+=1}
             }  
      else if(entryline.startsWith("HI   ")) {
             oiac = entryline.substring(5).split(" ")(0)
             oiid = (entryline.substring(5).split("!")(1)).substring(1)
             if(oiac == ac) {Console.err.println("Self-referencing HI: " + oiac + "/" + ac); errcnt+=1}
             if(idacmap(oiac) != oiid) {Console.err.println("Incorrect HI AC/ID pair: " + oiac + "/" + oiid); errcnt+=1}
      }
      })})
      
   println(Entries.length + " entries: " + errcnt + " error(s)")    
   println(nonUTF8cnt + " non-UTF8 character containing line(s), " + blankcnt + " blank line(s)")    
   
   if(stats)  {
      println("\n ===== Statistics =====\n")
      println(drcnt + " Xrefs")
      println(wwcnt + " Web links")
      println(syncnt + " synonyms")
      println(oxmap.size + " different species")
      oxmap.keys.foreach{ i =>  
                        if(i == "9606")  println("Human: " + oxmap(i) )
                        else if(i == "10090")  println("Mouse: " + oxmap(i) )
                        else if(i == "10116")  println("Rat: " + oxmap(i) )
                        }
      }
   }
}