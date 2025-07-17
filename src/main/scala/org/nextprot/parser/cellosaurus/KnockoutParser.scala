package org.nextprot.parser.cellosaurus

import scala.io.Source
import scala.xml._

class Knockout(val method: String, val xref: DbXref, val note: String) {
        
    override def toString() : String = {
        s"Knockout(method=$method, xref:$xref, note:${if (note==null) "(null)" else note})"
    }

    def toXML = 
        <knockout-cell method={method}>
            {xref.toXML}
            {
            if (note != null) <knockout-cell-note>{note}</knockout-cell-note> else Null
            }
        </knockout-cell>

}

object KnockoutParser {
    /*
    Format:    CC   Knockout cell: Method=<method>; <Resource_abbrev>; <ac>; <Gene/protein_name>[ (Note=<Free_text>)].
    Examples:
    CC   Knockout cell: Method=CRISPR/Cas9; MGI; MGI:88127; B2m.
    CC   Knockout cell: Method=KO mouse; MGI; MGI:104738; Cdkn2a (Note=1 of 2 alleles).
    CC   Knockout cell: Method=KO mouse; MGI; MGI:109583; Pten.
    CC   Knockout cell: Method=Homologous recombination; MGI; MGI:99204; Zfp57 (Note=1 of 2 alleles).
    */

    val allowed_methods: Set[String] = Set(
        "BAC homologous recombination", "CRISPR/Cas9", "CRISPR/Cas9n", "CRISPR/Cas12a", "Cre/loxP",  
        "EBV-based vector siRNA knockdown", "Floxing/Cre recombination", "Gamma radiation",  
        "Gene trap", "Gene-targeted KO mouse", "Helper-dependent adenoviral vector",  
        "Homologous recombination", "KO mouse", "KO pig", "Knockout-first conditional",  
        "Mutagenesis", "Not specified", "Null mutation", "P-element", "Prime editing",  
        "Promoterless gene targeting", "Recombinant Adeno-Associated Virus", "TALEN",  
        "Targeted integration", "X-ray", "ZFN", "miRNA knockdown", "shRNA knockdown", 
        "siRNA knockdown"      
    )

    def parseLine(rawtext: String) : Knockout = {
        val text = if (rawtext.endsWith(".")) rawtext.substring(0,rawtext.length-1) else rawtext
        val toklist = text.split("; ")
        val method = toklist(0).split("=")(1)
        if (! allowed_methods.contains(method)) throw new Exception(s"Unknown method name in '${rawtext}'") 
        val db = toklist(1)
        val ac = toklist(2)
        val dbac = db + "=" + ac
        if (! SourceChecker.isInDbSet(dbac, Set("HGNC", "MGI", "UniProtKB", "FlyBase_Gene", "VGNC", "RGD", "CGNC")) ) 
          throw new Exception(s"Invalid xref in Knockout cell comment '${rawtext}'")
        val geneAndNote = toklist(3).split(" \\(Note=")
        val geneName = geneAndNote(0)
        val note = if (geneAndNote.length>1) geneAndNote(1).split("\\)")(0) else null
        val xref = new DbXref(db, ac, label = geneName)
        return new Knockout(method, xref, note)
    }


    def main(args: Array[String]): Unit = {

        // sbt "run ../cellosaurus-api/data_in/cellosaurus_xrefs.txt knockout.txt"

        DbXrefInfo.load(args(0)) // load allowed db name with cat & url properties
        val datadir = "/home/pmichel/work/cellosaurus-api/data_in/"
        val instMap = SourceChecker.loadInstitutionFile(datadir + "institution_list")
        val childParentMap = SourceChecker.loadHierarchy(datadir + "cellosaurus.txt")
        SourceChecker.init(DbXrefInfo.getDbSet(), instMap, childParentMap)

        val filename = args(1)
        var lineNo = 0
        for (line <- Source.fromFile(filename).getLines()) {
            val topic = "CC   Knockout cell: "
            if (line.startsWith(topic)) {
                val linevalue = line.substring(topic.length).trim()
                lineNo += 1
                try {
                    println("--------------")
                    println(line)
                    val ko = parseLine(linevalue)
                    println(ko)
                    println(ko.toXML)
               } catch {
//                 case e: Exception => {throw e}
                   case e: Exception => {println(s"ERROR at line ${lineNo}")}
               }
            }
        }
    }

}