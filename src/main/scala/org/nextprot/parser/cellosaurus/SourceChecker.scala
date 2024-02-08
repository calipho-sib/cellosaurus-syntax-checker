package org.nextprot.parser.cellosaurus

import scala.io.Source


object SourceChecker {

    var knownPubliDbSet : Set[String] = null    
    var knownXrefDbSet : Set[String] = null
    var knownInstituteMap : Map[String, String] = null
    var knownMiscSet : Set[String] = null

    // MUST be called otherwise error occurs
    def init(xrefDbSet: Set[String], instMap: Map[String, String]) : Unit = {

        knownPubliDbSet = Set("PubMed", "DOI", "CelloPub", "Patent")
                            
        knownMiscSet = Set(
            "Direct_author_submission", "from autologous cell line", "from child cell line", 
            "from familial inference", "from inference of", "from parent cell line", 
            "inferred from genetic background"
            )

        if (xrefDbSet == null) {
            // default list for tests
            knownXrefDbSet = Set(
                "4DN", "Abcam", "ABCD", "ABM", "AddexBio", "ArrayExpress", "ATCC", "BCGO", "BCRC", "BCRJ", "BEI_Resources", 
                "BioGRID_ORCS", "BioSample", "BioSamples", "BTO", "cancercelllines", "CancerTools", "CBA", "CCLV", "CCRID", 
                "CCTCC", "Cell_Biolabs", "Cell_Model_Passport", "Cellosaurus", "CGH-DB", "ChEBI", "ChEMBL-Cells", 
                "ChEMBL-Targets", "CL", "CLDB", "ClinVar", "CLO", "CLS", "ColonAtlas", "Coriell", "Cosmic", "Cosmic-CLP", "dbGAP", 
                "dbMHC", "dbSNP", "DepMap", "DGRC", "DiscoverX", "DrugBank", "DSHB", "DSMZ", "DSMZCellDive", "EBiSC", "ECACC", 
                "EFO", "EGA", "ENCODE", "ESTDAB", "FCDI", "FCS-free", "FlyBase_Cell_line", "FlyBase_Gene", "GDSC", "GeneCopoeia", 
                "GEO", "HGNC", "HipSci", "HIVReagentProgram", "Horizon_Discovery", "hPSCreg", "IARC_TP53", "IBRC", "ICLC", "ICLDB", 
                "IGRhCellID", "IGSR", "IHW", "Imanis", "Innoprot", "IPD-IMGT/HLA", "ISCR", "IZSLER", "JCRB", "KCB", "KCLB", "Kerafast", 
                "KYinno", "LiGeA", "LIMORE", "LINCS_HMS", "LINCS_LDP", "Lonza", "MCCL", "MeSH", "MetaboLights", "MGI", "Millipore", 
                "MMRRC", "NCBI_Iran", "NCBI_TaxID", "NCI-DTP", "NCIt", "NHCDR", "NIHhESC", "NISES", "NRFC", "ORDO", 
                "PerkinElmer", "PharmacoDB", "PRIDE", "Progenetix", "PubChem", "PubChem_Cell_line", "RCB", "RGD", "Rockland", 
                "RSCB", "SKIP", "SKY/M-FISH/CGH", "SLKBase", "TKG", "TNGB", "TOKU-E", "UBERON", "Ubigene", "UniProtKB", "VGNC", 
                "WiCell", "Wikidata", "Ximbio")
        } else {
            // real and up to date list for production time, excludes publi refs PubMed, DOI, CelloPub and Patent
            knownXrefDbSet = xrefDbSet -- knownPubliDbSet
        }

        if (instMap == null)  {
            knownInstituteMap = Map("Sanger" -> "Sanger", "Boston_University" -> "Boston_University", "BNLC" -> "BNLC", "Cedars-Sinai" -> "Cedars-Sinai") // just a samples
        } else {
            // read from file institution_list
            knownInstituteMap = instMap
        }
        println("INFO, init SourceChecker")
    }

    /*
    The 2 sets just above should be set by the main parser using init() and loadInstituteFile() based on files that are up to date
    */

    def isKnownXref(db_ac: String): Boolean = {
        val elems = db_ac.split("=")
        if (elems.length != 2) return false
        val db = elems(0)
        return knownXrefDbSet.contains(db)
    }

    def isKnownPubliRef(db_ac: String): Boolean = {
        val elems = db_ac.split("=")
        if (elems.length != 2) return false
        val db = elems(0)
        return knownPubliDbSet.contains(db)
    }

    def isKnownOrgRef(name: String): Boolean = {
        if (knownXrefDbSet.contains(name)) return true
        if (knownInstituteMap.contains(name)) return true
        return false
    }

    def isKnownMiscRef(name: String): Boolean = {
        return knownMiscSet.find(el => name.startsWith(el)) != None
    }


    def isKnown(name: String): Boolean = {
        if (isKnownXref(name)) return true
        if (isKnownPubliRef(name)) return true
        if (isKnownOrgRef(name)) return true
        if (isKnownMiscRef(name)) return true
        return false      
    }

    // helper function to be used by parser to tune source validation
    def isInDbSet(db_ac: String, db_set: Set[String]): Boolean = {
        val elems = db_ac.split("=")
        val db = elems(0)
        if (db_set.contains(db)) return true 
        return false  
    }

    def loadInstitutionFile(filename: String): Map[String, String] = {
        var instMap = Map[String,String]() 
        println("Loading " + filename)
        val lines = Source.fromFile(filename).getLines()
        var lineNo = 0
        for (line <- lines) {
            lineNo += 1
            val elems = line.strip().split("; ")
            var terms = List[String]()
            var id: String = null
            for (el <- elems) {
                if (el.startsWith("Synonym=")) { // we assume it is the last element of the line when len(elems) > 1
                    id = el.substring(8)
                    terms = id :: terms
                } else  {
                    terms = el :: terms
                }
            }
            if (id == null) id = terms(0)
            for (t <- terms) instMap += (t -> id)
        }
        //println("file " + filename + "content:")
        //for (el <- instMap) println(el)
        return instMap
    }


    def main(args: Array[String]): Unit = {
  
        val datadir = "/home/pmichel/work/cellosaurus-api/data_in/"
        DbXrefInfo.load(datadir + "cellosaurus_xrefs.txt")
        val instMap = SourceChecker.loadInstitutionFile(datadir + "institution_list")
        SourceChecker.init(DbXrefInfo.getDbSet(), instMap)

        println("ICLAC: " + SourceChecker.isKnownOrgRef("ICLAC"))
        println("Center for iPS Cell Research and Application: " + SourceChecker.isKnownOrgRef("Center for iPS Cell Research and Application"))
        println("CiRA: " + SourceChecker.isKnownOrgRef("CiRA"));

        println("from parent cell line bla bla: " + SourceChecker.isKnownMiscRef("from parent cell line bla bla"))
        println("from parent cell line bla bla: " + SourceChecker.isKnown("from parent cell line bla bla"))

    }
  

}