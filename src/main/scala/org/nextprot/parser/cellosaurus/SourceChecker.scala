package org.nextprot.parser.cellosaurus

import scala.io.Source

case class ParentLink(id: String, ac: String, parentId: String) {
  override def toString() : String = {
    s"ParentLink($id - $ac == parent => $parentId)"
  }
}

object SourceChecker {

    var knownPubliDbSet : Set[String] = null    
    var knownXrefDbSet : Set[String] = null
    var knownInstituteMap : Map[String, String] = null
    var knownMiscSet : Set[String] = null
    var knownParentLinkMap : Map[String, ParentLink] = null

    // MUST be called otherwise error occurs
    def init(xrefDbSet: Set[String], instMap: Map[String, String], parentLinkMap: Map[String, ParentLink]) : Unit = {

        knownParentLinkMap = parentLinkMap

        knownPubliDbSet = Set("PubMed", "DOI", "CelloPub", "Patent")
                            
        knownMiscSet = Set(
            // "Direct_author_submission",        -- not considered a source
            "from autologous cell line",   // + " " + $id -         in fields: var, donor-info 
            "from autologous cell lines",  // + " " + $id; ...; $id in fields: var, donor-info 
            "from child cell line",        // + " " + $id           in fields: var
            "from child cell lines",       // + " " + $id; ...; $id in fields: var
            "from familial inference of",  // + " " + $id; ... in fields: var
            "from parent cell line"        // + (nothing) in fields: var, donor-info, karyo, characteristics, virology
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

    def isKnownCellosaurusId(id: String): Boolean = {
        return knownParentLinkMap.contains(id)
    }

    def getCellosaurusAcFromId(id: String) : String = {
        return knownParentLinkMap(id).ac
    }

    def getCellosaurusParentIdFromId(id: String) : String = {
        return knownParentLinkMap(id).parentId
    }

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

    def getKnownOrgRefId(name:String): String = {
        if (knownXrefDbSet.contains(name)) return name
        if (knownInstituteMap.contains(name)) return knownInstituteMap(name)
        return null
    }

    def isKnownMiscRef(name: String): Boolean = {
        return knownMiscSet.find(el => name.startsWith(el)) != None
    }

    def getMiscRefPrefix(name:String) : String = {
        return knownMiscSet.filter(el => name.startsWith(el)).maxBy(_.length)
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

    def loadHierarchy(filename: String): Map[String, ParentLink] = {
        var parentLinkMap = Map[String, ParentLink]()
        println("Reading " + filename)
        val lines = Source.fromFile(filename).getLines()
        var lineNo = 0
        var id: String = null
        var ac: String = null
        var parentId: String = null
        for (line <- lines) {
            lineNo += 1
            if (line.startsWith("ID   ")) {
                id = line.substring(5).strip()
            } else if (line.startsWith("AC   ")) {
                ac = line.substring(5).strip()
            } else if (line.startsWith("HI   ")) {
                parentId = line.substring(5).split(" ! ")(1).strip()
            } else if (line.startsWith("//")) {
                val parentLink = ParentLink(id, ac, parentId)
                parentLinkMap += (id, parentLink)
                id = null
                ac = null
                parentId = null
            }
        }
        return parentLinkMap
    }


    def main(args: Array[String]): Unit = {
  
        val datadir = "/home/pmichel/work/cellosaurus-api/data_in/"
        DbXrefInfo.load(datadir + "cellosaurus_xrefs.txt")
        val instMap = SourceChecker.loadInstitutionFile(datadir + "institution_list")
        val childParentMap = SourceChecker.loadHierarchy(datadir + "cellosaurus.txt")
        SourceChecker.init(DbXrefInfo.getDbSet(), instMap, childParentMap)

        // check that parentId (found in HI) is also present in ID.
        childParentMap.foreach {
            case (id, record) => {
                if (record.parentId != null) {
                    if (! childParentMap.contains(record.parentId)) {
                        println(s"ERROR: parent found in HI never appears in ID: ${record.parentId}")
                    }
                } 
            }
        }

        println("ICLAC: " + SourceChecker.isKnownOrgRef("ICLAC"))
        println("Center for iPS Cell Research and Application: " + SourceChecker.isKnownOrgRef("Center for iPS Cell Research and Application"))
        println("CiRA: " + SourceChecker.isKnownOrgRef("CiRA"));

        println("from parent cell line bla bla: " + SourceChecker.isKnownMiscRef("from parent cell line bla bla"))
        println("from parent cell line bla bla: " + SourceChecker.isKnown("from parent cell line bla bla"))

        println("check org ids:")
        println("ICLAC -- id --> " + SourceChecker.getKnownOrgRefId("ICLAC"))
        println("Center for iPS Cell Research and Application -- id --> " + SourceChecker.getKnownOrgRefId("Center for iPS Cell Research and Application"))
        println("UniProtKB --> " + SourceChecker.getKnownOrgRefId("UniProtKB"))
        println("Schtroupf -- id --> " + SourceChecker.getKnownOrgRefId("Schtroupf"))
    }
  

}