package org.nextprot.parser.cellosaurus

object SourceChecker {

    var knownPubliDbSet : Set[String] = null    
    var knownXrefDbSet : Set[String] = null
    var knownOrgSet : Set[String] = null

    // MUST be called otherwise error occur
    def init(xrefDbSet: Set[String]) : Unit = {
        knownPubliDbSet = Set("PubMed", "DOI", "CelloPub", "Patent")
        knownOrgSet = Set("Sanger", "Direct_author_submission")
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
            // real and up to date list for production time
            // excludes publi refs PubMed, DOI, CelloPub and Patent
            knownXrefDbSet = xrefDbSet -- knownPubliDbSet
        }
        println("INFO, init SourceChecker")
    }

    /*
    The 3 sets above should be set by the main parser using init() based on files that are up to date
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
        if (knownOrgSet.contains(name)) return true
        return false
    }

    def isKnown(name: String): Boolean = {
        if (isKnownXref(name)) return true
        if (isKnownPubliRef(name)) return true
        if (isKnownOrgRef(name)) return true  
        return false      
    }

    // helper function to be used by parser to tune source validation
    def isInDbSet(db_ac: String, db_set: Set[String]): Boolean = {
        val elems = db_ac.split("=")
        val db = elems(0)
        if (db_set.contains(db)) return true 
        return false  
    }

}