#' SampleDetail
#' 
#' Details of GSM samples given GSE accession ID.
#'
#' This function returns an integrated table containing details of all GSM samples for a list of GSE accession ID.
#' 
#' @param GSEid A character vector of GSE accession ID.
#' @return A data frame containing the search results returned from NCBI GEO. First column: GEO Series Accesion Number; Second column: GEO Sample Accesion Number; Third column: Title; Fourth column: Type of experiment; Fifth column: Source; Sixth column: Organism; Seventh column: Characteristic; Eighth column: Description.
#' @export 
#' @author Zhicheng Ji, Hongkai Ji <zji4@@zji4.edu>
#' @examples
#' SampleDetail(c("GSE69322","GSE64008"))

SampleDetail <- function(GSEid) {       
      readURL <- function(URL) {
            while(!exists("URLdata")) {
                  tryCatch(URLdata <- readLines(URL), error = function(e) {}, warning = function(w) {})
            }      
            URLdata
      }
      allres <- NULL
      for (GSEname in GSEid) {
      sampledata <- readURL(paste0("http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=",GSEname,"&targ=gsm&form=text&view=brief"))      
      breakid <- grep("SAMPLE",sampledata)
      
      for (i in 1:length(breakid)) {
            if (i == length(breakid)) {
                  singlesample <- sampledata[breakid[i]:length(sampledata)]
            } else {
                  singlesample <- sampledata[breakid[i]:(breakid[i+1]-1)]
            }
            sampname <- sub("\\^SAMPLE = ","",singlesample[1])
            samptitle <- sub("!Sample_title = ","",singlesample[grep("Sample_title",singlesample)])
            if (length(grep("library",singlesample)) == 0) {
                  samptype <- "array"
            } else {
                  samptype <- sub("!Sample_library_strategy = ","",singlesample[grep("Sample_library_strategy",singlesample)])
            }
            sampsource <- paste0(sub("!Sample_source_name_ch1 = ","",singlesample[grep("Sample_source_name_ch1",singlesample)]),collapse = "; ")
            samporganism <- paste0(sub("!Sample_organism_ch1 = ","",singlesample[grep("Sample_organism_ch1",singlesample)]),collapse = "; ")
            sampchara <- paste0(sub("!Sample_characteristics_ch1 = ","",singlesample[grep("Sample_characteristics_ch1",singlesample)]),collapse = "; ")
            sampdesc <- paste0(sub("!Sample_description = ","",singlesample[grep("Sample_description",singlesample)]),collapse = "; ")
            allres <- rbind(allres,cbind(GSEname,sampname,samptitle,samptype,sampsource,samporganism,sampchara,sampdesc))
      }
      }
      colnames(allres) <- c("Experiment","Sample","Title","Type","Source","Organism","Characteristic","Description")
      allres
}


