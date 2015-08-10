#' GEOSearchTerm
#' 
#' Perform searching in NCBI GEO database.
#'
#' Search the terms one by one in NCBI GEO database and return an integrated table of search results. The returned results should contain exactly the same information as the results returned by directly searching in http://www.ncbi.nlm.nih.gov/geo/.
#' 
#' @param termlist A character vector of terms to be searched in NCBI GEO. Typically the direct output of function "termalias"
#' @return A data frame containing the search results returned from NCBI GEO. First column: GEO Series Accesion Number; Second column: Organism; Third column: Title; Fourth column: Type of experiment; Fifth column: Experiment Platform Sixth column: Number of Samples; Seventh column: URL for SRX download; Eighth column: Description Ninth column: Corresponding search term.
#' @export 
#' @author Zhicheng Ji, Hongkai Ji <zji4@@zji4.edu>
#' @examples
#' GEOSearchTerm("Oct4 RNA-seq")

GEOSearchTerm <- function(termlist) {
      readURL <- function(URL) {
            while(!exists("URLdata")) {
                  tryCatch(URLdata <- readLines(URL), error = function(e) {}, warning = function(w) {})
            }      
            URLdata
      }
      uidlist <- sapply(termlist, function(oriterm) {
            searchterm <- gsub(" ","+",oriterm)            
            uid <- readURL(paste0("http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=gds&term=GSE%5BETYP%5D+",searchterm,"&retmax=10000"))
            uid <- uid[grep("</Id>",uid)]
            uid <- gsub("[^0-9]","",uid)
      },simplify = FALSE)
      uniqueuid <- unique(unlist(uidlist))
      uidtable <- sapply(uniqueuid, function(uid) {
            tmp <- sapply(uidlist,function(i) uid %in% i)
            paste0(names(tmp)[tmp],collapse = ";")
      })
      if (length(uniqueuid) > 0) {
            rawcontent <- readURL(paste0("http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=gds&version=2.0&id=",paste0(uniqueuid,collapse = ",")))
            blankid <- which(rawcontent=="")
            res <- NULL
            for (i in 1:length(blankid)) {
                  if (i == length(blankid)) {
                        tmp <- rawcontent[(blankid[i]+1):length(rawcontent)]     
                  } else {
                        tmp <- rawcontent[(blankid[i]+1):(blankid[i+1]-1)]      
                  }                  
                  title <- sub("[0-9]*\\. ","",tmp[1])
                  description <- tmp[2]
                  description <- sub(" more...$","",description)
                  description <- sub("^\\(Submitter supplied\\) ","",description)
                  organism <- sub("Organism:\t","",tmp[grep("Organism:\t",tmp)])
                  type <- sub("Type:\t\t","",tmp[grep("Type:\t",tmp)])                        
                  platform <- tmp[grep("Platform",tmp)]
                  sampnum <- strsplit(platform," ")[[1]]
                  sampnum <- as.numeric(sampnum[length(sampnum)-1])
                  platform <- sub(" [0-9]* Samples","",platform)
                  platform <- sub("^.*Platforms?: ","",platform)
                  SRX <- strsplit(tmp[grep("FTP download: ",tmp)]," ")[[1]]
                  SRX <- SRX[length(SRX)]
                  series <- sub("Series\t\tAccession: ","",tmp[grep("Series\t",tmp)])
                  series <- sub("\t.*","",series)
                  res <- rbind(res,c(series,organism,title,type,platform,sampnum,SRX,description))
            }
            colnames(res) <- c("Series","Organism","Title","Type","Platform","Sample.Number","SRX","Description")            
            res <- data.frame(res,stringsAsFactors = FALSE)    
            res$Sample.Number <- as.numeric(res$Sample.Number)     
            res$Term <- uidtable
            res      
      } else {
            NULL
      }      
      
}

