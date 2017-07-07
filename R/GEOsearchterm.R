#' GEOSearchTerm
#' 
#' Perform searching in NCBI GEO database.
#'
#' Search the terms one by one in NCBI GEO database and return an integrated table of search results. The returned results should contain exactly the same information as the results returned by directly searching in http://www.ncbi.nlm.nih.gov/geo/.
#' 
#' @param termlist A character vector of terms to be searched in NCBI GEO. Typically the direct output of function "termalias"
#' @param type Either "GSE" or "GSM". The function will search either GSE and GSM on GEO.
#' @return A data frame containing the search results returned from NCBI GEO.
#' @export 
#' @author Zhicheng Ji, Hongkai Ji <zji4@@zji4.edu>
#' @examples
#' GEOSearchTerm("Oct4 RNA-seq")

GEOSearchTerm <- function(termlist,type="GSE") {
      uidlist <- sapply(termlist, function(oriterm) {
            searchterm <- gsub(" ","+",oriterm)            
            if (type=="GSE") {
                  uid <- readURL(paste0("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=gds&term=GSE%5BETYP%5D+",searchterm,"&retmax=10000"))      
            } else {
                  uid <- readURL(paste0("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=gds&term=GSM%5BETYP%5D+",searchterm,"&retmax=10000"))     
            }            
            uid <- uid[grep("</Id>",uid)]
            uid <- gsub("[^0-9]","",uid)
      },simplify = FALSE)
      uniqueuid <- unique(unlist(uidlist))
      uidtable <- sapply(uniqueuid, function(uid) {
            tmp <- sapply(uidlist,function(i) uid %in% i)
            paste0(names(tmp)[tmp],collapse = ";")
      })
      if (length(uniqueuid) > 0) {
            rawcontent <- NULL
            for (i in 1:max(1,floor(length(uniqueuid)/500))) {
                  if (i == floor(length(uniqueuid)/500)) {
                        tmpuniqueuid <- uniqueuid[(1+500 * (i-1)):length(uniqueuid)]
                  } else {
                        tmpuniqueuid <- uniqueuid[1:500 + 500 * (i-1)]
                  }
                  rawcontent <- c(rawcontent,readURL(paste0("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=gds&version=2.0&id=",paste0(tmpuniqueuid,collapse = ","))))
                  
            }
            blankid <- which(rawcontent=="")
            res <- NULL
            if (type == "GSE") {
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
                        series <- sub("Series\t\tAccession: ","",tmp[grep("Series\t",tmp)])
                        series <- sub("\t.*","",series)
                        res <- rbind(res,c(series,organism,title,type,platform,sampnum,description))
                  }
                  colnames(res) <- c("Series","Organism","Title","Type","Platform","Sample.Number","Description")            
                  res <- data.frame(res,stringsAsFactors = FALSE)    
                  res$Sample.Number <- as.numeric(res$Sample.Number)     
                  res$Term <- uidtable                  
            } else {
                  for (i in 1:length(blankid)) {
                        if (i == length(blankid)) {
                              tmp <- rawcontent[(blankid[i]+1):length(rawcontent)]     
                        } else {
                              tmp <- rawcontent[(blankid[i]+1):(blankid[i+1]-1)]      
                        }                  
                        title <- sub("[0-9]*\\. ","",tmp[1])                        
                        organism <- sub("Organism:\t","",tmp[grep("Organism:\t",tmp)])      
                        sourcename <- sub("Source name:\t","",tmp[grep("Source name:\t",tmp)])      
                        platform <- tmp[grep("Platform",tmp)]
                        platform <- sub(" Series.*","",platform)
                        platform <- sub("^.*Platforms?: ","",platform)                  
                        series <- sub(".*Series: ","",tmp[grep("Series: ",tmp)])
                        series <- sub("Dataset:.*","",series)
                        series <- sub(" $","",series)
                        series <- gsub(" ",",",series)
                        sample <- sub("Sample\t\tAccession: ","",tmp[grep("Sample\t",tmp)])
                        sample <- sub("\t.*","",sample)                        
                        res <- rbind(res,c(sample,series,organism,sourcename,title,platform))
                  }
                  colnames(res) <- c("Sample","Series","Organism","Source","Title","Platform")            
                  res <- data.frame(res,stringsAsFactors = FALSE)                      
                  res$Term <- uidtable                  
            }
            res                  
      } else {
            NULL
      }      
      
}

