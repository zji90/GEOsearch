#' BatchDownload
#' 
#' Generate shell file to download the SRA of multiple GSE or GSM
#'
#' This function returns a shell file that can download the SRA of multiple GSE or GSM. The shell file should be run on user's own computer with Linux/Unix system and wget installed.
#' The download process is to set up a bunch of directories under the path specified by users (GSMdownloadpath) and download the SRA files inside the paths.
#' Note: it is the users' job to make sure the GSE and GSM names are correct. Otherwise the function will run forever due to inner error control system.
#' Also, it could take a long time for the function to complete if there are lots of GSE series in namelist.
#' 
#' @param namelist A character list of GSE or GSM ID.
#' @param GSMdownloadpath A character value specifying the path to download the SRA files on users own Linux/Unix computer.
#' @return A character string of commands to be run on users' computer. 
#' @import RCurl
#' @export 
#' @author Zhicheng Ji, Hongkai Ji <zji4@@zji4.edu>
#' @examples
#' BatchDownload("GSM1274542")

BatchDownload <- function(namelist, GSMdownloadpath=".") {       
      readURL <- function(URL) {
            while(!exists("URLdata")) {
                  tryCatch(URLdata <- readLines(URL), error = function(e) {}, warning = function(w) {})
            }      
            URLdata
      }
      mygetURL <- function(URL) {
            while (!exists("URLdata")) {
                  tryCatch(URLdata <- getURL(URL,dirlistonly = TRUE), error = function(e) {
                  })
            }
            URLdata
      }
      findSRX <- function(SRXlink) {
            pathlist <- NULL                        
            allSRRname <- strsplit(mygetURL(SRXlink),"\n")[[1]]            
            for (SRRid in 1:length(allSRRname)) {
                  SRRname <- allSRRname[SRRid]
                  SRAname <- strsplit(mygetURL(paste0(SRXlink,SRRname,"/")),"\n")[[1]]
                  pathlist <- c(pathlist,paste0(SRXlink,SRRname,"/",SRAname))                        
            }         
            pathlist
      }      
      allGSMname <- NULL
      for (singlename in namelist) {
            if (grepl("GSE",singlename)) {
                  allGSMname <- c(allGSMname,SampleDetail(singlename)[,2])
            } else {
                  allGSMname <- c(allGSMname,singlename)
            }
      }
      shfile <- NULL
      for (singlename in allGSMname) {
            GSMcontent <- readURL(paste0("http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=",singlename,"&targ=self&form=text&view=brief"))
            SRXlink <- paste0(strsplit(GSMcontent[grepl("SRX",GSMcontent) & grepl("supp",GSMcontent)]," = ")[[1]][2],"/")
            allSRX <- findSRX(SRXlink)
            if (length(allSRX) == 1) {
                  tmppath <- paste0(GSMdownloadpath,"/",singlename,"/")
                  shfile <- paste0(shfile,"mkdir ",tmppath,"\n")
                  shfile <- paste0(shfile,"wget -O ",tmppath,"data.sra ",allSRX,"\n") 
            } else {
                  for (singleSRXid in 1:length(allSRX)) {
                        tmppath <- paste0(GSMdownloadpath,"/",singlename,"_",singleSRXid,"/")
                        shfile <- paste0(shfile,"mkdir ",tmppath,"\n")
                        shfile <- paste0(shfile,"wget -O ",tmppath,"data.sra ",allSRX[singleSRXid],"\n")
                  }      
            }                        
      }      
      shfile
}
