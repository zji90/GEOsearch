#' TermAlias
#' 
#' Search Alias of a Term
#'
#' This function first picks gene names from the searh term. It then searches alias for all the gene names contained in the search term. It next queries GEO and retain alias that appear frequently enough in GEO database. Finally it returns a combinatory results of retained alias.
#' 
#' @param searchterm A character value specifying the term being searched. Words should be separated by space.
#' @param allspecies A character vector specifying the species in which the alias of gene names will be searched for. Available species: Anopheles,Arabidopsis,Bovine,Worm,Canine,Fly,Zebrafish,E coli strain K12,E coli strain Sakai,Chicken,Human,Mouse,Rhesus,Malaria,Chimp,Rat,Yeast,Pig,Xenopus
#' @param mincount An integer value specifying the minimum number of appearance in the GEO database for the alias to be retained. If an alias has too few appearance it may not be of great interest.
#' @return A character vector of combinatory results of alias.
#' @export
#' @import org.Hs.eg.db org.Mm.eg.db
#' @author Zhicheng Ji, Hongkai Ji <zji4@@zji4.edu>
#' @examples
#' TermAlias("Oct4 RNA-seq")


TermAlias <- function(searchterm, allspecies = c("Human","Mouse"), mincount = 5) {
      data(database)
      readURL <- function(URL, n = -1L) {
            while(!exists("URLdata")) {
                  tryCatch(URLdata <- readLines(URL,n), error = function(e) {}, warning = function(w) {})
            }      
	    URLdata
      }      
      allterm <- strsplit(searchterm," ")[[1]]
      tmp <- sapply(allterm, function(term) {            
            term <- tolower(term)
            allname <- NULL
            for (species in allspecies) {
                  base <- database[database[,2]==species,1]
                  eval(parse(text=paste0("library(",base,")")))
                  eval(parse(text=paste0('db <- get("',base,'")')))                  
                  matchid <- which(term == tolower(keys(db,"ALIAS")))
                  if (length(matchid) > 0) {
                        symbol <- select(db, keys=keys(db,"ALIAS")[matchid], columns="SYMBOL", keytype="ALIAS")[,2]
                        allname <- c(allname,tolower(select(db, keys=symbol, columns="ALIAS", keytype="SYMBOL")[,2]))
                  }     
            }
            
            allname <- unique(allname)
            
            if (!is.null(allname)) {
                  dupname <- NULL
                  for (i in 1:length(allname)) {
                        singlename <- allname[i]
                        dupname <- c(dupname,setdiff(grep(singlename,allname)[-i],i))
                  }
                  
                  allname <- allname[-dupname]
                  
                  termcount <- sapply (allname, function(singlename) {
                        content <- readURL(paste0("http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=gds&term=GSE%5BETYP%5D+",singlename,"&retmax=1"),n=3)
                        content <- content[3]
                        as.numeric(strsplit(content,"(<Count>)|(</Count>)")[[1]][2])
                  })      
                  termname <- names(termcount)[termcount >= mincount]
                  if (length(termname) == 0)
                        termname <- term
            } else {
                  termname <- term
            }
            termname
      },simplify=FALSE)
      
      tmp <- expand.grid(tmp)
      apply(tmp,1,function(x) paste0(x,collapse = " "))
}

