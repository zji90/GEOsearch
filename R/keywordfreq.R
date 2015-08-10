#' KeyWordFreq
#' 
#' Frequencies of common biology keywords appearing in search results
#'
#' This function calculates the frequencies of each common biology keyword appearing in the given search table. The list of common biology keywords is compiled from http://www.atcc.org/. The list contains three categories: cell types, diseases and tissues. Users can specify which category to be used. The function also returns log fold change and FDR of fisher test to check whether each keyword has significantly more appearance compared to base frequency. The base frequency is defined as the number of appearance of the key word in all samples (roughly 40000 samples) included in GEO database.
#' 
#' @param searchtable The direct output of function "GEOsearchterm"
#' @param category A character vector specifying which category in the common biology keyword list to be used. Should be contain "celltype", "disease" or "tissue".
#' @return A data.frame with the frequency of each common biology keyword. First column: keyword name; Second column: kyeword frequency; Third column: log fold change of the frequency; Fourth column: FDR of fisher test.
#' @export 
#' @author Zhicheng Ji, Hongkai Ji <zji4@@zji4.edu>
#' @examples
#' KeyWordFreq(GEOSearchTerm("Oct4 RNA-seq"))

KeyWordFreq <- function(searchtable, category = c("celltype","disease","tissue")) {      
      data(term)
      term <- term[term[,2] %in% category,]      
      termselected <- unique(term[,1])
      res <- sapply(termselected, function(i) {
            i <- gsub(" ","( .* | )",i)
            i <- paste0("( |^)",i,"( |$)")
            length(grep(i,paste(searchtable$Title,searchtable$Description)))
      })
      tmp <- sort(res[res > 0],decreasing = TRUE)            
      basefreq <- term[match(names(tmp),term[,1]),3]
      logfoldchange <- log(tmp/nrow(searchtable)/(basefreq/40000))
      pval <- p.adjust(sapply(1:length(tmp),function(i) {
            fisher.test(matrix(c(tmp[i],nrow(searchtable)-tmp[i],basefreq[i],40000-basefreq[i]),2),alternative = "greater")$p.value
      }),method="fdr")
      res <- data.frame(term=names(tmp),frequency=tmp,logfoldchange=logfoldchange,FDR=pval,stringsAsFactors = FALSE)
      res[order(res$FDR),]
}
