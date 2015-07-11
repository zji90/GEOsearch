######################################################
##                      GEOsearch                   ##
##             Interactive User Interface           ##
##                     Server File                  ##
##           Author:Zhicheng Ji, Hongkai Ji         ##
##       Maintainer:Zhicheng Ji (zji4@jhu.edu)      ##
######################################################
library(shiny)
library(DT)
library(org.Hs.eg.db)
library(org.Mm.eg.db)

shinyServer(function(input, output,session) {
      Maindata <- reactiveValues()
      
      ### Search ###
      
      observe({
            if (input$searchbutton > 0) {
                  isolate({
                        if (nchar(input$searchterm) > 0) {
                              if (input$searchoption=="Traditional") {
                                    tmpterm <- input$searchterm
                              } else {
                                    withProgress(message = 'Seaching Gene Alias...',{
                                          tmpterm <- termalias(input$searchterm,allspecies = input$searchexpandselectspecies,database=database)                                          
                                    })
                              }
                              withProgress(message = 'Compiling GEO Search Results...',{
                                    Maindata$rawsearchres <- tmp <- GEOsearchterm(tmpterm)
                              })
                              tmp$Series <- paste0('<a href="http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=',tmp$Series,'" target=_blank>',tmp$Series,'</a>')
                              SRXname <- sapply(tmp$SRX, function(i) {
                                    tmp <- strsplit(i,"/")[[1]]
                                    tmp[length(tmp)]
                              })
                              tmp$SRX <- paste0('<a href="',tmp$SRX,'" target=_blank>',SRXname,'</a>')
                              tmp$Term <- factor(tmp$Term)
                              Maindata$searchres <- tmp
                        }    
                  })      
            }            
      })
      
      output$searchshowtable <- DT::renderDataTable(
            if (!is.null(Maindata$searchres))
                  DT::datatable(Maindata$searchres,escape = F,filter='top',rownames = F, extensions = 'ColVis', options = list(dom = 'C<"clear">lfrtip',columnDefs = list(list(visible=FALSE, targets=4:8)),colVis = list(showAll = "Show all",restore = "Restore")))
      ) 
      
      output$searchdownloadbutton <- downloadHandler(
            filename = function() { "Search Results.txt" },
            content = function(file) {                  
                  if (input$searchdownloadselectedparttf) {
                        write.table(Maindata$rawsearchres[input$searchshowtable_rows_selected,],file=file,quote=F,row.names=F,sep="\t")                        
                  } else {
                        write.table(Maindata$rawsearchres,file=file,quote=F,row.names=F,sep="\t")                        
                  }                  
            }
      )
      
      ### Keyword ###
      
      output$keywordshowkeyword <- DT::renderDataTable(
            if (!is.null(Maindata$searchres)) {
                  withProgress(message = 'Calculating key word frequencies...',{
                        Maindata$keywordres <- keywordfreq(Maindata$searchres,category = input$keywordselectterm,term)                  
                  })
                  DT::datatable(Maindata$keywordres,filter='top',rownames = F)
            }
      )
      
      output$keywordshowselectsample <- DT::renderDataTable(
            if (!is.null(Maindata$searchres)) {                 
                  tmp <- NULL
                  for (i in Maindata$keywordres[input$keywordshowkeyword_rows_selected,1]) {
                        i <- gsub(" ","( .* | )",i)
                        i <- paste0("( |^)",i,"( |$)")
                        tmp <- union(tmp,grep(i,paste(Maindata$searchres$Title,Maindata$searchres$Description)))
                  }                  
                  Maindata$searchreskeyword <- Maindata$searchres[tmp,]
                  Maindata$rawsearchreskeyword <- Maindata$rawsearchres[tmp,]
                  DT::datatable(Maindata$searchreskeyword,escape = F,filter='top',rownames = F, extensions = 'ColVis', options = list(dom = 'C<"clear">lfrtip',columnDefs = list(list(visible=FALSE, targets=4:8)),colVis = list(showAll = "Show all",restore = "Restore")))                  
            }
      )      
      
      output$keyworddownloadbuttonkeyword <- downloadHandler(
            filename = function() { "Key Word Table.txt" },
            content = function(file) {                                    
                  write.table(Maindata$keywordres,file=file,quote=F,row.names=F,sep="\t")                                          
            }
      )
      
      output$keyworddownloadbuttontable <- downloadHandler(
            filename = function() { "Search Results.txt" },
            content = function(file) {                  
                  if (input$keyworddownloadselectedparttf) {
                        write.table(Maindata$rawsearchreskeyword[input$keywordshowselectsample_rows_selected,],file=file,quote=F,row.names=F,sep="\t")                        
                  } else {
                        write.table(Maindata$rawsearchreskeyword,file=file,quote=F,row.names=F,sep="\t")                        
                  }                  
            }
      )
            
      ### Sample ###
      
      output$sampleselectui <- renderUI({            
            if (input$sampleselectmethod=='select1') {
                  selectizeInput("GSMsampleselect1","",Maindata$rawsearchres[input$searchshowtable_rows_selected,1],multiple=T)
            } else if (input$sampleselectmethod=='select2') {
                  selectizeInput("GSMsampleselect2","",Maindata$rawsearchreskeyword[input$keywordshowselectsample_rows_selected,1],multiple=T)
            } else {
                  tagList(
                        helpText("Multiple GSE name should be separated by ;"),
                        textInput("GSMsamplenew","Type GSM name")
                        )
            }            
      })
      
      observe({
            if (input$samplesearchbutton > 0) {
                  isolate({
                        if (input$sampleselectmethod=='select1') {
                              GSEnamelist <- input$GSMsampleselect1
                        } else if (input$sampleselectmethod=='select2') {
                              GSEnamelist <- input$GSMsampleselect2
                        } else {
                              GSEnamelist <- gsub(" ","",input$GSMsamplenew)
                              GSEnamelist <- strsplit(GSEnamelist,";")[[1]]
                        }
                        if (!is.null(GSEnamelist)) {
                              withProgress(message = 'Retrieving sample information...',{
                              Maindata$sampleres <- sampledetail(GSEnamelist)      
                              })
                        }                        
                  })
            }
      })
      
      output$sampleshowtable <- DT::renderDataTable(
            if (!is.null(Maindata$sampleres))
                  DT::datatable(Maindata$sampleres,escape = F,filter='top',rownames = F, extensions = 'ColVis', options = list(dom = 'C<"clear">lfrtip',columnDefs = list(list(visible=FALSE, targets=6:7)),colVis = list(showAll = "Show all",restore = "Restore")))
      )
      
      output$sampledownloadbutton <- downloadHandler(
            filename = function() { "Sample.txt" },
            content = function(file) {                  
                  if (input$sampledownloadselectedparttf) {
                        write.table(Maindata$sampleres[input$sampleshowtable_rows_selected,],file=file,quote=F,row.names=F,sep="\t")                        
                  } else {
                        write.table(Maindata$sampleres,file=file,quote=F,row.names=F,sep="\t")                        
                  }                  
            }
      )
      
})