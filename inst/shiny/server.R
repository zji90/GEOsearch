######################################################
##                      GEOsearch                   ##
##             Interactive User Interface           ##
##                     Server File                  ##
##           Author:Zhicheng Ji, Hongkai Ji         ##
##       Maintainer:Zhicheng Ji (zji4@jhu.edu)      ##
######################################################
library(GEOsearch)
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
                                          tmpterm <- TermAlias(input$searchterm,allspecies = input$searchexpandselectspecies)                                          
                                    })
                              }
                              withProgress(message = 'Compiling GEO Search Results for GSE...',{
                                    Maindata$rawsearchres <- tmp <- GEOSearchTerm(tmpterm,type="GSE")
                              })
                              tmp$Series <- paste0('<a href="https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=',tmp$Series,'" target=_blank>',tmp$Series,'</a>')
                              tmp$Term <- factor(tmp$Term)      
                              Maindata$searchres <- tmp                              
                              
                              if (input$searchGSMTF) {
                                    withProgress(message = 'Compiling GEO Search Results for GSM...',{
                                          Maindata$rawsearchresGSM <- tmp <- GEOSearchTerm(tmpterm,type="GSM")
                                    })
                                    tmp$Sample <- paste0('<a href="https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=',tmp$Sample,'" target=_blank>',tmp$Sample,'</a>')                                    
                                    tmp$Series <- sapply(tmp$Series, function(i) {
                                          tmpSeries <- strsplit(i,",")[[1]]      
                                          paste(paste0('<a href="https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=',tmpSeries,'" target=_blank>',tmpSeries,'</a>'),collapse = ",")
                                    })      
                                    tmp$Term <- factor(tmp$Term)      
                                    Maindata$searchresGSM <- tmp                              
                              }                              
                        }    
                  })      
            }            
      })
      
      DTfunc <- function(df,target=NULL) {
            row.names(df) <- NULL
            if (is.null(target)) {
                  DT::datatable(df,escape = F,filter='top', extensions = c('Buttons','ColReorder','RowReorder'), options = list(dom = 'Bfrtip',columnDefs = list(list(className="dt-left","targets"="_all")),buttons = c('copy', 'csv', 'excel', 'pdf', 'print','colvis'),colReorder = TRUE,rowReorder = TRUE))     
            } else {
                  DT::datatable(df,escape = F,filter='top', extensions = c('Buttons','ColReorder','RowReorder'), options = list(dom = 'Bfrtip',columnDefs = list(list(visible=FALSE, targets=target),list(className="dt-left","targets"="_all")),buttons = c('copy', 'csv', 'excel', 'pdf', 'print','colvis'),colReorder = TRUE,rowReorder = TRUE))      
            }
      }
      
      output$searchshowtable <- DT::renderDataTable(
            if (!is.null(Maindata$searchres))
                  DTfunc(Maindata$searchres,5:8)
      )
      
      output$searchshowtableGSM <- DT::renderDataTable(
            if (!is.null(Maindata$searchresGSM))
                  DTfunc(Maindata$searchresGSM,6:7)
      )
      
      output$searchshowtableui <- renderUI({
            if (input$searchGSMTF) {
                  tabsetPanel(
                        tabPanel("GSE Series",br(),DT::dataTableOutput("searchshowtable")),
                        tabPanel("GSM Samples",br(),DT::dataTableOutput("searchshowtableGSM"))
                  )     
            } else {
                  DT::dataTableOutput("searchshowtable")            
            }            
      })      
      
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
      
      output$searchdownloadGSMbutton <- downloadHandler(
            filename = function() { "Search Results.txt" },
            content = function(file) {                  
                  if (input$searchdownloadGSMselectedparttf) {
                        write.table(Maindata$rawsearchresGSM[input$searchshowtableGSM_rows_selected,],file=file,quote=F,row.names=F,sep="\t")                        
                  } else {
                        write.table(Maindata$rawsearchresGSM,file=file,quote=F,row.names=F,sep="\t")                        
                  }                  
            }
      )
            
      ### Keyword ###
      
      output$keywordshowkeyword <- DT::renderDataTable(
            if (!is.null(Maindata$searchres)) {
                  withProgress(message = 'Calculating key word frequencies...',{
                        Maindata$keywordres <- KeyWordFreq(Maindata$searchres,category = input$keywordselectterm)                  
                  })
                  DTfunc(Maindata$keywordres)
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
                  DTfunc(Maindata$searchreskeyword,4:8)
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
                        textInput("GSMsamplenew","Enter GSE name")
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
                                    Maindata$sampleres <- SampleDetail(GSEnamelist)      
                              })
                        }                        
                  })
            }
      })
      
      output$sampleshowtable <- DT::renderDataTable(
            if (!is.null(Maindata$sampleres))
                  DTfunc(Maindata$sampleres,6:7)
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
      
      ### Batch Download ###
      observe({
            if (input$Batchrunbutton > 0) {
                  isolate({
                        if (!is.null(input$Batchinputtext)) {                                 
                              withProgress(message = 'Making shell file...',{
                                    Maindata$batchdownloadres <- BatchDownload(strsplit(input$Batchinputtext,";")[[1]],input$Batchinputpath)
                              })
                        }                        
                  })                  
            }
      })
      
      output$showbatchdownloadres <- renderText({            
            if (!is.null(Maindata$batchdownloadres))             
                  Maindata$batchdownloadres
      })
      
      output$Batchdownloadbutton <- downloadHandler(
            filename = function() { "Download.sh" },
            content = function(file) {                                    
                  writeLines(Maindata$batchdownloadres,file)                                                            
            }
      )
      
})