######################################################
##                    GEOsearch                     ##
##             Interactive User Interface           ##
##                     UI File                      ##
##           Author:Zhicheng Ji, Hongkai Ji         ##
##       Maintainer:Zhicheng Ji (zji4@jhu.edu)      ##
######################################################

shinyUI(      
      pageWithSidebar(
            
            headerPanel('GEOsearch: Extendable Search Engine for Gene Expression Omnibus'),
            
            sidebarPanel(
                  wellPanel(
                        radioButtons("Mainmenu","Main Menu",list("Perform Search"="search","Key Word Enrichment"="keyword","Sample Details"="sample","Batch Download"="batch"))
                  ),
                  conditionalPanel(condition="input.Mainmenu=='search'",
                                   wellPanel(
                                         h4("Search GEO"),                                         
                                         radioButtons("searchoption","",list("Traditional Search"="Traditional","Extended Search"="Expand")),
                                         checkboxInput("searchGSMTF","In addition to GSE series, also search GSM samples (this step may take a long time for extended search)",value=FALSE),
                                         conditionalPanel(condition="input.searchoption=='Expand'",                                         
                                                          selectizeInput("searchexpandselectspecies","Select species in which gene alias will be searched",choices=c("Anopheles","Arabidopsis","Bovine","Worm","Canine","Fly","Zebrafish","E coli strain K12","E coli strain Sakai","Chicken","Human","Mouse","Rhesus","Malaria","Chimp","Rat","Yeast","Pig","Xenopus"),selected=c("Human","Mouse"),multiple=T),
                                                          helpText("Expand search will search all possible gene alias in the database. Search speed is slower for more species chosen.")
                                         ),
                                         textInput("searchterm","Enter Search Term"),
                                         actionButton("searchbutton","Start Search")                        
                                   ),
                                   wellPanel(
                                         h4("Download"),
                                         checkboxInput("searchdownloadselectedparttf","Download Selected Part",value=FALSE),
                                         downloadButton("searchdownloadbutton","Download Search Results for GSE series"),
                                         conditionalPanel(condition="input.searchGSMTF=='1'",
                                                          checkboxInput("searchdownloadGSMselectedparttf","Download Selected Part",value=FALSE),
                                                          downloadButton("searchdownloadGSMbutton","Download Search Results for GSM samples")
                                         )
                                   )
                  ),
                  
                  conditionalPanel(condition="input.Mainmenu=='keyword'",
                                   wellPanel(
                                         h4("Key Word Enrichment Analysis"),
                                         checkboxGroupInput("keywordselectterm","Include Following Information",choices = list("Disease"="disease","Cell Type"="celltype","Tissue"="tissue"),selected = c("disease","celltype","tissue"))
                                   ),
                                   wellPanel(
                                         h4("Download Results"),
                                         downloadButton("keyworddownloadbuttonkeyword","Download Keyword Table"),
                                         hr(),
                                         checkboxInput("keyworddownloadselectedparttf","Download Selected Part",value=FALSE),
                                         downloadButton("keyworddownloadbuttontable","Download Search Results")
                                   )
                  ),
                  conditionalPanel(condition="input.Mainmenu=='sample'",   
                                   wellPanel(
                                         h4("Sample Details"),
                                         radioButtons("sampleselectmethod","",list("Choose from selected GSE in step 1"="select1","Choose from selected GSE in step 2"="select2","Enter new GSE"="new")),
                                         uiOutput("sampleselectui"),                                       
                                         actionButton("samplesearchbutton","Search Samples")
                                   ),
                                   wellPanel(
                                         h4("Download"),
                                         checkboxInput("sampledownloadselectedparttf","Download Selected Part",value=FALSE),
                                         downloadButton("sampledownloadbutton","Download Search Results")
                                   )
                  ),                  
                  conditionalPanel(condition="input.Mainmenu=='batch'",
                                   wellPanel(
                                         helpText("Before running this function, see instructions on the right."),
                                         helpText("Multiple GSE or GSM name should be separated by ;"),
                                         textInput("Batchinputtext","Enter GSE or GSM name"),
                                         textInput("Batchinputpath","Specify the download path of the shell file","./"),
                                         actionButton("Batchrunbutton","Make Shell File"),
                                         downloadButton("Batchdownloadbutton","Download Shell File")
                                   )                 
                  )
                  ,width=3),
            mainPanel(
                  conditionalPanel(condition="input.Mainmenu=='search'",
                                   uiOutput("searchshowtableui")                             
                  ),
                  conditionalPanel(condition="input.Mainmenu=='keyword'",                                                                                          
                                   tabsetPanel(
                                         tabPanel("Key Word",DT::dataTableOutput("keywordshowkeyword")),
                                         tabPanel("Selected GSE",DT::dataTableOutput("keywordshowselectsample"))                                             
                                   )                                 
                  ),
                  conditionalPanel(condition="input.Mainmenu=='sample'",                                                                                                
                                   DT::dataTableOutput("sampleshowtable")                                                                            
                  ),
                  conditionalPanel(condition="input.Mainmenu=='batch'",
                                   wellPanel(
                                   p("This function will return a shell file that will automatically download all the SRA files of the GSE or GSM if run."),
                                   p("The shell file must be run on user's own Linux/Unix computer with wget installed."),
                                   p("Note that GSE series with lots of samples will take the function a long time to finish. Use with care.")                                   
                                   ),
                                   h4("Shell File:"),
                                   verbatimTextOutput("showbatchdownloadres")
                  )
            )            
      ))