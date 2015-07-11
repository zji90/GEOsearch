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
                        radioButtons("Mainmenu","Main Menu",list("Step 1: Perform Search"="search","Step 2: Key Word Enrichment"="keyword","Step 3: Sample Details"="sample"))
                  ),
                  conditionalPanel(condition="input.Mainmenu=='search'",
                                   wellPanel(
                                         h4("Search GEO"),
                                         radioButtons("searchoption","",list("Traditional Search"="Traditional","Extended Search"="Expand")),
                                         conditionalPanel(condition="input.searchoption=='Expand'",                                         
                                                          selectizeInput("searchexpandselectspecies","Select species in which gene alias will be searched",choices=c("Anopheles","Arabidopsis","Bovine","Worm","Canine","Fly","Zebrafish","E coli strain K12","E coli strain Sakai","Chicken","Human","Mouse","Rhesus","Malaria","Chimp","Rat","Yeast","Pig","Xenopus"),selected=c("Human","Mouse"),multiple=T),
                                                          helpText("Expand search will search all possible gene alias in the database. Search speed is slower for more species chosen.")
                                         ),
                                         textInput("searchterm","Enter Search Term","Oct4"),
                                         actionButton("searchbutton","Start Search")                        
                                   ),
                                   wellPanel(
                                         h4("Download"),
                                         checkboxInput("searchdownloadselectedparttf","Download Selected Part",value=FALSE),
                                         downloadButton("searchdownloadbutton","Download Search Results")
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
                  )
                  ,width=3),
      
      mainPanel(
            conditionalPanel(condition="input.Mainmenu=='search'",
                             DT::dataTableOutput("searchshowtable")
                             ),
            conditionalPanel(condition="input.Mainmenu=='keyword'",                                                                                          
                             tabsetPanel(
                                   tabPanel("Key Word",DT::dataTableOutput("keywordshowkeyword")),
                                   tabPanel("Selected GSE",DT::dataTableOutput("keywordshowselectsample"))                                             
                             )                                 
            ),
            conditionalPanel(condition="input.Mainmenu=='sample'",                                                                                                
                             DT::dataTableOutput("sampleshowtable")                                                                            
            )
      )            
))