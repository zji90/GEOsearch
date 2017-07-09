######################################################
##                    GEOsearch                     ##
##             Interactive User Interface           ##
##                     UI File                      ##
##           Author:Zhicheng Ji, Hongkai Ji         ##
##       Maintainer:Zhicheng Ji (zji4@jhu.edu)      ##
######################################################

shinyUI(      
      navbarPage("",
                 tabPanel("GEOsearch",
                          
                          p()  
                          
                 ),
                 tabPanel("Perform Search",
                          hr(),
                          hr(),
                          sidebarPanel(
                                h4("Search GEO"),                                         
                                radioButtons("searchoption",NULL,list("Traditional Search"="Traditional","Extended Search"="Expand")),
                                checkboxInput("searchGSMTF","Search GSM samples in addition to GSE series (may take a long time)",value=FALSE),
                                conditionalPanel(condition="input.searchoption=='Expand'",                                         
                                                 selectizeInput("searchexpandselectspecies","Select species in which gene alias will be searched",choices=c("Anopheles","Arabidopsis","Bovine","Worm","Canine","Fly","Zebrafish","E coli strain K12","E coli strain Sakai","Chicken","Human","Mouse","Rhesus","Malaria","Chimp","Rat","Yeast","Pig","Xenopus"),selected=c("Human","Mouse"),multiple=T),
                                                 helpText("Expand search will search all possible gene alias in the database. Search speed is slower for more species chosen.")
                                ),
                                textInput("searchterm","Enter Search Term"),
                                actionButton("searchbutton","Start Search"),
                                hr(),
                                h4("Download Full Results"),
                                checkboxInput("searchdownloadselectedparttf","Download Selected Part",value=FALSE),
                                downloadButton("searchdownloadbutton","Download GSE Search Results"),
                                conditionalPanel(condition="input.searchGSMTF=='1'",
                                                 checkboxInput("searchdownloadGSMselectedparttf","Download Selected Part",value=FALSE),
                                                 downloadButton("searchdownloadGSMbutton","Download GSM Search Results")
                                )
                                ,width=3),
                          mainPanel(uiOutput("searchshowtableui")
                          )
                 ),
                 tabPanel("Key Word Enrichment",
                          hr(),
                          hr(),
                          sidebarPanel(
                                h4("Key Word Enrichment Analysis"),
                                checkboxGroupInput("keywordselectterm","Include Following Information",choices = list("Disease"="disease","Cell Type"="celltype","Tissue"="tissue"),selected = c("disease","celltype","tissue")),
                                hr(),
                                h4("Download Full Results"),
                                downloadButton("keyworddownloadbuttonkeyword","Download Keyword Table"),
                                hr(),
                                checkboxInput("keyworddownloadselectedparttf","Download Selected Part",value=FALSE),
                                downloadButton("keyworddownloadbuttontable","Download GSE for Selected Terms")
                                ,width=3),
                          mainPanel(tabsetPanel(
                                tabPanel("Key Word",br(),DT::dataTableOutput("keywordshowkeyword")),
                                tabPanel("GSE for Selected Terms",br(),DT::dataTableOutput("keywordshowselectsample"))
                          )  
                          )
                          
                 ),
                 tabPanel("Sample Details",
                          hr(),
                          hr(),
                          sidebarPanel(
                                h4("Sample Details"),
                                radioButtons("sampleselectmethod",NULL,list("Choose from selected GSE in step 1"="select1","Choose from selected GSE in step 2"="select2","Enter new GSE"="new")),
                                uiOutput("sampleselectui"),                                       
                                actionButton("samplesearchbutton","Search Samples"),
                                hr(),
                                h4("Download Full Results"),
                                checkboxInput("sampledownloadselectedparttf","Download Selected Part",value=FALSE),
                                downloadButton("sampledownloadbutton","Download Search Results")
                                ,width=3),
                          mainPanel(DT::dataTableOutput("sampleshowtable")
                          )
                 ),
                 tabPanel("Batch Download",
                          hr(),
                          hr(),
                          sidebarPanel(
                                helpText("Before running this function, read instructions on the right."),
                                helpText("Multiple GSE or GSM name should be separated by ;"),
                                textInput("Batchinputtext","Enter GSE or GSM name"),
                                textInput("Batchinputpath","Specify the download path of the shell file","./"),
                                actionButton("Batchrunbutton","Make Shell File"),
                                br(),
                                br(),
                                downloadButton("Batchdownloadbutton","Download Shell File")
                                ,width=3),
                          mainPanel(
                                wellPanel(
                                      p("This function will return a shell file that will automatically download all the SRA files of the GSE or GSM if run."),
                                      p("The shell file must be run on user's own Linux/Unix computer with wget installed."),
                                      p("Note that GSE series with lots of samples will take the function a long time to finish. Use with care.")                                   
                                ),
                                h4("Shell File:"),
                                verbatimTextOutput("showbatchdownloadres")
                          )
                 ),id="MainMenu",position = "fixed-top",inverse=T
                 
      )
)