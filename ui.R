library(shiny)
library(shinyWidgets)
listSugar <- list("arabinose","cellobiose","fructose","fucose","galactose","glucose","inositol",
                  "isomaltose","maltose","mannose","mannoseXylose","melbiose","melezitose","palatinose",
                  "raffinose","rhamnose","ribose","sucrose","trehalose","xylose")
listGlucosinolates<- list("x3mtp","x5mtp","x6msh","x7msh","x7mtp","x8mso","x8mtp","butyl",
                          "epigallocatechin","epiprogoitrin","glucoalysiin","glucobrassicin","glucoerucin","gluconapin",
                          "gluconasturtiin","glucoraphanin","glucoraphenin","glucosinalbin","hexyl","isobutyl",
                          "neoglucabrassicinpeak2","neoglucoabrassicinpeak1","progoitrin","sinirgin")
listSecondaryMetabolites<- list("apigeninRutinoside","caffeicAcid","chlorogenicAcid","citrat",
                                "cyanidinRhamnoside","cyanidinSophorosidGlucoside","dihydroCaffeoylGlucuronide","fumarat",
                                "kaempherolGlucosylRhamnoside","kaempherolRutinoside","kaempherolXylosylRhamnoside","malat",
                                "mCoumaricAcid","pelargonidinCumaroylDiglucoside","pelargonidinSambubioside","prenylNarigenin",
                                "quercetinGlucoside","succinat")
auth0::auth0_ui(fluidRow(useShinyjs(),
                         #---prevent page reload to crash----
                         tagList( tags$head( tags$script(htmlwidgets::JS("setTimeout(function(){history.pushState({}, 'Page Title', '/');},2000);")))),
  bootstrapPage('',
                tags$style(type="text/css",
                           HTML('.navbar {background-color: #50C21B; font-size: 18px;}
                           .navbar-default .navbar-brand {color: #ffffff; font-size: 20px;}
                           .navbar-default .navbar-nav > .active > a, 
                           .navbar-default .navbar-nav > .active > a:focus, 
                           .navbar-default .navbar-nav > .active > a:hover {color: #000000; background-color: #6AF32A;}
                           .well {background-color: #82E780;}
                                pre{color:red;background-color:#fbf65e;}')),
                tags$head(tags$link(href="/www/favicon.ico")),
                navbarPage(title = "NirsDB", id = "tabset", 
                           tabPanel("Home page",
                                    
                                    fluidRow(column(width = 6, offset = 3,
                                                    wellPanel(align = "justify",
                                                              HTML("<h1 align = 'center'>NirsDB <i>1.0.0</i> </h1>"),
                                                              br(),
                                                              p("NirsDB Application"),
                                                              fluidRow(actionButton("app", "Access the application"), align = "center"),
                                                              img(src = "plant.png", width = '100%', height = "auto")
                                                    )
                                    )
                                    )
                           ),
                           tabPanel("Application", value="app",
                                    fluidPage(
                                      
                                      p("NirsDB Application"),
                                      fluidRow(
                                                      tabsetPanel(
                                                        tabPanel("Submit Spectrum",column(width =4,
                                                                 p(""),wellPanel(span("The submited file must have headers",style="color:red"),
                                                                 fileInput('spectrumfile','Upload CSV File',accept = c('text/csv','text/comma-separated-values,tet/plain','.csv')),
                                                                 fluidRow(column(width=6,radioButtons("runMode","Mode",choices =list("Predictions using our model","Create new model + Predictions"))),
                                                                          column(width = 6,HTML(paste('<br>',p("Predictions only is fast while building new models will take several hours."))))),
                                                                 actionButton("runAnalysis","Run"),
                                                                 shinyjs::hidden(downloadButton('DlSpectrum', label="Download"))),
                                                                 wellPanel(span("Prediction with our models can be imprecise due to specific conditions of your samples. \n
                                                                                                In this case you can train against our deep learning scripts to build new model more suitable for your data to make predictions."))),column(width=2),column(width=6,img(src = "GenotypeByPosCustom2.png", width = '100%', height = "auto"))
                                                      ),
                                                      tabPanel("Consult Database",column(width =4,
                                                               #INFOS GENERAL###############
                                                               p(""),h4("General Informations"),wellPanel(fluidRow(column(width =6, pickerInput("location",'Location',multiple=TRUE,choices=NULL,options = list(`actions-box` = TRUE))),
                                                               column(width =6,pickerInput("exp",'Experimentation',multiple=TRUE,choices=NULL,options = list(`actions-box` = TRUE)))),
                                                               ###2EME LIGNE
                                                               fluidRow(column(width =6,pickerInput("contributor",'Contributor',multiple=TRUE,choices=NULL,options = list(`actions-box` = TRUE))),
                                                               column(width=6,dateRangeInput("date",'Dates',start = '2016-01-01',format='yyyy-mm')))),
                                                               #INFOS SAMPLE###############
                                                               p(""),h4("Sample Informations"),wellPanel(fluidRow(column(width =6, pickerInput("genotype",'Genotype',multiple=TRUE,choices=NULL,options = list(`actions-box` = TRUE))),
                                                               column(width =6,pickerInput("genetic_group",'Genetic group',multiple=TRUE,choices=NULL,options = list(`actions-box` = TRUE)))),
                                                               ###2EME LIGNE
                                                               fluidRow(column(width =6,pickerInput("condition",'Condition',multiple=TRUE,choices=NULL,options = list(`actions-box` = TRUE))),
                                                               column(width=6,pickerInput("leaf_stage",'Leaf Stage',multiple=TRUE,choices=NULL,options = list(`actions-box` = TRUE)))),
                                                               ###3EME LIGNE
                                                               fluidRow(column(width = 6,radioButtons("situation","Situation",choices =list("Both","Indoor","Outdoor"))),
                                                                        column(width = 6,radioButtons("leafAttach","Leaf Attachement",choices=list("Both","attached","detached")))),
                                                               ###4EME LIGNE
                                                               fluidRow(column(width =6,pickerInput("plant_stage",'Plant Stage',multiple=TRUE,choices=NULL,options = list(`actions-box` = TRUE))),
                                                               column(width=6,pickerInput("measurement",'Measurement',multiple=TRUE,choices=NULL,options = list(`actions-box` = TRUE)))),
                                                               ###5EME LIGNE
                                                               fluidRow(column(width =6,pickerInput("treatment",'Treatment',multiple=TRUE,choices=NULL,options = list(`actions-box` = TRUE))),
                                                                        column(width=6,radioButtons("nataccessions","Natural Accessions",choices=list("Included","Only","Excluded"))))),
                                                               #FORMAT##################
                                                               p(""),h4("Output"),wellPanel(fluidRow(column(width=6,radioButtons("outputformat","Output format",choices = list("All Data","Spectrum only","Phenotypic traits only","Custom"))),
                                                                                           column(width=6,actionButton("submit","Submit",icon("paper-plane"),style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))),
                                                                                           fluidRow(span(verbatimTextOutput('resText'),style="color:red;text-align:center;"),shinyjs::hidden(downloadButton('DlConsult', label="Download")))),
                                                               #CUSTOM OPTIONS#############
                                                               p(""),shinyjs::hidden(div(id="customoptions",h4("Custom Options"),wellPanel(fluidRow(column(width =6, pickerInput("CSR",'CSR',multiple=TRUE,choices=list("CSR_S","CSR_C","CSR_R"),options = list(`actions-box` = TRUE))),
                                                                                                                             column(width =6,pickerInput("sugar",'Sugars',multiple=TRUE,choices=listSugar,options = list(`actions-box` = TRUE)))),
                                                                                                                    ##2EME LIGNE
                                                                                                                    fluidRow(column(width =6,pickerInput("glucosinolates",'Glucosinolates',multiple=TRUE,choices=listGlucosinolates,options = list(`actions-box` = TRUE)))
                                                                                                                             ,column(width =6,pickerInput("secondary_metabolites",'Secondary Metabolites',multiple=TRUE,choices=listSecondaryMetabolites,options = list(`actions-box` = TRUE)))))))),
                                                               shinyjs::hidden(div(id="plotsOutput",column(width=8,column(width=11,withSpinner(plotOutput('MeanPlot',height=600)),
                                                               #column(width=6,img(src = "AllSpectraPCA.png")),
                                                               (column(width=6,withSpinner(plotOutput('allPCAPlot',height =500)))),
                                                              column(width=6,withSpinner(plotOutput('selectedPCAPlot',height =500)))))))),
                                                      
                                                      tabPanel("Become Contributor",column(width = 6,p(""),
                                                      wellPanel(p("We will examine your dataset and maybe use your data to extend our database."),fileInput('contributorfile','Upload CSV File',accept = c('text/csv','text/comma-separated-values,tet/plain','.csv')),
                                                                actionButton("sendContribution","Send")))
                                                      ))
                                                      ))
                ),
                tabPanel("About",fluidRow(column(width=6,offset=3,wellPanel(align = "justify",
                                                                            HTML("<h1 align = 'center'>NirsDB</h1>"),
                                                                            HTML("<p align = 'center'>NirsDB is tool allowing to make deep-learning predictions about phenotypic traits of A. thaliana by submiting related NIRS spectrum</p>"),
                                                                            HTML("<p align = 'center'><img src = 'cefe.png' width = '150px' height = 'auto'><img src = 'cnrs.png' width = '150px' height = 'auto'></p>"))))),tabPanel(logoutButton(label="Logout",id="logout")))
  )

), info = auth0_info)
