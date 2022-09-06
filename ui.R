library(shiny)
library(shinyWidgets)
  fluidRow(useShinyjs(),
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
                           .homepage{border-radius:25px}
                                pre{color:red;background-color:#fbf65e;}')),
                tags$head(tags$link(rel="shortcut icon", href="favicon.ico")),
                navbarPage(title = "NirsDB", id = "tabset", 
                           tabPanel("Home page",
                                    
                                    fluidRow(column(width = 6, offset = 3,
                                                    wellPanel(align = "justify",class="homepage",
                                                              HTML("<h1 align = 'center'>NirsDB <i>1.0.0</i> </h1>"),
                                                              br(),
                                                              HTML("<p> NirsDB is an application allowing to make predictions about phenotypic traits of A. thaliana by submiting related NIRS spectrum using deep-learning.
                                                              NirsDB has three functionalities : </p>
                                                                   <ul><li>The submition of your data to our deep-learning algorithms to get traits predictions.</li>
                                                                   <li>The consultation of the database used to construct our deep-learning models, 
                                                                   it's possible to target the data you're interested in with some filters and download the corresponding dataset , morevover graphical representation of the dataset will be printed. </li>
                                                                   <li>Contribute to the application by submiting your dataset so we can examine it and add it to our database/models</li></ul>"),
                                                              fluidRow(actionButton("app", "Access the application"), align = "center"),
                                                              img(src = "arabido.png", width = '100%', height = "auto",style="border-radius: 100px;")
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
                                                                 fileInput('spectrumfile','Upload Spectrum CSV File',accept = c('text/csv','text/comma-separated-values,tet/plain','.csv')),
                                                                 shinyjs::hidden(div(id="inputTrait",(fileInput('traitsfile','Upload Traits CSV File',accept = c('text/csv','text/comma-separated-values,tet/plain','.csv'))))),
                                                                 
                                                                 
                                                                 shinyjs::hidden(div(id="inputDataTest",column(width=6,fileInput('testSpectrumFile','Upload Test Spectrum CSV File',accept = c('text/csv','text/comma-separated-values,tet/plain','.csv'))),
                                                                                     column(width=6,(fileInput('testTraitsFile','Upload Test Traits CSV File',accept = c('text/csv','text/comma-separated-values,tet/plain','.csv')))))),
                                                                 
                                                                 fluidRow(column(width=6,radioButtons("runMode","Mode",choices =list("Predictions using our model","Create new model + Predictions","Multiple traits to predict","Complete, Test dataset needed")),
                                                                                 pickerInput("functionalTraits","Functional traits",multiple=TRUE,choices=listFunctionalTraits,options = list(`actions-box` = TRUE)),
                                                                          pickerInput("metabolites","Metabolites",multiple=TRUE,choices=list(Hormones=listHormones,Sugars=listSugar,Glucosinolates=listGlucosinolates,Secondary_Metabolites=listSecondaryMetabolites),options = list(`actions-box` = TRUE))),
                                                                          column(width = 6,HTML(paste('<br>',p("Predictions only is fast while building new models will take several hours.")),
                                                                                                paste('<br>',p("Select some traits or/and metabolites to predict. Beware that the more you select the longer it will be."))))),
                                                                 fluidRow(column(width = 6,shinyjs::disabled(actionButton("runAnalysis","Run",icon("paper-plane"))),p(""),span("You must provide a valid email adress before launch so you can receive your run's results",style="color:red")),
                                                                          column(width=6,textInput('mail',"",placeholder="Enter a valid email adress"),actionButton("Go","Register")))),
                                                                 wellPanel(span("Prediction with our models can be imprecise due to specific conditions of your samples. \n
                                                                                                In this case you can train against our deep learning scripts to build new model more suitable for your data to make predictions."))),column(width=3,p(""),img(src="stats.png", width='100%', height='auto')),column(width=5,img(src = "map.png", width = '100%', height = "auto"))
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
                                                               column(width=6,shinyjs::hidden(img(src = "AllSpectraPCA.png",height="105%",width="105%",id="imgPCA"))),
                                                              column(width=6,withSpinner(plotOutput('selectedPCAPlot',height =500)))))))),
                                                      
                                                      tabPanel("Become Contributor",column(width = 6,p(""),
                                                      wellPanel(p("We will examine your dataset and maybe use your data to extend our database."),fileInput('contributorfile','Upload CSV File',accept = c('text/csv','text/comma-separated-values,tet/plain','.csv')),
                                                                fluidRow(column(width=4,shinyjs::disabled(actionButton("sendContribution","Send")),p(""),span("You must provide a valid email adress before send so we can ask you further informations if your data is relevant",style="color:red")),
                                                                         column(width=8,textInput('mailcontrib',"",placeholder="Enter a valid email adress"),actionButton("Gocontrib","Register")))))
                                                      ))
                                                      ))
                ),
                tabPanel("About",fluidRow(column(width=6,offset=3,wellPanel(align = "justify",
                                                                            HTML("<h1 align = 'center'>NirsDB</h1>"),
                                                                            HTML("<p align = 'center'>NirsDB is a tool allowing to make deep-learning predictions about phenotypic traits of A. thaliana by submiting related NIRS spectrum</p>"),
                                                                            HTML("<p align = 'center'>'A Perspective on Plant Phenomics: Coupling Deep Learning and Near-Infrared Spectroscopy' - <a href=https://doi.org/10.3389/fpls.2022.836488>Vasseur et al.</a></p>"),
                                                                            HTML("<p align = 'center'>You can contact us on this email adress : nirsdb@post.com </p>"),
                                                                            HTML("<p align = 'center'><img src = 'cefe.png' width = '150px' height = 'auto'><img src = 'cnrs.png' width = '150px' height = 'auto'><img src = 'institutAgro.png' width = '150px' height = 'auto'>
                                                                                 <img src = 'cirad.png' width = '150px' height = 'auto'><img src = 'univMtp.svg' width = '150px' height = 'auto'><img src = 'inrae.png' width = '150px' height = 'auto'>
                                                                                 <img src = 'ird.png' width = '150px' height = 'auto'><img src = 'ephe.png' width = '150px' height = 'auto'></p>"))))))
  )

)
