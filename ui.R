library(shiny)
library(shinyWidgets)

ui <- function(){
  listSugar <- list("arabinose","cellobiose","fructose","fucose","galactose","glucose","inositol",
                    "isomaltose","maltose","mannose","mannose_xylose","melbiose","melezitose","palatinose",
                    "raffinose","rhammose","ribose","sucrose","trehalose","xylose")
  listGlucosinolates<- list("x3mtp","x5mtp","x6msh","x7msh","x7mtp","x8mso","x8mtp","butyl",
                            "epigallocatechin","epiprogoitrin","glucoalysiin","glucobrassicin","glucoerucin","gluconapin",
                            "gluconasturtiin","glucoraphanin","glucoraphenin","glucosinalbin","hexyl","isobutyl",
                            "neoglucabrassicin_peak2","neoglucoabrassicin_peak1","progoitrin","sinirgin")
  listSecondaryMetabolites<- list("apigenin_rutinoside","caffeic_acid","chlorogenic_acid","citrat",
                                  "cyanidin_rhamnoside","cyanidin_sophorosid_glucoside","dihydro_caffeoyl_glucuronide","fumarat",
                                  "kaempherol_glucosyl_rhamnoside","kaempherol_rutinoside","kaempherol_xylosyl_rhamnoside","malat",
                                  "m_coumaric_acid","pelargonidin_cumaroyl_diglucoside","pelargonidin_sambubioside","prenyl_narigenin",
                                  "quercetin_glucoside","succinat")
  bootstrapPage('',
                tags$style(type="text/css",
                           HTML('.navbar {background-color: #50C21B; font-size: 18px;}
                           .navbar-default .navbar-brand {color: #ffffff; font-size: 20px;}
                           .navbar-default .navbar-nav > .active > a, 
                           .navbar-default .navbar-nav > .active > a:focus, 
                           .navbar-default .navbar-nav > .active > a:hover {color: #000000; background-color: #6AF32A;}
                           .well {background-color: #82E780;}')),
                tags$head(tags$link(rel="shortcut icon", href="/www/favicon.ico")),
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
                                      fluidRow(column(width =4,
                                                      tabsetPanel(
                                                        tabPanel("Submit Spectrum",
                                                                 p(""),wellPanel(
                                                                 fileInput('spectrumfile','Upload CSV File',
                                                                           accept = c('text/csv','text/comma-separated-values,tet/plain','.csv')),
                                                                 tableOutput("spectrum"))
                                                      ),
                                                      tabPanel("Consult Database",
                                                               #INFOS GENERAL###############
                                                               p(""),h4("General Informations"),wellPanel(fluidRow(column(width =6, pickerInput("location",'Location',multiple=TRUE,choices=NULL,options = list(`actions-box` = TRUE))),
                                                               column(width =6,pickerInput("exp",'Experimentation',multiple=TRUE,choices=NULL,options = list(`actions-box` = TRUE)))),
                                                               ###2EME LIGNE
                                                               fluidRow(column(width =6,pickerInput("contributor",'Contributor',multiple=TRUE,choices=NULL,options = list(`actions-box` = TRUE))),
                                                               column(width=6,dateRangeInput("date",'Dates')))),
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
                                                               fluidRow(column(width =6,pickerInput("treatment",'Treatment',multiple=TRUE,choices=NULL,options = list(`actions-box` = TRUE))))),
                                                               #OTHER OPTIONS#############
                                                               p(""),h4("Other Options"),wellPanel(fluidRow(column(width =6, pickerInput("CSR",'CSR',multiple=TRUE,choices=list("CSR_S","CSR_C","CSR_R"),options = list(`actions-box` = TRUE))),
                                                               column(width =6,pickerInput("sugar",'Sugars',multiple=TRUE,choices=listSugar,options = list(`actions-box` = TRUE)))),
                                                               ##2EME LIGNE
                                                               fluidRow(column(width =6,pickerInput("glucosinolates",'Glucosinolates',multiple=TRUE,choices=listGlucosinolates,options = list(`actions-box` = TRUE)))
                                                               ,column(width =6,pickerInput("secondary_metabolites",'Secondary Metabolites',multiple=TRUE,choices=listSecondaryMetabolites,selected = listSecondaryMetabolites,options = list(`actions-box` = TRUE))))),
                                                               #FORMAT##################
                                                               p(""),h4("Output"),wellPanel(fluidRow(column(width=6,radioButtons("outputformat","Output format",choices = list("All Data","Spectrum only","Phenotypic traits only"))),
                                                                                           column(width=6,actionButton("submit","Submit",icon("paper-plane"),style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))),
                                                                                           fluidRow(span(textOutput('resText'),style="color:red;text-align:center;")))),
                                                      
                                                      tabPanel("Become Contributor",
                                                               p(""),
                                                      ))
                                                      ),
                                               column(width=7,plotOutput('MeanPlot',height=700))
                                               
                                    ))
                ),
                tabPanel("About"))
  )

}
  
