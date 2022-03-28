library(shiny)
library(shinythemes)

ui <- function(){
  #theme=shinytheme("flatly")
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
                                                              img(src = "homepage.jpg", width = '100%', height = "auto")
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
                                                                 p(""),
                                                                 fileInput('spectrumfile','Upload CSV File',
                                                                           accept = c('text/csv','text/comma-separated-values,tet/plain','.csv')),
                                                                 tableOutput("files")
                                                      ),
                                                      tabPanel("Consult Database",
                                                               #INFOS GENERAL
                                                               p(""),h4("General Informations"),fluidRow(column(width =6, selectInput("location",'Location',multiple=TRUE,choices=list("location1","location2"))),
                                                               column(width =6,selectInput("exp",'Experimentation',multiple=TRUE,choices=list("location1","location2")))),
                                                               ###2EME LIGNE
                                                               fluidRow(column(width =6,selectInput("contributor",'Contributor',multiple=TRUE,choices=list("location1","location2"))),
                                                               column(width=6,dateRangeInput("date",'Dates')))),

                                                      tabPanel("Become Contributor",
                                                               p(""),
                                                      ))
                                                      )
                                    ))
                ))
  )
}
  
