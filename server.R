library(shiny)

server <- function(input,output,session ){
  options(encoding = "UTF-8")
  output$files <- renderTable(input$upload)
  options(shiny.maxRequestSize=1000*1024^2)
  observe({
    if(is.null(input$files)){return(NULL)}
    file.copy(from = input$files$datapath[1], to= paste0("C:/Users/vaillant/Documents/Projets/ProjetsR/FromScratchNirsDB/uploads"))
  })
  
  
  #access to the app from the homepage link
  observeEvent(input$app, updateTabsetPanel(session = session, inputId = "tabset", selected = "app"))
  
}