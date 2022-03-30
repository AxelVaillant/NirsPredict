library(shiny)
library(RMySQL)
server <- function(input,output,session ){
  
  ############# ####DATABASE MANAGER##########################
  options(mysql = list(
    "host" = "127.0.0.1",
    "port" = 3306,
    "user" = "root",
    "password" = ""
  ))
  nirsdb <- "nirsdb"

  
  loadData <- function(data) {
    # Construct the fetching query
    query <- sprintf("SELECT * FROM %s", data)
    # Submit the fetch query and disconnect
    data <- dbGetQuery(con, query)
    dbDisconnect(con)
    data
  }
                
  #################################################
  ########      QUERY SELECT INPUT      ###########
  #################################################
  getData <- function(){
    # Connect to the database
    con <- dbConnect(MySQL(), dbname = nirsdb, host = options()$mysql$host, 
                     port = options()$mysql$port, user = options()$mysql$user, 
                     password = options()$mysql$password)
    
    ######################INPUT UPDATE###########################
    observe({
      listGenInfos <- list("exp_location","id_exp","main_contributor")
      listSampleInfos <- list("condition_exp","genetic_group","genotype","leaf_stage",
                              "measurement","plant_stage","treatment")
      for( i in listGenInfos){
        query <- paste("SELECT DISTINCT ",i," FROM general_informations")
        assign(paste("SqlOutput",i,sep=""),dbGetQuery(con, query))
      }
      for( i in listSampleInfos){
        query <- paste("SELECT DISTINCT ",i," FROM sample")
        assign(paste("SqlOutput",i,sep=""),dbGetQuery(con, query))
      }
        query <- paste("SELECT DISTINCT csr_strategy_class FROM CSR")
        SqlOutputcsr_strategy_class<- dbGetQuery(con, query)
        
      updatePickerInput(session, "location", choices = SqlOutputexp_location)
      updatePickerInput(session, "exp", choices = SqlOutputid_exp)
      updatePickerInput(session, "contributor", choices = SqlOutputmain_contributor)
      updatePickerInput(session, "genetic_group", choices = SqlOutputgenetic_group)
      updatePickerInput(session, "genotype", choices = SqlOutputgenotype)
      updatePickerInput(session, "condition", choices = SqlOutputcondition_exp)
      updatePickerInput(session, "leaf_stage", choices = SqlOutputleaf_stage)
      updatePickerInput(session, "plant_stage", choices = SqlOutputplant_stage)
      updatePickerInput(session, "measurement", choices = SqlOutputmeasurement)
      updatePickerInput(session, "treatment", choices = SqlOutputtreatment)
      updatePickerInput(session, "csr_strategy", choices = SqlOutputcsr_strategy_class)
      
      dbDisconnect(con)
    })
  }
  getData()
  
  ################FORM MANAGER####################
  observeEvent(input$submit, {
    # Connect to the database
    con <- dbConnect(MySQL(), dbname = nirsdb, host = options()$mysql$host, 
                     port = options()$mysql$port, user = options()$mysql$user, 
                     password = options()$mysql$password)
    res <- dbGetQuery(conn = con,statement = 'SELECT * FROM general_informations WHERE exp_location = ("LEPSE, Montpellier, France")')
    write.csv(res,file="C:/Users/vaillant/Documents/Projets/ProjetsR/FromScratchNirsDBtest")
    dbDisconnect(con)
  })
    

  
  ###OPTIONS
  options(encoding = "UTF-8")
  output$files <- renderTable(input$upload)
  options(shiny.maxRequestSize=1000*1024^2)
  
  ###UPLOAD HANDLING
  observe({
    if(is.null(input$files)){return(NULL)}
    file.copy(from = input$files$datapath[1], to= paste0("C:/Users/vaillant/Documents/Projets/ProjetsR/FromScratchNirsDB/uploads"))
  })
  
  
  #access to the app from the homepage link
  observeEvent(input$app, updateTabsetPanel(session = session, inputId = "tabset", selected = "app"))
  
}