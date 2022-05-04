observeEvent(input$submit, {
  # Connect to the database
  con <- dbConnect(MySQL(), dbname = nirsdb, host = options()$mysql$host, 
                   port = options()$mysql$port, user = options()$mysql$user, 
                   password = options()$mysql$password)
  inputList<-list(input$location,input$exp,input$contributor,input$genotype,input$genetic_group,
                  input$condition,input$leaf_stage,input$plant_stage,input$measurement,input$treatment,
                  input$csr_strategy,input$CSR,input$sugar,input$glucosinolates,input$secondary_metabolites)
  inputNameList<- list("exp_location","id_exp","main_contributor","genotype","genetic_group","condition_exp",
                       "leaf_stage","plant_stage","measurement","treatment","csr_strategy_class","CSR","sugar",
                       "glucosinolates","secondary_metabolites")
  selectquery<-""
  filterList<-""
  for(i in 1:length(inputList)){
    if(!is.null(inputList[i][[1]])){
      filterList<-c(filterList,inputNameList[i])
      selectquery<- paste(selectquery,inputNameList[i],",")
      assign(paste("whereQuery",inputNameList[i],sep=""),inputList[i])
      
    }
  }
  selectquery<-substr(selectquery,1,nchar(selectquery)-2)
  #############QUERY BUILDING##############################
  #######"WHERE"QUERY BUILDING#############
  filterList1<-""
  for(i in 1:length(inputList)){
    paramList<-""
    if(!is.null(inputList[i][[1]])){
      for(j in 1:length(inputList[[i]])){
        paramList<-paste(paramList,inputList[[i]][j],",")
      }
      paramList<-substr(paramList,1,nchar(paramList)-2)
      if(i<4){
        filterList1<-c(filterList1, assign(paste("Query",i,sep=""),paste("general_informations.",inputNameList[i] ," IN ","(",paramList,")",sep="")))
      } else if(i>3 && i<11){
        filterList1<-c(filterList1, assign(paste("Query",i,sep=""),paste("sample.",inputNameList[i] ," IN ","(",paramList,")",sep="")))
      } else if(i>10 && i<13){
        filterList1<-c(filterList1, assign(paste("Query",i,sep=""),paste("CSR.",inputNameList[i] ," IN ","(",paramList,")",sep="")))
      } else if(i==13){
        filterList1<-c(filterList1, assign(paste("Query",i,sep=""),paste("sugar.",inputNameList[i] ," IN ","(",paramList,")",sep="")))
      }else if(i==14){
        filterList1<-c(filterList1, assign(paste("Query",i,sep=""),paste("glucosinolates.",inputNameList[i] ," IN ","(",paramList,")",sep="")))
      }else if(i==15){
        filterList1<-c(filterList1,assign(paste("Query",i,sep=""),paste("secondary_metabolites.",inputNameList[i] ," IN ","(",paramList,")",sep="")))
      }
    }
  }
  
  filterList<-sapply(filterList,paste,collapse=", ")
  filterList<- paste(filterList, collapse = " and ")
  filterList<-substr(filterList,5,nchar(filterList))
  
  filterList1<- paste(filterList1, collapse = " and ")
  filterList1<-substr(filterList1,5,nchar(filterList1))
  
  query1<- paste("SELECT ",selectquery," FROM SPECTRUM WHERE ",filterList1)
  query=""
  
  res <- dbGetQuery(conn = con,statement = query)
  write.csv(res,file="res.csv")
  dbDisconnect(con)
})


###OPTIONS
options(encoding = "UTF-8")
#output$spectrum <- renderTable(input$upload)
options(shiny.maxRequestSize=1000*1024^2)

###UPLOAD HANDLING
destDir<-'C:/Users/vaillant/Documents/Projets/ProjetsR/FromScratchNirsDB/uploads'
output$spectrum <- renderPrint({
  inFile <- input$spectrumfile
  if(is.null(inFile)){
    return(FALSE)
  }
  if(dir.exists(destDir)){
    result<- file.copy(inFile$datapath,file.path(destDir,inFile$name))
  } else {
    result<- FALSE
  }
  result
})