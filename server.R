library(shiny)
library(RPostgreSQL)
library(RPostgres)
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
    con <- dbConnect(RPostgres::Postgres(), dbname = "nirsDB", host="localhost",port="5432",user="postgres",password="Sonysilex915@")
    
    ######################INPUT UPDATE###########################
    observe({
      listParams <- list("exp_location","idexp","main_contributor","conditionexp","genetic_group","genotype","leaf_stage",
                         "measurement","plant_stage","treatment")
      for( i in listParams){
        query <- paste("SELECT DISTINCT ",i," FROM individual")
        assign(paste("SqlOutput",i,sep=""),dbGetQuery(con, query))
      }
        
      updatePickerInput(session, "location", choices = SqlOutputexp_location)
      updatePickerInput(session, "exp", choices = SqlOutputidexp)
      updatePickerInput(session, "contributor", choices = SqlOutputmain_contributor)
      updatePickerInput(session, "genetic_group", choices = SqlOutputgenetic_group)
      updatePickerInput(session, "genotype", choices = SqlOutputgenotype)
      updatePickerInput(session, "condition", choices = SqlOutputconditionexp)
      updatePickerInput(session, "leaf_stage", choices = SqlOutputleaf_stage)
      updatePickerInput(session, "plant_stage", choices = SqlOutputplant_stage)
      updatePickerInput(session, "measurement", choices = SqlOutputmeasurement)
      updatePickerInput(session, "treatment", choices = SqlOutputtreatment)
      
      dbDisconnect(con)
    })
  }
  getData()
  
  ################FORM MANAGER####################
  observeEvent(input$submit, {
    # Connect to the database
    con <- dbConnect(RPostgres::Postgres(), dbname = "nirsDB", host="localhost",port="5432",user="postgres",password="Sonysilex915@")
    
    inputList<-list(input$location,input$exp,input$contributor,input$genotype,input$genetic_group,
                    input$condition,input$leaf_stage,input$plant_stage,input$measurement,input$treatment,
                    input$CSR,input$sugar,input$glucosinolates,input$secondary_metabolites)
    inputNameList<- list("exp_location","idexp","main_contributor","genotype","genetic_group","conditionexp",
                         "leaf_stage","plant_stage","measurement","treatment","CSR","sugar",
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
          paramList<-paste(paramList,"'",inputList[[i]][j],"'",",",sep ="")
        }
        paramList<-substr(paramList,1,nchar(paramList)-1)
        filterList1<-c(filterList1, assign(paste("Query",i,sep=""),paste("individual.",inputNameList[i] ," IN ","(",paramList,")",sep="")))
        
      }
    }
    
    filterList<-sapply(filterList,paste,collapse=", ")
    filterList<- paste(filterList, collapse = " and ")
    filterList<-substr(filterList,5,nchar(filterList))
    
    filterList1<- paste(filterList1, collapse = " and ")
    filterList1<-substr(filterList1,5,nchar(filterList1))
    
    ######PARTICULAR CASE#####################
    situation<-NULL
    leafattach<-NULL
    filterListFinal<-NULL
    if((!input$situation == "Both") || (!input$leafAttach == "Both")){
    if(!input$situation=="Both"){
      situation<-paste(" indout = '",input$situation,"'",sep="")
    }
    if(!input$leafAttach=="Both"){
      leafattach<-paste(" leaf_status = '",input$leafAttach,"'",sep="")
    }
      if(is.null(situation)){
        filterListFinal<-paste(c(filterList1,leafattach),collapse=" and ")
      }
      if(is.null(leafattach)){
        filterListFinal<-paste(c(filterList1,situation),collapse=" and ")
      }
      if((!is.null(situation)) && (!is.null(leafattach))){
        filterListFinal<-paste(c(filterList1,situation,leafattach),collapse=" and ")
      }
    }
    if(filterList1 == ""){
      filterListFinal<-substr(filterListFinal,5,nchar(filterListFinal))
    }
    ######SELECT CONDITION###########"
    spectrumselect<-"individual_id,wavelength_id,absorption,identification"
    parametersSelect <-"individual_id,identification,idexp,main_contributor,indout,exp_location,conditionexp,treatment,genotype,genetic_group,plant_stage,
    leaf_stage,type_sample,measurement,leaf_status,dateexp,CSR_C , CSR_S , CSR_R ,plant_lifespan int,SLA , 
    LDMC , delta13C , delta15N , LCC , thickness , plant_growth_rate , RWC , LNC , SA , JA , IAA , ABA , CMLX , 
    glucose , sucrose , fructose , arabinose , cellobiose , fucose , galactose , inositol , isomaltose , maltose , mannose_xylose ,
    melezitose , melbiose , palatinose , raffinose , rhamnose , ribose , trehalose , xylose , glucoalysiin , glucorassicin , glucoerucin , 
    gluconapin , gluconasturtiin , glucoraphanin , glucoraphenin , epigallocatechin , progoitrin , epiprogoitrin , isobutyl , glucosinalbin ,
    sinigrin , hexyl , butyl , neoglucobrassicinPeak1 , neoglucobrasssicinPeak2 , X3MTP , X5MTP , X6MSH , X7MSH , X7MTH , X8MSO ,
    X8MTO ,apigeninrutinoside , caffeicacid , chlorogenicacid , citrat , cyanidinRhamnoside , CyanidinSophorosidGlucoside , dihydroCaffeoylGlucuronide , 
    Fumarat , KaempherolGlucosylRhamnosylGlucoside , KaempherolRutinoside , KaempherolXylosylRhamnoside , malat , mcourmaricacid , pcoumaricacid ,
    pelargonidincumaroyldiglucoside , pelargonidinsambubioside , prenylnaringenin , quercetinglucoside , succinat,maltose"
    
    #SpectrumOnlyQuery
    spectrumOnlyQuery<- paste("SELECT DISTINCT ",spectrumselect," FROM SPECTRUM JOIN INDIVIDUAL ON spectrum.individual_id = individual.id WHERE ",
                   filterList1," ORDER BY individual_id" )
    #ParametersOnlyQuery
    ParametersOnlyQuery<- paste("SELECT DISTINCT",parametersSelect," FROM SPECTRUM JOIN INDIVIDUAL ON spectrum.individual_id = individual.id WHERE ",
                              filterList1,"ORDER BY individual_id" )
    
    ###########WRITING CSV OUTPUT#######################
    if(input$outputformat=="All Data"){
      res <- dbGetQuery(conn = con,statement = spectrumOnlyQuery)
      paramsOnlyRes <- dbGetQuery(conn = con,statement = ParametersOnlyQuery)
      newtab <-FormatData(res)
      write.table(newtab,file="selectedSpectrums.csv",sep=";",row.names = FALSE)
      write.table(paramsOnlyRes,file="paramsOnlyRes.csv",sep = ";",row.names = FALSE)
      allDataRes <- cbind(paramsOnlyRes[2:97],newtab[2:2152])
      write.table(allDataRes,file="allDataRes.csv",sep = ";",row.names = FALSE)
    }
    if(input$outputformat=="Spectrum only"){
      res <- dbGetQuery(conn = con,statement = spectrumOnlyQuery)
      newtab <-FormatData(res)
      write.table(newtab,file="selectedSpectrums.csv",sep=";",row.names = FALSE)
    }
    if(input$outputformat=="Phenotypic traits only"){
      paramsOnlyRes <- dbGetQuery(conn = con,statement = ParametersOnlyQuery)
      write.table(paramsOnlyRes,file="paramsOnlyRes.csv",sep = ";",row.names = FALSE)
    }
    
    #write.table(res,file="rawRes.csv",sep = ";",row.names = FALSE)
    print('Traitement terminé')
    dbDisconnect(con)
  })
   ##############FORMATING RAWDATA TO USER READABLE DATA#############################
   FormatData<- function(res){
     newtab=data.frame()
     newtab <- as.data.frame(matrix(double(),ncol = 2152))
     names(newtab)[2:2152] <- paste0('x', 350:2500)
     colnames(newtab)[1] <- "Id"
     
     start<-1
     end<-2151
     for(i in 1:(nrow(res)/2151)){
       id<-res[start,4]
       sub<-t(res[start:end,3])
       sub2<-as.data.frame(cbind(id,sub))
       names(sub2)[2:2152] <- paste0('x', 350:2500)
       colnames(sub2)[1] <- "Id"
       start<-start+2151
       end<-end+2151
       newtab<- rbind(newtab, sub2)
       print(i)
     }
     return (newtab)
   }
  
  ######UNIVARIATE GRAPHICAL OUTPUT#############
  normSpectrum=data.frame()
  normSpectrum <- as.data.frame(matrix(double(),ncol = 2))
  for(i in 350:2500){
    val<-paste('x',i,sep = "")
    abs<-newtab[,val]
    row<-as.data.frame(cbind(mean(as.numeric(abs)),i))
    normSpectrum<- rbind(normSpectrum, row)
  }
  colnames(normSpectrum)[1] <- "Abs"
  colnames(normSpectrum)[2] <- "Wavelength"
  
  graph1<-normSpectrum[,c("Abs","Wavelength")]
  plot(x = normSpectrum$Wavelength, y = normSpectrum$Abs,
       xlab = "Wavelength",
       ylab = "Asorption",
       xlim = c(350, 2500),
       ylim = c(0, 1.25),         
       main = "Wavelength vs Absorption",col=3
  )
  
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
  
  
  #######OUTPUTS##########
  
  
  #access to the app from the homepage link
  observeEvent(input$app, updateTabsetPanel(session = session, inputId = "tabset", selected = "app"))
  
}