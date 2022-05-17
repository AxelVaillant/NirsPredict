library(shiny)
library(RPostgreSQL)
library(RPostgres)
library(ggplot2)
library(FactoMineR)
library(factoextra)
library(ade4)
library(auth0)
library(sendmailR)
options(shiny.port = 8080)
auth0::auth0_server(function(input,output,session ){
  
  values <- reactiveValues(
    auth0_user_data = NULL #Cntain auth0 user data
  )
  #----Logout-------
  observeEvent( input$logout,{
    logout()
  })
  
  # ------ Auth0: Fetch a token ------------------------------------------------
  response <- httr::POST(
    url = paste0("https://nirsdb.eu.auth0.com/oauth/token"),
    body = paste0(
      '{"client_id":"', "W6zILNzpDYWDFI37ca6Bqvx5l5Wa8v0t", 
      '","client_secret":"',"zTX_vlx6DMJv_YDcKHoyJma9gqnuWanZPe_JWXfZ3CddDcVzIQELe3wftSrFrxdH", 
      '","audience":"https://nirsdb.eu.auth0.com/api/v2/"', 
      ',"grant_type":"client_credentials"}'
    ),
    httr::add_headers(
      `content-type` = "application/json"
    ),
    encode = "raw"
  )
  #if (response$status_code != 200) {
   # timestamp <- as.integer(Sys.time())
    #filename <- paste0(timestamp, ".RData")
    #save.image(file = filename)
  #}
  token <- jsonlite::fromJSON(rawToChar(response$content))
  
  # ------ Check account approval ----------------------------------------------
  observe({
    req(session$userData$auth0_info)
    #token<-session$userData$auth0_credentials
    request_user <- httr::GET(
      url = paste0(
        "https://nirsdb.eu.auth0.com/api/v2/users/", 
        session$userData$auth0_info$sub
      ),
      httr::add_headers(
        Authorization = paste(token$token_type, token$access_token)
      )
    )
    user_data <- jsonlite::fromJSON(rawToChar(request_user$content))
    values$auth0_user_data <- user_data
  })
    
    
  
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
    con <- dbConnect(RPostgres::Postgres(), dbname = "postgres", host="localhost",port="5432",user="postgres",password="Sonysilex915@")
    
    ######################INPUT UPDATE###########################
    observe({
      listParams <- list("exp_location","idexp","main_contributor","conditionexp","genetic_group","genotype","leaf_stage",
                         "measurement","plant_stage","treatment")
      for( i in listParams){
        query <- paste("SELECT DISTINCT ",i," FROM individual WHERE ",i," IS NOT NULL ORDER BY ",i)
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
    con <- dbConnect(RPostgres::Postgres(), dbname = "postgres", host="localhost",port="5432",user="postgres",password="Sonysilex915@")
    
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
    filterListFinal<-NULL
    startDate<-input$date[1]
    endDate<-input$date[2]
    dateParam<-paste(" dateexp BETWEEN '",startDate,"' and '",endDate,"'",sep="")
    filterListFinal<-paste(c(filterList1,dateParam),collapse=" and ")
    
    situation<-NULL
    leafattach<-NULL
    if((!input$situation == "Both") || (!input$leafAttach == "Both")){
    if(!input$situation=="Both"){
      situation<-paste(" indout = '",input$situation,"'",sep="")
    }
    if(!input$leafAttach=="Both"){
      leafattach<-paste(" leaf_status = '",input$leafAttach,"'",sep="")
    }
      if(is.null(situation)){
        filterListFinal<-paste(c(filterListFinal,leafattach),collapse=" and ")
      }
      if(is.null(leafattach)){
        filterListFinal<-paste(c(filterListFinal,situation),collapse=" and ")
      }
      if((!is.null(situation)) && (!is.null(leafattach))){
        filterListFinal<-paste(c(filterListFinal,situation,leafattach),collapse=" and ")
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
    
    if(filterList1==""){
      #SpectrumOnlyQuery
      spectrumOnlyQuery<- paste("SELECT DISTINCT ",spectrumselect," FROM SPECTRUM JOIN INDIVIDUAL ON spectrum.individual_id = individual.id  ",
                                filterListFinal," ORDER BY individual_id" )
      #ParametersOnlyQuery
      ParametersOnlyQuery<- paste("SELECT DISTINCT",parametersSelect," FROM SPECTRUM JOIN INDIVIDUAL ON spectrum.individual_id = individual.id  ",
                                  filterListFinal,"ORDER BY individual_id" )
    } else {
      #SpectrumOnlyQuery
      spectrumOnlyQuery<- paste("SELECT DISTINCT ",spectrumselect," FROM SPECTRUM JOIN INDIVIDUAL ON spectrum.individual_id = individual.id WHERE ",
                                filterListFinal," ORDER BY individual_id" )
      #ParametersOnlyQuery
      ParametersOnlyQuery<- paste("SELECT DISTINCT",parametersSelect," FROM SPECTRUM JOIN INDIVIDUAL ON spectrum.individual_id = individual.id WHERE ",
                                  filterListFinal,"ORDER BY individual_id" )
    }
    newtab= data.frame()
    ###########WRITING CSV OUTPUT#######################
    if(input$outputformat=="All Data"){
      res <- dbGetQuery(conn = con,statement = spectrumOnlyQuery)
      paramsOnlyRes <- dbGetQuery(conn = con,statement = ParametersOnlyQuery)
      newtab <-FormatData(res)
      write.table(newtab,file="selectedSpectrums.csv",sep=";",row.names = FALSE)
      write.table(paramsOnlyRes,file="paramsOnlyRes.csv",sep = ";",row.names = FALSE)
      allDataRes <- cbind(paramsOnlyRes[2:97],newtab[2:2152])
      output$resText<-renderText({
        if(is.na(res[1,1])){
          return("Your filters don't match any spectrums in the database.")
        } else{return(paste("Your filters matches ",nrow(newtab)," of 5325 spectra",sep = ""))}
      })
      write.table(allDataRes,file="allDataRes.csv",sep = ";",row.names = FALSE)
    }
    if(input$outputformat=="Spectrum only"){
      res <- dbGetQuery(conn = con,statement = spectrumOnlyQuery)
      newtab <-FormatData(res)
      output$resText<-renderText({
        if(is.na(res[1,1])){
          return("Your filters don't match any spectrums in the database.")
        } else{return(paste("Your filters match ",nrow(newtab)," of 5325 spectra",sep = ""))}
      })
      write.table(newtab,file="selectedSpectrums.csv",sep=";",row.names = FALSE)
    }
    if(input$outputformat=="Phenotypic traits only"){
      paramsOnlyRes <- dbGetQuery(conn = con,statement = ParametersOnlyQuery)
      output$resText<-renderText({
        if(is.na(paramsOnlyRes[1,1])){
          return("Your filters don't match any spectrums in the database.")
        } else{return(paste("Your filters matches ",nrow(newtab)," of 5325 spectra",sep = ""))}
      })
      write.table(paramsOnlyRes,file="paramsOnlyRes.csv",sep = ";",row.names = FALSE)
    }
    
    #write.table(res,file="rawRes.csv",sep = ";",row.names = FALSE)
    print('Traitement terminé')
    dbDisconnect(con)
  })
   ##############FORMATING RAWDATA TO USER READABLE DATA#############################
  #####BASIC LOOP#####
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
  
  ###########UNIVARIATE PLOT METHOD########
  plotMean<-function(newtab){
    allSpectrum<-read.table(file = "allSpectrum.csv",header = TRUE,sep = ";")
    newtab<-read.table(file = "selectedSpectrums.csv",header = TRUE,sep = ";")
    selecSpectrum <- as.data.frame(matrix(double(),ncol = 2))
    for(i in 350:2500){
      val<-paste('x',i,sep = "")
      absSelec<-newtab[,val]
      rowsel<-as.data.frame(cbind(mean(as.numeric(absSelec)),i))
      selecSpectrum<- rbind(selecSpectrum, rowsel)
    }
    
    absSelec<-selecSpectrum[,1]
    absAll<-allSpectrum[,1]
    c=data.frame(absSelec)
    for(j in 1:length(absAll)){
      c<-rbind(c,absAll[j])
    }
    wavelength=c(350:2500)
    c<-cbind(c,wavelength)
    name<-as.factor(rep(c("Individual","All"),each=2151))
    rdyToPlot<-data.frame(c,name)
    write.table(rdyToPlot,file="rdyToPlot.csv",sep=";",row.names = FALSE)
    #####PLOT######
    plot(rdyToPlot[rdyToPlot$name=="Individual","wavelength"], rdyToPlot[rdyToPlot$name=="Individual","absSelec"], col="firebrick3", type="l", lwd=3, ylim=c(0,1.2),
         main="Mean comparison",xlab="Wavelength",ylab="Absorption")
    points(rdyToPlot[rdyToPlot$name=="All","wavelength"], rdyToPlot[rdyToPlot$name=="All","absSelec"], col="dodgerblue3", type="l", lwd=3,lty=3)
    legend(1700,0.85,legend = c("Selected spectrums","All spectrums"),col =c("firebrick3","dodgerblue3"),lty=1:2,cex = 0.8)
  }

  observeEvent(input$submit, {
  ######UNIVARIATE GRAPHICAL OUTPUT#############
    # mean plot
    output$MeanPlot <- renderPlot({
      plotMean(newtab)
    })
    
  ######MULTIVARIATE GRAPHICAL OUTPUT#############
    # selected pca plot
    output$selectedPCAPlot <- renderPlot({
      newtab<-read.table(file = "selectedSpectrums.csv",header = TRUE,sep = ";")
      params<-read.table(file="paramsOnlyRes.csv",header=TRUE,sep=";")
      SelectedDataPca<-PCA(newtab[,2:2152],scale.unit = TRUE,ncp=5,graph=TRUE)
      fviz_pca_ind(SelectedDataPca,gemo.ind="point",label="none",col.ind = "green",addEllipses = TRUE,legend.title="Groups")+scale_shape_manual(values=c(0,1,2,3,4,5,6,7,9,10))+xlim(-100, 300)+ggtitle("Selected spectra PCA")
      
    })
  })
  
  
  ###OPTIONS
  options(encoding = "UTF-8")
  #output$spectrum <- renderTable(input$upload)
  options(shiny.maxRequestSize=1000*1024^2)
  
  ###UPLOAD HANDLING
  destDir<-'/home/vaillant/Documents/Projets R/RShinyNirsDB/uploads'
  output$spectrum <- renderPrint({
    req(input$spectrumfile)
    inFile <- input$spectrumfile
    if(is.null(inFile)){
      return(FALSE)
    } else {
    }
    if(dir.exists(destDir)){
      result<- file.copy(inFile$datapath,file.path(destDir,inFile$name))
    } else {
      result<- FALSE
    }
    result
  })
  #####DOWNLOAD HANDLING
  output$DlSpectrum <- downloadHandler(
    filename = function() {
           paste('Predictions-', Sys.Date(), '.csv', sep='')
         },
         content = function(con) {
           write.csv(data, con)
         }
  )

  
  ######LAUNCH RUN########
  observeEvent(input$runAnalysis, {
    
  #observeEvent(input$runAnalysis, {
  #email_user<- values$auth0_user_data$email
  #  system(paste("Rscript --vanilla run.R",email_user),wait = FALSE)
  #})
 
  ######EMAIL#############
  Server<-list(smtpServer<-"in-v3.mailjet.com")
  from<- "axel.vaillant@cefe.cnrs.fr"
  #to<- "axel.vaillant@yahoo.fr"
  #to <- session$userData$auth0_info$name
  
  to<-values$auth0_user_data$email
  
  system(
    paste("Rscript --vanilla run.R",to) , wait = FALSE
  )
  
  Subject = paste0("[NirsDB] Le calcul des prédictions de votre spectre a démarré.")
  TextPart = paste0(
    'Bonjour,\n\n',
    'L’outil NirsDB a démarré le calcul de prédictions lié à votre spectre. ',
    'Vous recevrez un email sous 24-48 heures avec les résultats.\n\n',
    'Cet email est automatisé. Merci de ne pas y répondre.'
  )
  #sendmail(from,to,Subject,TextPart,control=Server)
  })


  #######OUTPUTS##########
  # all pca plot
  output$allPCAPlot <- renderPlot({
    newtabAll<-read.table(file="newtabAll.csv",header=TRUE,sep=";")
    phenAll<-read.table(file="phenAll.csv",header=TRUE,sep=";")
    spectre<-newtabAll[,2:2152]
    #AllDataPca<-dudi.pca(spectre,center=T,scale=T,nf=5,scannf=FALSE)
    #fviz_pca_ind(AllDataPca,gemo.ind="point",label="none",col.ind = "grey",addEllipses = TRUE,legend.title="Groups")+scale_shape_manual(values=c(0,1,2,3,4,5,6,7,9,9))+ylim(-100,100 )+ggtitle("All spectra PCA")
  })

  
  #access to the app from the homepage link
  observeEvent(input$app, updateTabsetPanel(session = session, inputId = "tabset", selected = "app"))
  
}, info = auth0_info)