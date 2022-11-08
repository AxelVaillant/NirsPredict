plan(multisession)
function(input,output,session ){
    #-------Create unique temporary repository---------------------#
    system(paste("mkdir ",session$token,sep = ""))
    #-------Get credentials----------------------#
    credentials<-read.table(file = "csv/credentials.csv",header = TRUE,sep = ";")
    ipGpu<-credentials$ipGpu
    passwordGpu<-credentials$passwordGpu
    dbHost<-credentials$dbHost
    dbPort<-credentials$dbPort
    dbUser<-credentials$dbUser
    dbPassword<-credentials$dbPassword
    
    #################################################
    ############  DATABASE MANAGEMENT ###############
    #################################################

    ###################### CONSULTATION FILTERS UPDATE #########################
    observe({
      tryCatch({
      # Connect to the database
      con <- dbConnect(RPostgres::Postgres(), dbname = "NirsDB", host=dbHost, port=dbPort, user=dbUser,password=dbPassword)
      
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
      },error=function(err){showNotification("Database connexion error",type="error")
        return(NA)})
    })
    
    ###################### DATABASE QUERY MANAGEMENT ###########################
  dbManagement <- function(){
    inputList<-list(input$location,input$exp,input$contributor,input$genotype,input$genetic_group,
                    input$condition,input$leaf_stage,input$plant_stage,input$measurement,input$treatment,
                    input$CSR,input$sugar,input$glucosinolates,input$secondary_metabolites)
    inputNameList<- list("exp_location","idexp","main_contributor","genotype","genetic_group","conditionexp",
                         "leaf_stage","plant_stage","measurement","treatment","CSR","sugar",
                         "glucosinolates","secondary_metabolites")
    selectquery<-""
    filterList<-""
    #-Assign each input a corresponding variable
    for(i in 1:length(inputList)){
      if(!is.null(inputList[i][[1]])){
        filterList<-c(filterList,inputNameList[i])
        selectquery<- paste(selectquery,inputNameList[i],",")
        assign(paste("whereQuery",inputNameList[i],sep=""),inputList[i])
        
      }
    }
    selectquery<-substr(selectquery,1,nchar(selectquery)-2)
    ##################### QUERY BUILDING ##############################
    ############ "WHERE" QUERY BUILDING #############
    filterList1<-""
    for(i in 1:10){
      paramList<-""
      if(!is.null(inputList[i][[1]])){
        for(j in 1:length(inputList[[i]])){
          paramList<-paste(paramList,"'",inputList[[i]][j],"'",",",sep ="")
        }
        paramList<-substr(paramList,1,nchar(paramList)-1)
        filterList1<-c(filterList1, assign(paste("Query",i,sep=""),paste("individual.",inputNameList[i] ," IN ","(",paramList,")",sep="")))
        
      }
    }
    #-CONCATENATE AND SUBSTRING TO CONSTRUCT CORRECT QUERY-#
    filterList<-sapply(filterList,paste,collapse=", ")
    filterList<- paste(filterList, collapse = " and ")
    filterList<-substr(filterList,5,nchar(filterList))
    
    filterList1<- paste(filterList1, collapse = " and ")
    filterList1<-substr(filterList1,5,nchar(filterList1))
    
    ####### SPECIALS CASES #####################
    filterListFinal<-NULL
    #-DATE-#
    startDate<-input$date[1]
    endDate<-input$date[2]
    dateParam<-paste(" dateexp BETWEEN '",startDate,"' and '",endDate,"'",sep="")
    filterListFinal<-paste(c(filterList1,dateParam),collapse=" and ")
    #-SITUATION/LEAFATTACHEMENT/NATURAL_ACCESSIOONS-#
    situation<-NULL
    leafattach<-NULL
    nataccessions<-NULL
    if((!input$situation == "Both") || (!input$leafAttach == "Both") || (!input$nataccessions == "Included")){
      if(!input$situation=="Both"){
        situation<-paste(" indout = '",input$situation,"'",sep="")
      }
      if(!input$leafAttach=="Both"){
        leafattach<-paste(" leaf_status = '",input$leafAttach,"'",sep="")
      }
      if(input$nataccessions == "Only"){
        nataccessions<-paste(" genotype IS NULL",sep="")
      }
      if(input$nataccessions == "Excluded"){
        nataccessions<-paste(" genotype !=''",sep="")
      }
      if(!is.null(situation) && is.null(leafattach) && is.null(nataccessions)){
        filterListFinal<-paste(c(filterListFinal,situation),collapse=" and ")
      }
      if(!is.null(leafattach) && is.null(situation) && is.null(nataccessions)){
        filterListFinal<-paste(c(filterListFinal,leafattach),collapse=" and ")
      }
      if(!is.null(nataccessions) && is.null(situation) && is.null(leafattach)){
        filterListFinal<-paste(c(filterListFinal,nataccessions),collapse=" and ")
      }
      if((!is.null(situation)) && (!is.null(leafattach))){
        filterListFinal<-paste(c(filterListFinal,situation,leafattach),collapse=" and ")
      }
      if((!is.null(situation)) && (!is.null(nataccessions))){
        filterListFinal<-paste(c(filterListFinal,situation,nataccessions),collapse=" and ")
      }
      if((!is.null(leafattach)) && (!is.null(nataccessions))){
        filterListFinal<-paste(c(filterListFinal,leafattach,nataccessions),collapse=" and ")
      }
    }
    
    #-CONCATENATE AND SUBSTRING TO CONSTRUCT CORRECT QUERY-#
        if(filterList1 == ""){
      filterListFinal<-substr(filterListFinal,5,nchar(filterListFinal))
      filterListFinal<-paste("WHERE",filterListFinal)
    }
    
    ############ PARAMETERS FILTER (PHENOTYPIC TRAITS ONLY CASE) ###############
    basicParameters<-paste("individual_id,identification,idexp,main_contributor,indout,exp_location,conditionexp,treatment,genotype,genetic_group,plant_stage,",
                           "leaf_stage,type_sample,measurement,leaf_status,dateexp,plant_lifespan ,SLA,", 
                           "LDMC , delta13C , delta15N , LCC , thickness , plant_growth_rate , RWC , LNC , SA , JA , IAA , ABA , CMLX")
    otherFilterList<-""
    otherFilterFinal<-NULL
    for(i in 11:14){
      otherFilterList<-""
      if(!is.null(inputList[i][[1]])){
        for(j in 1:length(inputList[[i]])){
          otherFilterList<-paste(otherFilterList,inputList[[i]][j],",",sep ="")
        }
        
        otherFilterFinal<-paste(otherFilterFinal,otherFilterList,sep = "")
      }
    }
     #-CUSTOM SELECTION CASE-#
    if(!is.null(otherFilterFinal)){
      otherFilterFinal<-substr(otherFilterFinal,1,nchar(otherFilterFinal)-1)
      customSelect<-paste(basicParameters,",",otherFilterFinal,sep = "")
    } else {
      customSelect<-basicParameters
    }
    
    ############## SELECT CONDITION  ###############"
    spectrumselect<-"individual_id,wavelength_id,absorption,identification"
    parametersSelect <-paste("individual_id,identification,idexp,main_contributor,indout,exp_location,conditionexp,treatment,genotype,genetic_group,plant_stage,",
                             "leaf_stage,type_sample,measurement,leaf_status,dateexp,CSR_C , CSR_S , CSR_R ,plant_lifespan,SLA,", 
                             "LDMC , delta13C , delta15N , LCC , thickness , plant_growth_rate , RWC , LNC , SA , JA , IAA , ABA , CMLX , ",
                             "glucose , sucrose , fructose , arabinose , cellobiose , fucose , galactose , inositol , isomaltose , maltose , mannose_xylose ,",
                             "melezitose , melbiose , palatinose , raffinose , rhamnose , ribose , trehalose , xylose , glucoalysiin , glucobrassicin , glucoerucin , ",
                             "gluconapin , gluconasturtiin , glucoraphanin , glucoraphenin , epigallocatechin , progoitrin , epiprogoitrin , isobutyl , glucosinalbin ,",
                             "sinigrin , hexyl , butyl , neoglucobrassicinPeak1 , neoglucobrassicinPeak2 , X3MTP , X5MTP , X6MSH , X7MSH , X7MTH , X8MSO ,",
                             "X8MTO ,apigeninrutinoside , caffeicacid , chlorogenicacid , citrat , cyanidinRhamnoside , CyanidinSophorosidGlucoside , dihydroCaffeoylGlucuronide , ",
                             "Fumarat , KaempherolGlucosylRhamnosylGlucoside , KaempherolRutinoside , KaempherolXylosylRhamnoside , malat , mcoumaricacid , pcoumaricacid ,",
                             "pelargonidincumaroyldiglucoside , pelargonidinsambubioside , prenylnaringenin , quercetinglucoside , succinat,maltose",sep = "")
    
    if(filterList1==""){
      #SpectrumOnlyQuery
      spectrumOnlyQuery<- paste("SELECT DISTINCT ",spectrumselect," FROM SPECTRUM JOIN INDIVIDUAL ON spectrum.individual_id = individual.id  ",
                                filterListFinal," ORDER BY individual_id" )
      #ParametersOnlyQuery
      ParametersOnlyQuery<- paste("SELECT DISTINCT",parametersSelect," FROM SPECTRUM JOIN INDIVIDUAL ON spectrum.individual_id = individual.id  ",
                                  filterListFinal,"ORDER BY individual_id" )
      #CustomParametersQuery
      CustomQuery<-paste("SELECT DISTINCT",customSelect," FROM SPECTRUM JOIN INDIVIDUAL ON spectrum.individual_id = individual.id  ",
                         filterListFinal,"ORDER BY individual_id" )
      Queries <- list(spectrumOnlyQuery,ParametersOnlyQuery,CustomQuery)
    } else {
      #SpectrumOnlyQuery
      spectrumOnlyQuery<- paste("SELECT DISTINCT ",spectrumselect," FROM SPECTRUM JOIN INDIVIDUAL ON spectrum.individual_id = individual.id WHERE ",
                                filterListFinal," ORDER BY individual_id" )
      #ParametersOnlyQuery
      ParametersOnlyQuery<- paste("SELECT DISTINCT",parametersSelect," FROM SPECTRUM JOIN INDIVIDUAL ON spectrum.individual_id = individual.id WHERE ",
                                  filterListFinal,"ORDER BY individual_id" )
      #CustomParametersQuery
      CustomQuery<-paste("SELECT DISTINCT",customSelect," FROM SPECTRUM JOIN INDIVIDUAL ON spectrum.individual_id = individual.id WHERE ",
                         filterListFinal,"ORDER BY individual_id" )
      Queries <- list(spectrumOnlyQuery,ParametersOnlyQuery,CustomQuery)
    }
    return(Queries)
  }
  ##############################################################################
  ###################### FORM MANAGER - CONSULT DATABASE #######################
  observeEvent(input$submit, {
    tryCatch({
    progress <- AsyncProgress$new(message="Filtering in progress")
      shinyjs::show("plotsOutput")
      # Connect to the database
      con <- dbConnect(RPostgres::Postgres(), dbname = "NirsDB", host=dbHost, port=dbPort, user=dbUser, password=dbPassword)
      #------Get Queries----------------------#
      queries<-dbManagement()
      spectrumOnlyQuery <-queries[[1]]
      ParametersOnlyQuery <-queries[[2]]
      CustomQuery <-queries[[3]]
      #-----------Get spectrum res-----------------#
      res <- dbGetQuery(conn = con,statement = spectrumOnlyQuery)
      dbDisconnect(con)
      #--------Asynchronous way to filter and plot the results-------#
      future({
        return(FormatData(res))
        progress$close()
        }) %...>% (function(newtab){
          progress$close()
          outputManagement(spectrumOnlyQuery,ParametersOnlyQuery,CustomQuery,newtab,res)
          meanPlot(newtab)
          pcaSelectedPlot()
        })%...!% ( function(error){
          warning(error)
        })
      print('Traitement terminé')
      }, error = function(err){
        shinyalert("Error", "Database consultation error\n Try again or contact us if the error persist",type="error")
        return(NA)
      })
  })
    ################### WRITING CSV OUTPUT ####################################
  outputManagement<- function(spectrumOnlyQuery,ParametersOnlyQuery,CustomQuery,newtab,res){
    withProgress(message='Plot management ouput',value=0,{
      #-Database connection-#
      con <- dbConnect(RPostgres::Postgres(), dbname = "NirsDB", host=dbHost, port=dbPort, user=dbUser, password=dbPassword)
    incProgress(1/4, detail = paste("in progress"))
    paramsOnlyRes <- dbGetQuery(conn = con,statement = ParametersOnlyQuery)
    write.table(paramsOnlyRes,file=paste(session$token,"/paramsOnlyRes.csv",sep=""),sep = ";",row.names = FALSE)
    write.table(newtab,file=paste(session$token,"/selectedSpectrums.csv",sep =""),sep=";",row.names = FALSE)
    #----------All Data Output----------------
    if(input$outputformat=="All Data"){
      if(is.na(res[1,1])){
        output$resText<-renderText({
          HTML(paste("Your filters don't match any spectrums in the database.",
                     "Please retry with less specific criteria", sep="\n "))
        })
      } else {
        allDataRes <- cbind(paramsOnlyRes[2:97],newtab[2:2152])
        output$resText<-renderText({
          return(paste("Your filters matches ",nrow(newtab)," of 5325 spectra",sep = ""))
        })
        write.table(allDataRes,file=paste(session$token,"/allDataRes.csv",sep=""),sep = ";",row.names = FALSE)
        shinyjs::show("DlConsult")
        uploadData("allDataRes")
      }
    }
    #----------------Spectrum Only Output----------
    if(input$outputformat=="Spectrum only"){
      if(is.na(res[1,1])){
        output$resText<-renderText({
          HTML(paste("Your filters don't match any spectrums in the database.",
                     "Please retry with less specific criteria", sep="\n "))
        })
      } else {
        output$resText<-renderText({
          return(paste("Your filters matches ",nrow(newtab)," of 5325 spectra",sep = ""))
        })
      }
      shinyjs::show("DlConsult")
      uploadData("selectedSpectrums")
    }
    #---------All Phenotypic traits only Output-------
    if(input$outputformat=="Phenotypic traits only"){
      if(is.na(paramsOnlyRes[1,1])){
        output$resText<-renderText({
          HTML(paste("Your filters don't match any spectrums in the database.",
                     "Please retry with less specific criteria", sep="\n "))
        })
      } else {
        output$resText<-renderText({
          return(paste("Your filters matches ",nrow(paramsOnlyRes)," of 5325 spectra",sep = ""))
        })
      }
      shinyjs::show("DlConsult")
      uploadData("paramsOnlyRes")
    }
    
    #------------Custom Ouput-----------
    if(input$outputformat=="Custom"){
      customRes <- dbGetQuery(conn = con,statement = CustomQuery)
      if(is.na(customRes[1,1])){
        output$resText<-renderText({
          HTML(paste("Your filters don't match any spectrums in the database.",
                     "Please retry with less specific criteria", sep="\n "))
        })
      } else {
        output$resText<-renderText({
          return(paste("Your filters matches ",nrow(customRes)," of 5325 spectra",sep = ""))
        })
      }
      write.table(customRes,file=paste(session$token,"/customRes.csv",sep=""),sep = ";",row.names = FALSE)
      shinyjs::show("DlConsult")
      uploadData("customRes")
    }
      dbDisconnect(con)
    })
    }
  ##############################################################################
  ######### FORMATING QUERY RESULT'S RAWDATA TO USER READABLE DATA #############
  ##############################################################################
  ##### BASIC LOOP #####
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
      progress$inc(1/(nrow(res)/2151))
    }
    return(newtab)
  }
  
  ############## UNIVARIATE MEAN PLOT METHOD #################
  plotMean<-function(newtab){
    allSpectrum<-read.table(file = "csv/allSpectrum.csv",header = TRUE,sep = ";")
    if(!is.na(newtab[1,1])){
      selecSpectrum <- as.data.frame(matrix(double(),ncol = 2))
      #-Building usable dataframe from result of user's query-#
      for(i in 350:2500){
        val<-paste('x',i,sep = "")
        absSelec<-newtab[,val]
        rowsel<-as.data.frame(cbind(mean(as.numeric(absSelec)),i))
        selecSpectrum<- rbind(selecSpectrum, rowsel)
        incProgress(1/2150, detail = paste("Doing part", i))
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
      
      #####PLOT######
      plot(rdyToPlot[rdyToPlot$name=="Individual","wavelength"], rdyToPlot[rdyToPlot$name=="Individual","absSelec"], col="firebrick3", type="l", lwd=3, ylim=c(0,1.2),
           main="Mean comparison",xlab="Wavelength",ylab="Absorption")
      points(rdyToPlot[rdyToPlot$name=="All","wavelength"], rdyToPlot[rdyToPlot$name=="All","absSelec"], col="dodgerblue3", type="l", lwd=3,lty=3)
      legend(1700,0.85,legend = c("Selected spectrums","All spectrums"),col =c("firebrick3","dodgerblue3"),lty=1:2,cex = 0.8)
    }
  }
    ############ UNIVARIATE GRAPHICAL OUTPUT ######################
  meanPlot<-function(newtab){
    # mean plot
    output$MeanPlot <- renderPlot({
      withProgress(message = 'Meanplot in progress', value=0, {
        plotMean(newtab)
        shinyjs::show('imgPCA')
      })
    })
  }
    #############################################################
    ############# MULTIVARIATE GRAPHICAL OUTPUT #################
    #####-PCA OF USER'S QUERY RESULTS-#########
  pcaSelectedPlot<- function(){
    withProgress(message = "Pca Plot in progress", value= 0,{
      incProgress(1/4)
    #-Selected pca plot-#
    output$selectedPCAPlot <- renderPlot({
      newtab<-read.table(file=paste(session$token,"/selectedSpectrums.csv",sep =""),header = TRUE,sep = ";")
      params<-read.table(file=paste(session$token,"/paramsOnlyRes.csv",sep=""),header=TRUE,sep=";")
      if(!is.na(newtab[1,1])){
        SelectedDataPca<-PCA(newtab[,2:2152],scale.unit = TRUE,ncp=5,graph=FALSE)
        fviz_pca_ind(SelectedDataPca,label="none",col.ind = "green",addEllipses = TRUE,legend.title="Groups")+scale_shape_manual(values=c(0,1,2,3,4,5,6,7,9,10))+xlim(-100, 300)+ggtitle("Selected spectra PCA")
      }
    })
    })
  }
    
  ######################################################
  ############## DENSITY GRAPHIC COMPARISON ############
  ######################################################  
  DensityComparison<- function(trait,mode){
    #-Database connection-#
    con <- dbConnect(RPostgres::Postgres(), dbname = "NirsDB", host=dbHost, port=dbPort, user=dbUser, password=dbPassword)
    #-Get all values of the selected trait from the database-#
    Query = paste("SELECT ",trait," FROM individual WHERE ",trait," IS NOT NULL",sep = "");
    res <- dbGetQuery(conn = con,statement = Query)
    dbDisconnect(con)
    #-Read predictions-#
    pred<-read.table(file=paste(session$token,"/Res/Pred_",toupper(trait),".csv",sep = ""),header=FALSE,sep=";")
    #-----#
    d<-density(res[,1])
    png(paste(session$token,"/Res/density_comparaison",trait,".png",sep = ""))
    #,xlab = paste(trait," value"
    plot(d ,main=paste("Density of Database ",trait," vs Predicted ",trait,sep=""))
    lines(density(pred[,1]), col="red")
    legend("topright", c("Database values","Predictions values"), col = c("black","red"), lty=1)
    dev.off()
  }
    
  ################ OPTIONS ###############
  options(encoding = "UTF-8")
  options(shiny.maxRequestSize=1000*1024^2)
  
  ############################################################
  #################### FILE UPLOAD CHECKING ##################
  globalUploadCheck<-function(inFile,traitFile,testSpectrumFile,testTraitFile,destDir){
    if(isTRUE(spectrumUploadCheck(inFile,destDir))){
      if(input$runMode == "Build your own model + Predictions"){
        if(isTRUE(traitUploadCheck(traitFile,destDir))){
          return(TRUE)
        }
      } else if (input$runMode == "Complete, Test dataset needed"){
        if(isTRUE(traitUploadCheck(testTraitFile,destDir)) && isTRUE(spectrumUploadCheck(testSpectrumFile,destDir))){
          return(TRUE)
        }
      } else {
        return(TRUE)
      }
    }
  }
  ###########-TRAIT FILE CHECKING-###########
  traitUploadCheck<-function(traitFile,destDir){
    traits <- c(input$functionalTraits,input$metabolites)
    possibleTraits <- c(listFunctionalTraits,listSecondaryMetabolites,listHormones)
    list=""
    for(i in 1:length(possibleTraits)){
      list<-paste(list,possibleTraits[i],sep=" ")
    }
    if(is.null(traitFile)){
      shinyalert("Input missing", "No trait file has been provided", type="error")
    } else {
      data<-read.table(traitFile$datapath,sep=";")
      if(any(is.na(data))){
        shinyalert("Error missing data", "There are missing data in your dataset",type="error")
        reset('traitsfile')
        runjs('Shiny.onInputChange("traitsfile", null)')
      } else {
        for (i in 1:(length(traits)-1)) {
          if(!(grepl(traits[i],list))){
            shinyalert("Error wrong header", "Headers of your file doesn't match traits name",type="error")
            reset('traitsfile')
            runjs('Shiny.onInputChange("traitsfile", null)')
          } else {
            if(dir.exists(destDir)){
              result<- file.copy(traitFile$datapath,file.path(destDir,traitFile$name))
              return(TRUE)
            }  
          }
        }
      }
    }
  }
  ###########-SPECTRUM FILE CHECKING-###########
  spectrumUploadCheck<-function(inFile,destDir){
    if(is.null(inFile)){
      shinyalert("Input missing", "No spectrum file has been provided",type="error")
    } else {
      data<-read.table(inFile$datapath,sep=";")
      if(any(is.na(data))){
        shinyalert("Error missing data", "There are missing data in your dataset",type="error")
        reset('spectrumfile')
        runjs('Shiny.onInputChange("spectrumfile", null)')
      } else if(input$runMode == "Predictions using our model" && ncol(data)<400){
        shinyalert("Column Error", "There should be 2151 Columns in your Spectrum file",type="error")
        reset('spectrumfile')
        runjs('Shiny.onInputChange("spectrumfile", null)')
      } else {    
        if(dir.exists(destDir)){
          result<- file.copy(inFile$datapath,file.path(destDir,inFile$name))
          return(TRUE)
        }
      }
    }
  }
  ##############################################################################
  ################ GPU SERVER INTERFACE + RUNNING PYTHON PIPELINE ##############
  ##############################################################################
  destDir<-'uploads'
  observeEvent(input$runAnalysis,{
    tryCatch({
    inFile <- input$spectrumfile
    testSpectrumFile <- input$testSpectrumFile
    traitFile<- input$traitsfile
    testTraitFile <- input$testTraitsFile
    traits <- c(input$functionalTraits,input$metabolites)
    mail <- get(paste(session$token,"-","mail",sep=""))
    
    if(isTRUE(globalUploadCheck(inFile,traitFile,testSpectrumFile,testTraitFile,destDir))){
      #system(paste("Rscript --vanilla runJob.R",email_user),wait = FALSE)
      shinyalert("Run started","You will receive an email when the job is complete",type="success")
      ##-MODE 1-##    
      if(input$runMode == "Predictions using our model"){
            tryCatch({
            future({
              #----------Connect to GPU----------------
              sessionGpu<-ssh_connect(ipGpu,passwd = passwordGpu)
              print(sessionGpu)
              #-----------Transfer spectrum file-------
              ssh_exec_wait(sessionGpu, command = c(
                "cd /home/vaillant/Documents/pyNirs",
                paste("mkdir ",session$token,sep = ""),
                paste("mkdir ",session$token,"/Res",sep = ""),
                paste("mkdir ",session$token,"/Temp",sep = "")
              ))
              file.path<-inFile$datapath
              scp_upload(sessionGpu,file.path,to=paste("/home/vaillant/Documents/pyNirs/",session$token,sep = ""))
              ssh_exec_wait(sessionGpu,paste("mv /home/vaillant/Documents/pyNirs/",session$token,"/0.csv /home/vaillant/Documents/pyNirs/",session$token,"/Xcal.csv",sep=""))
              #-----------Execute python script--------
              listTraits<-""
              for (i in 1:length(traits)) {
                listTraits<-paste(listTraits,traits[i],sep=" ")
              }
              outBash<-ssh_exec_internal(sessionGpu,paste("bash /home/vaillant/Documents/pyNirs/setup.sh 1 ",session$token,listTraits,sep=""))
              outErr<-rawToChar(outBash$stderr)
              if(grepl("out of memory",outErr)){
                stop("Error : Out of memory error")
              }
              #-----------Get output files------------ --
              path<-paste("/home/vaillant/Documents/pyNirs/",session$token,"/Res",sep="")
              scp_download(sessionGpu,path, to = session$token)
              ssh_exec_internal(sessionGpu,paste("rm -Rf /home/vaillant/Documents/pyNirs/",session$token,sep=""))
              if(!is.null(traits[1])){
                for (i in 1:length(traits)) {
                  DensityComparison(traits[i],1)
                }
              }
              #-----------Send results by email-------#
              system(paste("Rscript --vanilla sendResults.R",mail,session$token),wait = FALSE)
              #-----------Disconnect-----------------
              ssh_disconnect(sessionGpu)
            })%...!% (error=function(error_message){shinyalert("Error", "Unexpected error",type="error")
              return(NA)})
              })
          } else if (input$runMode == "Create new model + Predictions"){
            tryCatch({
              future({
              #----------Connect to GPU----------------
              sessionGpu<-ssh_connect(ipGpu,passwd = passwordGpu)
              print(sessionGpu)
              #-----------Transfer spectrum file-------
              ssh_exec_wait(sessionGpu, command = c(
                "cd /home/vaillant/Documents/pyNirs",
                paste("mkdir ",session$token,sep = ""),
                paste("mkdir ",session$token,"/Res",sep = ""),
                paste("mkdir ",session$token,"/Temp",sep = "")
              ))
              file.path<-inFile$datapath
              scp_upload(sessionGpu,file.path,to=paste("/home/vaillant/Documents/pyNirs/",session$token,sep = ""))
              ssh_exec_wait(sessionGpu,paste("mv /home/vaillant/Documents/pyNirs/",session$token,"/0.csv /home/vaillant/Documents/pyNirs/",session$token,"/Xcal.csv",sep=""))
              #-----------Execute python script--------
              outBash<-ssh_exec_internal(sessionGpu,paste("bash /home/vaillant/Documents/pyNirs/setup.sh 2 ",session$token,sep=""))
              outErr<-rawToChar(outBash$stderr)
              if(grepl("out of memory",outErr)){
                stop("Error : Out of memory error")
              }
              #-----------Get output files------------ --
              path<-paste("/home/vaillant/Documents/pyNirs/",session$token,"/Res",sep="")
              scp_download(sessionGpu,path, to = session$token)
              ssh_exec_internal(sessionGpu,paste("rm -Rf /home/vaillant/Documents/pyNirs/",session$token,sep=""))
              #-----------Send results by email-------#
              system(paste("Rscript --vanilla sendResults.R",mail,session$token),wait = FALSE)
              #-----------Disconnect-----------------
              ssh_disconnect(sessionGpu)
            })%...!% (error=function(error_message){shinyalert("Error", "Unexpected error",type="error")
              return(NA)})
              }) 
            ##-MODE 2-##  
            } else if (input$runMode == "Build your own model + Predictions"){
            tryCatch({
            future({
              #----------Connect to GPU----------------
              sessionGpu<-ssh_connect(ipGpu,passwd = passwordGpu)
              print(sessionGpu)
              #-----------Transfer spectrum file-------
              ssh_exec_wait(sessionGpu, command = c(
                "cd /home/vaillant/Documents/pyNirs",
                paste("mkdir ",session$token,sep = ""),
                paste("mkdir ",session$token,"/Res",sep = ""),
                paste("mkdir ",session$token,"/Temp",sep = "")
              ))
              file.path<-inFile$datapath
              scp_upload(sessionGpu,file.path,to=paste("/home/vaillant/Documents/pyNirs/",session$token,sep = ""))
              ssh_exec_wait(sessionGpu,paste("mv /home/vaillant/Documents/pyNirs/",session$token,"/0.csv /home/vaillant/Documents/pyNirs/",session$token,"/Xcal.csv",sep=""))
              scp_upload(sessionGpu,traitFile$datapath,to=paste("/home/vaillant/Documents/pyNirs/",session$token,sep = ""))
              ssh_exec_wait(sessionGpu,paste("mv /home/vaillant/Documents/pyNirs/",session$token,"/0.csv /home/vaillant/Documents/pyNirs/",session$token,"/Ycal.csv",sep=""))
              #-----------Execute python script--------
              outBash<-ssh_exec_internal(sessionGpu,paste("bash /home/vaillant/Documents/pyNirs/setup.sh 2 ",session$token,sep=""))
              outErr<-rawToChar(outBash$stderr)
              if(grepl("out of memory",outErr)){
                stop("Error : Out of memory error")
              }
              #-----------Get output files------------ --
              path<-paste("/home/vaillant/Documents/pyNirs/",session$token,"/Res",sep="")
              scp_download(sessionGpu,path, to = session$token)
              ssh_exec_internal(sessionGpu,paste("rm -Rf /home/vaillant/Documents/pyNirs/",session$token,sep=""))
              if(!is.null(traits[1])){
                for (i in 1:(length(traits))) {
                  DensityComparison(traits[i],NULL)
                }
              }
              #-----------Send results by email-------#
              system(paste("Rscript --vanilla sendResults.R",mail,session$token),wait = FALSE)
              #-----------Disconnect-----------------
              ssh_disconnect(sessionGpu)
            })%...!% (error=function(error_message){shinyalert("Error", "Unexpected error",type="error")
              return(NA)})
              })
              ##-MODE 3-##  
            } else if (input$runMode == "Complete, Test dataset needed"){
              tryCatch({
                future({
                  #----------Connect to GPU----------------
                  sessionGpu<-ssh_connect(ipGpu,passwd = passwordGpu)
                  print(sessionGpu)
                  #-----------Transfer spectrum file-------
                  ssh_exec_wait(sessionGpu, command = c(
                    "cd /home/vaillant/Documents/pyNirs",
                    paste("mkdir ",session$token,sep = ""),
                    paste("mkdir ",session$token,"/Res",sep = ""),
                    paste("mkdir ",session$token,"/Temp",sep = "")
                  ))
                  #---Calibration Dataset--#
                  scp_upload(sessionGpu,inFile$datapath,to=paste("/home/vaillant/Documents/pyNirs/",session$token,sep = ""))
                  ssh_exec_wait(sessionGpu,paste("mv /home/vaillant/Documents/pyNirs/",session$token,"/0.csv /home/vaillant/Documents/pyNirs/",session$token,"/Xcal.csv",sep=""))
                  scp_upload(sessionGpu,traitFile$datapath,to=paste("/home/vaillant/Documents/pyNirs/",session$token,sep = ""))
                  ssh_exec_wait(sessionGpu,paste("mv /home/vaillant/Documents/pyNirs/",session$token,"/0.csv /home/vaillant/Documents/pyNirs/",session$token,"/Ycal.csv",sep=""))
                  #-Test Dataset-#
                  scp_upload(sessionGpu,testSpectrumFile$datapath,to=paste("/home/vaillant/Documents/pyNirs/",session$token,sep = ""))
                  ssh_exec_wait(sessionGpu,paste("mv /home/vaillant/Documents/pyNirs/",session$token,"/0.csv /home/vaillant/Documents/pyNirs/",session$token,"/Xval.csv",sep=""))
                  scp_upload(sessionGpu,testTraitFile$datapath,to=paste("/home/vaillant/Documents/pyNirs/",session$token,sep = ""))
                  ssh_exec_wait(sessionGpu,paste("mv /home/vaillant/Documents/pyNirs/",session$token,"/0.csv /home/vaillant/Documents/pyNirs/",session$token,"/Yval.csv",sep=""))
                  #-----------Execute python script--------
                  outBash<-ssh_exec_internal(sessionGpu,paste("bash /home/vaillant/Documents/pyNirs/setup.sh 3 ",session$token,sep=""))
                  outErr<-rawToChar(outBash$stderr)
                  if(grepl("out of memory",outErr)){
                    stop("Error : Out of memory error")
                  }
                  #-----------Get output files------------ --
                  path<-paste("/home/vaillant/Documents/pyNirs/",session$token,"/Res",sep="")
                  scp_download(sessionGpu,path, to = session$token)
                  ssh_exec_internal(sessionGpu,paste("rm -Rf /home/vaillant/Documents/pyNirs/",session$token,sep=""))
                  if(!is.null(traits[1])){
                    for (i in 1:(length(traits))) {
                      DensityComparison(traits[i],NULL)
                    }
                  }
                  #-----------Send results by email-------#
                  system(paste("Rscript --vanilla sendResults.R",mail,session$token),wait = FALSE)
                  #-----------Disconnect-----------------
                  ssh_disconnect(sessionGpu)
                })%...!% (error=function(error_message){shinyalert("Error", "Unexpected error",type="error")
                  return(NA)})
              })
            } 
          reset('spectrumfile')
        }
    })
  })
  #####################################################
  ######### CONSULTATION DOWNLOAD HANDLING ############
  uploadData <- function(outputname) {
    output$DlConsult <- downloadHandler(
      filename = function() {
        paste("SelectedSpectrums-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        file.copy(paste(session$token,"/",outputname,".csv",sep = ""),file)
      }
    )
  }
  ########################################
  ##### APPLICATION MANUAL DOWNLOAD ######
    output$manual <- downloadHandler(
      filename = "Manual.pdf",
      content = function(file) {
        file.copy("Manual.pdf",file)
      }
    )
  ########################################
  ##### CUSTOM OPTIONS TOGGLE BUTTON #####
  isshowed<<-FALSE;
  observeEvent(input$outputformat, {
    if(input$outputformat == "Custom"){
      toggle(id="customoptions")
      isshowed<<-TRUE;
      shinyjs::runjs("window.scrollTo(0, 2000)")
    }
    if(input$outputformat != "Custom" && isshowed == TRUE){
      toggle(id="customoptions")
      isshowed<<-FALSE;
    }
  })
  
  ######### UPLOAD BUTTON MANAGEMENT #############
  isshowedTraitInput<<-FALSE;
  isshowedTestInputs<<-FALSE;
  observeEvent(input$runMode, {
    tryCatch({
    if(input$runMode != "Build your own model + Predictions" && isshowedTraitInput == TRUE && isshowedTestInputs == FALSE){
      toggle(id="inputTrait")
      isshowedTraitInput<<-FALSE;
    }
    if(input$runMode == "Complete, Test dataset needed"){
      toggle(id="inputDataTest")
      isshowedTestInputs<<-TRUE;
      toggle(id="inputTrait")
      isshowedTraitInput<<-TRUE;
    }
    if(input$runMode != "Complete, Test dataset needed" && isshowedTestInputs == TRUE && isshowedTraitInput == TRUE ){
      toggle(id="inputDataTest")
      isshowedTestInputs<<-FALSE;
      toggle(id="inputTrait")
      isshowedTraitInput<<-FALSE;
    }
    if(input$runMode == "Build your own model + Predictions" &&  isshowedTraitInput == FALSE){
      toggle(id="inputTrait")
      isshowedTraitInput<<-TRUE;
    }
    },error=function(err){showNotification("Error",type="error")
      return(NA)})
  })
  
  ######### REGISTER BUTTON MANAGEMENT #################
  toListen <- reactive({
    list(input$functionalTraits,input$metabolites)
  })
  observeEvent(toListen(),{
    if(!is.null(input$functionalTraits[1]) || !is.null(input$metabolites[1])){
      enable('Go')
    } else {
      disable('Go')
    }
  }, ignoreNULL = FALSE)
  
  ######### EMAIL HANDLER ###################
  observeEvent(input$Go,{
    if(!input$mail=="" && isValidEmail(input$mail)){
      assign(paste(session$token,"-","mail",sep=""),input$mail, envir = .GlobalEnv)
      enable('runAnalysis')
      disable('Go')
    } else {
      shinyalert("Invalid email", "Invalid email",type="error")
    }
  })
  #-CONTRIBUTOR EMAIL-#
  observeEvent(input$Gocontrib,{
    if(!input$mailcontrib=="" && isValidEmail(input$mailcontrib)){
      assign(paste(session$token,"-","mailContrib",sep=""),input$mailcontrib, envir = .GlobalEnv)
      enable('sendContribution')
      disable('Gocontrib') 
    } else {
      shinyalert("Invalid email", "Invalid email",type="error")
    }
  })
  #-EMAIL PATTERN TO MATCH-#
  isValidEmail <- function(x) {
    grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", as.character(x), ignore.case=TRUE)
  }
  #########################################################
  #################### CONTRIBUTOR PAGE ###################
  observeEvent(input$sendContribution,{
    tryCatch({
    system(paste("mkdir contribution/",session$token,sep = ""))
    contribDir<-paste("contribution/",session$token,sep = "")
    contribFile <- input$contributorfile
    mailContrib <- get(paste(session$token,"-","mailContrib",sep=""))
    if(is.null(contribFile)){
      shinyalert("Input missing", "No file has been provided",type="error")
    } else{
      data<-read.table(contribFile$datapath,header = TRUE,sep = ";")
      if(ncol(data)!=2151){
        shinyalert("Column Error", "There should be 2151 Columns",type="error")
        reset('contributorfile')
      } else if(dir.exists(contribDir)){
        result<- file.copy(contribFile$datapath,file.path(contribDir,contribFile$name))
        system(paste("Rscript --vanilla newContribution.R",mailContrib,session$token),wait = FALSE)
        shinyalert("Success", "Dataset has been sent",type="success")
        reset('contributorfile')}
    }
    },error=function(err){shinyalert("Error","Contribution error",type="error")
      return(NA)})
  })
  
  #access to the app from the homepage link
  observeEvent(input$app, updateTabsetPanel(session = session, inputId = "tabset", selected = "app"))
  
  #Delete temporary repository
    session$onSessionEnded(function() {
      #wait 1 hour before deleting the session's folder
    delay(3600000,system(paste("rm -Rf ",session$token,sep = "")))
  })
}