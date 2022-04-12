observeEvent(input$submit, {
  # Connect to the database
  con <- dbConnect(MySQL(), dbname = nirsdbs, host = options()$mysql$host, 
                   port = options()$mysql$port, user = options()$mysql$user, 
                   password = options()$mysql$password)
  
  inputList<-list(input$location,input$exp,input$contributor,input$genotype,input$genetic_group,
                  input$condition,input$leaf_stage,input$plant_stage,input$measurement,input$treatment,
                  input$CSR,input$sugar,input$glucosinolates,input$secondary_metabolites)
  inputNameList<- list("exp_location","id_exp","main_contributor","genotype","genetic_group","condition_exp",
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
        paramList<-paste(paramList,inputList[[i]][j],",")
      }
      paramList<-substr(paramList,1,nchar(paramList)-2)
      filterList1<-c(filterList1, assign(paste("Query",i,sep=""),paste("individuals.",inputNameList[i] ," IN ","(",paramList,")",sep="")))
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
newtab<-FormatData(res)

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
###########ALL SPECTRUM OUTPUT##############
allSpectrum=data.frame()
allSpectrum <- as.data.frame(matrix(double(),ncol = 2))
for(i in 350:2500){
  val<-paste('x',i,sep = "")
  abs<-newtab[,val]
  row<-as.data.frame(cbind(mean(as.numeric(abs)),i))
  allSpectrum<- rbind(allSpectrum, row)
}
colnames(allSpectrum)[1] <- "Abs"
colnames(allSpectrum)[2] <- "Wavelength"

graph1<-allSpectrum[,c("Abs","Wavelength")]
plot(x = allSpectrum$Wavelength, y = allSpectrum$Abs,
     xlab = "Wavelength",
     ylab = "Asorption",
     xlim = c(350, 2500),
     ylim = c(0, 1.25),         
     main = "All Spectrum Mean",col=2
)

########DOUBLE GRAPH###########
set.seed(1234)
x <- c(rnorm(500, mean = -1), rnorm(500, mean = 1.5))
y <- c(rnorm(500, mean = 1), rnorm(500, mean = 1.7))
group <- as.factor(rep(c(1,2), each=500))
df <- data.frame(x, y, group)
head(df)

scatterPlot<-ggplot(df,aes(x,y,color=group))+geom_point()+
  scale_color_manual(values=c('#999999','#E69F00'))+ 
  theme(legend.position=c(0,1),legend.justification=c(0,1))

a<-normSpectrum[,1]
b<-normSpectrum2[,1]
c=data.frame(a)

for(j in 1:length(normSpectrum2[,1])){
  c<-rbind(c,normSpectrum2[j,1])
}
group1<-as.factor(rep(c("Individual","All"),each=2151))
df1<-data.frame(c,group1)

spectrumPlot<-ggplot(df1,aes(a,b,color=group1))+geom_point()+
  scale_color_manual(values = c('#999999','#E69F00'))+
  theme(legend.position=c(0,1),legend.justification=c(0,1))

spectrumPlot
