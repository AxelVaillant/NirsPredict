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

FormatData<- function(res){
  start<-1
  end<-2151
  n<-(nrow(res)/2151)
  newtab=data.frame()
  newtab <- as.data.frame(matrix(double(),ncol = 2152))
  names(newtab)[2:2152] <- paste0('x', 350:2500)
  colnames(newtab)[1] <- "Id"
  rep<-replicate(n,loop(res,start,end),simplify = TRUE)
  rep[-1,]<-as.numeric(rep[-1,])
  newtab<-as.data.frame(t(rep))
  newtab<-data.frame(lapply(newtab, as.character), stringsAsFactors=FALSE)
  return (newtab)
}

#####FASTER LOOP######
loop<-function(res,start,end){
  id<-res[start,4]
  sub<-t(res[start:end,3])
  sub2<-as.data.frame(cbind(id,sub))
  names(sub2)[2:2152] <- paste0('x', 350:2500)
  colnames(sub2)[1] <- "id"
  assign("start",start+2151)
  assign("end",end+2151)
  newtab<- rbind(newtab, sub2)
  return (newtab)
}
#####FASTER LOOP######
start<-1
end<-2151
n<-(nrow(res)/2151)
newtab=data.frame()
newtab <- as.data.frame(matrix(double(),ncol = 2152))
names(newtab)[2:2152] <- paste0('x', 350:2500)
colnames(newtab)[1] <- "Id"
rep<-replicate(n,loop(),simplify = TRUE)
rep[-1,]<-as.numeric(rep[-1,])
rep<-as.data.frame(rep)
blabla<-as.data.frame(t(rep))

blabla<-data.frame(lapply(blabla, as.character), stringsAsFactors=FALSE)

loop<-function(){
  id<-res[start,4]
  sub<-t(as.numeric(res[start:end,3]))
  sub2<-as.data.frame(cbind(id,sub))
  names(sub2)[2:2152] <- paste0('x', 350:2500)
  colnames(sub2)[1] <- "id"
  start<<-start+2151
  end<<-end+2151
  newtab<- rbind(newtab, sub2)
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

############
plot(rdyToPlot[rdyToPlot$name=="Individual","wavelength"], rdyToPlot[rdyToPlot$name=="Individual","absSelec"], col="firebrick3", type="l", lwd=3, ylim=c(0,1),
     main="Mean comparison",xlab="Wavelength",ylab="Absorption")
points(rdyToPlot[rdyToPlot$name=="All","wavelength"], rdyToPlot[rdyToPlot$name=="All","absSelec"], col="dodgerblue3", type="l", lwd=3,lty=3)
legend(1700,0.85,legend = c("Selected spectrums","All spectrums"),col =c("firebrick3","dodgerblue3"),lty=1:2,cex = 0.8)

#############ACP######################
res<-read.table(file = "resAll.csv",header = TRUE,sep = ";")
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
  newtab<-rbind(newtab,sub2)
  print(i)
}

allSpectrum <- as.data.frame(matrix(double(),ncol = 2))
for(i in 350:2500){
  val<-paste('x',i,sep = "")
  absSelec<-newtabAll[,val]
  rowsel<-as.data.frame(cbind(mean(as.numeric(absSelec)),i))
 allSpectrum<- rbind(allSpectrum, rowsel)
}
write.table(allSpectrum,"allSpectrum.csv",row.names = FALSE,sep=";")
write.table(newtabAll,"newtabAll.csv",row.names = FALSE,sep=";")

head(iris)
head(newtab)

head(ToothGrowth)
allDataRes<-read.table(file = "allDataRes.csv",header = TRUE,sep = ";")
library(ggplot2)
library(FactoMineR)
library(factoextra)
DfComplet2<-read.csv(file = "6-NIRS_Metadata_Fit.csv",header = TRUE,sep = ";")
DfComplet3<-read.table(file = "allSpectra.csv",header = TRUE,sep = ";")

boxplot(allDataRes$csr_c, xlim = c(0.5, 1.5), main = parse(text = "test"),
        outline = FALSE, col = "grey75",
        ylim = c(0,100))

pca = prcomp(allDataRes)
pca=prcomp(iris[,1:4])
plot(pca)
allDataRes[,cbind(allDataRes$plant_stage,allDataRes[,97:2247])]
plca=prcomp(allDataRes[,1:2247])

PCA(df,scale.unit = TRUE,ncp=5,graph=TRUE)
df<-cbind(DfComplet$plant_stage,DfComplet[,97:2247])
df<-allDataRes[,97:2247]

df1<-cbind(DfComplet$Plant_Stage,DfComplet[129:2279])
df2<-newtabAll[,2:2152]
AllDataPca<-PCA(df2,scale.unit = TRUE,ncp=5,graph=TRUE)

fviz_pca_ind(AllDataPca,gemo.ind="point",label="none",col.ind = Phen$Plant_Stage,addEllipses = TRUE,legend.title="Groups")+scale_shape_manual(values=c(0,1,2,3,4,5,6,7,9,9))
fviz_pca_ind(AllDataPca,gemo.ind="point",label="none",col.ind = Phen$Leaf_Status,addEllipses = TRUE,legend.title="Groups")+scale_shape_manual(values=c(0,1,2,3,4,5,6,7,9,9))
fviz_pca_ind(AllDataPca,gemo.ind="point",label="none",col.ind = Phen$Treatment,addEllipses = TRUE,legend.title="Groups")+scale_shape_manual(values=c(0,1,2,3,4,5,6,7,9,9))
fviz_pca_ind(AllDataPca,gemo.ind="point",label="none",col.ind = Phen$Genetic_group,addEllipses = FALSE,legend.title="Groups")+scale_shape_manual(values=c(0,1,2,3,4,5,6,7,9,10,11))
fviz_pca_ind(AllDataPca,gemo.ind="point",label="none",col.ind = Phen$Condition,addEllipses = TRUE,legend.title="Groups")+scale_shape_manual(values=c(0,1,2,3,4,5,6,7,9,10,11))

pca_res<-prcomp(df2,scale. = TRUE)   
df<-df[,2:2152]
pca_res2<-prcomp(df,scale. = TRUE)
plot(pca_res$x,col="firebrick3")
points(pca_res2$x,col='dodgerblue3')

write.table(Phen,file="phenAll.csv",sep=";",row.names = FALSE)

phenAll<-read.table(file="phenAll.csv",header=TRUE,sep=";")
AllDataPca<-PCA(newtabAll[,2:2152],scale.unit = TRUE,ncp=5,graph=TRUE)
summary(AllDataPca)

newtab<-read.table(file = "selectedSpectrums.csv",header = TRUE,sep = ";")
params<-read.table(file="paramsOnlyRes.csv",header=TRUE,sep=";")
SelectedDataPca<-PCA(newtab[,2:2152],scale.unit = TRUE,ncp=5,graph=TRUE)
fviz_pca_ind(SelectedDataPca,gemo.ind="point",label="none",col.ind = "green",addEllipses = TRUE,legend.title="Groups")+scale_shape_manual(values=c(0,1,2,3,4,5,6,7,9,10))+xlim(-100, 300)
fviz_pca_ind(AllDataPca,gemo.ind="point",label="none",col.ind = "grey",addEllipses = TRUE,legend.title="Groups")+scale_shape_manual(values=c(0,1,2,3,4,5,6,7,9,9))+ylim(-100,100 )

fviz_eig(AllDataPca, addlabels = TRUE, ylim = c(0, 50))
fviz_eig(SelectedDataPca, addlabels = TRUE, ylim = c(0, 60))

fviz_pca_var(AllDataPca, col.var = "black")
fviz_pca_var(SelectedDataPca, col.var = "black")


newtabAll<-read.table(file="newtabAll.csv",header=TRUE,sep=";")
phenAll<-read.table(file="phenAll.csv",header=TRUE,sep=";")
AllDataPca<-PCA(newtabAll[,2:2152],scale.unit = TRUE,ncp=5,graph=FALSE)
fviz_pca_ind(AllDataPca,gemo.ind="point",label="none",col.ind = "grey",addEllipses = TRUE,legend.title="Groups")+scale_shape_manual(values=c(0,1,2,3,4,5,6,7,9,9))+ylim(-100,100 )+ggtitle("All spectra PCA")

fast.prcomp(newtabAll[,2:2152])
write.infile(AllDataPca,file="OutputFile.csv")
importedPCA<-read.csv(file = "OutputFile.csv",sep = ";",header = TRUE)
points(x, y, pch = 16, cex = .2, col = rgb(0, 0, 0, alpha = .3))
plot(AllDataPca,axes=c(1,2),choix = c("ind","var","varcor"))


####ADE4TEST####
library(ade4)
spectre<-newtabAll[,2:2152]
res.pca<- dudi.pca(newtabAll[,2:2152],nf=5,scannf=FALSE)
system.time(res.pca<- dudi.pca(spectre,center=T,scale=T,nf=5,scannf=FALSE))
system.time(AllDataPca<-PCA(spectre,scale.unit = TRUE,ncp=5,graph=FALSE))


##########################################
######        WORLD MAP             ######
##########################################

#########Construction Dataset genotype/position######
tab<-read.table("phenAll.csv",header = TRUE,sep = ";")
newtab=data.frame()
newtab <- as.data.frame(matrix(double(),ncol = 2152))
for(i in 1:nrow(tab)){
  row<-tab[i,]
  if(!row$Genotype==''){
    newtab<-rbind(newtab,row)
  }
}
genotype<-read.table("/home/vaillant/Documents/genotype.csv",header = TRUE,sep = ';',colClasses = c("character","numeric","numeric"))

tabPos=data.frame()
tabPos <- as.data.frame(matrix(double(),ncol = 2152))
for(i in 1:nrow(tab)){
  row<-tab[i,]
  if(!row$Genotype==''){
    for(j in 1:nrow(genotype)){
      rowGen<-genotype[j,]
      if(row$Genotype == rowGen$genotype){
        mrow<-cbind(row$Identification,rowGen)
        tabPos<-rbind(tabPos,mrow)
        
      }
    }
  }
}
write.table(tabPos,"genotypeByPosition.csv", col.names = TRUE , row.names = FALSE,sep=";" )
######ApplicationToOurDataset##############
##-----------WORLD SCALE---------------- ########
for(i in 1:nrow(genotype)){
  genotype[i,2]<-as.numeric(sub(",",genotype[i,2],fixed=TRUE))
  genotype[i,3]<-as.numeric(sub(",",genotype[i,3],fixed=TRUE))
}

ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color="black",fill="lightgray",size=0.1
  ) +
  geom_point(
    data = genotype,
    aes(longitude, latitude, color = genotype),
    alpha = 0.7
  ) +
  theme_void() +
  theme(legend.position = "none")

##-----------EUROPE SCALE---------------- ########
worldmap <- ne_countries(scale = 'medium', type = 'map_units',
                         returnclass = 'sf')
ggplot() + geom_sf(data = worldmap) +
  coord_sf(xlim = c(-20, 45), ylim = c(30, 73), expand = FALSE) +
  geom_point(
    data = genotype,
    aes(longitude, latitude, color = genotype),
    alpha = 0.7
  ) +
  theme_void() +
  theme(legend.position = "none")
##-----------EUROPE CUSTOM SCALE 1 et 2---------------- ########
ggplot() + geom_sf(data = worldmap) +
  coord_sf(xlim = c(-12, 55), ylim = c(30, 73), expand = FALSE) +
  geom_point(
    data = genotype,
    aes(longitude, latitude, color = genotype),
    alpha = 0.7
  ) +
  theme_void() +
  theme(legend.position = "none")
#------#
ggplot() + geom_sf(data = worldmap) +
  coord_sf(xlim = c(-11, 30), ylim = c(35, 60), expand = FALSE) +
  geom_point(
    data = genotype,
    aes(longitude, latitude ),
    alpha = 0.7,col='red'
  ) +
  theme_void() +
  theme(legend.position = "none") +
  labs(title="Genotype locations")


##-----------WORLD CUSTOM SCALE---------------- ########
ggplot() + geom_sf(data = worldmap) +
  coord_sf(xlim = c(-130, 150), ylim = c(10, 73), expand = FALSE) +
  geom_point(
    data = genotype,
    aes(longitude, latitude),
    alpha = 0.7,col='red',size=0.8
  ) +
  theme_void() +
  theme(legend.position = "none") +
  labs(title="Genotype locations")

SLA<-X6_NIRS_Metadata_Fit$SLA_mm2_mg
b<-X6_NIRS_Metadata_Fit[,100:2250]
testtable = data.frame()
testtable<-cbind(SLA,b)
write.table(testtable,"testtable.csv",col.names = TRUE,row.names=FALSE,sep=";")

write.table(SLA,"SLA2.csv",col.names = FALSE,row.names=FALSE,sep=";")
SLA3<-SLA[,2:2152]
write.table(SLA3,"SLA3.csv",col.names = FALSE,row.names=FALSE,sep=";")

write.table(SLA[1:5,],"SLAmini.csv",col.names = FALSE,row.names=FALSE,sep=";")




