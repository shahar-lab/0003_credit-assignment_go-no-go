#PCA - prinicipal factor analysis 
rm(list = ls())
load('03_data/02_aggregated_data/wurs.Rdata')
load('03_data/02_aggregated_data/asrs.Rdata')
load('03_data/02_aggregated_data/bis.Rdata')
load('03_data/02_aggregated_data/bisbas.Rdata')

library(psych)
library(nFactors)
library(corrplot)

####PCA and compound scores ---------
df= data.frame(asrs.partA=asrs[,2],
               asrs.partB=asrs[,3],
               bis.attention=bis[,2],
               bis.motor=bis[,3],
               bis.nonplan=bis[,4],
               wurs=wurs[,2])

colnames(df)
df <-as.data.frame(scale(df))
summary(df)
l<-round(cor(df),1)
corrplot(l, method="circle",tl.cex=0.4)

#PCA
scree(df,factors=TRUE,pc=TRUE,main="Scree plot",hline=NULL,add=FALSE) 
parallel<-fa.parallel(df, fm="ml", fa="pc", main = "Parallel Analysis Scree Plots",
                      n.iter=20,error.bars=T,ylabel=NULL,show.legend=TRUE,sim=TRUE)
m<-principal(df,nfactors = 1,rotate = 'promax',scores=TRUE)
print(m$loadings,cutoff = 0.5)

adhd <-data.frame(factor1=m$scores[,1])
#cor(adhd[,1],df[,'asrs.partA'])   


# item level PCA ---------------------------------------------------------
rm(list = ls())
load('03_data/02_aggregated_data/asrs_raw.Rdata')
load('03_data/02_aggregated_data/wurs_raw.Rdata')
load('03_data/02_aggregated_data/bis_raw.Rdata')
load('03_data/02_aggregated_data/bisbas_raw.Rdata')
colnames(asrs)<-paste0('asrs_',colnames(asrs))
colnames(bis)<-paste0('bis_',colnames(bis))
colnames(bisbas)<-paste0('bisbas_',colnames(bisbas))
colnames(wurs)<-paste0('wurs_',colnames(wurs))

df<-cbind(asrs[,seq(1,20)],bis[,seq(1,30)],wurs[,seq(1,25)])
#bisbas[,seq(1,24)]

colnames(df)
df <-as.data.frame(scale(df))
summary(df)
l<-round(cor(df),3)
library(corrplot)
corrplot(l, method="circle",tl.cex=0.4)

#PCA
library(psych)
scree(df,factors=TRUE,pc=TRUE,main="Scree plot",hline=NULL,add=FALSE) 
parallel<-fa.parallel(df, fm="ml", fa="pc", main = "Parallel Analysis Scree Plots",
                      n.iter=100,error.bars=T,ylabel=NULL,show.legend=TRUE,sim=TRUE)
m<-principal(df,nfactors = 3,rotate = 'promax',scores=TRUE)
print(m$loadings,cutoff = 0.5)

adhd <-data.frame(factor1=m$scores[,1])
#cor(adhd[,1],df[,'asrs.partA'])   

####PCA and compound scores BISBAS ---------
df= data.frame(bisbas.bis=bisbas[,2],
               bisbas.drive=bisbas[,3],
               bisbas.fun_seeking=bisbas[,4],
               bisbas.reward_responsiveness=bisbas[,5])


colnames(df)
df <-as.data.frame(scale(df))
summary(df)
l<-round(cor(df),1)
corrplot(l, method="circle",tl.cex=0.4)

#PCA
scree(df,factors=TRUE,pc=TRUE,main="Scree plot",hline=NULL,add=FALSE) 
parallel<-fa.parallel(df, fm="ml", fa="pc", main = "Parallel Analysis Scree Plots",
                      n.iter=20,error.bars=T,ylabel=NULL,show.legend=TRUE,sim=TRUE)
m<-principal(df,nfactors = 2,rotate = 'promax',scores=TRUE)
print(m$loadings,cutoff = 0.5)

adhd <-data.frame(factor1=m$scores[,1])
#cor(adhd[,1],df[,'asrs.partA'])   




