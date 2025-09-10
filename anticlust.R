library(sf)
library(dplyr)
library(mapsf)
iris<-readRDS("data94/iris94.RDS") %>% st_transform(2154)
iris$size<-1000*iris$votants/sum(iris$votants,na.rm=T)
mf_map(iris, type="base")
mf_map(iris, type="prop",var="size",inches=0.1)
w<-st_coordinates(st_centroid(iris))
tab<-cbind(w,iris$size)
tab<-as.data.frame(tab)
names(tab)<-c("X","Y","W")
plot(tab$X,tab$Y,cex=tab$W/4, asp =1,col="red",pch=20)
plot(iris$geometry,add=T)
tab<-tab[complete.cases(tab)==T,]
dim(tab)
tab<-tab[1:468,]
library(anticlust)
dis<-st_distance(st_centroid(iris))
cl <- balanced_clustering(dis[1:468,1:468], K=12, method = "centroid")

plot(tab$X,tab$Y,cex=tab$W/3, asp =1,col=cl,pch=20)
plot(iris$geometry,add=T)
12*39
tab$cl<-cl
x <- tab %>% group_by(cl) %>% summarise(w= sum(W))
x
mf_map(iris, type="typo", var="circ_code")

summary(iris$inscrits)
sum(iris$inscrits,na.rm=T)

brek<-function(tab,n=500){
  tab$div<-round(tab$W/500,0)
  
}
