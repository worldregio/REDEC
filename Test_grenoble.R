
library(ggplot2)
library(dplyr)
library(leaflet)
library(sf)
library(readxl)

adm <-read_xlsx("data_dep/epcicom2026.xlsx") 
selmet <- adm %>% filter(siren==200040715) 
sel <- selmet$insee
sel

bv <- readRDS("map/bv38/bv38_soceco.RDS") 
gr <- readRDS("map/bv38/grid2021.RDS")


x <- bv %>% filter(comcode %in% sel) %>% 
  mutate(ratio=ins/ind) %>% 
  st_transform(4326)
plot(x$geometry)

y <- gr %>% filter(lcog_geo %in% sel) %>% 
      select(ind, geometry=geom) %>% st_transform(4326) 
yctr <- st_centroid(y)
leaflet() %>% addTiles() %>% addPolygons(map=y$geom)

q<-colorQuantile(palette="reds",domain = y$ind,n = 4)

yctr$size <- 1+8*sqrt(yctr$ind/max(yctr$ind))

# Choix des classes 
mycut<-c(0,0.25,0.5,1,2,4,8)
mypal <- colorBin('Spectral', 
                  reverse = T,
                  x$ratio,
                  bins=mycut)

# Choix de la palette (c'est une fonction !)
mypal <- colorBin('Spectral', 
                  reverse = T,
                  dvf$prixm2,
                  bins=mycut)



leaflet(x) %>% addProviderTiles(provider="Esri.WorldTopoMap") %>% 
    addPolygons(data=y, 
                weight=1, 
                fillOpacity = 0,
                col="blue"
                ) %>%
  addCircleMarkers(data=yctr,
                   col="blue",
                   fillColor = "blue",
                   stroke=F,
                   radius = ~ size,
                   fillOpacity = 0.7,
                   label = ~ind ) %>% 
      addPolygons(data=x, 
              weight=2, 
              fillOpacity = 0.6,
              col="red",
              fillColor = ~mypal(ratio),
              label = ~ratio
              ) 
#--------------------------------

myvar<-bv$txpauv
q<-quantile(myvar, probs = seq(0,1,1/20),na.rm=T)
q
bv$X <- cut(myvar, q,include.lowest = T)
table(bv$X)
levels(bv$X)<-c("D01","D02","D03","D04","D05","D06","D07","D08","D09","D10",
                "D11","D12","D13","D14","D15","D16","D17","D18","D19","D20")

res <- bv %>% group_by(X) %>% 
  summarise(Y=1-(sum(vot, na.rm=T)/sum(ins, na.rm=T)) )%>%
  filter(is.na(X)==F)
barplot(res$Y~res$X,horiz = T, las=1)
ggplot(res) +aes(x=X,y=Y) + geom_bar(stat="identity") + geom_smooth()
