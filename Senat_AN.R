library(mapsf)
library(sf)
library(dplyr)
library(ggplot2)
library(ggrepel)
map <- readRDS("data_dep/map_dept.RDS") %>% rename(Code=dept)
plot(map$geometry)
don<-read.csv2("data_dep/depsen.csv", header=T) %>% filter(!Code %in% c("2A","2B"))
mapdon <-map %>% left_join(don)

mapdon$NBDEPpct<-100*mapdon$NBDEP/sum(mapdon$NBDEP)
mapdon$NBSENpct<-100*mapdon$NBSEN/sum(mapdon$NBSEN)
mapdon$dif<-mapdon$NBDEPpct-mapdon$NBSENpct
mapdon$col<-as.factor(mapdon$dif>0)
levels(mapdon$col) <- c("% Sénateur > % Députés", "% Députés > % Sénateurs")
mapdon$den<-mapdon$P2023/mapdon$SUP
mf_map(mapdon, type="choro",var="den",
       leg_pos="topleft", 
       leg_val_rnd=0,
       leg_title ="Densité de population en 2023 (hab./km2)",
       pal="Grays",
       breaks=c(10,20,40, 80, 160, 320, 20000))
mf_map(mapdon, type="prop_typo", 
       var=c("dif","col"), 
       inches=0.1, 
       leg_pos="left",
       leg_val_rnd = c(2,0),
       leg_title = c("Différence de %", "Avantage relatif"))
mf_layout("Représentation relative des départements aux deux chambres",
          frame=T, credits = "Projet REDEC, 2026", arrow=F)
sum(mapdon$NBDEP)
sum(mapdon$NBSEN)
