#

library(sf)
library(dplyr)

## Données sur les bureaux de votes
bv<-readRDS("map/bv03/mapbv_voronoi.RDS")
sum(bv$ins)
plot(bv$geometry)
## Données sur grille
gri<-readRDS("map/bv03/grid2021.RDS") %>% st_centroid()
sum(gri$ind, na.rm=T)
plot(gri$geom, col="red",add=T, pch=20, cex=0.1)

## Centroïdes 


# Jointure
bvcode <- bv %>% select(bvocode)
x<-st_join(bvcode, gri)

## Agregation
bv_don <- x %>% 
  group_by(bvocode) %>%
  summarise(ind = sum(ind),
            men = sum(men),
            men_pauv = sum(men_pauv),
            men_mais = sum(men_mais),
            men_prop = sum(men_prop),
            log_soc = sum(log_soc),
            ind_snv = sum(ind_snv),
            ind_18_24 = sum(ind_18_24),
            ind_25_39 = sum(ind_25_39),
            ind_40_54 = sum(ind_40_54),
            ind_55_64 = sum(ind_55_64),   
            ind_65_79 = sum(ind_65_79),                       
            ind_80p = sum(ind_80p)                     
  ) %>% 
  mutate(ind_18p = ind_18_24+ind_25_39+ind_40_54+ind_55_64+ind_65_79+ind_80p) %>%
  st_drop_geometry()

## Indicators
bv_don <- bv_don %>% mutate(txpauv = 100*men_pauv/men,
                            txmais = 100*men_mais/men,
                            txprop = 100*men_prop/men,
                            txhlm = 100*log_soc/men,
                            revhab = ind_snv/ind,
                            tx1824 = 100*ind_18_24/ind_18p,
                            tx2539 = 100*ind_25_39/ind_18p, 
                            tx4054 = 100*ind_40_54/ind_18p,
                            tx5564 = 100*ind_55_64/ind_18p, 
                            tx6579 = 100*ind_65_79/ind_18p,
                            tx80p = 100*ind_80p/ind_18p,   
)

## Exportation
bv <- bv %>% left_join(bv_don)
sum(bv$ind, na.rm=T)
saveRDS(bv, "map/bv03/bv03_soceco.RDS")


