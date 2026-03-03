library(dplyr)
library(sf)
library(ggplot2)

bv<-readRDS(file = "bv94/bv_mapdon.RDS") %>% st_transform(2154) %>% arrange(codecom, numbv)
com<-readRDS(file = "data94/com94.RDS") %>% st_transform(2154)
listcom<-com$com_code

voronoi<- function(mycom) {
  points <- bv %>% filter(codecom %in% mycom )%>% st_centroid()
  lim <- com %>% filter(com_code %in% mycom ) %>% st_union()
  
  voronoi <- points %>% 
    st_geometry() %>% # to get sfc from sf
    st_union() %>% # to get a sfc of MULTIPOINT type
    st_voronoi(envelope = st_geometry(lim)) %>% # NC sized Voronoi polygon
    st_collection_extract(type = "POLYGON") %>% # a list of polygons
    st_sf() %>% # from list to sf object
    st_intersection(lim) %>% # cut to shape of NC state
    st_join(points) # put names back
  return(voronoi)
}

map<-voronoi("94001")
for (i in 2:47){
  map1 <- voronoi(listcom[i])
  map<-rbind(map,map1)
  
}


## Cheating corrections
mapOK <- map %>% filter(is.na(id_bv)==F)
mapbad <- map %>% filter(is.na(id_bv)==T)

bvbad<-bv %>% filter(!id_bv %in% mapOK$id_bv)
bvbad$geometry <-mapbad$geometry

mapfin<-rbind(mapOK, bvbad)

# Simplify
mapfin <-st_buffer(mapfin,dist = 0.01)
plot(w["codecom"])



# Check agregated
circ94voronoi <- mapfin%>% group_by(codecir) %>% summarise()
plot(circ94voronoi$geometry)

# save
saveRDS(mapfin,"bv94/mapbv94voronoi.RDS")
com<-mapfin %>% group_by(nomcom,codecom) %>% summarise()
saveRDS(com,"bv94/mapcom94voronoi.RDS")
cir<-mapfin %>% group_by(codecir) %>% summarise()
saveRDS(cir,"bv94/mapcir94voronoi.RDS")


# Use Magrit for cartogram #
# ...

carto<-st_read("bv94/mapbv94vornoicartogram.geojson") %>% st_transform(2154)
saveRDS(carto,"bv94/mapbv94cartogram.RDS")
st_write(carto,"bv94/mapbv94cartogram.geojson")
carto <-st_make_valid(carto)
com <- carto %>% group_by(codecom,nomcom) %>% summarise(Inscrits=sum(Inscrits))
saveRDS(com,"bv94/mapcom94cartogram.RDS")
st_write(com,"bv94/mapcom94cartogram.geojson")
cir <- carto %>% group_by(codecir) %>% summarise(Inscrits=sum(Inscrits))
cir<-st_buffer(cir,20)
saveRDS(cir,"bv94/mapcir94cartogram.RDS")
st_write(cir,"bv94/mapcir94cartogram.geojson")
plot(cir$geometry,lwd=2)
