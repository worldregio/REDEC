library(dplyr)
library(sf)
library(ggplot2)

bv<-readRDS(file = "bv94/bv_mapdon.RDS") %>% st_transform(2154)
com<-readRDS(file = "data94/com94.RDS") %>% st_transform(2154)

points <- bv %>% filter(codecom %in% c("94071","94004") )%>% st_centroid()
lim <- com %>% filter(com_code %in% c("94071","94004") ) %>% st_union()

voronoi <- points %>% 
  st_geometry() %>% # to get sfc from sf
  st_union() %>% # to get a sfc of MULTIPOINT type
  st_voronoi(envelope = st_geometry(lim)) %>% # NC sized Voronoi polygon
  st_collection_extract(type = "POLYGON") %>% # a list of polygons
  st_sf() %>% # from list to sf object
  st_intersection(lim) %>% # cut to shape of NC state
  st_join(points) # put names back

# display the Voronoi polygons
ggplot() +
  geom_sf(data = voronoi, aes(fill = id_bv), alpha = .5) +
  geom_sf(data = lim, lwd = .75, fill = NA) + 
  geom_sf(data = points, color = "red", pch = 4, size = 2) +
  labs(fill = "id_bv")
