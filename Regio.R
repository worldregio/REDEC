library(sf)
library(dplyr)
library(sp)
library(spdep)
library(adespatial)
library(mapsf)


base <- readRDS("map/bv80/mapbv.RDS")
Amiens <- base %>% filter(comname=="Amiens")
Amcont<-Amiens %>% group_by(comcode) %>% summarise(ins=sum(ins))

pdf(file = "Amiens.pdf",width = 8, height=8)
par(mfrow=c(2,2))

###



mapdon <- Amiens

# Calcul de la matrice de contiguïté
mat_conti <- st_intersects(mapdon, mapdon, sparse = FALSE)
colnames(mat_conti) <- mapdon$bvocode
rownames(mat_conti) <- mapdon$bvocode

# Suppression de la moitié de la matrice (et de la diagonale)
mat_conti[lower.tri(mat_conti, diag = TRUE)] <- FALSE

# Construction d'un tableau de lien (i, j) de contiguïté
reg_link_contig <- as.data.frame.table(mat_conti, responseName = "contig") |> 
  filter(contig == TRUE)

# Création de la couche géographique de liens
reg_links_contig  <- mf_get_links(x = mapdon, 
                                  df = reg_link_contig, 
                                  x_id = "bvocode", 
                                  df_id = c("Var1","Var2"))




# Graphe de voisinage
sel <- base %>% filter(comcode %in% mapdon$comcode) %>% group_by(comname)%>%
  summarise() %>% st_buffer(10)

mf_map(mapdon, type="base", col="gray90", border=NA)
#mf_map(sel, type="base", var="comcode", add=T, col=NA, lwd=2)

mf_map(reg_links_contig , col = "black", add = TRUE, lwd=0.4)
#mf_label(sel, var = "comname", cex = 0.8, col = "blue3", halo = TRUE, bg = "white")
mf_prop_typo(mapdon, var=c("ins","top"), inches=0.07, leg_title = c("nb. inscrits", "Pres. elect. 2022"))
mf_map(Amcont, type="base",col=NA,border="red", ad=T)
mf_layout("Commune d'Amiens", credits = "", arrow=F)














mapdon <- base %>% filter(circode=="8001")

# Calcul de la matrice de contiguïté
mat_conti <- st_intersects(mapdon, mapdon, sparse = FALSE)
colnames(mat_conti) <- mapdon$bvocode
rownames(mat_conti) <- mapdon$bvocode

# Suppression de la moitié de la matrice (et de la diagonale)
mat_conti[lower.tri(mat_conti, diag = TRUE)] <- FALSE

# Construction d'un tableau de lien (i, j) de contiguïté
reg_link_contig <- as.data.frame.table(mat_conti, responseName = "contig") |> 
  filter(contig == TRUE)

# Création de la couche géographique de liens
reg_links_contig  <- mf_get_links(x = mapdon, 
                                  df = reg_link_contig, 
                                  x_id = "bvocode", 
                                  df_id = c("Var1","Var2"))




# Graphe de voisinage
sel <- base %>% filter(comcode %in% mapdon$comcode) %>% group_by(comname)%>%
  summarise() %>% st_buffer(10)

mf_map(mapdon, type="base", col="gray90", border=NA)
#mf_map(sel, type="base", var="comcode", add=T, col=NA, lwd=2)

mf_map(reg_links_contig , col = "black", add = TRUE, lwd=0.4)
#mf_label(sel, var = "comname", cex = 0.8, col = "blue3", halo = TRUE, bg = "white")
mf_prop_typo(mapdon, var=c("ins","top"), inches=0.07, leg_title = c("nb. inscrits", "Pres. elect. 2022"))
mf_map(Amcont, type="base",col=NA,border="red", add=T)
mf_layout("Somme : 1ere circonscription", credits = "", arrow=F)





mapdon <- base %>% filter(circode=="8002")

# Calcul de la matrice de contiguïté
mat_conti <- st_intersects(mapdon, mapdon, sparse = FALSE)
colnames(mat_conti) <- mapdon$bvocode
rownames(mat_conti) <- mapdon$bvocode

# Suppression de la moitié de la matrice (et de la diagonale)
mat_conti[lower.tri(mat_conti, diag = TRUE)] <- FALSE

# Construction d'un tableau de lien (i, j) de contiguïté
reg_link_contig <- as.data.frame.table(mat_conti, responseName = "contig") |> 
  filter(contig == TRUE)

# Création de la couche géographique de liens
reg_links_contig  <- mf_get_links(x = mapdon, 
                                  df = reg_link_contig, 
                                  x_id = "bvocode", 
                                  df_id = c("Var1","Var2"))




# Graphe de voisinage
sel <- base %>% filter(comcode %in% mapdon$comcode) %>% group_by(comname)%>%
  summarise() %>% st_buffer(10)

mf_map(mapdon, type="base", col="gray90", border=NA)
#mf_map(sel, type="base", var="comcode", add=T, col=NA, lwd=2)

mf_map(reg_links_contig , col = "black", add = TRUE, lwd=0.4)
#mf_label(sel, var = "comname", cex = 0.8, col = "blue3", halo = TRUE, bg = "white")
mf_prop_typo(mapdon, var=c("ins","top"), inches=0.07, leg_title = c("nb. inscrits", "Pres. elect. 2022"))
mf_map(Amcont, type="base",col=NA,border="red", add=T)
mf_layout("Somme : 2e circonscription", credits = "", arrow=F)






mapdon <- base %>% filter(circode=="8004")

# Calcul de la matrice de contiguïté
mat_conti <- st_intersects(mapdon, mapdon, sparse = FALSE)
colnames(mat_conti) <- mapdon$bvocode
rownames(mat_conti) <- mapdon$bvocode

# Suppression de la moitié de la matrice (et de la diagonale)
mat_conti[lower.tri(mat_conti, diag = TRUE)] <- FALSE

# Construction d'un tableau de lien (i, j) de contiguïté
reg_link_contig <- as.data.frame.table(mat_conti, responseName = "contig") |> 
  filter(contig == TRUE)

# Création de la couche géographique de liens
reg_links_contig  <- mf_get_links(x = mapdon, 
                                  df = reg_link_contig, 
                                  x_id = "bvocode", 
                                  df_id = c("Var1","Var2"))




# Graphe de voisinage
sel <- base %>% filter(comcode %in% mapdon$comcode) %>% group_by(comname)%>%
  summarise() %>% st_buffer(10)

mf_map(mapdon, type="base", col="gray90", border=NA)
#mf_map(sel, type="base", var="comcode", add=T, col=NA, lwd=2)

mf_map(reg_links_contig , col = "black", add = TRUE, lwd=0.4)
#mf_label(sel, var = "comname", cex = 0.8, col = "blue3", halo = TRUE, bg = "white")
mf_prop_typo(mapdon, var=c("ins","top"), inches=0.07, leg_title = c("nb. inscrits", "Pres. elect. 2022"))
mf_map(Amcont, type="base",col=NA,border="red", add=T)
mf_layout("Somme : 4e circonscription", credits = "", arrow=F)

dev.off()

