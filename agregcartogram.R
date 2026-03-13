library(sf)
library(dplyr)
mydep="bv38"

bv <- st_read(paste0("map/",mydep,"/mapbv_inscrits.geojson") )%>% st_transform(2154) %>% st_make_valid()

saveRDS(bv, paste0("map/",mydep,"/mapbv_inscrits.RDS") )

## Commune
com <- bv %>% group_by(comcode,comname) %>%
  summarise(ins=sum(ins),
            vot=sum(vot),
            exp=sum(exp),
            Art=sum(Art),
            Rou= sum(Rou),
            Mac = sum(Mac),
            Las = sum(Las),
            Lep = sum(Lep),
            Zem = sum(Zem),
            Mel = sum(Mel),
            Hid = sum(Hid),
            Jad = sum(Jad),
            Pec = sum(Pec),
            Pou = sum(Pou),
            Dup = sum(Dup)
  ) %>%
  mutate(top = case_when(Mac > Lep & Mac >Mel ~ "Macron",
                         Lep > Mac & Lep >Mel ~ "Le Pen",
                         .default = "Mélenchon"))
saveRDS(com, paste0("map/",mydep,"/mapcom_inscrits.RDS") )

can <- bv %>% group_by(cancode,canname) %>% 
  summarise(ins=sum(ins),
            vot=sum(vot),
            exp=sum(exp),
            Art=sum(Art),
            Rou= sum(Rou),
            Mac = sum(Mac),
            Las = sum(Las),
            Lep = sum(Lep),
            Zem = sum(Zem),
            Mel = sum(Mel),
            Hid = sum(Hid),
            Jad = sum(Jad),
            Pec = sum(Pec),
            Pou = sum(Pou),
            Dup = sum(Dup)
  ) %>%
  mutate(top = case_when(Mac > Lep & Mac >Mel ~ "Macron",
                         Lep > Mac & Lep >Mel ~ "Le Pen",
                         .default = "Mélenchon"))
saveRDS(can, paste0("map/",mydep,"/mapcan_inscrits.RDS") )

cir <- bv %>% group_by(circode,cirname) %>% 
  summarise(ins=sum(ins),
            vot=sum(vot),
            exp=sum(exp),
            Art=sum(Art),
            Rou= sum(Rou),
            Mac = sum(Mac),
            Las = sum(Las),
            Lep = sum(Lep),
            Zem = sum(Zem),
            Mel = sum(Mel),
            Hid = sum(Hid),
            Jad = sum(Jad),
            Pec = sum(Pec),
            Pou = sum(Pou),
            Dup = sum(Dup)
  ) %>%
  mutate(top = case_when(Mac > Lep & Mac >Mel ~ "Macron",
                         Lep > Mac & Lep >Mel ~ "Le Pen",
                         .default = "Mélenchon"))
saveRDS(cir, paste0("map/",mydep,"/mapcir_inscrits.RDS") )

