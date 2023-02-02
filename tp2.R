#####ex 1

#install.packages("sf")
#install.packages("dplyr")
library(sf)
library(dplyr)
#q1
#Charger le dossier zip avec les 4 fichiers correspondants
com_frm21=st_read("fonds/commune_francemetro_2021.shp",options = "ENCODING=WINDOWS-1252")
#q2
summary(com_frm21)
str(com_frm21)
#q3
com_frm21[1:10,]

#q4 projection de la table
st_crs(com_frm21)  #Lambert-93

#q5 à 8
com_bret = com_frm21 %>%  
  filter(reg=="53") %>% 
  select(code, libelle, epc, dep, surf) 

plot(com_bret)

plot(st_geometry(com_bret)) #affichage carte

str(com_bret)  # toujours de type sf

#q9  ajout d'une variable 
com_bret = com_bret %>%  mutate(surf2=st_area(geometry)) 
str(com_bret)

#q10 et 11 convertion d'unité 
com_bret = com_bret %>% mutate(surf3 = units::set_units(surf2, "km^2"))
str(com_bret)

#q12 fusion des communes en départements
# groupby dep
# summarise
#st_union


#reg76 <- st_union(occ)
#dep76 <- aggregate(occ[,"POPULATION"], by = list(occ$INSEE_DEP), sum)
#plot(st_geometry(occ), col = "lightblue1", border = "white", lwd = .5)
#plot(st_geometry(dep76), col = NA, border = "lightblue2", lwd = 1, add = TRUE)
#plot(reg76, col = NA, border = "lightblue3", lwd = 2, add = TRUE)

depts_bretagne<- com_bret %>%
  group_by(dep) %>%
  summarise(
    surf = sum(surf)
    )
str(depts_bretagne)
plot(depts_bretagne) %>% st_geometry()

#13 par union des géométries

com_bret %>% st_union() %>% st_geometry() %>% plot()

depts_bretagne_geo <- com_bret %>% 
  group_by(dep) %>% 
  summarise(
    geometry = st_union(geometry)
  )
plot(depts_bretagne_geo)

#14 Centroïdes



  



