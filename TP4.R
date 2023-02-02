library(dplyr)
library(sf)
library(mapsf)
library(classInt)
library(leaflet)

#Exercice1
#1) Création du jeu de données
Pop_legales_2019 <- read_excel("Donnees/Pop_legales_2019.xlsx")
pop19<-Pop_legales_2019

#Toutes communes de code 75--- deviennent 75056
#Elles sont ensuite regroupées en 1 seule commune
#Dégroupement

pop19<-pop19 %>% 
mutate(COM=ifelse(substr(COM,1,2)=="75","75056",COM)
       ) %>% 
group_by(COM) %>% 
summarise(PMUN19=sum(PMUN19),.groups='drop')
str(pop19)

metro_sf<-st_read("fonds/commune_francemetro_2021.shp",options="ENCODING=WINDOWS-1252"
) %>%
  left_join(pop19,
  by=c('code'='COM')
  ) %>% 
  mutate(DENSITE = PMUN19/surf)

str(metro_sf)
  
#2) Distribution de la variable densité
summary(metro_sf$DENSITE)
hist(metro_sf$DENSITE,breaks=seq(0,30000,100))
#beaucoup de communes très peu denses : 75% avec une densité<95

#3) Carte choroplèthe
plot(metro_sf["DENSITE"],border=FALSE) 
#Carte peu explicite car palette de distribution continue

#4)Discrétisation de la densité : construire des classes d'équivalence (5 par défaut)
plot(metro_sf["DENSITE"],border=FALSE,breaks="quantile")
plot(metro_sf["DENSITE"],border=FALSE,breaks="jenks")
plot(metro_sf["DENSITE"],border=FALSE,breaks="pretty")
#Il vaut mieux faire les classes à la main

#5)Construction des classes
#a)
decoupage_quantile<-classIntervals(
  metro_sf$DENSITE,
  style="quantile",
  )


#b)
decoupage_breaks<-classIntervals(
  metro_sf$DENSITE,
  style="breaks",
  n=5
)


plot(decoupage_breaks)

#d)
metro_sf<-metro_sf %>% 
  mutate(
    DENSITE_cat=cut(DENSITE,
    breaks=c(0,40,162,1000,8000,27310),
    include.lowest=TRUE,
    right=FALSE,
    ordered_result=TRUE))

table(metro_sf$DENSITE_cat,useNA="always")

metro_sf %>% 
  ggplot()+
  geom_bar(aes(x=DENSITE_cat))

plot(metro_sf["DENSITE_cat"],border=FALSE)


