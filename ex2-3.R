#Exercice 2
#1)
library(DBI)
source(file = "TP5/connexion_db.R")
conn<-connecter()
DBI::dbListTables(conn)

#2)Variables de la table popnaiss
DBI::dbListFields(conn,"popnaiss_com")

#3)Copie de la table accessible à distance
popnaiss1 <- dbSendQuery(conn, "SELECT * FROM popnaiss_com")
str(popnaiss1)
dbfetch(popnaiss1)

#4)Id mais table physiquement présente dans l'environnement
popnaiss2 <- dbGetQuery(conn, "SELECT * FROM popnaiss_com")

#5)Informations sur la ville de Rennes
popnaiss3 <- dbGetQuery(conn, "SELECT * FROM popnaiss_com where codgeo='35238'")

#6)Jointure
popnaiss4 <- dbGetQuery(conn, "SELECT * FROM bpe21_metro INNER JOIN popnaiss_com ON popnaiss_com.codgeo=bpe21_metro.depcom where codgeo='35047'")
str(popnaiss4)

#7)a)
library(dplyr)
library(dbplyr)

# Connexion à la table popnaiss
popnaiss<-tbl(conn,"popnaiss_com")
str(popnaiss) # ! ce n'est pas un data.frame


# Reprise de la question 5
popnaiss %>% 
  filter(codgeo=="35047") %>% 
  show_query()

pop_bruz <- popnaiss %>% 
  filter(codgeo=="35047") %>% 
  collect()
str(pop_bruz)

#b)
pbe_pop_bruz<-tbl(conn,"popnaiss_com") %>% 
  filter(codgeo=="35047") %>% 
  inner_join(
    tbl(conn,"bpe21_metro"),
     by = c("codgeo"="depcom")
  ) %>% 
  collect()
pbe_pop_bruz %>% str()


#Exercice 3

library(sf)
library(dplyr)

"1)"
bpe_dep50<-tbl(conn,"bpe21_metro") %>% 
  filter(dep=="50") %>% 
  select( "id", "depcom","dom", "sdom", "typequ", "geometry") %>% 
  collect()
str(bpe_dep50)
#On obtient un dataframe

#2)
bpe_dep50_bis<-st_read(conn,query="SELECT ID,DEPCOM,DOM, SDOM, TYPEQU, GEOMETRY
                   FROM bpe21_metro WHERE DEP='50';")
str(bpe_dep50_bis) 
#On obtient un sf dataframe

#3)Système de projection associé
dbGetQuery(conn, "SELECT DISTINCT(ST_SRID(geometry)) FROM bpe21_04;")
st_crs(bpe_dep50_bis)

#4)Nombre de maternités par région
maternites<-dbGetQuery(conn, statement= "SELECT REG, COUNT(id) FROM bpe21_metro
                       WHERE TYPEQU='D107'GROUP BY REG ORDER BY COUNT(id);")

#5)Cinémas
#cinemas_bpe<-st_read(conn,query='SELECT * FROM bpe21_metro WHERE TYPEQU='F303';")

