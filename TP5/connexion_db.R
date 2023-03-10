# Informations requises pour la connexion à votre bdd personnelle
# Les valeurs ici sont mentionnées à titre indicatif:

name_database <- "defaultdb"
user_name <- "user-catpesin" # à modifier
password <- "nqlxnuskk5dyjp8z2ygg" # à modifier
url <- "postgresql-533660" # à modifier : conservez uniquement la partie de l'url entre les // et les :
port <- "5432" # partie de l'url après les :

# RQ: pour l'exercice, les informations sont écrites en clair sur le pgm. Il faudra, 
# en situation réelle de travail, veiller à ne pas diffuser ces informations,
# par exemple en stockant ces informations dans un autre fichier protégé.

# Fonction pour se connecter à la base de données ####
connecter <- function(){
  conn <- DBI::dbConnect(
    RPostgres::Postgres(),
    dbname=name_database,
    host=url, 
    user=user_name, 
    password=password, 
    port=port
  )
  return(conn)
}

