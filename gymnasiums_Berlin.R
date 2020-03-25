library(RCurl)
library(tidyverse)
library(scales)
library(XML)
library(rvest)
library(rlist)
library(DescTools)
library(rlang)

listeFull <- read_csv("listesGymnasien.csv") ;
liste <- filter(listeFull,type=="Gymnasium"|type=="Integrierte Sekundarschule") ;

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


url <- read_html("http://www.gymnasium-berlin.net/schulliste")

links <- url %>%
  html_nodes("table") %>% 
  # html_nodes("Liste der Gymnasien in Berlin") %>%
  html_nodes("a") %>% 
  html_attr("href")

links <- links[1:91]
url <- paste0("https://www.gymnasium-berlin.net",links)

#################################################################################### 
##                                                                                ##
##          des fonctions pour extraire les différentes variables                 ##
##                                                                                ##
####################################################################################

get_name <- function(x) {
    x %>% 
    html_nodes("#block-system-main") %>%
    html_nodes("h2") %>% 
    html_text()
}

get_stats <- function(x) {
  # sélectionne le paragraphe où il y a ces infos
  x <- x %>%
    html_nodes("p:nth-child(5)") %>% 
    html_text()
  # extrait le nombre de profs de ce paragraphe
  profs <-  x %>% str_extract("Lehrer:\\s\\d*") %>% 
    str_remove("Lehrer: ")
  # extrait le nombre d'élèves
  effectif <- x %>% str_extract("Schüler: \\d*") %>% 
    str_remove("Schüler: ") 
  # extrait le numéro de l'école
  no <- x %>%  str_extract("\\d\\d\\D\\d\\d")
  # extrait le quartier
  quartier <- x %>%  str_extract("Bezirk: .*Ortsteil") %>% 
    str_remove("Bezirk: ") %>% 
    str_remove("Ortsteil")
  # extrait le sous-quartier
  sousquartier <- x %>%  str_extract("Ortsteil: .*Schulnummer") %>% 
    str_remove("Ortsteil: ") %>% 
    str_remove("Schulnummer")
  # renvoie l'ensemble de ces indicateurs
  c(profs,effectif,no,quartier,sousquartier)
}

get_results <- function(x) {
  
  # on extrait les moyennes à l'abitur
  x1 <-x %>% 
    html_nodes(".field-name-abitur-schule") %>%
    html_text() %>% 
    str_extract_all("[1234]\\.\\d\\d") %>% 
    unlist()
  
  # on extrait le pourcentage de réussite
  x2 <- x %>% 
    html_nodes(".field-name-abitur-schule") %>%
    html_text() %>% 
    str_extract_all("\\d{2,3}") %>% 
    unlist()
  x2 <- x2[c(3,6,9,12,15,18)]
  
    # on calcule la moyenne abitur lors des 5 dernières années
  abiNotes <- x1
  AbiMoyenne2013_18 <- x1 %>% 
    as.numeric() %>%
    mean()
    
  # on calcule la moyenne de réussite au bac
  abiErfolg <- x2 %>%
    as.numeric() %>%
    mean()
  
  c(AbiMoyenne2013_18, abiErfolg,abiNotes)
}


get_options <- function(x) {

    # sélectionne le paragraphe où il y a ces infos
    x <- x %>%
    html_nodes("p:nth-child(11)") %>%
    html_text()
  
    # ajoute un 1 ou 0 selon que la matière est disponible comme "Leistungskurse" ou non  
    
   grecAncien <- ifelse(grepl("Alt-Griechisch",x),"1","0")
   bio <- ifelse(grepl("Biologie",x),"1","0")
   chimie <- ifelse(grepl("Chemie",x),"1","0")
   allemand <- ifelse(grepl("Deutsch",x),"1","0")
   anglais <- ifelse(grepl("Englisch",x),"1","0")
   geo <- ifelse(grepl("Erdkunde",x),"1","0")
   français <- ifelse(grepl("Französisch",x),"1","0")
   histoire <- ifelse(grepl("Geschichte",x),"1","0")
   informatique <- ifelse(grepl("Informatik",x),"1","0")
   italien <- ifelse(grepl("Italienisch",x),"1","0")
   art <- ifelse(grepl("Kunst",x),"1","0")
   latin <- ifelse(grepl("Latein",x),"1","0")
   mathematiques <- ifelse(grepl("Mathematik",x),"1","0")
   musique <- ifelse(grepl("Musik",x),"1","0")
   grec <- ifelse(grepl("Neu-Griechisch",x),"1","0")
   philosophie <- ifelse(grepl("Philosophie",x),"1","0")
   physique <- ifelse(grepl("Physik",x),"1","0")
   politique <- ifelse(grepl("Politische Weltkunde",x),"1","0")
   droit <- ifelse(grepl("Recht",x),"1","0")
   sociologie <- ifelse(grepl("Sozialwissenschaften",x),"1","0")
   espagnol <- ifelse(grepl("Spanisch",x),"1","0")
   infoApplique <- ifelse(grepl("Wirtschaftsinformatik",x),"1","0")
   eco <- ifelse(grepl("Wirtschaftswissenschaft",x),"1","0")
   LK <- c(grecAncien,bio,chimie,allemand,anglais,geo,français,histoire,informatique,italien,art,
     latin,mathematiques,musique,grec,philosophie,physique,politique,droit,sociologie,
     espagnol,infoApplique,eco)
   LK
}

####################################################################################
##                                                                                ##
##   une fonction pour les appeler toutes et unir les résultats dans un tableau   ##
##                                                                                ##
#################################################################################### 


get_data_table <- function(x) {

names <- get_name(x)
stats <- get_stats(x)
results <- get_results(x)
LK <- get_options(x)

tibble(
  name = names,
  
  no = stats[3],
  effectif = stats[2],
  profs = stats[1],
  quartier = stats[4],
  sousquartier = stats[5],
  
  AbiMoyenne2013_18 = results[1],
  abiReussite = results[2],
  abiNote2013 = results[length(results)-5],
  abiNote2014 = results[length(results)-4],
  abiNote2015 = results[length(results)-3],
  abiNote2016 = results[length(results)-2],
  abiNote2017 = results[length(results)-1],
  abiNote2018 = results[length(results)],
  
  
  grecAncien = LK[1],
  bio = LK[2],
  chimie = LK[3],
  allemand = LK[4],
  anglais = LK[5],
  geo = LK[6],
  français = LK[7],
  histoire = LK[8],
  informatique = LK[9],
  italien = LK[10],
  art = LK[11],
  latin = LK[12],
  mathematiques = LK[13],
  musique = LK[14],
  grec = LK[15],
  philosophie = LK[16],
  physique = LK[17],
  politique = LK[18],
  droit = LK[19],
  sociologie = LK[20],
  espagnol = LK[21],
  infoApplique = LK[22],
  eco = LK[23]
  
  type = "Gymnasium"
)
}




#################################################################################### 
##                                                                                ##
##   on ajoute à ça une fonction pour charger les pages via read_html()           ##
##                                                                                ##
#################################################################################### 


get_data_from_url <- function (x) {
  x <- read_html(x)
  get_data_table(x)
}


#################################################################################### 
##                                                                                ##
##   extraction des données de tous les URL des gymnasiums de Berlin,             ##
##   d'abord dans un fichier tsv puis dans une variabe "data"                     ##
##                                                                                ##
#################################################################################### 


url[-37] %>%
  map(get_data_from_url) %>% 
  bind_rows() %>%                           
  # Write a tab-separated file
  write_tsv("test2.tsv")

data<-read_tsv("test2.tsv")


################################################################################################ 
##                                                                          		      ##
##        aller chercher les taux de "personnes avec b migratoire"		      ##
##                                                                          		      ##
################################################################################################ 


code <- read_html("https://www.gymnasium-berlin.net/migrationshintergrund")

#
#   On crée un tableau qu'on va remplir ensuite
#

TxMigration <- data.frame(name=character(),
                 tx=numeric(),
                 stringsAsFactors=FALSE) 

# crée un tibble à avec les noms des Gymnasiums

name <- code %>% 
  html_nodes("#content") %>% 
  html_nodes(".views-field-title") %>%
  html_nodes("a") %>% 
  html_text() %>% 
  as.tibble()

# crée un tibble avec les taux de migration

tx <-  code %>% 
    html_nodes("td.views-align-center") %>% 
    html_text() %>%
    str_extract_all("\\d{1,2},\\d") %>% 
    unlist() %>% 
    as.tibble()

# On colle ça dans le tibble vide précédemment créé

txMigration <- cbind(name,tx)

names(txMigration) <- c("name","tx")

# merge(as.data.frame(data),as.data.frame(TxMigration),by.x = "name",by.y = "name")

data<-left_join(data,txMigration)

data %>% write_tsv("Gymnasium.tsv")

# créer une nouvelle colonne pour la moyenne des abi seulement de 2015 à 2018

data$AbiMoyenne2015_18 = (data$abiNote2015+data$abiNote2016+ 
                           data$abiNote2017+data$abiNote2018) / 4

data %>% write_tsv("Gymnasium.tsv")
