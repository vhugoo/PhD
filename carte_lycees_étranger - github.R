#
#   carte des lycées français à l'étranger
#

library(RCurl)
library(tidyverse)
library(scales)
library(XML)
library(rvest)
library(rlist)
library(classInt)
library(DescTools)
library(RColorBrewer)
library(maptools)
library(rworldmap)
library(ggmap)
library(mapproj)

# 
# si besoin, installer les packages suivants :
# 
# install.packages("tidyverse")
# install.packages("RCurl")
# install.packages("scales")
# install.packages("rvest")
# install.packages("rlist")
# install.packages("classInt")
# install.packages("DescTools")
# install.packages("RColorBrewer")
# install.packages("rworldmap")
# install.packages("ggmap")
# install.packages("mapproj")
# install.packages("maptools")
# 


#
#  lister les url des pages web à scrapper
#

url <- read_html("https://www.aefe.fr/reseau-scolaire-mondial/rechercher-un-etablissement")

links <- url %>%
  html_nodes("#block-system-main") %>%
  html_nodes("a") %>%
  html_attr("href")

links <- paste0("https://www.aefe.fr",links)

links <- links[-1]

# t <- read_html(links[78])
# t %>%
#   html_nodes("#block-system-main") %>%
#   html_node("h2") %>%
#   html_text() %>%
#   str_split(", ") %>%
#   unlist()

t <- read_html(links[119])

t %>% html_nodes(".label") %>%
  html_attr("href") %>%
  str_remove("http://maps.google.fr\\?q=") %>% 
  str_split("%2C%20") %>% 
  unlist()

#
#  fonctions de scrappage des données (qui sera à appliquer à chacune des pages web listées)
#

get_stats <- function(x) {
  
  # nom
  x0 <- x %>%
    html_nodes("#block-system-main") %>%
    html_node("h1") %>% 
    html_text()
  
  # lien.AEFE
  x_t <- x %>% html_nodes(".statut_etablissement")
  if (grepl("partenaire", x_t)) {
    x1 <- "1"
  }
  if (grepl("conventionne", x_t)) {
    x1 <- "2"
  }
  if (grepl("gestion-directe", x_t)) {
    x1 <- "3"
  }
  
  # ville & pays
  x_t2 <- x %>%
    html_nodes("#block-system-main") %>%
    html_node("h2") %>%
    html_text() %>%
    str_split(", ") %>%
    unlist()
  x2 <- x_t2[1]
  x3 <- x_t2[2]
  
  # url google google maps & coordonées pures
  x41 <- x %>% html_nodes(".label") %>%
    html_attr("href")
  x_t <- x %>% html_nodes(".label") %>%
    html_attr("href") %>%
    str_remove("http://maps.google.fr\\?q=") %>% 
    str_split("%2C%20") %>% 
    unlist()
  x421 <- x_t[1]
  x422 <- x_t[2]
  
  # effectifs & effectifs de français
  x_t <- x %>% html_nodes(".field-name-field-ets-effectif") %>%
    html_nodes(".even") %>%
    html_text() %>%
    str_extract_all("\\d+") %>%
    unlist()
  x51 <- x_t[1]
  x52 <- x_t[2]
  
  # homologation.lycee
  x_t <- x %>% html_nodes("tr:last-child") %>%
    html_nodes("td:nth-child(2)") %>%
    html_text
  x6 <- ifelse(grepl("oui", x_t), "1", "0")
  
  
  # mission.laïque
  x7 <- ifelse(grepl("block_mlf clearfix", x), "1", "0")
  
  c(x0, x1, x2, x3, x41, x421, x422, x51, x52, x6, x7)
}


#
#   on ajoute à la fonction de scrappage une fonction qui agrège les données dans un tableau
#

get_data_table <- function(x) {
  # id puis nb élèves puis nb profs puis quartier puis sous-quartier puis moyenne des notes Abi
  
  stats <- get_stats(x)
  
  tibble (
    nom = stats[1],
    lien.AEFE = stats[2],
    ville = stats[3],
    pays = stats[4],
    url.google = stats[5],
    coord_x = stats[6],
    coord_y = stats[7],
    effectif = stats[8],
    effectif.fr = stats[9],
    homolog.lycees = stats[10],
    mission = stats[11]
  )
 
}

#
# application de la fonction crée juste avant à l'ensemble des pages web listées
#


get_data <- function (x) {
  download.file(x, destfile = 'whatever.html')
  x <- read_html('whatever.html')
  get_data_table(x)
}


#
# assemblage dans un tableau de toutes les données
#

links %>%
  purrr::map(get_data) %>% 
  bind_rows() %>%                           
  # Write a tab-separated file
  write_tsv("LF2.tsv")

# read_html(links[1])

data<-read_tsv("LF2-clean.tsv")

dim(data)

data <- data %>% filter(effectif>10)

summary(data$effectif)

data <- data %>% group_by(pays) %>% mutate(effectifp = sum(effectif)) %>% ungroup()
data <- data %>% group_by(pays) %>% mutate(effectifp.fr = sum(effectif.fr)) %>%  ungroup()
dataPays <- tibble(pays=data$pays,effectif = data$effectifp, effectif.fr = data$effectifp.fr ) %>% distinct()
dataPays <- dataPays %>%  mutate(taux.fr = effectif.fr/effectif)
write_csv(dataPays,"lf_pays.csv")

table(dataMap$lien.AEFE,dataMap$tx.fr.c)

dataMap[dataMap$lien.AEFE == "3" & dataMap$tx.fr.c == "< 1", ] %>% View()

dataMap <- data %>% 
  filter(effectif>100) %>% 
  mutate (tx.fr = effectif.fr/effectif) %>%  
  mutate (tx.fr.c =
            ifelse(tx.fr<0.50,"< 0,50",
                   ifelse(tx.fr<0.70,"< 0,70","< 1")))

table(dataMap$tx.fr.c)
summary(dataMap$tx.fr)

#
#  réalisation de la carte
#


#  newmap <- getMap(resolution = "low")
#  points(data$coord_y, data$coord_x, col = "red", cex = .6)
# 
# map <- get_map(source="osm")
# 
# ggmap(map)
# 
# world_map <- map_data("world")


### trace le fond de carte

p <- ggplot() + coord_fixed() +
  xlab("") + ylab("")

base_world_messy <- p + geom_polygon(data=world_map, aes(x=long, y=lat, group=group), 
                                     colour="black", fill="grey98", size = 0.05)


### supprime l'arrière-plan (prévu pour des graphiques classiques)

cleanup <-  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill = 'white', colour = 'white'), 
        axis.line = element_line(colour = "white"), 
        # legend.position="none",
        # legend.title="effectif",
        axis.ticks=element_blank(), axis.text.x=element_blank(),
        axis.text.y=element_blank())

base_world <- base_world_messy + cleanup


### dessine la carte des effectifs

map_data <- 
  base_world2 +
  geom_point(data=dataMap, 
             aes(x=coord_y, y=coord_x, size = effectif),
             # colour="firebrick4", 
             # fill="firebrick2",
             # pch=21,
             alpha=I(0.7)
             # show.legend = T
  ) +
  ggtitle("Effectifs des lycées français dans le monde") +
  theme (plot.title = element_text(hjust = 0.5))


### dessine la carte des effectifs et du taux de Français

map_data <- 
  base_world2 +
  geom_point(data=dataMap, 
             aes(x=coord_y, y=coord_x, size = effectif, colour = tx.fr.c),
             # colour="firebrick4", 
             # fill="firebrick2",
             # pch=21,
             alpha=I(0.7)
             # show.legend = T
             ) +
  scale_colour_discrete(drop=TRUE,
                        limits=levels(dataMap$tx.fr.c),
                        aesthetics = "fill") +
  ggtitle("Effectifs des lycées français dans le monde") +
  theme (plot.title = element_text(hjust = 0.5))


#
# la même mais zoomée sur l'Europe
#

dataMapEurope <- subset(dataMap, dataMap$coord_y>-20 &
                          dataMap$coord_y<40 &
                          dataMap$coord_x>20 &
                          dataMap$coord_x<71)

europe_map <- subset(world_map, world_map$long>-20 &
                       world_map$long<40 &
                       world_map$lat>20 &
                       world_map$lat<71)

base_europe_messy <- p + geom_polygon(data=europe_map, aes(x=long, y=lat, group=group), 
                                      colour="black", fill="grey99", size = 0.05)



base_europe <- base_europe_messy + cleanup

map_data_europe <- 
  base_europe +
  geom_point(data=dataMapEurope, 
             aes(x=coord_y, y=coord_x, size = effectif, colour = tx.fr.c),
             # colour="firebrick4", 
             # fill="firebrick2",
             # pch=21,
             alpha=I(0.7)
             # show.legend = T
  ) +
    scale_colour_discrete(drop=T,
                        limits=levels(dataMapEurope$tx.fr.c),
                        aesthetics = "fill") +
  ggtitle("Effectifs des lycées français en Europe") +.
  theme (plot.title = element_text(hjust = 0.5))
