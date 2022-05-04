# Emile & Emma - Copyright 2022
# Packages
if (!require("splitstackshape")) install.packages("splitstackshape")
library(splitstackshape)

# Functions
distance = function(x1, y1, x2, y2){
  return(sqrt((x1 - x2)^2 + (y1 - y2)^2))
}


# Initialisation des jeux de données
freq_stations = read.csv("trafic-annuel-entrant-par-station-du-reseau-ferre-2021.csv", header = TRUE, sep = ";") # Fréquentations des staions
freq_stations = freq_stations[freq_stations$Reseau == "Metro" & freq_stations$Ville == "Paris", ]
freq_stations = freq_stations[, c("Rang", "Station", "Trafic", "Arrondissement.pour.Paris")]
freq_stations$Station = gsub("-", " ", freq_stations$Station, fixed = TRUE)
freq_stations$Station = gsub("'", " ", freq_stations$Station, fixed = TRUE)
freq_stations$Station = gsub(".", "", freq_stations$Station, fixed = TRUE)

local_stations = read.csv("emplacement-des-gares-idf.csv", header = TRUE, sep = ";") # Localisation des stations
local_stations = local_stations[local_stations$mode == "METRO", ]
local_stations = local_stations[, c("nom", "Geo.Point")]
local_stations <- cSplit(local_stations, "Geo.Point", ",")
local_stations$nom = toupper(gsub("-", " ", local_stations$nom, fixed = TRUE))
local_stations$nom = gsub("'", " ", local_stations$nom, fixed = TRUE)
local_stations$nom = gsub(".", "", local_stations$nom, fixed = TRUE)

freq_stations$GeoPoint_1 <- local_stations$Geo.Point_1[match(freq_stations$Station, local_stations$nom)]
freq_stations$GeoPoint_2 <- local_stations$Geo.Point_2[match(freq_stations$Station, local_stations$nom)]
freq_stations = freq_stations[!is.na(freq_stations$GeoPoint_1), ]

compteurs_velos = read.csv("comptage-velo-donnees-compteurs.csv", header = TRUE, sep = ",") # Fréquentation de certains points en vélo
compteurs_velos <- cSplit(compteurs_velos, "Coordonnees", ",")

# Fusion en un maxi jeu de données
for(compteur in compteurs_velos$Compteur){
  x = compteurs_velos$Coordonnees_1[compteurs_velos$Compteur == compteur]
  y = compteurs_velos$Coordonnees_2[compteurs_velos$Compteur == compteur]
  
  compteurs_velos$StationProche[compteurs_velos$Compteur == compteur] <- freq_stations$Station[which.min(distance(x, y, freq_stations$GeoPoint_1, freq_stations$GeoPoint_2))]
}
compteurs_velos$CompteurStation <- freq_stations$Trafic[match(compteurs_velos$StationProche, freq_stations$Station)]

for(station in freq_stations$Station){
  x = freq_stations$GeoPoint_1[freq_stations$Station == station]
  y = freq_stations$GeoPoint_2[freq_stations$Station == station]
  
  freq_stations$CmptVeloID[freq_stations$Station == station] <- compteurs_velos$Compteur[which.min(distance(x, y, compteurs_velos$Coordonnees_1, compteurs_velos$Coordonnees_2))]
}
freq_stations$CmptVelo <- compteurs_velos$Total.sur.2021[match(freq_stations$CmptVeloID, compteurs_velos$Compteur)]

# Plots
plot(compteurs_velos$Total.sur.2021, compteurs_velos$CompteurStation)
