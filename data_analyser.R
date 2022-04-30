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
freq_stations$Station = sub("-", " ", freq_stations$Station)
freq_stations$Station = sub("'", " ", freq_stations$Station)

local_stations = read.csv("emplacement-des-gares-idf.csv", header = TRUE, sep = ";") # Localisation des stations
local_stations = local_stations[local_stations$mode == "METRO", ]
local_stations = local_stations[, c("nom", "Geo.Point")]
local_stations <- cSplit(local_stations, "Geo.Point", ",")
local_stations$nom = toupper(sub("-", " ", local_stations$nom))
local_stations$nom = sub("'", " ", local_stations$nom)
local_stations$nom = sub(".", "", local_stations$nom, fixed = TRUE)

compteurs_velos = read.csv("comptage-velo-donnees-compteurs.csv", header = TRUE, sep = ",") # Fréquentation de certains points en vélo
compteurs_velos <- cSplit(compteurs_velos, "Coordonnees", ",")
