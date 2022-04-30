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

local_stations = read.csv("emplacement-des-gares-idf.csv", header = TRUE, sep = ";") # Localisation des stations
local_stations = local_stations[local_stations$mode == "METRO", ]
