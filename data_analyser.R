# Emile & Emma - Copyright 2022

# Initialisation des jeux de données
freq_stations = read.csv("trafic-annuel-entrant-par-station-du-reseau-ferre-2021.csv", header = TRUE, sep = ";") # Fréquentations des staions
freq_stations = freq_stations[freq_stations$Reseau == "Metro" & freq_stations$Ville == "Paris", ]

local_stations = read.csv("emplacement-des-gares-idf.csv", header = TRUE, sep = ";") # Localisation des stations
local_stations = local_stations[local_stations$mode == "METRO", ]
