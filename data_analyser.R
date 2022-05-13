# Emile & Emma - Copyright 2022
# Packages
if (!require("splitstackshape")) install.packages("splitstackshape")
library(splitstackshape)
if (!require("psych")) install.packages("psych")
library(psych)
if (!require("vioplot")) install.packages("vioplot")
library(vioplot)
if (!require("corrplot")) install.packages("corrplot")
library(corrplot)

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

compteurs_voitures = read.csv("comptages-routiers-permanents_cleaned.csv", header = TRUE, sep = ",") # Fréquentations de points en voiture

# Fusion en un maxi jeu de données
for(compteur in compteurs_velos$Compteur){ # Stations et compteurs voiture les plus proche de chaque compteur vélo
  x = compteurs_velos$Coordonnees_1[compteurs_velos$Compteur == compteur]
  y = compteurs_velos$Coordonnees_2[compteurs_velos$Compteur == compteur]
  
  compteurs_velos$StationProche[compteurs_velos$Compteur == compteur] <- freq_stations$Station[which.min(distance(x, y, freq_stations$GeoPoint_1, freq_stations$GeoPoint_2))]
  compteurs_velos$CmptVoitProche[compteurs_velos$Compteur == compteur] <- compteurs_voitures$ï..Libelle[which.min(distance(x, y, compteurs_voitures$geo_point.1, compteurs_voitures$geo_point.2))]
}
compteurs_velos$CompteurStation <- freq_stations$Trafic[match(compteurs_velos$StationProche, freq_stations$Station)]
compteurs_velos$CompteurVoiture <- compteurs_voitures$Count[match(compteurs_velos$CmptVoitProche, compteurs_voitures$ï..Libelle)]

for(station in freq_stations$Station){ # Compteur voiture le plus proche de chaque station
  x = freq_stations$GeoPoint_1[freq_stations$Station == station]
  y = freq_stations$GeoPoint_2[freq_stations$Station == station]
  
  freq_stations$CmptVoitureID[freq_stations$Station == station] <- compteurs_voitures$ï..Libelle[which.min(distance(x, y, compteurs_voitures$geo_point.1, compteurs_voitures$geo_point.2))]
}
freq_stations$CmptVoiture <- compteurs_voitures$Count[match(freq_stations$CmptVoitureID, compteurs_voitures$ï..Libelle)]

# Raccourcis
cv = data.frame(velo=compteurs_velos$Total.sur.2021, metro=compteurs_velos$CompteurStation, voit=compteurs_velos$CompteurVoiture) # Compteurs vélos
cm = data.frame(metro=freq_stations$Trafic, voit=freq_stations$CmptVoiture) # Compteurs métro

## EXECUTER JUSQU'ICI ##

# Boites à moustache
boxplot(
  cv$velo * 5,
  cv$metro,
  cv$voit,
  main="Répartition des données",
  names=c("Vélos (x5)", "Métros", "Voitures"),
  xlab="Traffic par point",
  col=c("cyan", "#00AA91", "red")
)
vioplot(
  cv$velo * 5,
  cv$metro,
  cv$voit,
  main="Répartition des données",
  names=c("Vélos (x5)", "Métros", "Voitures"),
  xlab="Traffic par point",
  col=c("cyan", "#00AA91", "red")
)

# Nuages de points
cv = cv[cv$metro < 10000000, ]
cv = cv[cv$velo < 1500000, ]
plot(cv$velo, cv$metro) # Nombre d'entrées dans les stations en fonction du nombre de cyclistes

plot(cv$velo, cv$voit) # Nombre de véhicules en fonction du nombre de cyclistes

cm = cm[cm$metro < 10000000, ]
cm = cm[cm$voit < 10000000, ]
plot(cm$metro, cm$voit) # Nombre de voiture en fonction du nombre de véhicules

# Calcul des corrélations
cor(cv$velo, cv$metro) # Calcul de corrélation  Métro -> vélo
cor(cv$velo, cv$voite) # Calcul de corrélation  Voiture -> vélo
cor(cv) # Matrice de corrélation

cor(cm) # Calcul de corrélation Voiture -> métro

corrplot(cor(cv), method = "circle") # Corrélogramme
