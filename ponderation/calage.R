## Chargement des données 
# load(here("data", "data.RData"))
# source(here("import", "recodages_individus.R"))

###### Téléchargement du package icarus
library(icarus)

# matrice des marges de calage

mar1 <- c("sexe", 2, 52.1, 47.9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 100)
mar2 <- c("PCS_mod", 19, 1.6, 1.4, 1.3, 3.0, 4.8, 5.5, 4.5, 2.3, 1.0, 5.7, 3.2, 2.6, 3.7, 6.9, 4.7, 2.0, 7.4, 14.7, 23.7, 100)
mar3 <- c("regparis", 2, 81, 19, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 100)
mar4 <- c("agec", 6, 7.1, 4.2, 16, 14, 15, 43.7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 100)
marges <- rbind(mar1, mar2, mar3, mar4)
rm(mar1, mar2, mar3, mar4)

# ajout d'une variable poids = à 1
i <- mutate(
  i, 
  poids = 1)

###### Vague 2019
# Pour chaque vague
j <- vector("list", length = 7)
for(v in paste0("p", 13L:19L)){
  # réduire le dataset a cette vague
  df <- filter(i, vague == v)
  # calculer la longueur
  N <- nrow(df)
  # Calibrer
  cal <- calibration(data = df, marginMatrix = marges,
                     colWeights = "poids", method = "logit",
                     bounds = c(0.3,2.8), description = TRUE, pct = TRUE, popTotal = N)
  # stocker les résultats (on remplace poids par cal)
  j[[v]] <- mutate(df, poids = cal)
}
# recombiner les résultats
i <- bind_rows(j)

rm(j, marges, df)
