#Chargement des tables
trajpro <- read.csv("trajpro.csv", sep = ";", header = TRUE)
indiv <- read.csv("indiv.csv", sep = ";", header = TRUE)

#Librairies
library(TraMineR)
library(dplyr)
library(tidyr)

#Drop avriables non indiquées et identifier chaque années 
trajpro_clean <- trajpro %>%
  drop_na(debproan, p_gan) %>%  # Supprime les lignes avec NA
  rowwise() %>%
  mutate(Années = list(seq(debproan, p_gan))) %>%
  unnest(Années)

head(trajpro_clean)

#Supprimer colonnes non utilisées
trajpro_clean <- trajpro_clean %>% select(-c(p_nlig, p_gan, debproag ,p_gage, debproan))

#Format wide
trajpro_wide <- trajpro_clean %>%
  pivot_wider(names_from = Années, values_from = p_gactiv)

#Trouver liste de doublon la plus grande
max_length <- max(sapply(trajpro_wide, function(col) max(sapply(col, length))))
print(max_length)

#Trouver un exemple de liste 14
found <- FALSE  # Indicateur pour arrêter la recherche dès qu'on trouve une cellule
for (col_name in names(trajpro_wide)) {
  for (i in seq_along(trajpro_wide[[col_name]])) {
    if (length(trajpro_wide[[col_name]][[i]]) == 14) {
      print(paste("Ligne:", i, "| Colonne:", col_name))
      found <- TRUE
      break
    }
  }
  if (found) break
}

#Trouver toutes les longueurs de listes
unique_lengths <- unique(unlist(lapply(trajpro_wide, function(col) lapply(col, length))))
print(unique_lengths)

#Nombre de fois où apparait une liste de longueur données
table(unlist(lapply(trajpro_wide, function(col) lapply(col, length))))

#Trouver un exemple de liste 4
found <- FALSE  # Indicateur pour arrêter la recherche dès qu'on trouve une cellule
for (col_name in names(trajpro_wide)) {
  for (i in seq_along(trajpro_wide[[col_name]])) {
    if (length(trajpro_wide[[col_name]][[i]]) == 4) {
      print(paste("Ligne:", i, "| Colonne:", col_name))
      found <- TRUE
      break
    }
  }
  if (found) break
}

# Identifier les lignes contenant des listes de taille 4, 5, 6 ou 14 
rows_to_extract <- apply(trajpro_wide, 1, function(row) {
  any(sapply(row, function(cell) length(cell) %in% c(4, 5, 6, 14)))
})
valeurs_bizarre <- trajpro_wide[rows_to_extract, ]
print(valeurs_bizarre)




