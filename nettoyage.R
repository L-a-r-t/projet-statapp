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



