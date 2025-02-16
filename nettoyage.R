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

#Proposition format long
trajpro_long <- trajpro_clean %>% select(-c(p_gan, debproag ,p_gage, debproan))

head(trajpro_long)

# Tables test petites avec les situations types qui posent problème pour tester des fonctions de transformations 
df_initial <- data.frame(
  p_nlig = c(1, 2, 3, 1, 2, 3, 4),
  ident  = c(1, 1, 1, 2, 2, 2, 2),
  p_gactiv  = c("Formation", "Emploi", "Chômage", "Emploi", "Chômage", "Formation", "Emploi"),
  p_gan = c(2003, 2005, 2006, 2006, 2008, 2008, 2009),
  debproan = c(2000, 2003, 2005, 2004, 2006, 2008, 2008),
  debproag  = c(18, 21, 36, 18, 20, 22, 22),
  p_gage  = c(21, 36, 37, 20, 22, 22, 23)
)

df_final <- data.frame(
  ident = c(1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2),
  p_gactiv = c("Formation", "Formation", "Formation", "Emploi", "Emploi", "Chômage", "Chômage", "Emploi", "Emploi", "Chômage", "Chômage", "Formation", "Emploi"),
  année = c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2004, 2005, 2006, 2007, 2008, 2009),
  trans = c("-", "-","-","Formation -> Emploi","-","Emploi -> Chômage","-","-","-","Emploi -> Chômage","-","Chômage -> Formation","Formation -> Emploi")
)



#Ce que j'avais fait avant pour format wide mais je pense que le format long est plus approprié
#Supprimer colonnes non utilisées
trajpro_c <- trajpro_clean %>% select(-c(p_nlig, p_gan, debproag ,p_gage, debproan))

#Format wide
trajpro_wide <- trajpro_c %>%
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

#Gérer les listes à deux ou trois éléments
# Remplacer les listes de taille 2 par la dernière valeur
# Remplacer les listes de taille 3 par la valeur du milieu

trajpro_wide_modifie <- trajpro_wide  # Créer une copie de votre dataframe pour ne pas modifier l'original

# Appliquer les remplacements
trajpro_wide_modifie[] <- lapply(trajpro_wide, function(col) {
  sapply(col, function(cell) {
    # Si la cellule est une liste de taille 2, on garde le dernier élément
    if (length(cell) == 2) {
      return(cell[2])
    }
    # Si la cellule est une liste de taille 3, on garde l'élément du milieu
    else if (length(cell) == 3) {
      return(cell[2])
    }
    # Sinon, on retourne la cellule telle quelle
    else {
      return(cell)
    }
  })
})

# Afficher le résultat modifié
print(trajpro_wide_modifie)


# Identifiant des individus avec traj trop chaotiques
identifiants_to_remove <- unique(valeurs_bizarre$ident) 

# Supprimer les lignes dans trajpro_wide_modifie concernée
trajpro_wide_modifie_clean <- trajpro_wide_modifie[!trajpro_wide_modifie$ident %in% identifiants_to_remove, ]
print(trajpro_wide_modifie_clean)

#Trouver toutes les longueurs de listes
unique_lengths_2 <- unique(unlist(lapply(trajpro_wide_modifie_clean, function(col) lapply(col, length))))
print(unique_lengths_2)


# Gestions problème de période
trajpro$debproan <- as.integer(trajpro$debproan)  
trajpro$p_gan <- as.integer(trajpro$p_gan)  

# Identifier les individus avec des dates inversées (start_date > end_date)
dates_inversées <- trajpro %>% 
  filter(debproan > p_gan)

# Afficher les individus avec des dates inversées
print("Individus avec des dates inversées :")
print(dates_inversées)

