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

# Ebauche code (il a des pb à régler)
# Initialisation du dataframe final
df_transformed <- data.frame(
  ident = integer(),
  p_gactiv = character(),
  année = integer(),
  trans = character()
)
# Parcourir les lignes de df_initial pour créer df_final
for (i in 2:(nrow(df_initial) )) {
      
    # Sélectionner les lignes successives pour chaque ident
    if (df_initial$ident[i] == df_initial$ident[i - 1]) {
             
          # Vérifier la transition des activités
          transition <- ifelse(df_initial$p_nlig[i] != df_initial$p_nlig[i - 1], 
                                paste(df_initial$p_gactiv[i-1], "->", df_initial$p_gactiv[i]), 
                                "-")
               
          # Ajouter une ligne au dataframe final
          df_transformed <- rbind(df_transformed, data.frame(
                     ident = df_initial$ident[i],
                     p_gactiv = df_initial$p_gactiv[i],
                     année = df_initial$debproan[i],
                     trans = transition
                   ))
               }
}

# Afficher le dataframe transformé
print(df_transformed)




#Ce que j'avais fait avant pour format wide (jsp lequel des deux est plus approprié)
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

trajpro_wide_modifie <- trajpro_wide  

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


# Extraire les noms des colonnes sauf "ident"
seq_cols <- setdiff(colnames(trajpro_wide_modifie_clean), "ident")

# Convertir en numérique et trier
sorted_cols <- sort(as.numeric(seq_cols))

# Réorganiser le dataframe
trajpro_wide_modifie_clean <- trajpro_wide_modifie_clean[, c("ident", sorted_cols)]

# Vérifier le nouvel ordre des colonnes
colnames(trajpro_wide_modifie_clean)



                                         
# Gestions problème de période
trajpro$debproan <- as.integer(trajpro$debproan)  
trajpro$p_gan <- as.integer(trajpro$p_gan)  

# Identifier les individus avec des dates inversées (start_date > end_date)
dates_inversées <- trajpro %>% 
  filter(debproan > p_gan)

# Afficher les individus avec des dates inversées
print("Individus avec des dates inversées :")
print(dates_inversées)



                                         
#Visualiser les séquences

# Fusionner les tables en ajoutant 'group1' (det si origine migratoire) et 'anaise'(année de naissance pour contrôler les cohortes)
df <- merge(trajpro_wide_modifie_clean, indiv[, c("ident", "group1", "anaise")], by = "ident", all.x = TRUE)
head(df)

# Isoler les enfants d'au moins un immigré
df <- subset(df, group1 == 3)
head(df)

# Créer une variable qui permet d'isoler des cohortes dont la séquence commence au même moment
df <- df %>%
  mutate(debseq = apply(select(., -ident), 1, function(x) {
    # Trouver la première année où l'individu a une valeur non-NULL
    valid_years <- which(!sapply(x, is.null))  # Trouver les indices des années non-NULL
    if(length(valid_years) > 0) {
      # Retourner la première année non-NULL
      return(as.numeric(names(x)[valid_years[1]]))  # Les noms de colonnes contiennent l'année
    } else {
      return(NA)  # Si aucune année valide, retourner NA
    }
  }))
head(df[, c("ident", "debseq")])

                                         
#Exemple de chronogramme sur une cohorte spécifique
df_cohorte <- subset(df, debseq == 1990)

# Se débarasser des valeurs NULL pour pouvoir plot le chronogramme
                                                                                 
# Identifier les colonnes des années (exclure "ident" et autres colonnes non année)
years_cols <- colnames(df_cohorte)[grepl("^[0-9]{4}$", colnames(df_cohorte))]  # Sélectionner uniquement les colonnes des années
# Filtrer les colonnes des années >= 1990
years_cols <- years_cols[as.numeric(years_cols) >= 1990]
print(years_cols)
# Sélectionner les colonnes "ident" et les colonnes des années >= 1990
df_cohorte <- df_cohorte[, c("ident", years_cols)]
head(df_cohorte)

# Remplacer les valeurs NULL restantes (celles en milieu de séquence) par 8 (en gros on sait pas ce que c'est) dans le dataframe
df_cohorte <- df_cohorte
df_cohorte[] <- lapply(df_cohorte, function(x) {
  # Remplacer NULL par 8
  x[sapply(x, is.null)] <- 8
  return(x)
})
head(df_cohorte)
