#Chargement des tables
trajpro <- read.csv("trajpro.csv", sep = ";", header = TRUE)
indiv <- read.csv("indiv.csv", sep = ";", header = TRUE)

#Librairies
library(TraMineR)
library(dplyr)
library(tidyr)

# Gestions problème de période
trajpro$debproan <- as.integer(trajpro$debproan)  
trajpro$p_gan <- as.integer(trajpro$p_gan)  

# Identifier les individus avec des dates inversées (start_date > end_date)
dates_inversées <- trajpro %>% 
  filter(debproan > p_gan)

# Afficher les individus avec des dates inversées
print("Individus avec des dates inversées :")
print(dates_inversées)

# Identifiant des individus avec traj trop chaotiques
identifiants_to_remove <- unique(dates_inversées$ident) 

# Supprimer les lignes dans trajpro_wide_modifie concernée
trajpro_cln <- trajpro[!trajpro$ident %in% identifiants_to_remove, ]


#Drop avriables non indiquées et identifier chaque années 
trajpro_clean <- trajpro_cln%>%
  drop_na(debproan, p_gan) %>%  # Supprime les lignes avec NA
  rowwise() %>%
  mutate(Années = list(seq(debproan, p_gan))) %>%
  unnest(Années)

trajpro_clean$age <-trajpro_clean$Années-trajpro_clean$debproan+trajpro_clean$debproag

fin<-35

trajpro_c <- trajpro_clean %>% select(-c(p_nlig, p_gan, debproan, debproag, p_gage, Années))

#Format wide
trajpro_wide <- trajpro_c %>%
  pivot_wider(names_from = age, values_from = p_gactiv, id_cols = c(ident))

# Extraire les noms des colonnes sauf "ident"
seq_cols <- setdiff(colnames(trajpro_wide), "ident")

# Convertir en numérique et trier
sorted_cols <- sort(as.numeric(seq_cols))

# Réorganiser le dataframe
trajpro_wide<- trajpro_wide[, c("ident", sorted_cols)]

#Trouver toutes les longueurs de listes
unique_lengths <- unique(unlist(lapply(trajpro_wide, function(col) lapply(col, length))))
print(unique_lengths)

#Nombre de fois où apparait une liste de longueur données
table(unlist(lapply(trajpro_wide, function(col) lapply(col, length))))

# Identifier les lignes contenant des listes de taille 4, 5, 6 ou 14 
rows_to_extract <- apply(trajpro_wide, 1, function(row) {
  any(sapply(row, function(cell) length(cell) %in% c(4, 5, 6, 14)))
})
valeurs_bizarre <- trajpro_wide[rows_to_extract, ]


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

# Identifiant des individus avec traj trop chaotiques
identifiants_to_remove <- unique(valeurs_bizarre$ident) 

# Supprimer les lignes dans trajpro_wide_modifie concernée
trajpro_wide_modifie_clean <- trajpro_wide_modifie[!trajpro_wide_modifie$ident %in% identifiants_to_remove, ]
print(trajpro_wide_modifie_clean)

#Trouver toutes les longueurs de listes
unique_lengths_2 <- unique(unlist(lapply(trajpro_wide_modifie_clean, function(col) lapply(col, length))))
print(unique_lengths_2)

trajpro_wide_modifie_clean <- trajpro_wide_modifie_clean %>% select(-c("0":"13"))

df<-trajpro_wide_modifie_clean

df <- merge(trajpro_wide_modifie_clean, indiv[, c("ident", "group1", "anaise", "finetu_an", "finetu_age", "origine_tous_g2")], by = "ident", all.x = TRUE)
head(df)

df <- df %>%
  mutate(across(matches("^\\d+$"), 
                ~ ifelse(as.numeric(cur_column()) < finetu_age, 4, .)))

library(purrr)
library(ggplot2)

# Exemple de fonction filtrant selon limit_age
# Hypothèse : on supprime (on ne garde pas) les lignes dont l'âge < limit_age
# Donc on conserve celles pour lesquelles (2020 - anaise) >= limit_age
filter_df_by_age <- function(data, limit_age) {
  data %>%
    filter((2020 - anaise) > limit_age)
}

# On applique la fonction pour des valeurs de 14 à 40
# et on calcule le nombre de lignes restant pour chaque limit_age
df_counts <- tibble(
  limit_age = 14:40,
  n_rows = map_int(limit_age, ~ nrow(filter_df_by_age(df, .x)))
)

# Exemple de tracé rapide en histogramme (barres)
ggplot(df_counts, aes(x = limit_age, y = n_rows)) +
  geom_col() +
  labs(x = "limit_age", y = "Nombre de lignes") +
  theme_minimal()
                                         
df_35 <- filter_df_by_age(df, 35)

df_35 <- df_35 %>% filter(!is.na(`14`))

df_35 <- df_35 %>% select(-c("36":"60"))

df_35 <- df_35 %>% filter(group1 %in% c(3,4,5))

# Vérifier la longueur de chaque colonne (après unlist)
col_lengths <- sapply(df_35[2:23], function(col) length(unlist(col)))
print(col_lengths)

for(colname in names(df_35)[2:23]) {
  # On transforme chaque colonne en vecteur
  col_unlist <- unlist(df_35[[colname]])
  # Si la longueur est trop courte, on complète avec NA
  if(length(col_unlist) < nrow(df_35)){
    col_unlist <- c(col_unlist, rep(NA, nrow(df_35) - length(col_unlist)))
  }
  df_35[[colname]] <- col_unlist
}

df_35 <- df_35 %>% filter(if_all(2:23, ~ !is.na(.)))

df_35[,2:23] <- lapply(df_35[,2:23], unlist)
sapply(df_35, class)

library(TraMineR)

df_35.labels <- c("Salariat", "Indépendant", "Chômage", "Etudes", "Foyer", "Autres", "Variables")
df_35.scode <- c(1, 2, 3, 4, 5, 6, 7)

df_35.seq <- seqdef(df_35, 2:23, states = df_35.scode, labels = df_35.labels)

# Plot sur toutes la population
par(mfrow = c(2, 2),   # 2 lignes, 2 colonnes
    mar = c(4, 4, 3, 5))  

seqiplot(df_35.seq, withlegend = F, title = "Index plot (10 first sequences)", border = NA)
seqfplot(df_35.seq, withlegend = F, border = NA, title = "Sequence frequency plot")
seqdplot(df_35.seq, withlegend = F, border = NA, title = "State distribution plot")
seqlegend(df_35.seq, fontsize = 0.8)

par(mfrow = c(1,2))
# Entropy index
seqHtplot(df_35.seq, title = "Entropy index")
#Histogramme turbulence
Turbulence <- seqST(df_35.seq) 
summary(Turbulence) 
hist(Turbulence, col = "cyan", main = "Sequence turbulence")