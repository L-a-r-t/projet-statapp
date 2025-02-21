#Chargement des tables
trajpro <- read.csv("trajpro.csv", sep = ";", header = TRUE)
indiv <- read.csv("indiv.csv", sep = ";", header = TRUE)

#Librairies
library(TraMineR)
library(dplyr)
library(tidyr)

#### Recodage p_gactiv. On regroupe les modalités 6 et 7 
trajpro <- trajpro %>% mutate(p_gactiv2 = case_when(
  p_gactiv == 1 ~ 1,  # Salarié.e
  p_gactiv == 2 ~ 2,  # À son compte ou indépendant
  p_gactiv == 3 ~ 3,  # Chômage
  p_gactiv == 4 ~ 4,  # Études ou stage non rémunéré (formation continue)
  p_gactiv == 5 ~ 5,  # Femme ou homme au foyer
  p_gactiv == 6 | p_gactiv == 7 ~ 6,  # Autres
  TRUE ~ NA_real_  # NA en format numérique
))

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

trajpro_c <- trajpro_clean %>% select(-c(p_nlig, p_gan, debproan, debproag, p_gage, Années, p_gactiv))

#Format wide
trajpro_wide <- trajpro_c %>%
  pivot_wider(names_from = age, values_from = p_gactiv2, id_cols = c(ident))

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


#### Recodage origine_tous_g2
indiv <- indiv %>% mutate(origine_tous_g2bis = case_when(
  (origine_tous_g2 == 0 | origine_tous_g2 == 10 | origine_tous_g2 == 11) ~ 1, # population sans ascendance migratoire directe
  (origine_tous_g2 == 20) ~ 20, # Originaires d'outre-mer
  (origine_tous_g2 == 22) ~ 22, # Descendant.es d'originaires d'outre-mer
  (origine_tous_g2 == 30 | origine_tous_g2 == 40) ~ 30, # Immigré.es du Maghreb
  (origine_tous_g2 == 33 |origine_tous_g2 == 44) ~ 33, # Descendant.es d'immigré.es originaires du Maghreb
  (origine_tous_g2 == 50 | origine_tous_g2 == 60 | origine_tous_g2 == 70) ~ 50, # Immigré.es originaires d'Afrique Subsaharienne
  (origine_tous_g2 == 55 | origine_tous_g2 == 66 | origine_tous_g2 == 77) ~ 55, # Descendant.es d'immigré.es originaires d'Afrique Subsaharienne
  (origine_tous_g2 == 80 |  origine_tous_g2 == 90 | origine_tous_g2 == 100 |  origine_tous_g2 == 110) ~ 60, # Immigré.es originaires d'Asie et du Moyen-Orient
  (origine_tous_g2 == 88 | origine_tous_g2 == 99 | origine_tous_g2 == 111) ~ 66, # Descendant.es d'immigré.es originaires d'Asie et du Moyen-Orient
  (origine_tous_g2 == 120 | origine_tous_g2 == 130) ~ 70, # Immigré.es originaires d'Europe du Sud
  (origine_tous_g2 == 121 | origine_tous_g2 == 131) ~ 77, # Descendant.es d'originaires d'immigré.es d'Europe du Sud
  (origine_tous_g2== 140 | origine_tous_g2 == 150) ~ 80, # Immigré.es originaires du reste de l'Europe
  (origine_tous_g2 == 141 | origine_tous_g2 == 151) ~ 88, # Descendant.es d'immigré.es originaires du reste de l'Europe
  (origine_tous_g2 == 160 ~ 90), # Immigré.es d'autres pays
  (origine_tous_g2 == 161 ~99), # Descendant.es d'immigré.es d'autres pays
  TRUE ~ NA_real_
))

df<-trajpro_wide_modifie_clean

df <- merge(trajpro_wide_modifie_clean, indiv[, c("ident", "group1", "anaise", "finetu_an", "finetu_age", "f_finetg", "f_finetuag", "f_finetu_drap", "origine_tous_g2bis")], by = "ident", all.x = TRUE)
head(df)

# On perd environ 300 personnes qui n'ont pas voulu déclarer la date
# à laquelle iels ont fini leurs études...
df <- df %>%
  mutate(across(matches("^\\d+$"), 
                ~ ifelse(as.numeric(cur_column()) < finetu_age, 4, .)))

library(purrr)
library(ggplot2)

# Exemple de fonction filtrant selon limit_age
# Hypothèse : on supprime (on ne garde pas) les lignes dont l'âge < limit_age
# Donc on conserve celles pour lesquelles (2020 - anaise) >= limit_age
filter_df_by_age <- function(data, limit_age) {
  data <- data %>%
    filter((2020 - anaise) > limit_age)
  data <- data %>% filter(!is.na(`14`))
  
  data <- data %>% select(-c("36":"60"))
  
  data <- data %>% filter(group1 %in% c(3,4,5))
}

# On applique la fonction pour des valeurs de 14 à 40
# et on calcule le nombre de lignes restant pour chaque limit_age
df_counts <- tibble(
  limit_age = 14:60,
  n_rows = map_int(limit_age, ~ nrow(filter_df_by_age(df, .x)))
)

# Exemple de tracé rapide en histogramme (barres)
ggplot(df_counts, aes(x = limit_age, y = n_rows)) +
  geom_col() +
  labs(x = "limit_age", y = "Nombre de lignes") +
  theme_minimal()

# Avec 35 ans on perd 4 000 individus parmi tous ceux qui pourraient nous 
# intéresser (pop maj ou descendant d'immigrés)
# Avec 30 ans, on en perd 5 000
                                         
df_35 <- filter_df_by_age(df, 35)

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

# On veut connaître la distribution des modalités de 
# origin_tous_g2bis sur notre échantillon filtré

# On ne garde que les vraies modalités de origin_tous_g2bis
df_35$origine_tous_g2bis <- as.factor(df_35$origine_tous_g2bis)
df_35$origine_tous_g2bis <- droplevels(df_35$origine_tous_g2bis)

# Barplot
ggplot(df_35, aes(x = origine_tous_g2bis)) +
  geom_bar(fill = "skyblue") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.1, size = 4, angle = 70) +
  labs(
    title = "Distribution des Modalités de la Variable origine_tous_g2",
    x = "Modalités de origine_tous_g2",
    y = "Nombre d'Observations"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


df_35[,2:23] <- lapply(df_35[,2:23], unlist)
sapply(df_35, class)

library(TraMineR)


get_distribution_for_age <- function(seq, age) {
  age_col <- as.character(age)
  if (!age_col %in% colnames(seq)) {
    stop("L'âge demandé n'est pas présent dans les colonnes de la séquence.")
  }
  states <- seq[, age_col]
  distribution <- table(states)
  percentages <- prop.table(distribution) * 100
  return(percentages)
}

df_35.labels <- c("Salariat", "Indépendant", "Chômage", "Etudes", "Au foyer", "Autres")
df_35.scode <- c(1, 2, 3, 4, 5, 6)

df_35.seq <- seqdef(df_35, 2:23, states = df_35.scode, labels = df_35.labels)

get_distribution_for_age(df_35.seq, 30)

# Plot sur pop maj+descendants d'immig+domiens
par(mfrow = c(2, 2), mar = c(4, 4, 3, 5), oma = c(0, 0, 3, 0))  

seqiplot(df_35.seq, withlegend = F, title = "Index plot (10 premières trajectoires)", border = NA)
seqfplot(df_35.seq, withlegend = F, border = NA, title = "Sequence frequency plot")
seqdplot(df_35.seq, withlegend = F, border = NA, title = "State distribution plot")
seqlegend(df_35.seq, fontsize = 0.7)

# Ajouter un titre général
mtext("Trajectoires dans la population d'intérêt", outer = TRUE, cex = 1.5, font = 1)


par(mfrow = c(1,2))
# Entropy index
seqHtplot(df_35.seq, title = "Entropy index")
#Histogramme turbulence
Turbulence <- seqST(df_35.seq) 
summary(Turbulence) 
hist(Turbulence, col = "cyan", main = "Sequence turbulence")

df_35_as <- df_35 %>% filter(origine_tous_g2bis == 33)
df_35_mag <- df_35 %>% filter(origine_tous_g2bis == 66)
df_35_maj <- df_35 %>% filter(origine_tous_g2bis == 1)
df_35_mag.seq <- seqdef(df_35_mag, 2:23, states = df_35.scode, labels = df_35.labels)
df_35_as.seq <- seqdef(df_35_as, 2:23, states = df_35.scode, labels = df_35.labels)
df_35_maj.seq <- seqdef(df_35_maj, 2:23, states = df_35.scode, labels = df_35.labels)
seqdplot(df_35_mag.seq, with.legend = T, border = NA, main = "Chronogramme pour les descendants d'immigrés du Maghreb")
seqdplot(df_35_as.seq, with.legend = T, border = NA, main = "Chronogramme pour les descendants d'immigrés d'Asie et du Moyen Orient")

# Plot de comparaison
par(mfrow = c(2, 2), mar = c(4, 4, 2.5, 5.5), oma = c(0, 0, 3, 0))  

seqdplot(df_35_mag.seq, withlegend = F, border = NA, title = "Maghreb")
seqdplot(df_35_as.seq, withlegend = F, border = NA, title = "Asie et Moyen Orient")
seqdplot(df_35_maj.seq, withlegend = F, border = NA, title = "Population majoritaire")
seqlegend(df_35.seq, fontsize = 0.8)

mtext("Chronogrammes comparés", outer = TRUE, cex = 1.5, font = 2)




#Problème ici, à checker
df_40 <- filter_df_by_age(df, 40)

df_40 <- df %>% filter(!is.na(`14`))

df_40 <- df %>% select(-c('41':'60'))

df_40 <- df %>% filter(group1 %in% c(3,4,5))
