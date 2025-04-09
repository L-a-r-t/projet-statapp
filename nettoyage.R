#Chargement des tables
trajpro <- read.csv("trajpro.csv", sep = ";", header = TRUE)
indiv <- read.csv("indiv.csv", sep = ";", header = TRUE)

#Librairies
library(TraMineR)
library(dplyr)
library(tidyr)


##################
# Recodage de variables
##################


# Recodage p_gactiv
trajpro <- trajpro %>% mutate(p_gactiv = case_when(
  p_gactiv == 1 ~ 1,
  p_gactiv == 2 ~ 2,
  p_gactiv == 3 ~ 3,
  p_gactiv == 4 ~ 4,
  p_gactiv == 5 ~ 5,
  (p_gactiv == 6 | p_gactiv == 7) ~ 6,
  TRUE ~ NA_real_
))


# Recodage origine_tous_g2
indiv <- indiv %>% mutate(origine_tous_g2bis = case_when(
  (origine_tous_g2 == 0 | origine_tous_g2 == 10 | origine_tous_g2 == 11) ~ 1, # population sans ascendance migratoire directe
  (origine_tous_g2 == 20) ~ 20, # Originaires d'outre-mer
  (origine_tous_g2 == 22) ~ 22, # Descendant.es d'originaires d'outre-mer
  (origine_tous_g2 == 30 | origine_tous_g2 == 40) ~ 30, # Immigré.es du Maghreb
  (origine_tous_g2 == 33 |origine_tous_g2 == 44) ~ 33, # Descendant.es d'immigré.es originaires du Maghreb
  (origine_tous_g2 == 50 | origine_tous_g2 == 60 | origine_tous_g2 == 70) ~ 50, # Immigré.es originaires d'Afrique Subsaharienne
  (origine_tous_g2 == 55 | origine_tous_g2 == 66 | origine_tous_g2 == 77) ~ 55, # Descendant.es d'immigré.es originaires d'Afrique Subsaharienne
  (origine_tous_g2 == 90) ~ 60, # Immigré.es originaires de Turquie et du Moyen-Orient
  (origine_tous_g2 == 99) ~ 66, # Descendant.es d'immigré.es originaires de Turquie et du Moyen-Orient
  (origine_tous_g2 == 80 | origine_tous_g2 == 100 | origine_tous_g2 == 110) ~ 70, # Immigré.es originaires du reste de l'Asie
  (origine_tous_g2 == 88 | origine_tous_g2 == 111) ~ 77, # Descendant.es d'immigré.es originaires de reste de l'Asie
  (origine_tous_g2 == 120 | origine_tous_g2 == 130) ~ 80, # Immigré.es originaires d'Europe du Sud
  (origine_tous_g2 == 121 | origine_tous_g2 == 131) ~ 88, # Descendant.es d'originaires d'immigré.es d'Europe du Sud
  (origine_tous_g2== 140 | origine_tous_g2 == 150) ~ 90, # Immigré.es originaires du reste de l'Europe
  (origine_tous_g2 == 141 | origine_tous_g2 == 151) ~ 99, # Descendant.es d'immigré.es originaires du reste de l'Europe
  (origine_tous_g2 == 160 ~ 100), # Immigré.es d'autres pays
  (origine_tous_g2 == 161 ~ 111), # Descendant.es d'immigré.es d'autres pays
  TRUE ~ NA_real_
))



##################
# Restructuration du DataFrame
##################


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

df <- merge(trajpro_wide_modifie_clean, indiv[, c("ident", "group1", "anaise", "finetu_an", "finetu_age", "f_finetg", "f_finetuag", "f_finetu_drap", "origine_tous_g2bis", "sexee", "andebtr", "duretu", "poidsi")], by = "ident", all.x = TRUE)
head(df)

# Age au premier travail
df <- df %>%
  mutate(agedebtr = andebtr - anaise)



##################
# Statistiques descriptives introductive (sur toute la population de notre df, sans longueur de carrière imposée)
##################



# Age moyen du premier emploi et durée moyenne des études selon groupe d'origine
tableau_statistiques <- df %>%
  filter(origine_tous_g2bis %in% c(33, 55, 66, 77, 88, 99, 1)) %>%
  filter(!duretu %in% c(7777, 8888)) %>%  # Exclure les individus dont duretu est 7777 ou 8888
  group_by(origine_tous_g2bis) %>%
  summarise(
    "Age moyen du premier emploi" = mean(agedebtr, na.rm = TRUE),  
    "Durée moyenne des études" = mean(duretu, na.rm = TRUE)
  )

tableau_statistiques <- tableau_statistiques %>%
  mutate(origine_tous_g2bis = case_when(
    origine_tous_g2bis == 33 ~ "Maghreb",
    origine_tous_g2bis == 55 ~ "Afrique Subsaharienne",
    origine_tous_g2bis == 66 ~ "Turquie et Moyen-Orient",
    origine_tous_g2bis == 77 ~ "Reste de l'Asie",
    origine_tous_g2bis == 88 ~ "Europe du Sud",
    origine_tous_g2bis == 99 ~ "Reste de l'Europe",
    origine_tous_g2bis == 1  ~ "Population sans ascendance migratoire"
  ))


tableau_statistiques <- tableau_statistiques %>%
  rename(
    "Origine des parents" = origine_tous_g2bis
  )

# Afficher le tableau 
print(tableau_statistiques)



##################
# Création d'un dataframe avec seulement les carrières assez longues
##################


# On perd environ 300 personnes qui n'ont pas voulu déclarer la date à laquelle iels ont fini leurs études...
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
    x = "Origine migratoires des parents",
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


##################
# Définition de notre objet séquence
##################

df_35.labels <- c("Salariat", "Indépendant", "Chômage", "Etudes", "Au foyer", "Instabilité")
df_35.scode <- c(1, 2, 3, 4, 5, 6)

df_35.seq <- seqdef(df_35, 2:23, states = df_35.scode, labels = df_35.labels, weights=df_35$poidsi)

get_distribution_for_age(df_35.seq, 15)



##################
# Premières statistiques descriptives
##################


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

#DF séquences par origine (A MODIFIER)
df_35_maj <- df_35 %>% filter(origine_tous_g2bis == 1)
df_35_mag <- df_35 %>% filter(origine_tous_g2bis == 33)
df_35_af <- df_35 %>% filter(origine_tous_g2bis == 55)
df_35_mo <- df_35 %>% filter(origine_tous_g2bis == 66)
df_35_as <- df_35 %>% filter(origine_tous_g2bis == 77)
df_35_eurs <- df_35 %>% filter(origine_tous_g2bis == 88)
df_35_eurr <- df_35 %>% filter(origine_tous_g2bis == 99)

#Séquences par origine
df_35_mag.seq <- seqdef(df_35_mag, 2:23, states = df_35.scode, labels = df_35.labels, weights=df_35_mag$poidsi)
df_35_as.seq <- seqdef(df_35_as, 2:23, states = df_35.scode, labels = df_35.labels, weights=df_35_as$poidsi)
df_35_maj.seq <- seqdef(df_35_maj, 2:23, states = df_35.scode, labels = df_35.labels, weights=df_35_maj$poidsi)
df_35_af.seq <- seqdef(df_35_af, 2:23, states = df_35.scode, labels = df_35.labels, weights=df_35_af$poidsi)
df_35_eurr.seq <- seqdef(df_35_eurr, 2:23, states = df_35.scode, labels = df_35.labels, weights=df_35_eurr$poidsi)
df_35_eurs.seq <- seqdef(df_35_eurs, 2:23, states = df_35.scode, labels = df_35.labels, weights=df_35_eurs$poidsi)
df_35_mo.seq <- seqdef(df_35_mo, 2:23, states = df_35.scode, labels = df_35.labels, weights=df_35_mo$poidsi)

# Plot de comparaison
png("Chronogrammes comparés.png", width = 2200, height = 2700, res = 300)

par(mfrow = c(4, 2), mar = c(4, 4, 2, 2), oma = c(0, 0, 3, 0))  

seqdplot(df_35_maj.seq, withlegend = F, border = NA, main = "Population majoritaire")
seqdplot(df_35_mag.seq, withlegend = F, border = NA, main = "Descendants d'immigrés - Maghreb")
seqdplot(df_35_af.seq, withlegend = F, border = NA, main = "Descendants d'immigrés - Afrique subsaharienne")
seqdplot(df_35_eurs.seq, withlegend = F, border = NA, main = "Descendants d'immigrés - Europe du sud")
seqdplot(df_35_eurr.seq, withlegend = F, border = NA, main = "Descendants d'immigrés - Reste de l'Europe")
seqdplot(df_35_as.seq, withlegend = F, border = NA, main = "Descendants d'immigrés - Reste de l'Asie")
seqdplot(df_35_eurr.seq, withlegend = F, border = NA, main = "Descendants d'immigrés - Moyen Orient et Turquie")

seqlegend(df_35.seq, ncol = 2, position = "top", cex=1.4)

dev.off()

#Plot par genre et comp entre asie et pop maj (asie car assez d'enquêtés sinon techniques)
df_35_h_as <- df_35 %>% filter(sexee == 1 & origine_tous_g2bis == 77)
df_35_f_as <- df_35 %>% filter(sexee == 2 & origine_tous_g2bis == 77)
df_35_h_maj <- df_35 %>% filter(sexee == 1 & origine_tous_g2bis == 1)
df_35_f_maj <- df_35 %>% filter(sexee == 2 & origine_tous_g2bis == 1)
df_35_h_as.seq <- seqdef(df_35_h_as, 2:23, states = df_35.scode, labels = df_35.labels, weights=df_35_h_as$poidsi)
df_35_f_as.seq <- seqdef(df_35_f_as, 2:23, states = df_35.scode, labels = df_35.labels, weights=df_35_f_as$poidsi)
df_35_h_maj.seq <- seqdef(df_35_h_maj, 2:23, states = df_35.scode, labels = df_35.labels, weights=df_35_h_maj$poidsi)
df_35_f_maj.seq <- seqdef(df_35_f_maj, 2:23, states = df_35.scode, labels = df_35.labels, weights=df_35_f_maj$poidsi)


png("Genre_Orig.png", width = 2000, height = 1900, res = 300)

par(mfrow = c(3, 2), mar = c(4, 4, 2, 2), oma = c(0, 0, 3, 0))  

seqdplot(df_35_h_maj.seq, withlegend = F, border = NA, main = "Population majoritaire (Hommes)")
seqdplot(df_35_f_maj.seq, withlegend = F, border = NA, main = "Population majoritaire (Femmes)")
seqdplot(df_35_h_as.seq, withlegend = F, border = NA, main = "Descendants d'immigrés - Asie (Hommes)")
seqdplot(df_35_f_as.seq, withlegend = F, border = NA, main = "Descendants d'immigrés - Asie (Femmes)")

seqlegend(df_35.seq, ncol = 2, position = "top", cex=1.2)

dev.off()

# Sequence des états modaux (état "normal" à chaque moment, celui qui apparait le plus)
par(mfrow = c(1, 1))
seqmsplot(df_35.seq, with.legend = FALSE, border = NA)

# Temps moyen passé dans chaque état
seqmtplot(df_35.seq, with.legend = FALSE)



##################
# Clustering
##################

library(cluster)
library(WeightedCluster)

# Matrice de coûts constants (arbitraire)
couts <- seqsubm(df_35.seq, method="CONSTANT", cval=2) #changer seqact

### Définition de différentes distances
# OM classique
disOM <- seqdist(df_35.seq, method="OM", sm=couts, indel=1)
# OM avec fréquence des transisions
disOMfreq <- seqdist(df_35.seq, method = "OM", indel = 1, sm = "TRATE") 
# Sequencing (flexible)
disOMtr <- seqdist(df_35.seq, method="OMstran", otto=0.1, sm=couts, indel=1)
# Sequencing (strict) (basé sur sous séquences communes)
disNMSmst <- seqdist(df_35.seq, method="NMSMST", kweights=22, tpow=1)
# Timing/Positionnement
disham <- seqdist(df_35.seq, method="HAM", sm=couts)
# IDK
disDHD <- seqdist(df_35.seq, method="DHD", sm=NULL)


# Fonctions : clustering selon 3 méthodes au choix (CAH, PAM, combinaison)
clusterForMethodAndDist <- function(method, distance, k) {
  if (method == "PAM") {
    cluster <- wcKMedoids(distance, k = k, weights = df_35$poidsi)
    treeCluster <- cluster$clustering
    quality <- wcClusterQuality(distance, treeCluster, weights = df_35$poidsi)
    stats <- quality$stats
    
    return(list(cluster=cluster, stats=stats))
  }
  else if (method == "CAH") {
    cluster <- hclust(as.dist(distance), method = "ward.D", members = df_35$poidsi)
    range <- as.clustrange(cluster, diss = distance, weights = df_35$poidsi, ncluster = k)
    tree <- as.seqtree(cluster, seqdata = df_35.seq, diss = distance, ncluster = k)
    treeCluster <- cutree(cluster, k = k)
    quality <- wcClusterQuality(distance, treeCluster, weights = df_35$poidsi)
    stats <- quality$stats
    
    return(list(cluster=cluster, stats=stats, tree=tree, cutree=treeCluster))
  }
  else if (method == "CAHPAM") {
    CAHcluster <- hclust(as.dist(distance), method = "ward.D", members = df_35$weight)
    cluster <- wcKMedoids(distance, k = k, weights = df_35$poidsi, initialclust = CAHcluster)
    treeCluster <- cluster$clustering
    quality <- wcClusterQuality(distance, treeCluster, weights = df_35$poidsi)
    stats <- quality$stats
    
    return(list(cluster=cluster, stats=stats))
  }
  else {
    stop("La méthode n'est pas reconnue, utiliser 'PAM' ou 'CAH' ou 'CAHPAM'.")
  }
}

result <- clusterForMethodAndDist(method="CAH", distance=disDHD, k=6)
cluster <- result$cutree
seqdplot(df_35.seq, group = cluster, border = NA)

##################
# Visulisation des stats de qualités des différentes distances
##################

ranges_hierarchical <- list()
range_names <- c("Optimal Matching", "OM Freq", "OM Transit", "NMSmst", "Hamming", "Hamming Dynamique")

# 1) Clustering hiérarchique
for (distance in list(disOMfreq, disOMtr, disNMSmst, disham)) {
  h_clust <- hclust(as.dist(distance), method = "ward.D", members = df_35$poidsi)
  ranges_hierarchical[[length(ranges_hierarchical) + 1]] <-
    as.clustrange(h_clust, diss = distance, weights = df_35$weight, ncluster = 10)
}

# 2) K-médoïdes
pam_range <- wcKMedRange(disOM, kvals = 2:10, weights = df_35$poidsi)
pamOM <- wcKMedoids(disOM, k = 10, weights = df_35$poidsi)
pam_range_10 <- subset(pam_range, k = 10)
ranges_pam <- as.clustrange(pam_range, diss = disOM, ncluster = 10)
ranges_pam <- as.clustrange(pam_range_10, diss = disOM, ncluster = 10)
ranges_pam <- as.clustrange(pamOM, diss = disOM, ncluster = 10)

library(ggplot2)

makeComparisonPlot <- function(stat_name, algo="Hierarchique") {
  if (algo == "PAM") {
    ranges <- pam_range
    nom_algo <- "K-Médoïdes"
  } else {
    ranges <- ranges_hierarchical
    nom_algo <- "Clustering Hiérarchique"
  }
  # On combine en une df les valeurs de la statistique choisie pour chaque distance 
  df_list <- lapply(seq_along(ranges), function(i) {
    stats_obj <- ranges[[i]]$stats
    
    if (is.data.frame(stats_obj)) {
      # Hierarchical output: a data frame
      cluster_nums <- as.numeric(gsub("cluster", "", rownames(stats_obj)))
      vals <- stats_obj[, stat_name]
      data.frame(
        cluster = cluster_nums,
        statistique = vals,
        distance = range_names[i]
      )
    } else if (is.numeric(stats_obj)) {
      # PAM output: just a named numeric
      # Return one row, e.g. for k=10
      data.frame(
        cluster = 10,
        statistique = stats_obj[stat_name],
        distance = range_names[i]
      )
    } else {
      stop("Unexpected stats format")
    }
  })
  
  df <- do.call(rbind, df_list)
  
  # Plot ASW vs cluster, with one line per element in `ranges`
  ggplot(df, aes(x = cluster, y = statistique, color = distance)) +
    geom_line() +
    geom_point() +
    labs(x = "Nombre de Clusters", y = stat_name, title = sprintf("%s pour différentes distances (%s)", stat_name, nom_algo)) +
    theme_minimal()
}


df_list <- lapply(seq_along(pam_range), function(i) {
  stats_obj <- pam_range[i]$stats
  
  if (is.data.frame(stats_obj)) {
    # Hierarchical output: a data frame
    cluster_nums <- as.numeric(gsub("cluster", "", rownames(stats_obj)))
    vals <- stats_obj[, stat_name]
    data.frame(
      cluster = cluster_nums,
      statistique = vals,
      distance = "Optimal Matching"
    )
  } else if (is.numeric(stats_obj)) {
    # PAM output: just a named numeric
    # Return one row, e.g. for k=10
    data.frame(
      cluster = 10,
      statistique = stats_obj["ASW"],
      distance = "Optimal Matching"
    )
  } else {
    stop("Unexpected stats format")
  }
})

df <- do.call(rbind, df_list)

# Plot ASW vs cluster, with one line per element in `ranges`
ggplot(pam_range, aes(x = cluster, y = statistique, color = distance)) +
  geom_line() +
  geom_point() +
  labs(x = "Nombre de Clusters", y = stat_name, title = sprintf("%s pour différentes distances (%s)", stat_name, nom_algo)) +
  theme_minimal()


makeComparisonPlot("ASW")
makeComparisonPlot("HC")
makeComparisonPlot("PBC")

makeComparisonPlot("ASW", algo="PAM")
makeComparisonPlot("HC", algo="PAM")
makeComparisonPlot("PBC", algo="PAM")

################
################
library(dplyr)
library(ggplot2)

# 1) For PAM: Add k-values, keep only ASW, rename method
pam_range$stats$k <- pam_range$kvals
df_pam <- pam_range$stats %>%
  select(k, HC) %>%
  mutate(distance = "Optimal Matching (PAM)")

# 2) For Hierarchical: loop over the 4 objects, rename each method
hier_labels <- c("OM Freq", "OM Transit", "NMSmst", "Hamming")

df_hier <- do.call(rbind, lapply(seq_along(ranges_hierarchical), function(i) {
  x <- ranges_hierarchical[[i]]
  x$stats$k <- x$kvals
  x$stats %>%
    select(k, HC) %>%
    mutate(distance = hier_labels[i])
}))

# 3) Combine PAM and hierarchical results
df_combined <- bind_rows(df_pam, df_hier)

# 4) Plot: color by method
ggplot(df_combined, aes(x = k, y = HC, color = distance)) +
  geom_line() +
  labs(
    title = "HC pour différentes distances",
    x = "Nombre de Clusters",
    y = "HC"
  ) +
  theme_minimal()

################
################


# Analyse en cluster

# Distance OM
# Clustering hiérarchique 
wardClusterOM <- hclust(as.dist(disOM), method = "ward.D", members = df_35$poidsi)

# Nombre de partition
wardRangeOM <- as.clustrange(wardClusterOM, diss = disOM, weights = df_35$poidsi, ncluster = 10)
summary(wardRangeOM, max.rank = 2)
dev.off()
plot(wardRangeOM, stat = c("ASW", "HG", "PBC","HC"), norm = "zscore")

# Séparation en cluster
wardTreeOM <- as.seqtree(wardClusterOM, seqdata = df_35.seq, diss = disOM, ncluster = 7)
clust8OM <- cutree(wardClusterOM, k = 7)

# Visualisations des cluster
seqtreedisplay(wardTreeOM, type = "d", border = NA, show.depth = TRUE, file = "treeOM.png")
library(knitr)
include_graphics("treeOM.png")

graphics.off()
seqdplot(df_35.seq, group = clust8OM, border = NA)

# PAM 
pamclust8OM <- wcKMedoids(disOM, k = 7, weights = df_35$poidsi)
seqdplot(df_35.seq, group = pamclust8OM$clustering, border = NA)
print(df_35.seq[unique(pamclust8OM$clustering),], format = "SPS")

# Comparaison des algos
clustqual8OM <- wcClusterQuality(disOM, clust8OM, weights = df_35$poidsi)
clustqual8OM$stats
pamclust8OM$stats
# pas mal, jsp comment idscriminer mais imo trop de cluster


# Distance OMfreq
# Clustering hiérarchique 
wardClusterOMfreq <- hclust(as.dist(disOMfreq), method = "ward.D", members = df_35$poidsi)

# Nombre de partition
wardRangeOMfreq <- as.clustrange(wardClusterOMfreq, diss = disOMfreq, weights = df_35$poidsi, ncluster = 10)
summary(wardRangeOMfreq, max.rank = 2)
plot(wardRangeOMfreq, stat = c("ASW", "PBC","HC"), norm = "zscore")

# Séparation en cluster
wardTreeOMfreq <- as.seqtree(wardClusterOMfreq, seqdata = df_35.seq, diss = disOMfreq, ncluster = 6)
clust6OMfreq <- cutree(wardClusterOMfreq, k = 6)

# Visualisations des cluster
seqtreedisplay(wardTreeOMfreq, type = "d", border = NA, show.depth = TRUE, file = "treeOMfreq.png")
library(knitr)
include_graphics("treeOMfreq.png")

seqdplot(df_35.seq, group = clust6OMfreq, border = NA)

# PAM 
pamclust6OMfreq <- wcKMedoids(disOMfreq, k = 6, weights = df_35$poidsi)
seqdplot(df_35.seq, group = pamclust6OMfreq$clustering, border = NA)
print(df_35.seq[unique(pamclust6OMfreq$clustering),], format = "SPS")

# Comparaison des algos
clustqual6OMfreq <- wcClusterQuality(disOMfreq, clust6OMfreq, weights = df_35$poidsi)
clustqual6OMfreq$stats
pamclust6OMfreq$stats
# ???? mais globalement pas mal


# Distance OMtr
# Clustering hiérarchique 
wardClusterOMtr <- hclust(as.dist(disOMtr), method = "ward.D", members = df_35$poidsi)

# Nombre de partition
wardRange <- as.clustrange(wardClusterOMtr, diss = disOMtr, weights = df_35$poidsi, ncluster = 10)
summary(wardRange, max.rank = 2)
plot(wardRange, stat = c("ASW", "PBC","HC"), norm = "zscore")

# Séparation en cluster
wardTreeOMtr <- as.seqtree(wardClusterOMtr, seqdata = df_35.seq, diss = disOMtr, ncluster = 4)
clust4OMtr <- cutree(wardClusterOMtr, k = 4)

# Visualisations des cluster
seqtreedisplay(wardTreeOMtr, type = "d", border = NA, show.depth = TRUE, file = "tree.png")
library(knitr)
include_graphics("tree.png")

seqdplot(df_35.seq, group = clust4OMtr, border = NA)

# PAM 
pamclust4OMtr <- wcKMedoids(disOMtr, k = 4, weights = df_35$poidsi)
seqdplot(df_35.seq, group = pamclust4OMtr$clustering, border = NA)
print(df_35.seq[unique(pamclust4OMtr$clustering),], format = "SPS")

# Comparaison des algos
clustqual4OMtr <- wcClusterQuality(disOMtr, clust4OMtr, weights = df_35$poidsi)
clustqual4OMtr$stats
pamclust4OMtr$stats
# PAM semble être meilleur choix


# Distance NMSmst
# Clustering hiérarchique 
wardClusterNMSmst <- hclust(as.dist(disNMSmst), method = "ward.D", members = df_35$poidsi)

# Nombre de partition
wardRangeNMSmst <- as.clustrange(wardClusterNMSmst, diss = disNMSmst, weights = df_35$poidsi, ncluster = 10)
summary(wardRangeNMSmst, max.rank = 2)
plot(wardRangeNMSmst, stat = c("ASW", "HG", "PBC","HC"), norm = "zscore")

# Séparation en cluster (selon meilleure partition)
wardTreeNMSmst <- as.seqtree(wardClusterNMSmst, seqdata = df_35.seq, diss = disNMSmst, ncluster = 3)
clust3NMSmst <- cutree(wardClusterNMSmst, k = 3)

# Visualisations des cluster
seqtreedisplay(wardTreeNMSmst, type = "d", border = NA, show.depth = TRUE, file = "treeNMSmst.png")
include_graphics("treeNMSmst.png")

seqdplot(df_35.seq, group = clust3NMSmst, border = NA)
# Archi nul en gros

# PAM 
pamclust3NMSmst <- wcKMedoids(disNMSmst, k = 3, weights = df_35$poidsi)
seqdplot(df_35.seq, group = pamclust3NMSmst$clustering, border = NA)
print(df_35.seq[unique(pamclust3NMSmst$clustering),], format = "SPS")

# Comparaison des algos
clustqual3NMSmst <- wcClusterQuality(disNMSmst, clust3NMSmst, weights = df_35$poidsi)
clustqual3NMSmst$stats
pamclust3NMSmst$stats
# CAH un peu meilleur mais les deux pue la merde


# Distance Hamming
# Clustering hiérarchique 
wardClusterham <- hclust(as.dist(disham), method = "ward.D", members = df_35$poidsi)

# Nombre de partition
wardRangeham <- as.clustrange(wardClusterham, diss = disham, weights = df_35$poidsi, ncluster = 10)
summary(wardRangeham, max.rank = 2)
plot(wardRangeham, stat = c("ASW", "PBC","HC"), norm = "zscore")

# Séparation en cluster (selon meilleure partition)
wardTreeham <- as.seqtree(wardClusterham, seqdata = df_35.seq, diss = disham, ncluster = 4)
clust4ham <- cutree(wardClusterham, k = 4)

# Visualisations des cluster
seqtreedisplay(wardTreeham, type = "d", border = NA, show.depth = TRUE, file = "treeham.png")
include_graphics("treeham.png")

seqdplot(df_35.seq, group = clust4ham, border = NA)

# PAM 
pamclust4ham <- wcKMedoids(disham, k = 4, weights = df_35$poidsi)
seqdplot(df_35.seq, group = pamclust4ham$clustering, border = NA)
print(df_35.seq[unique(pamclust4ham$clustering),], format = "SPS")

# Comparaison des algos
clustqual4ham <- wcClusterQuality(disham, clust4ham, weights = df_35$poidsi)
clustqual4ham$stats
pamclust4ham$stats
# CAH un peu meilleur mais pas fou


# Distance Hamming dynamique
# Clustering hiérarchique 
wardClusterDHD <- hclust(as.dist(disDHD), method = "ward.D", members = df_35$poidsi)

# Nombre de partition
wardRangeDHD <- as.clustrange(wardClusterDHD, diss = disDHD, weights = df_35$poidsi, ncluster = 10)
summary(wardRangeDHD, max.rank = 2)
dev.off()
plot(wardRangeDHD, stat = c("ASW", "HG", "PBC","HC"), norm = "zscore")
plot(wardRangeDHD, stat = c("ASW", "HG", "PBC","HC"))

# Séparation en cluster
wardTreeDHD <- as.seqtree(wardClusterDHD, seqdata = df_35.seq, diss = disDHD, ncluster = 5)
clust6DHD <- cutree(wardClusterDHD, k = 5)

# Visualisations des cluster
seqtreedisplay(wardTreeDHD, type = "d", border = NA, show.depth = TRUE, file = "treeDHD.png")
library(knitr)
include_graphics("treeDHD.png")

graphics.off()
seqdplot(df_35.seq, group = clust6DHD, border = NA)

# PAM 
pamclust6DHD <- wcKMedoids(disDHD, k = 5, weights = df_35$poidsi)
seqdplot(df_35.seq, group = pamclust6DHD$clustering, border = NA)
print(df_35.seq[unique(pamclust6DHD$clustering),], format = "SPS")

# Comparaison des algos
clustqual8DHD <- wcClusterQuality(disDHD, clust8DHD, weights = df_35$poidsi)
clustqual8DHD$stats
pamclust8DHD$stats
# pas mal, jsp comment idscriminer mais imo trop de cluster



# Description

png("Silhouette OM freq.png", width = 2200, height = 2700, res = 300)
silOMfreq <- wcSilhouetteObs(disOMfreq, clust6OMfreq, weights = df_35$weight)
seqIplot(df_35.seq, group = clust6OMfreq, sortv = silOMfreq)
dev.off()

png("Silhouette OM tr.png", width = 2000, height = 1900, res = 300)
silOMtr <- wcSilhouetteObs(disOMtr, clust4OMtr, weights = df_35$weight)
seqIplot(df_35.seq, group = clust4OMtr, sortv = silOMtr)
dev.off()


# Nommer cluster
df_35$OMfreq6 <- factor(clust6OMfreq, 
                        levels = c(1, 2, 3, 4, 5, 6), 
                        labels = c("Ecole-Emploi",
                                   "Etudes-Emploi", 
                                   "Etude-Indep.", 
                                   "Au foyer",
                                   "Etudes longues",
                                   "Ecole-Instabilité pro"))

table(df_35$OMfreq6)

df_35$OMtr4 <- factor(clust4OMtr, 
                        levels = c(1, 2, 3, 4), 
                        labels = c("Etudes-Emploi",
                                   "Traj. plus variables", 
                                   "Etudes-Chômage-Emploi",
                                   "Autres (?)"))

table(df_35$OMtr4)

seqdplot(df_35.seq, group = df_35$OMtr4, border = NA)

png("Chronog OM freq.png", width = 2200, height = 2700, res = 300)
seqdplot(df_35.seq, group = df_35$OMfreq6, border = NA)
dev.off()

png("Chronog OM tr.png", width = 2000, height = 1900, res = 300)
seqdplot(df_35.seq, group = df_35$OMtr4, border = NA)
dev.off()




#df_40
df_40 <- filter_df_by_age(df, 40)
df_40 <- df %>% filter(!is.na(`14`))
df_40 <- df %>% select(-c('41':'60'))
df_40 <- df %>% filter(group1 %in% c(3,4,5))
#problème ici
df_40[,2:28] <- lapply(df_40[,2:28], unlist)
df_40.seq <- seqdef(df_40, 2:28, states = df_35.scode, labels = df_35.labels)

#Entropie
seqHtplot(df_40.seq)
