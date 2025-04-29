library(readr)
install.packages("devtools")
library(devtools)


# On importe les tables indiv et trajpro
indiv <- read_delim("Bases de données/indiv.csv", 
                    delim = ";", escape_double = FALSE, trim_ws = TRUE)
trajpro <- read_delim("Bases de données/trajpro.csv", 
                      delim = ";", escape_double = FALSE, trim_ws = TRUE)

# On exécute le code qui va permettre de faire le clustering 
source("C:/Users/vctrl/Desktop/ENSAE 2024-2025/Statapp/Code_principal/nettoyage_court.R")

##################
# Clustering
##################

library(cluster)
library(WeightedCluster)

# Matrice de coûts constants (arbitraire)
couts <- seqsubm(df_35.seq, method="CONSTANT", cval=2) #changer seqact

### Définition de la distance choisie : OM classique
disOM <- seqdist(df_35.seq, method="OM", sm=couts, indel=1)

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
    CAHcluster <- hclust(as.dist(distance), method = "ward.D", members = df_35$poidsi)
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


# On fait la clusterisation PAM + OM
###########OM

# On teste les différents algos de clustering et on regarde leur qualité à chaque fois

# PAM
result <- clusterForMethodAndDist(method="PAM", distance=disOM, k=6)
cluster_PAM_OM <- result$cluster$clustering
seqdplot(df_35.seq, group = cluster_PAM_OM, border = NA)

# On fusionne deux clusters très ressemblants
cluster_fusion_PAM_OM <- cluster_PAM_OM
cluster_fusion_PAM_OM[cluster_PAM_OM %in% c(8964)] <- 1  # On fusionne deux clusters très similaires
seqdplot(df_35.seq, group = cluster_fusion_PAM_OM, border = NA)


#######
# On va nommer les clusters ("avec Céreq Études no 57 • Les trajectoires d’entrée dans la vie active : de la sortie des études jusqu’à 6 ans après")
#######

cluster_fusion_PAM_OM <- factor(cluster_fusion_PAM_OM,
                                levels = c(1, 7798, 8533, 9020, 9065),
                                labels = c("Accès à l'emploi après des études courtes", "Trajectoire marquée par une longue période au foyer", "Trajectoire marquée par une longue période en tant qu'indépendant.e", "Accès à l'emploi détérioré et marqué par une situation instable", "Accès à l'emploi après des études longues"))
seqdplot(df_35.seq, group = cluster_fusion_PAM_OM, border = NA)

# On ajoute l'appartenance aux cluster comme un variable de df 35
df_35$cluster_fusion_PAM_OM <- cluster_fusion_PAM_OM

# On va créer les séquences en séparant les individus en fonction de leur appartenance à un cluster


# Accès à l'emploi après des études courtes
df_35_1 <- df_35 %>%  filter(cluster_fusion_PAM_OM == "Accès à l'emploi après des études courtes")
df_35_1.seq <- seqdef(df_35_1, 2:23, states = df_35.scode, labels = df_35.labels, weights=df_35_1$poidsi)
# On regarde l'entropie pour ce groupe
seqHtplot(df_35_1.seq, main = "Entropy index")

# Accès à l'emploi après des études longues
df_35_2 <- df_35 %>%  filter(cluster_fusion_PAM_OM == "Accès à l'emploi après des études longues")
df_35_2.seq <- seqdef(df_35_2, 2:23, states = df_35.scode, labels = df_35.labels, weights=df_35_2$poidsi)
# On regarde l'entropie pour ce groupe
seqHtplot(df_35_2.seq, main = "Entropy index")

# Trajectoire marquée par une longue période au foyer
df_35_3 <- df_35 %>%  filter(cluster_fusion_PAM_OM == "Trajectoire marquée par une longue période au foyer")
df_35_3.seq <- seqdef(df_35_3, 2:23, states = df_35.scode, labels = df_35.labels, weights=df_35_3$poidsi)
# On regarde l'entropie pour ce groupe
seqHtplot(df_35_3.seq, main = "Entropy index")

# Trajectoire marquée par une longue période en tant qu'indépendant.e
df_35_4 <- df_35 %>%  filter(cluster_fusion_PAM_OM == "Trajectoire marquée par une longue période en tant qu'indépendant.e")
df_35_4.seq <- seqdef(df_35_4, 2:23, states = df_35.scode, labels = df_35.labels, weights=df_35_4$poidsi)
# On regarde l'entropie pour ce groupe
seqHtplot(df_35_4.seq, main = "Entropy index")

# Accès à l'emploi détérioré et marqué par une situation instable
df_35_5 <- df_35 %>%  filter(cluster_fusion_PAM_OM == "Accès à l'emploi détérioré et marqué par une situation instable")
df_35_5.seq <- seqdef(df_35_5, 2:23, states = df_35.scode, labels = df_35.labels, weights=df_35_5$poidsi)
# On regarde l'entropie pour ce groupe
seqHtplot(df_35_5.seq, main = "Entropie pour les individus du cluster 'Accès à l'emploi détérioré et marqué par une situation instable'")



### On regroupe ça sur un seul et même graphique
# On garde dans une liste l'entropie de chaque groupe
groupes <- unique(df_35$cluster_fusion_PAM_OM)
entropies <- list()

for (g in groupes) {
  seq_subset <- df_35.seq[df_35$cluster_fusion_PAM_OM == g, ]
  stats <- seqstatd(seq_subset)
  entropies[[as.character(g)]] <- stats$Entrop
}

# On crée la table avec les entropies
entropie_df <- do.call(rbind, lapply(names(entropies), function(g) {
  tibble(
    position = 14:35,
    entropie = entropies[[g]],
    groupe = g
  )
}))

# On affiche les courbes d'entropie sur un seul graph
library(ggplot2)

ggplot(entropie_df, aes(x = position, y = entropie, color = groupe)) +
  geom_line(linewidth = 1) +
  labs(title = "Comparaison de l'évolution de l'entropie par cluster",
       x = "Âge",
       y = "Indice d'entropie") +
  theme_minimal()





# Maintenant qu'on a fait ça, on va créer des stats descriptives pour les différents clusters


####################################################################################################################################################################
### Quelle est la répartition intra cluster en terme d'origines
clusters <- list(df_35_1, df_35_2, df_35_3, df_35_4, df_35_5, df_35)
pourcentages <- list()


for (i in 1:6) {
  # Pondération
  table_origine_pond <- tapply(clusters[[i]]$poidsi, clusters[[i]]$origine_tous_g2bis, sum, default = 0)

  # Pourcentages
  pourcentage_origine <- 100 * table_origine_pond / sum(table_origine_pond)
  
  pourcentage_origine <- round(pourcentage_origine, 2)
  
  pourcentages[[i]] <- pourcentage_origine
}

# On nomme les clusters
names(pourcentages) <- paste0("Cluster_", 1:5)

# On obtient le tableau suivant :
pourcentages_df <- do.call(cbind, pourcentages)
####################################################################################################################################################################

####################################################################################################################################################################
# Répartition des origines interclusters
#DF séquences par origine
df_35_maj <- df_35 %>% filter(origine_tous_g2bis == 1)
df_35_mag <- df_35 %>% filter(origine_tous_g2bis == 33)
df_35_af <- df_35 %>% filter(origine_tous_g2bis == 55)
df_35_mo <- df_35 %>% filter(origine_tous_g2bis == 66)
df_35_as <- df_35 %>% filter(origine_tous_g2bis == 77)
df_35_eurs <- df_35 %>% filter(origine_tous_g2bis == 88)
df_35_eurr <- df_35 %>% filter(origine_tous_g2bis == 99)
df_35_autres <- df_35 %>% filter(origine_tous_g2bis == 111)

origines <- list(df_35_maj, df_35_om,df_35_mag, df_35_af, df_35_mo, df_35_as, df_35_eurs, df_35_eurr, df_35_autres, df_35)
pourcentages2 <- list()

for (i in 1:10) {
  # Pondération
  table_cluster_pond <- tapply(origines[[i]]$poidsi, origines[[i]]$cluster_fusion_PAM_OM, sum, default = 0)
  
  # Pourcentages
  pourcentage_cluster <- 100 * table_cluster_pond / sum(table_cluster_pond)
  
  pourcentage_cluster <- round(pourcentage_cluster, 2)
  
  pourcentages2[[i]] <- pourcentage_cluster
}


# On obtient le tableau suivant :
pourcentages_cluster_df <- do.call(cbind, pourcentages2)


