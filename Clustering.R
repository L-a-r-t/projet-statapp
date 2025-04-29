library(readr)

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






##################
###########DHD

# On teste les différents algos de clustering et on regarde leur qualité à chaque fois

# PAM
result <- clusterForMethodAndDist(method="PAM", distance=disDHD, k=6)
cluster_PAM <- result$cluster$clustering

cluster_fusion_PAM <- cluster_PAM
cluster_fusion_PAM[cluster_PAM %in% c(9066)] <- 9004  # On fusionne deux clusters très similaires


# Qualité pour clusterisation PAM sans fusion 
sil <- silhouette(cluster_PAM, dist = disDHD)
sil_PAM <- as.data.frame(sil)
asw_PAM <- sil_PAM %>%
  group_by(cluster) %>%
  summarise(ASW = mean(sil_width)) %>%
  arrange(desc(ASW))
View(asw_PAM)

# Qualité pour clusterisation PAM avec fusion 
sil <- silhouette(cluster_fusion_PAM, dist = disDHD)
sil_PAM <- as.data.frame(sil)
asw_PAM_fusion <- sil_PAM %>%
  group_by(cluster) %>%
  summarise(ASW = mean(sil_width)) %>%
  arrange(desc(ASW))
View(asw_PAM_fusion)


cluster_PAM <- factor(cluster_PAM, # On renomme et on visualise nos clusters
                      levels = c(3369, 6982, 8964, 9004, 9053, 9066),
                      labels = c("Indépendant", "Instabilité", "Ecole- Emploi", "Etudes - Emploi", " Au Foyer", "Etudes - Emploi Bis"))
seqdplot(df_35.seq, group = cluster_PAM, border = NA)

cluster_fusion_PAM <- factor(cluster_fusion_PAM, # idem mais avec le cas fusionné
                             levels = c(3369, 6982, 8964, 9004, 9053),
                             labels = c("Indépendant", "Instabilité", "Ecole- Emploi", "Etudes - Emploi", " Au Foyer"))
seqdplot(df_35.seq, group = cluster_fusion_PAM, border = NA)

# CAH
result <- clusterForMethodAndDist(method="CAH", distance=disDHD, k=6)
cluster_CAH <- result$cutree
cluster_CAH <- factor(cluster_CAH,
                             levels = c(1, 2, 3, 4, 5, 6),
                             labels = c("Ecole - Emploi", "Etudes - Emploi", "Indépendant", "instabilité", "Au Foyer", "Etudes - Emploi Bis"))
seqdplot(df_35.seq, group = cluster_CAH, border = NA)

cluster_fusion_CAH <- cluster_CAH
cluster_fusion_CAH[cluster_CAH %in% c(2)] <- 6  # On fusionne deux clusters très similaires
cluster_fusion_CAH <- factor(cluster_fusion_CAH,
                             levels = c(1, 3, 4, 5, 6),
                             labels = c("Ecole - Emploi", "Indépendant", "instabilité", "Au Foyer", "Etudes - Emploi ?"))
seqdplot(df_35.seq, group = cluster_fusion_CAH, border = NA)

# Qualité pour clusterisation CAH sans fusion 
sil <- silhouette(cluster_CAH, dist = disDHD)
sil_CAH <- as.data.frame(sil)
asw_CAH <- sil_CAH %>%
  group_by(cluster) %>%
  summarise(ASW = mean(sil_width)) %>%
  arrange(desc(ASW))
View(asw_CAH)

# Qualité pour clusterisation CAH sans fusion 
sil <- silhouette(cluster_fusion_CAH, dist = disDHD)
sil_fusion_CAH <- as.data.frame(sil)
asw_fusion_CAH <- sil_fusion_CAH %>%
  group_by(cluster) %>%
  summarise(ASW = mean(sil_width)) %>%
  arrange(desc(ASW))
View(asw_fusion_CAH)


# CAHPAM
result <- clusterForMethodAndDist(method="CAHPAM", distance=disDHD, k=6)
cluster_CAHPAM <- result$cluster$clustering
cluster_CAHPAM <- factor(cluster_CAHPAM,
                                levels = c(131, 2, 2614, 36, 4, 5260),
                                labels = c("Ecole - Emploi", "Etudes - Emploi", "Instabilité", "Etudes - Emploi Bis","Indépendant", "Au foyer"))
seqdplot(df_35.seq, group = cluster_CAHPAM, border = NA)



cluster_fusion_CAHPAM <- cluster_CAHPAM
cluster_fusion_CAHPAM[cluster_CAHPAM %in% c(36)] <- 2  # On fusionne deux clusters très similaires
cluster_fusion_CAHPAM <- factor(cluster_fusion_CAHPAM,
                      levels = c(131, 2, 2614, 4, 5260),
                      labels = c("Ecole - Emploi", "Etudes - Emploi", "Instabilité", "Indépendant", "Au foyer"))
seqdplot(df_35.seq, group = cluster_fusion_CAHPAM, border = NA)




# Qualité pour clusterisation CAHPAM sans fusion 
sil <- silhouette(cluster_CAHPAM, dist = disDHD)
sil_CAHPAM <- as.data.frame(sil)
asw_CAHPAM <- sil_CAHPAM %>%
  group_by(cluster) %>%
  summarise(ASW = mean(sil_width)) %>%
  arrange(desc(ASW))
View(asw_CAHPAM)

# Qualité pour clusterisation CAHPAM avec fusion 
sil <- silhouette(cluster_fusion_CAHPAM, dist = disDHD)
sil_fusion_CAHPAM <- as.data.frame(sil)
asw_fusion_CAHPAM <- sil_fusion_CAHPAM %>%
  group_by(cluster) %>%
  summarise(ASW = mean(sil_width)) %>%
  arrange(desc(ASW))
View(asw_fusion_CAHPAM)











##################
###########OM

# On teste les différents algos de clustering et on regarde leur qualité à chaque fois

# PAM
result <- clusterForMethodAndDist(method="PAM", distance=disOM, k=6)
cluster_PAM_OM <- result$cluster$clustering
cluster_PAM_OM <- factor(cluster_PAM_OM,
                                levels = c(1, 7798, 8533, 8964, 9020, 9065),
                                labels = c("Ecole - Emploi", "Au Foyer", "Indépendant",  "Ecole - Emploi Bis","Instabilité", "Etudes - Emploi"))
seqdplot(df_35.seq, group = cluster_PAM_OM, border = NA)

cluster_fusion_PAM_OM <- cluster_PAM_OM
cluster_fusion_PAM_OM[cluster_PAM_OM %in% c(8964)] <- 1  # On fusionne deux clusters très similaires
cluster_fusion_PAM_OM <- factor(cluster_fusion_PAM_OM,
                                levels = c(1, 7798, 8533, 9020, 9065),
                                labels = c("Ecole - Emploi", "Au Foyer", "Indépendant", "Instabilité", "Etudes - Emploi"))
seqdplot(df_35.seq, group = cluster_fusion_PAM_OM, border = NA)

# Qualité pour clusterisation PAM sans fusion (OM)
sil <- silhouette(cluster_PAM_OM, dist = disOM)
sil_PAM_OM <- as.data.frame(sil)
asw_PAM_OM <- sil_PAM_OM %>%
  group_by(cluster) %>%
  summarise(ASW = mean(sil_width)) %>%
  arrange(desc(ASW))
View(asw_PAM_OM)

# Qualité pour clusterisation PAM avec fusion 
sil <- silhouette(cluster_fusion_PAM_OM, dist = disOM)
sil_PAM_OM <- as.data.frame(sil)
asw_PAM_fusion_OM <- sil_PAM_OM %>%
  group_by(cluster) %>%
  summarise(ASW = mean(sil_width)) %>%
  arrange(desc(ASW))
View(asw_PAM_fusion_OM)


# CAH
result <- clusterForMethodAndDist(method="CAH", distance=disOM, k=6)
cluster_CAH_OM <- result$cutree
seqdplot(df_35.seq, group = cluster_CAH_OM, border = NA)

cluster_fusion_CAH_OM <- cluster_CAH_OM
cluster_fusion_CAH_OM[cluster_CAH_OM %in% c(6)] <- 2  # On fusionne deux clusters très similaires
seqdplot(df_35.seq, group = cluster_fusion_CAH_OM, border = NA)

# Qualité pour clusterisation CAH sans fusion 
sil <- silhouette(cluster_CAH, dist = disOM)
sil_CAH_OM <- as.data.frame(sil)
asw_CAH_OM <- sil_CAH_OM %>%
  group_by(cluster) %>%
  summarise(ASW = mean(sil_width)) %>%
  arrange(desc(ASW))
View(asw_CAH_OM)

# Qualité pour clusterisation CAH avec fusion 
sil <- silhouette(cluster_fusion_CAH_OM, dist = disOM)
sil_fusion_CAH_OM <- as.data.frame(sil)
asw_fusion_CAH_OM <- sil_fusion_CAH_OM %>%
  group_by(cluster) %>%
  summarise(ASW = mean(sil_width)) %>%
  arrange(desc(ASW))
View(asw_fusion_CAH_OM)

# CAHPAM
result <- clusterForMethodAndDist(method="CAHPAM", distance=disOM, k=6)
cluster_CAHPAM_OM <- result$cluster$clustering
seqdplot(df_35.seq, group = cluster_CAHPAM_OM, border = NA)

cluster_fusion_CAHPAM_OM <- cluster_CAHPAM_OM
cluster_fusion_CAHPAM_OM[cluster_CAHPAM_OM %in% c(131)] <- 1  # On fusionne deux clusters très similaires
seqdplot(df_35.seq, group = cluster_fusion_CAHPAM_OM, border = NA)


# Qualité pour clusterisation CAHPAM sans fusion 
sil <- silhouette(cluster_CAHPAM_OM, dist = disOM)
sil_CAHPAM_OM <- as.data.frame(sil)
asw_CAHPAM_OM <- sil_CAHPAM_OM %>%
  group_by(cluster) %>%
  summarise(ASW = mean(sil_width)) %>%
  arrange(desc(ASW))
View(asw_CAHPAM_OM)

# Qualité pour clusterisation CAHPAM avec fusion 
sil <- silhouette(cluster_fusion_CAHPAM_OM, dist = disOM)
sil_fusion_CAHPAM_OM <- as.data.frame(sil)
asw_fusion_CAHPAM_OM <- sil_fusion_CAHPAM_OM %>%
  group_by(cluster) %>%
  summarise(ASW = mean(sil_width)) %>%
  arrange(desc(ASW))
View(asw_fusion_CAHPAM_OM)











##################

###########ham

# On teste les différents algos de clustering et on regarde leur qualité à chaque fois

# PAM
result <- clusterForMethodAndDist(method="PAM", distance=disham, k=6)
cluster_PAM_ham <- result$cluster$clustering
seqdplot(df_35.seq, group = cluster_PAM_ham, border = NA)

cluster_fusion_PAM_ham <- cluster_PAM_ham
cluster_fusion_PAM_ham[cluster_PAM_ham %in% c(9066)] <- 9004  # On fusionne deux clusters très similaires
seqdplot(df_35.seq, group = cluster_fusion_PAM_ham, border = NA)

# Qualité pour clusterisation PAM sans fusion 
sil <- silhouette(cluster_PAM_ham, dist = disham)
sil_PAM_ham <- as.data.frame(sil)
asw_PAM_ham <- sil_PAM_ham %>%
  group_by(cluster) %>%
  summarise(ASW = mean(sil_width)) %>%
  arrange(desc(ASW))
View(asw_PAM_ham)

# Qualité pour clusterisation PAM avec fusion 
sil <- silhouette(cluster_fusion_PAM_ham, dist = disham)
sil_PAM_ham <- as.data.frame(sil)
asw_PAM_fusion_ham <- sil_PAM_ham %>%
  group_by(cluster) %>%
  summarise(ASW = mean(sil_width)) %>%
  arrange(desc(ASW))
View(asw_PAM_fusion_ham)


# CAH
result <- clusterForMethodAndDist(method="CAH", distance=disham, k=6)
cluster_CAH_ham <- result$cutree
seqdplot(df_35.seq, group = cluster_CAH_ham, border = NA)

cluster_fusion_CAH_ham <- cluster_CAH_ham
cluster_fusion_CAH_ham[cluster_CAH_ham %in% c(2)] <- 1  # On fusionne deux clusters très similaires
seqdplot(df_35.seq, group = cluster_fusion_CAH_ham, border = NA)

# Qualité pour clusterisation CAH sans fusion 
sil <- silhouette(cluster_CAH_ham, dist = disham)
sil_CAH_ham <- as.data.frame(sil)
asw_CAH_ham <- sil_CAH_ham %>%
  group_by(cluster) %>%
  summarise(ASW = mean(sil_width)) %>%
  arrange(desc(ASW))
View(asw_CAH_ham)

# Qualité pour clusterisation CAH sans fusion 
sil <- silhouette(cluster_fusion_CAH_ham, dist = disham)
sil_fusion_CAH_ham <- as.data.frame(sil)
asw_fusion_CAH_ham <- sil_fusion_CAH_ham %>%
  group_by(cluster) %>%
  summarise(ASW = mean(sil_width)) %>%
  arrange(desc(ASW))
View(asw_fusion_CAH_ham)


# CAHPAM
result <- clusterForMethodAndDist(method="CAHPAM", distance=disham, k=6)
cluster_CAHPAM_ham <- result$cluster$clustering
seqdplot(df_35.seq, group = cluster_CAHPAM_ham, border = NA)



cluster_fusion_CAHPAM_ham <- cluster_CAHPAM_ham
cluster_fusion_CAHPAM_ham[cluster_CAHPAM_ham %in% c(1)] <- 3  # On fusionne deux clusters très similaires
seqdplot(df_35.seq, group = cluster_fusion_CAHPAM_ham, border = NA)




# Qualité pour clusterisation CAHPAM sans fusion 
sil <- silhouette(cluster_CAHPAM_ham, dist = disham)
sil_CAHPAM_ham <- as.data.frame(sil)
asw_CAHPAM_ham <- sil_CAHPAM_ham %>%
  group_by(cluster) %>%
  summarise(ASW = mean(sil_width)) %>%
  arrange(desc(ASW))
View(asw_CAHPAM_ham)

# Qualité pour clusterisation CAHPAM avec fusion 
sil <- silhouette(cluster_fusion_CAHPAM_ham, dist = disham)
sil_fusion_CAHPAM_ham <- as.data.frame(sil)
asw_fusion_CAHPAM_ham <- sil_fusion_CAHPAM_ham %>%
  group_by(cluster) %>%
  summarise(ASW = mean(sil_width)) %>%
  arrange(desc(ASW))
View(asw_fusion_CAHPAM_ham)
##################
##################

###########OMtr

# On teste les différents algos de clustering et on regarde leur qualité à chaque fois

# PAM
result <- clusterForMethodAndDist(method="PAM", distance=disOMtr, k=6)
cluster_PAM_omtr <- result$cluster$clustering
seqdplot(df_35.seq, group = cluster_PAM_omtr, border = NA)

cluster_fusion_PAM_omtr <- cluster_PAM_omtr
cluster_fusion_PAM_omtr[cluster_PAM_omtr %in% c(8964)] <- 11  # On fusionne deux clusters très similaires
seqdplot(df_35.seq, group = cluster_fusion_PAM_omtr, border = NA)

# Qualité pour clusterisation PAM sans fusion 
sil <- silhouette(cluster_PAM_omtr, dist = disOMtr)
sil_PAM_omtr <- as.data.frame(sil)
asw_PAM_omtr <- sil_PAM_omtr %>%
  group_by(cluster) %>%
  summarise(ASW = mean(sil_width)) %>%
  arrange(desc(ASW))
View(asw_PAM_omtr)

# Qualité pour clusterisation PAM avec fusion 
sil <- silhouette(cluster_fusion_PAM_omtr, dist = disOMtr)
sil_PAM_omtr <- as.data.frame(sil)
asw_PAM_fusion_omtr <- sil_PAM_omtr %>%
  group_by(cluster) %>%
  summarise(ASW = mean(sil_width)) %>%
  arrange(desc(ASW))
View(asw_PAM_fusion_omtr)


# CAH
result <- clusterForMethodAndDist(method="CAH", distance=disOMtr, k=6)
cluster_CAH_omtr <- result$cutree
seqdplot(df_35.seq, group = cluster_CAH_omtr, border = NA)

cluster_fusion_CAH_omtr <- cluster_CAH_omtr
cluster_fusion_CAH_omtr[cluster_CAH_omtr %in% c(5)] <- 1  # On fusionne deux clusters très similaires
seqdplot(df_35.seq, group = cluster_fusion_CAH_omtr, border = NA)

# Qualité pour clusterisation CAH sans fusion 
sil <- silhouette(cluster_CAH_omtr, dist = disOMtr)
sil_CAH_omtr <- as.data.frame(sil)
asw_CAH_omtr <- sil_CAH_omtr %>%
  group_by(cluster) %>%
  summarise(ASW = mean(sil_width)) %>%
  arrange(desc(ASW))
View(asw_CAH_omtr)

# Qualité pour clusterisation CAH sans fusion 
sil <- silhouette(cluster_fusion_CAH_omtr, dist = disOMtr)
sil_fusion_CAH_omtr <- as.data.frame(sil)
asw_fusion_CAH_omtr <- sil_fusion_CAH_omtr %>%
  group_by(cluster) %>%
  summarise(ASW = mean(sil_width)) %>%
  arrange(desc(ASW))
View(asw_fusion_CAH_omtr)


# CAHPAM
result <- clusterForMethodAndDist(method="CAHPAM", distance=disOMtr, k=6)
cluster_CAHPAM_omtr <- result$cluster$clustering
seqdplot(df_35.seq, group = cluster_CAHPAM_omtr, border = NA)



cluster_fusion_CAHPAM_omtr <- cluster_CAHPAM_omtr
cluster_fusion_CAHPAM_omtr[cluster_CAHPAM_omtr %in% c(119)] <- 2  # On fusionne deux clusters très similaires
seqdplot(df_35.seq, group = cluster_fusion_CAHPAM_omtr, border = NA)




# Qualité pour clusterisation CAHPAM sans fusion 
sil <- silhouette(cluster_CAHPAM_omtr, dist = disOMtr)
sil_CAHPAM_omtr <- as.data.frame(sil)
asw_CAHPAM_omtr <- sil_CAHPAM_omtr %>%
  group_by(cluster) %>%
  summarise(ASW = mean(sil_width)) %>%
  arrange(desc(ASW))
View(asw_CAHPAM_omtr)

# Qualité pour clusterisation CAHPAM avec fusion 
sil <- silhouette(cluster_fusion_CAHPAM_omtr, dist = disOMtr)
sil_fusion_CAHPAM_omtr <- as.data.frame(sil)
asw_fusion_CAHPAM_omtr <- sil_fusion_CAHPAM_omtr %>%
  group_by(cluster) %>%
  summarise(ASW = mean(sil_width)) %>%
  arrange(desc(ASW))
View(asw_fusion_CAHPAM_omtr)
###########






###########Stricte (disNMSmst)

# On teste les différents algos de clustering et on regarde leur qualité à chaque fois

# PAM
result <- clusterForMethodAndDist(method="PAM", distance=disNMSmst, k=6)
cluster_PAM_NMSmst <- result$cluster$clustering
seqdplot(df_35.seq, group = cluster_PAM_NMSmst, border = NA)


# Qualité pour clusterisation PAM sans fusion 
sil <- silhouette(cluster_PAM_NMSmst, dist = disNMSmst)
sil_PAM_NMSmst <- as.data.frame(sil)
asw_PAM_NMSmst <- sil_PAM_NMSmst %>%
  group_by(cluster) %>%
  summarise(ASW = mean(sil_width)) %>%
  arrange(desc(ASW))
View(asw_PAM_NMSmst)



# CAH
result <- clusterForMethodAndDist(method="CAH", distance=disNMSmst, k=6)
cluster_CAH_NMSmst <- result$cutree
seqdplot(df_35.seq, group = cluster_CAH_NMSmst, border = NA)


# Qualité pour clusterisation CAH sans fusion 
sil <- silhouette(cluster_CAH_NMSmst, dist = disNMSmst)
sil_CAH_NMSmst <- as.data.frame(sil)
asw_CAH_NMSmst <- sil_CAH_NMSmst %>%
  group_by(cluster) %>%
  summarise(ASW = mean(sil_width)) %>%
  arrange(desc(ASW))
View(asw_CAH_NMSmst)



# CAHPAM
result <- clusterForMethodAndDist(method="CAHPAM", distance=disNMSmst, k=6)
cluster_CAHPAM_NMSmst <- result$cluster$clustering
seqdplot(df_35.seq, group = cluster_CAHPAM_NMSmst, border = NA)



cluster_fusion_CAHPAM_NMSmst <- cluster_CAHPAM_NMSmst
cluster_fusion_CAHPAM_NMSmst[cluster_CAHPAM_NMSmst %in% c(131)] <- 8  # On fusionne deux clusters très similaires
seqdplot(df_35.seq, group = cluster_fusion_CAHPAM_NMSmst, border = NA)




# Qualité pour clusterisation CAHPAM sans fusion 
sil <- silhouette(cluster_CAHPAM_NMSmst, dist = disNMSmst)
sil_CAHPAM_NMSmst <- as.data.frame(sil)
asw_CAHPAM_NMSmst <- sil_CAHPAM_NMSmst %>%
  group_by(cluster) %>%
  summarise(ASW = mean(sil_width)) %>%
  arrange(desc(ASW))
View(asw_CAHPAM_NMSmst)

# Qualité pour clusterisation CAHPAM avec fusion 
sil <- silhouette(cluster_fusion_CAHPAM_NMSmst, dist = disNMSmst)
sil_fusion_CAHPAM_NMSmst <- as.data.frame(sil)
asw_fusion_CAHPAM_NMSmst <- sil_fusion_CAHPAM_NMSmst %>%
  group_by(cluster) %>%
  summarise(ASW = mean(sil_width)) %>%
  arrange(desc(ASW))
View(asw_fusion_CAHPAM_NMSmst)
##################









# On part du principe qu'on garde PAM + OM
#######
# On va nommer les clusters ("avec Céreq Études no 57 • Les trajectoires d’entrée dans la vie active : de la sortie des études jusqu’à 6 ans après")
#######
result <- clusterForMethodAndDist(method="PAM", distance=disOM, k=6)
cluster_PAM_OM <- result$cluster$clustering
seqdplot(df_35.seq, group = cluster_PAM_OM, border = NA)

cluster_fusion_PAM_OM <- cluster_PAM_OM
cluster_fusion_PAM_OM[cluster_PAM_OM %in% c(8964)] <- 1  # On fusionne deux clusters très similaires
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

