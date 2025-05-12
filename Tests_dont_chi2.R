##########################################################################
# Tests sur le cluster indep
#######################################################################



# On veut regarder combien d'individus du cluster indépendant ont eu un emploi avant d'être indépendant.es

# On recrée notre df indépendants
df_35$cluster_fusion_PAM_OM <- cluster_fusion_PAM_OM # ajout de la variable d'appartenance aux clusters
df_35_indep <- df_35 %>%  filter(cluster_fusion_PAM_OM == "Indépendant")
df_35_indep.seq <- seqdef(df_35_indep, 2:23, states = df_35.scode, labels = df_35.labels, weights=df_35_indep$poidsi) # séquences


# On fait la fonction à proprement parler
check_salarie_before_indep <- function(row) {
  pos1 <- which(row == 1)
  pos2 <- which(row == 2)
  if (length(pos1) == 0 || length(pos2) == 0) return(FALSE)  # Si l'une des deux modalités est absente
  return(min(pos1) < min(pos2))  # Vérifie si le premier 1 vient avant le premier 2
}

# on ajoute les résultats dans notre df pour pouvoir faire nos stats
res <- apply(df_35_indep.seq, 1, check_salarie_before_indep)
df_35_indep.seq <- cbind(df_35_indep.seq, salarie_avant_indep = as.integer(res))  # 1 pour TRUE, 0 pour FALSE

# 42,9% ont été en emploi avant d'être indépendants



# On va faire une vérification similaire avec le fait q'iels aient connu une situation d'instabilité avant dêtre indep
check_instab_before_indep <- function(row) {
  pos_instab <- which(row == 3)
  pos_indep <- which(row == 2)
  pos_trans <- which(row == 6)
  
  # Si indépendance (2) est absente, on ne peut rien conclure
  if (length(pos_indep) == 0) return(FALSE)
  
  # Vérifie si instabilité (3) OU  (6) arrivent avant la première indépendance (2)
  return(
    (length(pos_instab) > 0 && min(pos_instab) < min(pos_indep)) |
      (length(pos_trans) > 0 && min(pos_trans) < min(pos_indep))
  )
}


# on ajoute les résultats dans notre df pour pouvoir faire nos stats
res1 <- apply(df_35_indep.seq, 1, check_instab_before_indep)
df_35_indep.seq <- cbind(df_35_indep.seq, instab_avant_indep = as.integer(res1))  # 1 pour TRUE, 0 pour FALSE


# Exemple de données (à adapter avec tes propres données)
mat <- as.matrix(read.table("chemin/vers/ton_fichier.csv", sep = ",", header = TRUE))

# Fonction pour vérifier si la modalité 1 apparaît avant 2
check_instab_before_indep2 <- function(row) {
  pos3 <- which(row == 3)
  pos2 <- which(row == 2)
  if (length(pos3) == 0 || length(pos2) == 0) return(FALSE)  # Si l'une des deux modalités est absente
  return(min(pos3) < min(pos2))  # Vérifie si le premier 1 vient avant le premier 2
}

# Seulement 8,0824,5% ont connu une situation d'instabilité avant d'être independant.e (24,5% si on inclue la situation "autre" ou pas dedans)





##########################################################################
# Tests du chi2 sur les tables croisées
#######################################################################
library(questionr)
library(survey)
######## ORIGINE ####################################################
# Créer le tableau croisé avec effectifs pondérés
tableau_origine <- xtabs(poidsi ~  cluster_PAM_OM + origine_tous_g2bis, data = df_35)
print(tableau_origine)

# On fait le test du chi² : premier package = survey
design <- svydesign(ids = ~1, data = df_35, weights = ~poidsi) # on crée un plan de sondage pondéré
svychisq(~ cluster_PAM_OM + origine_tous_g2bis, design)


# On teste de regarder les résidus du chi², pas conseillé quand on a des fréquences pondérées
obs <- svytable(~ cluster_PAM_OM + origine_tous_g2bis, design)
expected <- outer(rowSums(obs), colSums(obs)) / sum(obs) # effectifs attendus sous H0 : indépendance
residus <- (obs - expected) / sqrt(expected) # résidus de Pearson
round(residus, 2)


# On recrée les strates du plan de sondage################
strates <- indiv %>% select(ident, origine_tous_g2)
df_35 <- left_join(df_35, strates)
df_35$strate_origine <- with(df_35, case_when(
  origine_tous_g2 %in% c(33) ~ "Algérie",
  origine_tous_g2 %in% c(44) ~ "Maroc_Tunisie",
  origine_tous_g2 %in% c(55) ~ "Afrique_Sahel",
  origine_tous_g2 %in% c(66) ~ "Afrique_Centrale_Guinee",
  origine_tous_g2 %in% c(77) ~ "Afrique_Autre",
  origine_tous_g2 %in% c(88) ~ "Asie_SE",
  origine_tous_g2 %in% c(99) ~ "Turquie_MoyOrient",
  origine_tous_g2 %in% c(121, 131) ~ "Portugal_ESP_ITA",
  origine_tous_g2 %in% c(141) ~ "UE27_autres",
  origine_tous_g2 %in% c(151) ~ "Europe_autres",
  origine_tous_g2 %in% c(161) ~ "Autres_pays",
  origine_tous_g2 %in% c(22) ~ "DOM",
  TRUE ~ "Hors_G2"  # Inclut les 0, 10, 11, etc.
))

# On crée un plan de sondage qui est censé respecter celui de TeO2
design_strate <- svydesign(
  ids = ~1,
  strata = ~strate_origine,
  weights = ~poidsi,
  data = df_35
)
####################################################################
# On refait le test
svychisq(~ cluster_PAM_OM + origine_tous_g2bis, design_strate)
# On regarde à nouveau les résidus
obs <- svytable(~ cluster_PAM_OM + origine_tous_g2bis, design_strate)
expected <- outer(rowSums(obs), colSums(obs)) / sum(obs) # effectifs attendus sous H0 : indépendance
residus <- (obs - expected) / sqrt(expected) # résidus de Pearson
round(residus, 2)





######## activité mère ####################################################
meract <- indiv %>% select(ident, t_meract)%>% mutate(t_meract = case_when(
  t_meract == 1 ~ 1, #  a toujours travaillé
  t_meract == 2 ~ 2, # jamais travaillée
  t_meract == 3 ~ 3, # alterne les deux
  TRUE ~99 # valeurs manquantes, refus, etc
))
df_35 <- inner_join(df_35, meract, by=join_by(ident))

# On fait le test chi² pour activité de la mère
design <- svydesign(ids = ~1, data = df_35, weights = ~poidsi) # on crée un plan de sondage pondéré
svychisq(~ cluster_PAM_OM + t_meract.y, design)
# On regarde à nouveau les résidus
obs <- svytable(~ cluster_PAM_OM + t_meract.y, design)
expected <- outer(rowSums(obs), colSums(obs)) / sum(obs) # effectifs attendus sous H0 : indépendance
residus <- (obs - expected) / sqrt(expected) # résidus de Pearson
round(residus, 2)

######## pcs père ####################################################
# On fait le test chi² pour la pcs du père
design <- svydesign(ids = ~1, data = df_35, weights = ~poidsi) # on crée un plan de sondage pondéré
svychisq(~ cluster_PAM_OM + pcs_pere_bis, design)
# On regarde à nouveau les résidus
obs <- svytable(~ cluster_PAM_OM + pcs_pere_bis, design)
expected <- outer(rowSums(obs), colSums(obs)) / sum(obs) # effectifs attendus sous H0 : indépendance
residus <- (obs - expected) / sqrt(expected) # résidus de Pearson
round(residus, 2)

######## pcs mère ####################################################
pcsmere <- indiv %>% select(ident, csnq_mere) %>%  mutate(pcs_mere_bis = case_when(
  csnq_mere == 0 ~ 0, #jamais L
  csnq_mere == 10 ~1, #agri
  csnq_mere == 20 ~2, #articomm
  csnq_mere==30~3,#cadrepl
  csnq_mere==40~4,#pi
  csnq_mere %in% c(50,60)~5,#qualifié
  csnq_mere %in% c(51,61)~6,#nonqualif
  TRUE ~ 99#autre
))
df_35 <- inner_join(df_35,pcsmere)

# On fait le test du chi² pour la pcs de la mere
design <- svydesign(ids = ~1, data = df_35, weights = ~poidsi) # on crée un plan de sondage pondéré
svychisq(~ cluster_PAM_OM + pcs_mere_bis, design)
# On regarde à nouveau les résidus
obs <- svytable(~ cluster_PAM_OM + pcs_mere_bis, design)
expected <- outer(rowSums(obs), colSums(obs)) / sum(obs) # effectifs attendus sous H0 : indépendance
residus <- (obs - expected) / sqrt(expected) # résidus de Pearson
round(residus, 2)


######## parents = couple mixte ou pas ####################################################
# On fait chi² avec couple mixte
design <- svydesign(ids = ~1, data = df_35, weights = ~poidsi) # on crée un plan de sondage pondéré
svychisq(~ cluster_PAM_OM + parents_mixte, design)
# On regarde à nouveau les résidus
obs <- svytable(~ cluster_PAM_OM + parents_mixte, design)
expected <- outer(rowSums(obs), colSums(obs)) / sum(obs) # effectifs attendus sous H0 : indépendance
residus <- (obs - expected) / sqrt(expected) # résidus de Pearson
round(residus, 2)