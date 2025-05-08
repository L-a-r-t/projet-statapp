library(dplyr)
library(Hmisc)
library(rlang)
library(purrr)
# On commence par enlever les "autres" (origine) du df_35
df_35_bis <- df_35 %>% filter(origine_tous_g2bis != 111)


library(dplyr)
library(Hmisc)
library(purrr)
library(rlang)

library(dplyr)
library(questionr)
# Tables croisées origines
### On prépare les dataframes
# Par origine
df_35$cluster_fusion_PAM_OM <- cluster_fusion_PAM_OM
df_35_ <- df_35_bis %>% filter(origine_tous_g2bis == 1)
df_35_om <- df_35_bis %>% filter(origine_tous_g2bis == 22)
df_35_mag <- df_35_bis %>% filter(origine_tous_g2bis == 33)
df_35_afr <- df_35_bis %>% filter(origine_tous_g2bis == 55)
df_35_tur <- df_35_bis %>% filter(origine_tous_g2bis == 66)
df_35_asi <- df_35_bis %>% filter(origine_tous_g2bis == 77)
df_35_eur <- df_35_bis %>% filter(origine_tous_g2bis %in% c(88, 99))

# Par clusters (pour sexe)
df_35_ecole <- df_35_bis %>%  filter(cluster_fusion_PAM_OM == 1)
df_35_etudes <- df_35_bis %>%  filter(cluster_fusion_PAM_OM == 9065)
df_35_instab <- df_35_bis %>%  filter(cluster_fusion_PAM_OM == 9020)
df_35_indep <- df_35_bis %>%  filter(cluster_fusion_PAM_OM == 8533)
df_35_foyer <- df_35_bis %>%  filter(cluster_fusion_PAM_OM == 7798)

# Par PCS du père
### On recode la variable csnq
pcs <- indiv %>% select(ident, csnq_pere, csnq_mere, csnq_ego)
df_35_bis <- left_join(df_35_bis, pcs)
df_35_bis <- df_35_bis %>% mutate(pcs_pere_final = case_when(
  csnq_pere == 0 ~ 0, # jamais travaillé
  csnq_pere == 10 ~ 1, # agri
  csnq_pere == 20 ~2, # arti-comm
  csnq_pere == 30 ~3, #cadre-proflib
  csnq_pere == 40 ~4, #PI
  csnq_pere == 50 | csnq_pere == 60 ~ 5, #qualif
  csnq_pere == 51 | csnq_pere == 61 ~6, #non-qualif
  csnq_pere == 90 ~ 99 #NSP, refus, incodable
))

df_35_agri <- df_35_bis %>% filter(pcs_pere_final == 1)
df_35_arti <- df_35_bis %>% filter(pcs_pere_final == 2)
df_35_cadre <- df_35_bis %>% filter(pcs_pere_final == 3)
df_35_pi <- df_35_bis %>% filter(pcs_pere_final == 4)
df_35_qualif <- df_35_bis %>% filter(pcs_pere_final == 5)
df_35_nonqua <- df_35_bis %>% filter(pcs_pere_final == 6)
df_35_autres <- df_35_bis %>% filter(pcs_pere_final == 99)
df_35_jamais <- df_35_bis %>% filter(pcs_pere_final == 0)

# PCS mere
df_35_bis <- df_35_bis %>% mutate(pcs_mere_final = case_when(
  csnq_mere == 0 ~ 0, # jamais travaillé
  csnq_mere == 10 ~ 1, # agri
  csnq_mere == 20 ~2, # arti-comm
  csnq_mere == 30 ~3, #cadre-proflib
  csnq_mere == 40 ~4, #PI
  csnq_mere == 50 | csnq_mere == 60 ~ 5, #qualif
  csnq_mere == 51 | csnq_mere == 61 ~6, #non-qualif
  csnq_mere == 90 ~ 99 #NSP, refus, incodable
))

df_35_agri_m <- df_35_bis %>% filter(pcs_mere_final == 1)
df_35_arti_m <- df_35_bis %>% filter(pcs_mere_final == 2)
df_35_cadre_m <- df_35_bis %>% filter(pcs_mere_final == 3)
df_35_pi_m <- df_35_bis %>% filter(pcs_mere_final == 4)
df_35_qualif_m <- df_35_bis %>% filter(pcs_mere_final == 5)
df_35_nonqua_m <- df_35_bis %>% filter(pcs_mere_final == 6)
df_35_autres_m <- df_35_bis %>% filter(pcs_mere_final == 99)
df_35_jamais_m <- df_35_bis %>% filter(pcs_mere_final == 0)

# activité mère
df_35_toujours <- df_35_bis %>% filter(t_meract == 1)
df_35_jamais_taff <- df_35_bis %>% filter(t_meract == 2)
df_35_alterne <- df_35_bis %>% filter(t_meract == 3)
df_35_autres <- df_35_bis %>% filter(t_meract == 99)










##### On passe à la constitution des df à propremement parler
# Origines
maj_clusters <- df_35_maj %>%
  with(wtd.table(cluster_fusion_PAM_OM, weights = poidsi)) %>%
  prop.table() %>%
  `*`(100) %>%
  as.data.frame() %>%
  rename(PopMaj = Freq) %>%  select(-Var1)

om_clusters <- df_35_om %>%
  with(wtd.table(df_35_om$cluster_fusion_PAM_OM, weights = df_35_om$poidsi)) %>%
  prop.table() %>%
  `*`(100) %>%
  as.data.frame() %>%
  rename(OM = Freq) %>%  select(-Var1)

mag_clusters <- df_35_mag %>%
  with(wtd.table(df_35_mag$cluster_fusion_PAM_OM, weights = df_35_mag$poidsi)) %>%
  prop.table() %>%
  `*`(100) %>%
  as.data.frame() %>%
  rename(Magh = Freq) %>%  select(-Var1)

afr_clusters <- df_35_afr %>%
  with(wtd.table(df_35_afr$cluster_fusion_PAM_OM, weights = df_35_afr$poidsi)) %>%
  prop.table() %>%
  `*`(100) %>%
  as.data.frame() %>%
  rename(AfSub = Freq) %>%  select(-Var1)

tur_clusters <- df_35_tur %>%
  with(wtd.table(df_35_tur$cluster_fusion_PAM_OM, weights = df_35_tur$poidsi)) %>%
  prop.table() %>%
  `*`(100) %>%
  as.data.frame() %>%
  rename(TurqMo = Freq) %>%  select(-Var1)

asi_clusters <- df_35_asi %>%
  with(wtd.table(df_35_asi$cluster_fusion_PAM_OM, weights = df_35_asi$poidsi)) %>%
  prop.table() %>%
  `*`(100) %>%
  as.data.frame() %>%
  rename(Asie = Freq) %>%  select(-Var1)

eur_clusters <- df_35_eur %>%
  with(wtd.table(df_35_eurs$cluster_fusion_PAM_OM, weights = df_35_eurs$poidsi)) %>%
  prop.table() %>%
  `*`(100) %>%
  as.data.frame() %>%
  rename(Eur = Freq) %>%  select(-Var1)

global_clusters <- df_35_bis %>%
  with(wtd.table(df_35_bis$cluster_fusion_PAM_OM, weights = df_35_bis$poidsi)) %>%
  prop.table() %>%
  `*`(100) %>%
  as.data.frame() %>%
  rename(Global = Freq, Clusters = Var1)

df_croise_ori <- bind_cols(
  global_clusters,
  maj_clusters,
  mag_clusters,
  afr_clusters,
  tur_clusters,
  asi_clusters,
  eur_clusters
)  %>% mutate(across(where(is.numeric), ~ round(.x, 1)))



# activité de la mère
toujours_clusters <- as.data.frame(prop.table(wtd.table(df_35_toujours$cluster_fusion_PAM_OM , weights = df_35_toujours$poidsi)) * 100) %>%
  as.data.frame() %>%
  rename(Toujours = Freq)%>% select(-Var1)

jamais_clusters <- df_35_jamais_taff %>%
  with(wtd.table(df_35_jamais_taff$cluster_fusion_PAM_OM, weights = df_35_jamais_taff$poidsi)) %>%
  prop.table() %>%
  `*`(100) %>%
  as.data.frame() %>%
  rename(Jamais = Freq) %>%  select(-Var1)


alterne_clusters <- as.data.frame(prop.table(wtd.table(df_35_alterne$cluster_fusion_PAM_OM , weights = df_35_alterne$poidsi)) * 100) %>%
  as.data.frame() %>%
  rename(Alterne = Freq)%>% select(-Var1)

autres_clusters <- as.data.frame(prop.table(wtd.table(df_35_autres$cluster_fusion_PAM_OM , weights = df_35_autres$poidsi)) * 100) %>%
  as.data.frame() %>%
  rename(autres_clusters = Freq)%>% select(-Var1)



df_croise_activ_mere <- bind_cols(
  global_clusters,
  toujours_clusters,
  jamais_clusters,
  alterne_clusters,
  autres_clusters
)  %>% mutate(across(where(is.numeric), ~ round(.x, 1)))



# pcs pere
agri_clusters <- as.data.frame(prop.table(wtd.table(df_35_agri$cluster_fusion_PAM_OM , weights = df_35_agri$poidsi)) * 100) %>%
  as.data.frame() %>%
  rename(Agri = Freq)%>% select(-Var1)

arti_clusters <- df_35_arti %>%
  with(wtd.table(df_35_arti$cluster_fusion_PAM_OM, weights = df_35_arti$poidsi)) %>%
  prop.table() %>%
  `*`(100) %>%
  as.data.frame() %>%
  rename(Arti = Freq) %>%  select(-Var1)


cadre_clusters <- as.data.frame(prop.table(wtd.table(df_35_cadre$cluster_fusion_PAM_OM , weights = df_35_cadre$poidsi)) * 100) %>%
  as.data.frame() %>%
  rename(Cadre = Freq)%>% select(-Var1)

pi_clusters <- as.data.frame(prop.table(wtd.table(df_35_pi$cluster_fusion_PAM_OM , weights = df_35_pi$poidsi)) * 100) %>%
  as.data.frame() %>%
  rename(PI = Freq)%>% select(-Var1)

qual_clusters <- as.data.frame(prop.table(wtd.table(df_35_qualif$cluster_fusion_PAM_OM , weights = df_35_qualif$poidsi)) * 100) %>%
  as.data.frame() %>%
  rename(Qualif = Freq)%>% select(-Var1)

nonqual_clusters <- as.data.frame(prop.table(wtd.table(df_35_nonqua$cluster_fusion_PAM_OM , weights = df_35_nonqua$poidsi)) * 100) %>%
  as.data.frame() %>%
  rename(Nonqualif = Freq)%>% select(-Var1)

autres_clusters <- as.data.frame(prop.table(wtd.table(df_35_autres$cluster_fusion_PAM_OM , weights = df_35_autres$poidsi)) * 100) %>%
  as.data.frame() %>%
  rename(Autres = Freq)%>% select(-Var1)

jamais_clusters_pcsp <- as.data.frame(prop.table(wtd.table(df_35_jamais$cluster_fusion_PAM_OM , weights = df_35_jamais$poidsi)) * 100) %>%
  as.data.frame() %>%
  rename(Jamais = Freq)%>% select(-Var1)

global_clusters <- as.data.frame(prop.table(wtd.table(df_35_bis$cluster_fusion_PAM_OM , weights = df_35_bis$poidsi)) * 100) %>%
  as.data.frame() %>%
  rename(Global = Freq, Clusters = Var1)


df_croise_pcsp <- bind_cols(
  global_clusters,
  agri_clusters,
  arti_clusters,
  cadre_clusters,
  pi_clusters,
  qual_clusters,
  nonqual_clusters,
  jamais_clusters_pcsp,
  autres_clusters
)  %>% mutate(across(where(is.numeric), ~ round(.x, 1)))





# PCS mere
agri_clusters_m <- as.data.frame(prop.table(wtd.table(df_35_agri_m$cluster_fusion_PAM_OM , weights = df_35_agri_m$poidsi)) * 100) %>%
  as.data.frame() %>%
  rename(Agri = Freq)%>% select(-Var1)

arti_clusters_m <- df_35_arti_m %>%
  with(wtd.table(df_35_arti_m$cluster_fusion_PAM_OM, weights = df_35_arti_m$poidsi)) %>%
  prop.table() %>%
  `*`(100) %>%
  as.data.frame() %>%
  rename(Arti = Freq) %>%  select(-Var1)


cadre_clusters_m <- as.data.frame(prop.table(wtd.table(df_35_cadre_m$cluster_fusion_PAM_OM , weights = df_35_cadre_m$poidsi)) * 100) %>%
  as.data.frame() %>%
  rename(Cadre = Freq)%>% select(-Var1)

pi_clusters_m <- as.data.frame(prop.table(wtd.table(df_35_pi_m$cluster_fusion_PAM_OM , weights = df_35_pi_m$poidsi)) * 100) %>%
  as.data.frame() %>%
  rename(PI = Freq)%>% select(-Var1)

qual_clusters_m <- as.data.frame(prop.table(wtd.table(df_35_qualif_m$cluster_fusion_PAM_OM , weights = df_35_qualif_m$poidsi)) * 100) %>%
  as.data.frame() %>%
  rename(Qualif = Freq)%>% select(-Var1)

nonqual_clusters_m <- as.data.frame(prop.table(wtd.table(df_35_nonqua_m$cluster_fusion_PAM_OM , weights = df_35_nonqua_m$poidsi)) * 100) %>%
  as.data.frame() %>%
  rename(Nonqualif = Freq)%>% select(-Var1)

autres_clusters_m <- as.data.frame(prop.table(wtd.table(df_35_autres_m$cluster_fusion_PAM_OM , weights = df_35_autres_m$poidsi)) * 100) %>%
  as.data.frame() %>%
  rename(Autres = Freq)%>% select(-Var1)

jamais_clusters_pcsp_m <- as.data.frame(prop.table(wtd.table(df_35_jamais_m$cluster_fusion_PAM_OM , weights = df_35_jamais_m$poidsi)) * 100) %>%
  as.data.frame() %>%
  rename(Jamais = Freq)%>% select(-Var1)

global_clusters <- as.data.frame(prop.table(wtd.table(df_35_bis$cluster_fusion_PAM_OM , weights = df_35_bis$poidsi)) * 100) %>%
  as.data.frame() %>%
  rename(Global = Freq, Clusters = Var1)


df_croise_pcsm <- bind_cols(
  global_clusters_m,
  agri_clusters_m,
  arti_clusters_m,
  cadre_clusters_m,
  pi_clusters_m,
  qual_clusters_m,
  nonqual_clusters_m,
  jamais_clusters_pcsp_m,
  autres_clusters_m
)  %>% mutate(across(where(is.numeric), ~ round(.x, 1)))




#sexe
global_sexe <- df_35_bis%>%
  with(wtd.table(df_35_bis$sexee, weights = df_35_bis$poidsi)) %>%
  prop.table() %>%
  `*`(100) %>%
  as.data.frame() %>%
  rename(Sexe = Var1, Global = Freq)

etudes_sexe <- df_35_etudes %>%
  with(wtd.table(df_35_etudes$sexee, weights = df_35_etudes$poidsi)) %>%
  prop.table() %>%
  `*`(100) %>%
  as.data.frame() %>%
  rename(Etudes = Freq) %>%  select(-Var1)

ecole_sexe <- as.data.frame(prop.table(wtd.table(df_35_ecole$sexee , weights = df_35_ecole$poidsi)) * 100) %>%
  as.data.frame() %>%
  rename(Ecole = Freq) %>% select(-Var1)

foyer_sexe <- as.data.frame(prop.table(wtd.table(df_35_foyer$sexee, weights = df_35_foyer$poidsi)) * 100) %>%
  as.data.frame() %>%
  rename(Foyer = Freq)%>% select(-Var1)

instab_sexe <- as.data.frame(prop.table(wtd.table(df_35_instab$sexee , weights = df_35_instab$poidsi)) * 100) %>%
  as.data.frame() %>%
  rename(Instab = Freq)%>% select(-Var1)

indep_sexe <- as.data.frame(prop.table(wtd.table(df_35_indep$sexee , weights = df_35_indep$poidsi)) * 100) %>%
  as.data.frame() %>%
  rename(Indep = Freq)%>% select(-Var1)

df_croise_sexe <- bind_cols(
  global_sexe,
  etudes_sexe,
  ecole_sexe,
  foyer_sexe,
  instab_sexe,
  indep_sexe
)  %>% mutate(across(where(is.numeric), ~ round(.x, 1)))