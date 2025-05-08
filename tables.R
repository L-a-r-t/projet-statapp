# Tables croisées origines
df_35_bis <- df_35

df_35_maj <- df_35_bis %>% filter(origine_tous_g2bis == 1)
df_35_om <- df_35_bis %>% filter(origine_tous_g2bis == 22)
df_35_mag <- df_35_bis %>% filter(origine_tous_g2bis == 33)
df_35_afr <- df_35_bis %>% filter(origine_tous_g2bis == 55)
df_35_tur <- df_35_bis %>% filter(origine_tous_g2bis == 66)
df_35_asi <- df_35_bis %>% filter(origine_tous_g2bis == 77)
df_35_eur <- df_35_bis %>% filter(origine_tous_g2bis == 88)

# Par clusters (pour sexe)
df_35_ecole <- df_35_bis %>%  filter(cluster_fusion_PAM_OM == "Accès rapide et stable à l’emploi")
df_35_etudes <- df_35_bis %>%  filter(cluster_fusion_PAM_OM == "Insertion tardive mais stable après des études longues")
df_35_instab <- df_35_bis %>%  filter(cluster_fusion_PAM_OM == "Instabilité durable")
df_35_indep <- df_35_bis %>%  filter(cluster_fusion_PAM_OM == "Travail indépendant et emploi salarié")
df_35_foyer <- df_35_bis %>%  filter(cluster_fusion_PAM_OM == "Allers-retours entre emploi et foyer")

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
df_35_jamais_m <- df_35_bis %>% filter(pcs_mere_final == 0)

# activité mère
df_35_toujours <- df_35_bis %>% filter(t_meract == 1)
df_35_jamais_taff <- df_35_bis %>% filter(t_meract == 2)
df_35_alterne <- df_35_bis %>% filter(t_meract == 3)


get_cluster_stats <- function(data, poids_var = "poidsi", label) {
  data %>%
    group_by(cluster_fusion_PAM_OM) %>%
    summarise(
      n_brut = n(),
      n_pond = sum(.data[[poids_var]], na.rm = TRUE)
    ) %>%
    mutate(
      pct_pond = round(prop.table(n_pond) * 100, 1)
    ) %>%
    rename(Clusters = cluster_fusion_PAM_OM) %>%
    select(Clusters, n_brut, n_pond, pct_pond) %>%
    mutate(Source = label)
}






##### On passe à la constitution des df à propremement parler
# Origines
liste_df_or <- list(
  "Global" = df_35_bis,
  "Population majoritaire" = df_35_maj,
  "Outre-Mer"   = df_35_om,
  "Maghreb"  = df_35_mag,
  "Afrique subsaharienne" = df_35_afr,
  "Turquie&Moyen-Orient"  = df_35_tur,
  "Reste de l'Asie" = df_35_asi,
  "Europe" = df_35_eur
)

stats_list_or <- purrr::imap_dfr(liste_df_or, ~ get_cluster_stats(.x, label = .y))

df_n_brut_or <- stats_list_or %>%
  select(Clusters, Source, n_brut) %>%
  pivot_wider(names_from = Source, values_from = n_brut)

df_n_pond_or <- stats_list_or %>%
  select(Clusters, Source, n_pond) %>%
  pivot_wider(names_from = Source, values_from = n_pond)

df_pct_or <- stats_list_or %>%
  select(Clusters, Source, pct_pond) %>%
  pivot_wider(names_from = Source, values_from = pct_pond)



# activité de la mère
liste_df_acm <- list(
  "Global" = df_35_bis,
  "Toujours" = df_35_toujours,
  "Jamais" =df_35_jamais_taff,
  "Alterne"=df_35_alterne
)

stats_list_acm <- purrr::imap_dfr(liste_df_acm, ~ get_cluster_stats(.x, label = .y))

df_n_brut_acm <- stats_list_acm %>%
  select(Clusters, Source, n_brut) %>%
  pivot_wider(names_from = Source, values_from = n_brut)

df_n_pond_acm <- stats_list_acm %>%
  select(Clusters, Source, n_pond) %>%
  pivot_wider(names_from = Source, values_from = n_pond)

df_pct_acm <- stats_list_acm %>%
  select(Clusters, Source, pct_pond) %>%
  pivot_wider(names_from = Source, values_from = pct_pond)



# pcs pere
liste_df <- list(
  "Global" = df_35_bis,
  "Agri"   = df_35_agri,
  "Arti"   = df_35_arti,
  "Cadre"  = df_35_cadre,
  "PI"     = df_35_pi,
  "Qual"   = df_35_qualif,
  "Non Qual" = df_35_nonqua,
  "Jamais" = df_35_jamais
)

stats_list <- purrr::imap_dfr(liste_df, ~ get_cluster_stats(.x, label = .y))

df_n_brut_pcsp <- stats_list %>%
  select(Clusters, Source, n_brut) %>%
  pivot_wider(names_from = Source, values_from = n_brut)

df_n_pond_pcsp <- stats_list %>%
  select(Clusters, Source, n_pond) %>%
  pivot_wider(names_from = Source, values_from = n_pond)

df_pct_pcsp <- stats_list %>%
  select(Clusters, Source, pct_pond) %>%
  pivot_wider(names_from = Source, values_from = pct_pond)


# PCS mere
liste_df_m <- list(
  "Global" = df_35_bis,
  "Agri"   = df_35_agri_m,
  "Arti"   = df_35_arti_m,
  "Cadre"  = df_35_cadre_m,
  "PI" = df_35_pi_m,
  "Qual"   = df_35_qualif_m,
  "Non Qual" = df_35_nonqua_m,
  "Jamais" = df_35_jamais_m
)

stats_list_m <- purrr::imap_dfr(liste_df_m, ~ get_cluster_stats(.x, label = .y))

df_n_brut_pcsm <- stats_list_m %>%
  select(Clusters, Source, n_brut) %>%
  pivot_wider(names_from = Source, values_from = n_brut)

df_n_pond_pcsm <- stats_list_m %>%
  select(Clusters, Source, n_pond) %>%
  pivot_wider(names_from = Source, values_from = n_pond)

df_pct_pcsm <- stats_list_m %>%
  select(Clusters, Source, pct_pond) %>%
  pivot_wider(names_from = Source, values_from = pct_pond)

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

ecole_sexe <- as.data.frame(prop.table(wtd.table(df_35_ecole$sexee , weights = df_35_ecole$poidsi)) * 100) %>%
  as.data.frame() %>%
  rename(Ecole = Freq) %>% select(-Var1)

foyer_sexe <- as.data.frame(prop.table(wtd.table(df_35_foyer$sexee, weights = df_35_foyer$poidsi)) * 100) %>%
  as.data.frame() %>%
  rename(Foyer = Freq)%>% select(-Var1)

instab_sexe <- as.data.frame(prop.table(wtd.table(df_35_instab$sexee , weights = df_35_instab$poidsi)) * 100) %>%
  as.data.frame() %>%
  rename(Instab = Freq)%>% select(-Var1)


df_croise_sexe <- bind_cols(
  global_sexe,
  etudes_sexe,
  ecole_sexe,
  foyer_sexe,
  instab_sexe,
  indep_sexe
)  %>% mutate(across(where(is.numeric), ~ round(.x, 1)))

df_croise_sexe[[1]] <- as.character(df_croise_sexe[[1]])
df_croise_sexe[1, 1] <- "Homme"
df_croise_sexe[2, 1] <- "Femme"
