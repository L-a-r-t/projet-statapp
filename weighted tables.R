# On recode la variable pour avoir des résultats plus lisibles
test <- test %>% mutate(t_meract = case_when(
  t_meract == 1 ~ 1, #  a toujours travaillé
  t_meract == 2 ~ 2, # jamais travaillée
  t_meract == 3 ~ 3, # alterne les deux
  TRUE ~99 # valeurs manquantes, refus, etc
))

#On va faire les tables 
### On commence par faire une table par cluster
# attention penser à avoir la variable pcs_pere_bis
df_35_ecole <- df_35 %>%  filter(cluster_fusion_PAM_OM == 1)
df_35_etudes <- df_35 %>%  filter(cluster_fusion_PAM_OM == 9065)
df_35_instab <- df_35 %>%  filter(cluster_fusion_PAM_OM == 9020)
df_35_indep <- df_35 %>%  filter(cluster_fusion_PAM_OM == 8533)
df_35_foyer <- df_35 %>%  filter(cluster_fusion_PAM_OM == 7798)


# tables pondérées formule
prop.table(wtd.table(df_35_etudes$#variable , weights = df_35$poidsi)) * 100
#etc
  

#penser à faire une table par modalité puis des tables avec la variable cluster_fusion_PAM_OM


