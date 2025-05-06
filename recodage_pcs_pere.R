#### On va préparer les variables nécessaires aux régressions emboîtées
test <- indiv %>% select(ident, t_meract, pcs_pere, sexee, anaise)

# On pouvait aussi choisir t_situm : + détaillé

# On recode la variable pour avoir des résultats plus lisibles
test <- test %>% mutate(t_meract = case_when(
  t_meract == 1 ~ 1, #  a toujours travaillé
  t_meract == 2 ~ 2, # jamais travaillée
  t_meract == 3 ~ 3, # alterne les deux
  TRUE ~99 # valeurs manquantes, refus, etc
))

# On va recoder la pcs du pere
test <- test %>%
  mutate(pcs_pere_bis = case_when(
    grepl("^1", pcs_pere)|pcs_pere==7100 ~ "1", #agriculteurs (dont retraités)
    grepl("^2", pcs_pere)|pcs_pere==7200 ~ "2", #artisans et commerçants (dont retraités)
    grepl("^3", pcs_pere)|pcs_pere==7400 ~ "3", #cadres (dont retraités)
    grepl("^4", pcs_pere)|pcs_pere==7500 ~ "4", # professions intermédiaires (dont retraités)
    grepl("^54", pcs_pere)|grepl("^55", pcs_pere)|grepl("^56", pcs_pere)|grepl("^64", pcs_pere)|grepl("^65", pcs_pere)|grepl("^66", pcs_pere)|grepl("^67", pcs_pere)|grepl("^68", pcs_pere)|grepl("^69", pcs_pere) ~ "5", # employés et ouvirers qualifiés 
    grepl("^51", pcs_pere)|grepl("^52", pcs_pere)|grepl("^53", pcs_pere)|grepl("^61", pcs_pere)|grepl("^62", pcs_pere)|grepl("^63", pcs_pere)|pcs_pere==7700|pcs_pere==7800|pcs_pere==5000|pcs_pere==6000 ~"6", # employés et ouvriés non qualifiés (dont retraités de TOUS les employés et ouvriers)
    is.na(pcs_pere)|pcs_pere=="0000" ~"999", # valeur manquante
    TRUE ~ pcs_pere  # garde la valeur originale sinon
  ))

test <- test %>% select(-pcs_pere)