##### Recodage de la variable p_gactiv
unique(trajpro$p_gactiv)

# Création du barplot pour observer la répartion des modalités
ggplot(trajpro, aes(x = p_gactiv)) +
  geom_bar(fill = "skyblue") +  # Barres bleues avec bordures noires
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.1, size = 4, angle = 70) +  # Ajout des étiquettes au-dessus des barres
  labs(title = "Distribution des Modalités de la Variable p_gactiv",
       x = "Modalités de p_gactiv",
       y = "Nombre d'Observations") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation des labels des modalités


#### On regroupe les modalités 6 et 7 et on passe les modalités en chaînes de caractères
trajpro <- trajpro %>% mutate(p_gactiv2 = case_when(
  p_gactiv == 1 ~ "sal", # Salari.ée
  p_gactiv == 2 ~ "ind", # à son compte ou indépendant
  p_gactiv == 3 ~ "cho", # chômage
  p_gactiv == 4 ~ "fc", # études ou stage non rémunéré (formation continue)
  p_gactiv == 5 ~  "auf", # femme ou homme au foyer
  p_gactiv == 6 | p_gactiv == 7 ~ "autres", # Autres
  TRUE ~ NA_character_
))


# On refait le graphique
ggplot(trajpro, aes(x = p_gactiv2)) +
  geom_bar(fill = "skyblue") +  # Barres bleues avec bordures noires
  geom_text(stat = "count", aes(label = ..count..), vjust = 0, size = 4, angle = 0) +  # Ajout des étiquettes au-dessus des barres
  labs(title = "Distribution des Modalités de la Variable p_gactiv",
       x = "Modalités de p_gactiv",
       y = "Nombre d'Observations") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation des labels des modalités
