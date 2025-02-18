library(ggplot2)


#### On va recoder la variable orgine_tous_g2

# Pour cela, on commence par faire un histogramme pour constater la taille de chaque modalité de la variable
length(unique(indiv$origine_tous_g2)) # il y a 32 modalités différentes

# Création du barplot avec les étiquettes
ggplot(indiv, aes(x = origine_tous_g2)) +
  geom_bar(fill = "skyblue") +  # Barres bleues avec bordures noires
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.1, size = 4, angle = 70) +  # Ajout des étiquettes au-dessus des barres
  labs(title = "Distribution des Modalités de la Variable origine_tous_g2",
       x = "Modalités de origine_tous_g2",
       y = "Nombre d'Observations") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation des labels des modalités

# La grande question qui se pose est désormais celle des "Autres Français.es nés hors de France de métro" qui sont très nombreux.ses
# On procède au recodage suivant :
#### On commence par recoder des groupes qui semblent faire sens
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

# On refait notre histograme
library(ggplot2)
library(dplyr)

# Création du barplot avec les étiquettes
ggplot(indiv, aes(x = origine_tous_g2bis)) +
  geom_bar(fill = "skyblue") +  # Barres bleues avec bordures noires
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.1, size = 4, angle = 70) +  # Ajout des étiquettes au-dessus des barres
  labs(title = "Distribution des Modalités de la Variable origine_tous_g2",
       x = "Modalités de origine_tous_g2",
       y = "Nombre d'Observations") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation des labels des modalités

