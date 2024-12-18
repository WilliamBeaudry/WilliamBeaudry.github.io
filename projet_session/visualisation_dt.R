# Comparaison du taux de participation entre le Québec et les autres provinces
library(ggplot2)
library(tidyr)
install.packages("stringr")
library(stringr)
library(dplyr)

# Préparer les données
df_long <- df_cln %>%
  pivot_longer(cols = -"Province/territoire", names_to = "Année", values_to = "Taux_participation")

# Convertir la colonne Année en numérique et filtrer les données
df_long_filtered <- df_long %>%
  filter(`Province/territoire` != "Sections de vote dans les réserves") %>%
  filter(Année != "Moyenne cumulative(%)") %>%
  mutate(Année = as.numeric(gsub("[^0-9]", "", Année)))

ggplot(df_long_filtered, aes(x = Année, y = Taux_participation, group = `Province/territoire`, color = `Province/territoire`)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(
    title = str_wrap("Taux de participation aux élections fédérales dans les communautés autochtones par province (2004-2011)"),
    x = "Année", y = "Taux de participation (%)",
    color = "Province"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

# Créer un graphique boxplot
ggplot(df_long, aes(x = Année, y = Taux_participation, group = "Province/territoire", fill = "Province/territoire")) +
  geom_line() +
  geom_point(aes(color = `Province/territoire`)) +
  theme_minimal() +
  labs(title = str_wrap("Taux de participation aux élections fédérales dans les communautés autochtones par province (2004-2011)"),
       x = "Année", y = "Taux de participation (%)",
       color = "Province") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right") +
  scale_fill_manual(values = c(
    "Québec" = "blue",
    "Alberta" = "red",
    "Manitoba" = "red",
    "Colombie-Britannique" = "red",
    "Terre-Neuve-et-Labrador" = "red",
    "Yukon" = "red",
    "Nunavut" = "red",
    "Canada (national)" = "red",
    "Île-du-Prince-Édouard" = "red",
    "Nouvelle-Écosse" = "red",
    "Nouveau-Brunswick" = "red",
    "Ontario" = "red",
    "Saskatchewan" = "red",
    "Territoires du Nord-Ouest" = "red"
  )) +
  scale_color_manual(values = c(
    "Québec" = "blue",
    "Alberta" = "red",
    "Manitoba" = "red",
    "Colombie-Britannique" = "red",
    "Terre-Neuve-et-Labrador" = "red",
    "Yukon" = "red",
    "Nunavut" = "red",
    "Canada (national)" = "red",
    "Île-du-Prince-Édouard" = "red",
    "Nouvelle-Écosse" = "red",
    "Nouveau-Brunswick" = "red",
    "Ontario" = "red",
    "Saskatchewan" = "red",
    "Territoires du Nord-Ouest" = "red"
  ))

# Calcul de la proportion d'électeurs autochtones dans la population
df <- ED_Canada_2021_prov_tx_part %>%
  mutate(prop_indigenous = ELECTEURS_AUTOCHTONES / POPULATION)

# Calcul du coefficient de corrélation global
correlation <- cor(df$prop_indigenous, df$TAUX_PARTICIPATION, use = "complete.obs")

# Affichage du résultat
print(paste("Le coefficient de corrélation est:", round(correlation, 3)))

library(ggplot2)

# Création du graphique avec échelles logarithmiques
ggplot(df, aes(x = prop_indigenous, y = TAUX_PARTICIPATION)) +
  geom_point(alpha = 0.6, color = "blue") + # Nuage de points
  geom_smooth(method = "lm", color = "red", se = TRUE) + # Ligne de régression
  scale_x_log10() + # Échelle logarithmique pour l'axe X
  labs(
    title = "Relation entre la proportion d'électeurs autochtones et le taux de participation aux élections fédérales de 2021",
    x = "Proportion d'électeurs autochtones (log10)",
    y = "Taux de participation (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    text = element_text(size = 12)
  )

print(prop_elec_tx_part_coefcan)

library(dplyr)

# Calculer la proportion d'électeurs autochtones
df <- df %>%
  mutate(prop_indigenous = ELECTEURS_AUTOCHTONES/POPULATION)

# Calcul des coefficients de corrélation par province
correlation_by_province <- df %>%
  group_by(PROV) %>%
  summarize(
    correlation = cor(prop_indigenous, TAUX_PARTICIPATION, use = "complete.obs")
  )

# Affichage des résultats
print(correlation_by_province)

library(ggplot2)
library(dplyr)

# Calcul de la proportion d'électeurs autochtones
df <- df %>%
  mutate(prop_indigenous = ELECTEURS_AUTOCHTONES / POPULATION)

# Création d'un graphique avec une facette par province
ggplot(df, aes(x = prop_indigenous, y = TAUX_PARTICIPATION, color = PROV)) +
  geom_point(size = 1, alpha = 0.7) + # Points avec transparence
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", size = 1) + # Droites de régression
  scale_x_log10() + # Transformation logarithmique sur l'axe X
  labs(
    title = "Corrélation entre la proportion d'électeurs autochtones et le taux de participation aux élections fédérales de 2021",
    x = "Proportion d'électeurs autochtones (log10)",
    y = "Taux de participation (%)"
  ) +
  facet_wrap(~PROV) + # Facettage par province
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5), # Centrage du titre
    text = element_text(size = 12),
    legend.position = "none" # Supprime la légende pour chaque facette
  )


colnames(ED_Canada_2021_prov_tx_part)


# Convertir la colonne Année en numérique et filtrer les données
df_long_filtered <- df_long %>%
  filter(`Province/territoire` != "Sections de vote dans les réserves") %>%
  filter(Année != "Moyenne cumulative(%)") %>%
  mutate(Année = as.numeric(gsub("[^0-9]", "", Année)))

ggplot(df_long_filtered, aes(x = Année, y = Taux_participation, group = `Province/territoire`, color = `Province/territoire`)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(
    title = str_wrap("Taux de participation aux élections fédérales dans les communautés autochtones par province (2004-2011)"),
    x = "Année", y = "Taux de participation (%)",
    color = "Province"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )



  