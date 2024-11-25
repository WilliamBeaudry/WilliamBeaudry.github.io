# Comparaison du taux de participation entre le Québec et les autres provinces
library(ggplot2)
library(tidyr)

# Préparer les données
df_long <- df_cln %>%
  pivot_longer(cols = -"Province/territoire", names_to = "Année", values_to = "Taux_participation")

# Graphique linéaire
ggplot(df_long, aes(x = Année, y = Taux_participation, group = `Province/territoire`, color = `Province/territoire`)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(
    title = "Taux de participation aux élections fédérales par province (2004-2011)",
    x = "Année", y = "Taux de participation (%)",
    color = "Province"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  ) +
  scale_color_manual(values = couleurs)

# Créer un graphique boxplot
ggplot(df_long, aes(x = Année, y = Taux_participation, group = "Province/territoire", color = "Province/territoire")) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "Taux de participation aux élections fédérales par province (2004-2011)",
       x = "Année", y = "Taux de participation (%)",
       color = "Province") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right") +
  scale_color_manual(values = "red")

# Visualisation du nombre d'électeurs par circonscription, par province
# Charger les bibliothèques nécessaires
library(tidyverse)

# Lire les données
data <- read.csv("projet_session/ED-Canada_2021_prov_tx_part.csv")

# Calculer le nombre moyen d'électeurs par province
moyennes_par_province <- data %>%
  group_by("Province") %>%
  summarise(NombreMoyenElecteurs = mean("POPULATION", na.rm = TRUE))

# Créer le graphique avec ggplot2
ggplot(moyennes_par_province, aes(x = reorder(Province, -NombreMoyenElecteurs), y = NombreMoyenElecteurs)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() + # Tourne le graphique pour une meilleure lisibilité
  labs(title = "Nombre moyen d'électeurs par province",
       x = "Province",
       y = "Nombre moyen d'électeurs") +
  theme_minimal()







  