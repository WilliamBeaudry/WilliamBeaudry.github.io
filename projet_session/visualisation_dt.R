# Comparaison du taux de participation entre le Québec et les autres provinces
library(ggplot2)
library(tidyr)

# Préparer les données
df_long <- df_cln %>%
  pivot_longer(cols = -"Province/territoire", names_to = "Année", values_to = "Taux_participation")

# Définir une palette de couleurs personnalisée
couleurs <- c("Québec" = "blue", 
              setNames(scales::hue_pal()(length(unique(df_long$"Province/territoire")) - 1), 
                       setdiff(unique(df_long$"Province/territoire"), "Québec")))

# Créer le graphique
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

#Créer un graphique boxplot
ggplot(df_long, aes(x = Année, y = Taux_participation)) +
  geom_boxplot() +
  geom_point(data = df_long %>% filter("Province/territoire" == "Québec"),
             aes(color = "Québec"),
             size = 3) +
  scale_color_manual(values = c("Québec" = "red")) +
  theme_minimal() +
  labs(title = "Distribution des taux de participation par année",
       subtitle = "Points rouges représentent le Québec",
       x = "Année",
       y = "Taux de participation (%)",
       color = "Province/territoire") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  