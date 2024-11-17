# Charger les bibliothèques nécessaires
library(rvest)
library(dplyr)

# URL de la page
url <- "https://www.elections.ca/content.aspx?dir=rec/part/fvt&document=p4&lang=f&section=res"

# Lire le contenu HTML de la page
page <- read_html(url)

# Extraire les lignes du tableau avec les balises "tr"
table_rows <- page %>%
  html_elements("tr")

# Récupérer le contenu des balises "th" (en-têtes de colonnes)
headers <- table_rows %>%
  .[1] %>%
  html_elements("th") %>%
  html_text()

# Extraire les données des balises "td" pour les rangées restantes
data <- table_rows %>%
  .[-1] %>%
  lapply(function(row) {
    row %>% html_elements("td") %>% html_text()
  })

# Convertir les données en data frame
df <- do.call(rbind, data) %>%
  as.data.frame(stringsAsFactors = FALSE)

# Attribuer les noms de colonnes extraits
names(df) <- headers

# Nettoyer et convertir les données numériques
df_cln <- df %>%
  mutate(across(
    .cols = -1, 
    .fns = ~ {
      # Supprimer les espaces et remplacer la virgule par un point
      cleaned <- gsub(" ", "", .)
      cleaned <- gsub(",", ".", cleaned)
      # Convertir en numérique
      as.numeric(cleaned)
    }
  ))

# Afficher le data frame nettoyé
print(df_cln)

# Inspecter la structure des données
str(df_cln)

# Afficher le résumé des données
summary(df_cln)

