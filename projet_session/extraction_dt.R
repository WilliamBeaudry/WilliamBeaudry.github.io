# Installation des packages pour extraire les métadonnées d'un document PDF
install.packages("pdftools")
install.packages("xml2")
library("pdftools")
library("rvest")
library("tidyverse")
library("xml2")

# Charger les bibliothèques nécessaires
library(pdftools)
library(stringr)
library(dplyr)

# Chemin vers le fichier PDF
pdf_file <- "C:/Users/willi/OneDrive/documents/WilliamBeaudry.github.io/projet_session/doc_tx_part_pn_2004-11.pdf"

# Lire le texte brut du PDF
pdf_text_content <- pdf_text(pdf_file)

# Identifier la page contenant le tableau
page_index <- 3  # Remplacez par la page exacte où se trouve le tableau

# Extraire le contenu brut de la page du tableau
page_content <- pdf_text_content[page_index]

# Diviser la page en lignes
lines <- str_split(page_content, "\n")[[1]]

# Filtrer les lignes du tableau
table_start <- which(str_detect(lines, "Province/territoire"))  # Ligne du titre
table_lines <- lines[(table_start + 1):length(lines)]  # Toutes les lignes après les titres
table_lines <- table_lines[str_detect(table_lines, "\\S")]  # Supprimer les lignes vides

# Reconstruire les données en un tableau brut
table_data <- read.table(text = paste(table_lines, collapse = "\n"), 
                         header = FALSE, 
                         sep = "", 
                         fill = TRUE, 
                         stringsAsFactors = FALSE)

# Ajouter des noms de colonnes (ajuster selon vos besoins)
colnames(table_data) <- c("Province/territoire", "2004 (%)", "2006 (%)", "2008 (%)", 
                          "2011 (%)", "Moyenne (%)")

# Nettoyer les colonnes pour ne conserver que les données numériques
numeric_table <- table_data %>%
  mutate(across(where(is.character), ~ str_replace_all(., "[^0-9.,-]", ""))) %>%  # Supprimer les caractères non numériques
  mutate(across(where(is.character), ~ as.numeric(.)))  # Convertir en numérique

# Supprimer les lignes où toutes les colonnes numériques sont NA
numeric_table <- numeric_table %>%
  filter(rowSums(!is.na(select(., -Province/territoire))) > 0)

# Visualiser le tableau final nettoyé
print(numeric_table)

# Enregistrer les données nettoyées dans un fichier CSV si nécessaire
write.csv(numeric_table, "tableau_participation_numerique.csv", row.names = FALSE)








