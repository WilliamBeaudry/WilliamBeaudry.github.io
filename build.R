# ===========================================================================
#  build.R — Génère index.html à partir des fichiers YAML du dossier contenu/
#
#  UTILISATION
#    1. Ouvre le projet WilliamBeaudry.github.io.Rproj dans RStudio
#    2. Modifie ce que tu veux dans contenu/*.yml
#    3. Exécute :  source("build.R")
#    4. Vérifie le résultat en ouvrant index.html dans un navigateur
#    5. Publie :   source("publier.R")
#
#  N'ÉDITE JAMAIS index.html À LA MAIN : il est écrasé à chaque build.
# ===========================================================================

if (!requireNamespace("yaml", quietly = TRUE)) {
  stop("Le paquet 'yaml' est requis. Exécute : install.packages(\"yaml\")")
}

# --- Petits utilitaires ----------------------------------------------------

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || identical(a, "")) b else a

txt <- function(x) if (is.null(x)) "" else trimws(paste(as.character(x), collapse = " "))

# Accepte une chaîne OU une liste YAML — lieu: ["Université Laval", "Québec"]
# devient « Université Laval, Québec ». Les éléments vides sont ignorés.
joindre <- function(x, sep = ", ") {
  if (is.null(x)) return("")
  v <- trimws(as.character(unlist(x)))
  paste(v[nzchar(v)], collapse = sep)
}

lire_fichier <- function(chemin) {
  brut <- readBin(chemin, "raw", file.size(chemin))
  x <- rawToChar(brut)
  Encoding(x) <- "UTF-8"
  x
}

lire_yaml <- function(chemin, defaut = list()) {
  if (!file.exists(chemin)) {
    message("  (fichier absent, ignoré : ", chemin, ")")
    return(defaut)
  }
  # Lecture explicite en UTF-8 : évite les problèmes d'accents selon la
  # configuration régionale de l'ordinateur.
  res <- tryCatch(
    yaml::yaml.load(lire_fichier(chemin)),
    error = function(e) stop(
      "Erreur de syntaxe dans ", chemin, " :\n  ", conditionMessage(e),
      "\n  Vérifie l'indentation (2 espaces, jamais de tabulation) et les guillemets.",
      call. = FALSE))
  if (is.null(res)) defaut else res
}

ecrire_fichier <- function(x, chemin) {
  # IMPORTANT : convertir x AVANT d'ouvrir la connexion. open = "wb" vide le
  # fichier immédiatement ; si x devait encore être lu depuis ce même fichier,
  # il serait déjà effacé au moment de l'évaluation.
  brut <- charToRaw(enc2utf8(x))
  con <- file(chemin, open = "wb")
  on.exit(close(con))
  writeBin(brut, con)
}

# Échappe les caractères réservés du HTML
esc <- function(x) {
  x <- txt(x)
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;",  x, fixed = TRUE)
  x <- gsub(">", "&gt;",  x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x
}

# Mini-markdown : **gras**, *italique*, [libellé](url), paragraphes
# Mini-Markdown maison. Règles de mise en forme d'un bloc de texte :
#   ligne vide            -> nouveau paragraphe
#   ligne commençant par « - » ou « * » -> puce d'une liste
#   « \\ » en fin de ligne  -> saut de ligne forcé (comme en LaTeX)
#   sinon, les lignes consécutives sont recollées en un seul paragraphe
# Inline : **gras**, *italique*, [libellé](url), \rouge{}, \gras{}, \italique{}
md <- function(x, balise = "p") {
  if (is.null(x)) return("")
  s <- paste(as.character(x), collapse = "\n")
  if (!nzchar(trimws(s))) return("")
  s <- gsub("&", "&amp;", s, fixed = TRUE)
  s <- gsub("<", "&lt;",  s, fixed = TRUE)
  s <- gsub(">", "&gt;",  s, fixed = TRUE)
  s <- gsub("\\[([^]]+)\\]\\(([^)]+)\\)", '<a href="\\2">\\1</a>', s)
  # Commandes à la TeX : \rouge{...}, \gras{...}, \italique{...}
  s <- gsub("\\\\rouge\\{([^{}]*)\\}",    "<span class=\"rouge\">\\1</span>", s)
  s <- gsub("\\\\gras\\{([^{}]*)\\}",     "<strong>\\1</strong>", s)
  s <- gsub("\\\\italique\\{([^{}]*)\\}", "<em>\\1</em>", s)
  s <- gsub("\\*\\*([^*]+)\\*\\*", "<strong>\\1</strong>", s)
  s <- gsub("\\*([^*]+)\\*", "<em>\\1</em>", s)

  blocs <- trimws(strsplit(s, "\n[ \t]*\n")[[1]])
  blocs <- blocs[nzchar(blocs)]
  if (length(blocs) == 0) return("")

  rendre <- function(bloc) {
    lignes <- trimws(strsplit(bloc, "\n")[[1]])
    lignes <- lignes[nzchar(lignes)]
    sortie <- character(0)
    tampon <- character(0)          # lignes de texte en attente
    puces  <- character(0)          # puces en attente

    vider_texte <- function() {
      if (!length(tampon)) return(invisible(NULL))
      t <- paste(tampon, collapse = "\n")
      # « \\ » en fin de ligne : saut forcé ; sinon les lignes se recollent
      t <- gsub("\\\\\\\\[ \t]*\n", "<br />\n", t)
      t <- gsub("\n", " ", t)
      sortie <<- c(sortie, paste0("<", balise, ">", t, "</", balise, ">"))
      tampon <<- character(0)
    }
    vider_puces <- function() {
      if (!length(puces)) return(invisible(NULL))
      sortie <<- c(sortie, paste0('<ul class="md-liste">\n',
                                  paste0("  <li>", puces, "</li>", collapse = "\n"),
                                  "\n</ul>"))
      puces <<- character(0)
    }

    for (l in lignes) {
      if (grepl("^[-*+][[:space:]]+", l)) {
        vider_texte()
        puces <- c(puces, sub("^[-*+][[:space:]]+", "", l))
      } else {
        vider_puces()
        tampon <- c(tampon, l)
      }
    }
    vider_texte(); vider_puces()
    paste(sortie, collapse = "\n")
  }

  paste(vapply(blocs, rendre, character(1)), collapse = "\n")
}

# Version « une seule ligne », sans balise <p>
md_ligne <- function(x) {
  s <- md(x)
  s <- gsub("</p>\\s*<p>", " ", s)
  gsub("^<p>|</p>$", "", s)
}

MOIS <- c("janvier", "février", "mars", "avril", "mai", "juin",
          "juillet", "août", "septembre", "octobre", "novembre", "décembre")

# "2024-11" -> "novembre 2024" ; "2024" -> "2024" ; "" -> ""
fmt_date <- function(d) {
  d <- txt(d)
  if (!nzchar(d)) return("")
  p <- strsplit(d, "[-/]")[[1]]
  m <- suppressWarnings(as.integer(p[2]))
  if (length(p) >= 2 && !is.na(m) && m >= 1 && m <= 12) paste(MOIS[m], p[1]) else p[1]
}

# Clé numérique pour trier (plus grand = plus récent) ; "" -> +Inf (en cours)
cle_date <- function(d, en_cours_max = FALSE) {
  d <- txt(d)
  if (!nzchar(d)) return(if (en_cours_max) Inf else 0)
  p <- suppressWarnings(as.integer(strsplit(d, "[-/]")[[1]]))
  a <- if (is.na(p[1])) 0 else p[1]
  m <- if (length(p) < 2 || is.na(p[2])) 0 else p[2]
  a * 100 + m
}

trier_par_date <- function(liste, champ = "date", en_cours_max = FALSE) {
  if (length(liste) == 0) return(liste)
  cles <- vapply(liste, function(e) cle_date(e[[champ]], en_cours_max), numeric(1))
  epingle <- vapply(liste, function(e) isTRUE(e$epingle), logical(1))
  liste[order(-epingle, -cles)]
}

periode <- function(debut, fin) {
  d <- fmt_date(debut); f <- fmt_date(fin)
  if (nzchar(d) && nzchar(f)) paste(d, "&ndash;", f)
  else if (nzchar(d))         paste(d, "&ndash; à ce jour")
  else f
}

# --- Générateurs de sections ----------------------------------------------

# Bouton d'appel a l'action, optionnel, sur n'importe quelle section.
# Se declare dans site.yml sous la section, cle "bouton:".
bouton_section <- function(s) {
  b <- s$bouton %||% list()
  lien <- txt(b$lien)
  if (!nzchar(lien)) return("")
  paste0(
    "\n",
    if (nzchar(txt(b$note))) paste0('<p class="sec-note">', md_ligne(b$note), "</p>\n") else "",
    sprintf('<p class="sec-action"><a href="%s"%s class="button %s">%s</a></p>',
            esc(lien),
            if (identical(b$nouvel_onglet, FALSE)) "" else ' target="_blank"',
            esc(verifier_style(b$style %||% "primary")),
            esc(b$libelle %||% "En savoir plus")))
}



# Pictogramme et libellé de bouton déduits de l'extension du fichier.
# Ajoute une ligne à ces listes pour prendre en charge un nouveau format.
FORMATS <- list(
  pdf   = list(icone = "fa-file-pdf",        libelle = "Consulter le PDF"),
  ppt   = list(icone = "fa-file-powerpoint", libelle = "Télécharger la présentation"),
  pptx  = list(icone = "fa-file-powerpoint", libelle = "Télécharger la présentation"),
  doc   = list(icone = "fa-file-word",       libelle = "Télécharger le document"),
  docx  = list(icone = "fa-file-word",       libelle = "Télécharger le document"),
  xls   = list(icone = "fa-file-excel",      libelle = "Télécharger le tableur"),
  xlsx  = list(icone = "fa-file-excel",      libelle = "Télécharger le tableur"),
  csv   = list(icone = "fa-file-csv",        libelle = "Télécharger les données"),
  zip   = list(icone = "fa-file-archive",    libelle = "Télécharger l'archive"),
  html  = list(icone = "fa-file-alt",        libelle = "Consulter et imprimer"),
  htm   = list(icone = "fa-file-alt",        libelle = "Consulter et imprimer")
)

format_fichier <- function(chemin) {
  ext <- tolower(sub(".*\\.", "", txt(chemin)))
  FORMATS[[ext]] %||% list(icone = "fa-file-alt", libelle = "Télécharger le document")
}

# Rangée de boutons à partir d'une liste YAML "documents:".
# Chaque élément accepte : fichier (ou lien), libelle, style.
# Sans libelle, le texte est déduit de l'extension (voir FORMATS).
STYLES_BOUTON <- c("small", "primary", "large", "fit", "icon")

verifier_style <- function(style) {
  style <- txt(style)
  if (!nzchar(style)) return("")
  jetons <- strsplit(style, "[[:space:]]+")[[1]]
  inconnus <- jetons[!jetons %in% STYLES_BOUTON]
  if (length(inconnus)) {
    warning("Style de bouton inconnu : \"", paste(inconnus, collapse = "\", \""),
            "\".\n  Les styles valides sont : ", paste(STYLES_BOUTON, collapse = ", "),
            ".\n  Attention aux majuscules : \"Primary\" n'est pas \"primary\".",
            call. = FALSE)
  }
  style
}

boutons_documents <- function(items) {
  if (!is.list(items) || length(items) == 0) return("")
  b <- vapply(items, function(d) {
    if (is.character(d)) d <- list(fichier = d)   # forme courte : juste le chemin
    lien <- txt(d$lien %||% d$fichier)
    if (!nzchar(lien)) return("")
    sprintf('<a href="%s" target="_blank" class="button %s">%s</a>',
            esc(lien),
            esc(verifier_style(d$style %||% "small")),
            esc(d$libelle %||% format_fichier(lien)$libelle))
  }, character(1))
  b <- b[nzchar(b)]
  if (!length(b)) return("")
  paste0('\n    <p class="liens-docs">', paste(b, collapse = "\n      "), "</p>")
}

gen_texte <- function(s, ...) {
  bloc <- ""
  if (nzchar(txt(s$banniere))) {
    bloc <- sprintf(
      '<div class="image main" data-position="center"><img src="%s" alt="" /></div>\n',
      esc(s$banniere))
  }
  entete <- ""
  if (nzchar(txt(s$entete)) || nzchar(txt(s$sous_titre))) {
    entete <- sprintf('<header class="major"><h2>%s</h2><p>%s</p></header>\n',
                      esc(s$entete), md_ligne(s$sous_titre))
  } else if (nzchar(txt(s$titre))) {
    entete <- sprintf("<h3>%s</h3>\n", esc(s$titre))
  }
  paste0(bloc, '<div class="container">\n', entete, md(s$texte), bouton_section(s), "\n</div>")
}

gen_texte_icones <- function(s, ...) {
  faits <- s$faits %||% list()
  li <- vapply(faits, function(f) sprintf('  <li class="icon %s">%s</li>',
                                          esc(f$icone %||% "solid fa-check"),
                                          md_ligne(f$texte)), character(1))
  paste0('<div class="container">\n<h3>', esc(s$titre), "</h3>\n", md(s$texte), bouton_section(s),
         if (length(li)) paste0('\n<ul class="feature-icons">\n', paste(li, collapse = "\n"),
                                "\n</ul>") else "",
         "\n</div>")
}

gen_parcours <- function(s, donnees, ...) {
  p <- donnees$parcours

  # Chaque liste de premier niveau de parcours.yml devient un bloc de la ligne
  # du temps, dans l'ordre du fichier. Pour ajouter une rubrique, il suffit
  # d'ajouter une cl\u00e9 : aucune modification de ce script n'est n\u00e9cessaire.
  # L'intitul\u00e9 affich\u00e9 vient de titre_<cl\u00e9>, sinon de la cl\u00e9 elle-m\u00eame.

  premier <- function(e, noms) {
    for (n in noms) if (nzchar(txt(e[[n]]))) return(e[[n]])
    ""
  }

  joli <- function(cle) {
    t <- gsub("_", " ", cle)
    paste0(toupper(substring(t, 1, 1)), substring(t, 2))
  }

  bloc <- function(titre, items) {
    if (length(items) == 0) return("")
    items <- items[order(-vapply(items, function(e) cle_date(e$fin, TRUE), numeric(1)),
                         -vapply(items, function(e) cle_date(e$debut), numeric(1)))]
    li <- vapply(items, function(e) {
      intitule <- premier(e, c("poste", "diplome", "formation", "titre", "nom"))
      if (!nzchar(txt(intitule))) {
        warning("Une entr\u00e9e de '", titre,
                "' n'a pas d'intitul\u00e9 (poste, diplome, formation, titre ou nom).",
                call. = FALSE)
      }
      # lien: rend l'intitule cliquable
      intitule <- if (nzchar(txt(e$lien)))
        sprintf('<a href="%s" target="_blank">%s</a>', esc(e$lien), esc(intitule))
      else esc(intitule)
      sprintf(
        '  <li class="pc-item">\n    <span class="pc-date">%s</span>\n    <h5 class="pc-titre">%s</h5>\n    <span class="pc-org">%s</span>\n    %s\n  </li>',
        periode(e$debut, e$fin),
        intitule,
        paste0(esc(premier(e, c("organisation", "etablissement", "lieu_org"))),
               if (nzchar(txt(e$lieu))) paste0(" &middot; ", esc(e$lieu)) else ""),
        md(e$description))
    }, character(1))
    paste0("<h4>", esc(titre), "</h4>\n<ul class=\"pc-liste\">\n",
           paste(li, collapse = "\n"), "\n</ul>\n")
  }

  cles <- names(p)
  rubriques <- cles[!grepl("^titre_", cles)]

  # Un titre_x sans rubrique x correspondante = faute de frappe
  orphelins <- setdiff(sub("^titre_", "", cles[grepl("^titre_", cles)]), rubriques)
  if (length(orphelins)) {
    warning("parcours.yml : titre_", paste(orphelins, collapse = ", titre_"),
            " ne correspond \u00e0 aucune rubrique.\n  Rubriques pr\u00e9sentes : ",
            paste(rubriques, collapse = ", "), ".", call. = FALSE)
  }

  blocs <- vapply(rubriques, function(cle) {
    items <- p[[cle]]
    if (!is.list(items) || length(items) == 0) return("")
    bloc(txt(p[[paste0("titre_", cle)]]) %||% joli(cle), items)
  }, character(1))

  paste0('<div class="container">\n<h3>', esc(s$titre), "</h3>\n", md(s$texte), bouton_section(s), "\n",
         paste(blocs, collapse = ""),
         "</div>")
}

gen_documents <- function(s, donnees, ...) {
  docs <- trier_par_date(donnees$documents)
  if (length(docs) == 0) return(NULL)
  li <- vapply(docs, function(d) {
    fmt  <- format_fichier(d$fichier)
    meta <- paste(Filter(nzchar, c(esc(d$categorie), fmt_date(d$date))), collapse = " &middot; ")
    apercu <- if (nzchar(txt(d$apercu)))
      sprintf('<a href="%s" target="_blank" class="doc-vignette"><img src="%s" alt="Aperçu : %s" /></a>',
              esc(d$fichier), esc(d$apercu), esc(d$titre)) else ""
    sprintf(
paste0('  <li class="doc-item">\n%s',
       '    <div class="doc-corps">\n',
       '      <h4><a href="%s" target="_blank"><span class="icon solid %s"></span> %s</a></h4>\n',
       '      <span class="doc-meta">%s</span>\n%s',
       '      <p><a href="%s" target="_blank" class="button %s">%s</a></p>\n%s',
       '    </div>\n  </li>'),
      if (nzchar(apercu)) paste0("    ", apercu, "\n") else "",
      esc(d$fichier), esc(fmt$icone), esc(d$titre), meta,
      if (nzchar(txt(d$description))) paste0("      ", md(d$description), "\n") else "",
      esc(d$fichier),
      esc(verifier_style(d$bouton %||% "small")),
      esc(d$libelle %||% fmt$libelle),
      if (nzchar(boutons_documents(d$documents)))
        paste0("     ", boutons_documents(d$documents), "\n") else "")
  }, character(1))
  paste0('<div class="container">\n<h3>', esc(s$titre), "</h3>\n", md(s$texte), bouton_section(s),
         '\n<ul class="doc-liste">\n', paste(li, collapse = "\n"), "\n</ul>\n</div>")
}

gen_publications <- function(s, donnees, ...) {
  pubs <- trier_par_date(donnees$publications)
  if (length(pubs) == 0) return(NULL)
  li <- vapply(pubs, function(p) {
    lien <- txt(p$url %||% p$fichier)
    titre <- if (nzchar(lien))
      sprintf('<a href="%s" target="_blank">%s</a>', esc(lien), esc(p$titre)) else esc(p$titre)
    # md_ligne : protège le HTML puis applique *italique*, **gras** et les liens
    ref <- paste(Filter(nzchar, c(md_ligne(p$auteurs), md_ligne(p$editeur), md_ligne(p$numero),
                                  md_ligne(joindre(p$lieu)), fmt_date(p$date))), collapse = ", ")
    # type absent ou vide : pas d'étiquette du tout
    etiquette <- if (nzchar(txt(p$type)))
      sprintf('    <span class="pub-type">%s</span>\n', esc(p$type)) else ""
    sprintf('  <li class="pub-item">\n%s    <h4 class="pub-titre">%s</h4>\n    <span class="pub-ref">%s</span>\n    %s%s\n  </li>',
            etiquette, titre, ref, md(p$resume),
            boutons_documents(p$documents))
  }, character(1))
  paste0('<div class="container">\n<h3>', esc(s$titre), "</h3>\n", md(s$texte), bouton_section(s),
         '\n<ul class="pub-liste">\n', paste(li, collapse = "\n"), "\n</ul>\n</div>")
}

gen_projets <- function(s, donnees, ...) {
  prj <- trier_par_date(donnees$projets)
  if (length(prj) == 0) return(NULL)
  art <- vapply(prj, function(p) sprintf(
paste0('  <article>\n',
       '    <a href="%s" class="image" target="_blank"><img src="%s" alt="Aperçu : %s" /></a>\n',
       '    <div class="inner">\n      <h4>%s</h4>\n      %s\n',
       '      <p><a href="%s" target="_blank" class="button primary">%s</a></p>\n',
       '    </div>\n  </article>'),
    esc(p$lien), esc(p$image), esc(p$titre), esc(p$titre), md(p$resume),
    esc(p$lien), esc(p$libelle_bouton %||% "Voir le projet")), character(1))
  paste0('<div class="container">\n<h3>', esc(s$titre), "</h3>\n", md(s$texte), bouton_section(s),
         '\n<div class="features">\n', paste(art, collapse = "\n"), "\n</div>\n</div>")
}

gen_contact <- function(s, donnees, ...) {
  courriel <- txt(donnees$site$courriel)
  paste0('<div class="container">\n<h3>', esc(s$titre), "</h3>\n", md(s$texte), bouton_section(s),
         '\n<div class="row gtr-uniform">\n  <div class="col-12">\n',
         sprintf('    <a href="mailto:%s?subject=Prise%%20de%%20contact" class="button primary">Envoyer un courriel</a>\n',
                 esc(courriel)),
         "  </div>\n</div>\n</div>")
}

# --- Version imprimable du CV (cv.html) ------------------------------------
# Reprend les rubriques de parcours.yml, mises en page aux couleurs du CV.
# Se règle dans contenu/site.yml, bloc "cv:".

gen_cv <- function(donnees, racine = ".") {
  site <- donnees$site
  cfg  <- site$cv %||% list()
  if (identical(cfg$produire, FALSE)) return(invisible(NULL))

  p <- donnees$parcours
  cles <- names(p)
  rubriques <- cles[!grepl("^titre_", cles)]

  joli <- function(cle) {
    t <- gsub("_", " ", cle)
    paste0(toupper(substring(t, 1, 1)), substring(t, 2))
  }
  premier <- function(e, noms) {
    for (n in noms) if (nzchar(txt(e[[n]]))) return(e[[n]])
    ""
  }
  titre_section <- function(t) sprintf(
    '      <div class="cv-section"><h2>%s</h2><span class="filet"></span></div>', esc(t))

  corps <- character(0)

  for (cle in rubriques) {
    items <- p[[cle]]
    if (!is.list(items) || length(items) == 0) next
    items <- items[order(-vapply(items, function(e) cle_date(e$fin, TRUE), numeric(1)),
                         -vapply(items, function(e) cle_date(e$debut), numeric(1)))]
    intitule_rub <- txt(p[[paste0("titre_", cle)]]) %||% joli(cle)
    entrees <- vapply(items, function(e) {
      org <- premier(e, c("organisation", "etablissement"))
      sprintf(
        paste0('      <div class="cv-entree">\n',
               '        <div class="cv-ligne"><span class="cv-intitule">%s</span><span class="cv-dates">%s</span></div>\n',
               '%s',
               '        <div class="cv-desc">%s</div>\n',
               '      </div>'),
        if (nzchar(txt(e$lien)))
          sprintf('<a href="%s">%s</a>', esc(e$lien),
                  esc(premier(e, c("poste", "diplome", "formation", "titre", "nom"))))
        else esc(premier(e, c("poste", "diplome", "formation", "titre", "nom"))),
        periode(e$debut, e$fin),
        if (nzchar(txt(org)))
          sprintf('        <div class="cv-org">%s</div>\n',
                  paste0(esc(org), if (nzchar(txt(e$lieu))) paste0(", ", esc(e$lieu)) else ""))
        else "",
        md(e$description))
    }, character(1))
    corps <- c(corps, titre_section(intitule_rub), entrees)
  }

  if (isTRUE(cfg$publications)) {
    pubs <- trier_par_date(donnees$publications)
    if (length(pubs)) {
      bloc <- vapply(pubs, function(pb) {
        lien <- txt(pb$url %||% pb$fichier)
        titre <- if (nzchar(lien))
          sprintf('<a href="%s">%s</a>', esc(lien), esc(pb$titre)) else esc(pb$titre)
        sprintf(
          paste0('      <div class="cv-pub">\n',
                 '        <div class="cv-pub-groupe">%s</div>\n',
                 '        <div class="cv-pub-titre">%s</div>\n',
                 '      </div>'),
          paste(Filter(nzchar, c(md_ligne(pb$type), md_ligne(pb$editeur),
                                 md_ligne(joindre(pb$lieu)))),
                collapse = " &mdash; "),
          paste0(titre, if (nzchar(fmt_date(pb$date))) paste0(" (", fmt_date(pb$date), ")") else ""))
      }, character(1))
      corps <- c(corps, titre_section(txt(cfg$titre_publications) %||% "Publications"), bloc)
    }
  }

  if (nzchar(txt(cfg$note))) {
    corps <- c(corps, sprintf('      <p class="cv-note">%s</p>', md_ligne(cfg$note)))
  }

  bouts <- character(0)
  if (nzchar(txt(cfg$lieu))) bouts <- c(bouts, esc(cfg$lieu))
  if (nzchar(txt(site$courriel)))
    bouts <- c(bouts, sprintf('<a href="mailto:%s">%s</a>', esc(site$courriel), esc(site$courriel)))
  if (nzchar(txt(cfg$telephone))) bouts <- c(bouts, esc(cfg$telephone))
  if (nzchar(txt(cfg$site))) bouts <- c(bouts, sprintf('<a href="%s">%s</a>', esc(cfg$site), esc(cfg$site)))
  contact <- paste(bouts, collapse = '<span class="sep"></span>')

  profil <- if (nzchar(txt(cfg$profil)))
    paste0('      <div class="cv-profil">', md(cfg$profil), "</div>") else ""

  gabarit <- lire_fichier(file.path(racine, "modeles", "gabarit_cv.html"))
  page <- sub("<!--GABARIT-DEBUT.*?GABARIT-FIN-->\n", "", gabarit)
  rempl <- function(page, cle, valeur) gsub(paste0("{{", cle, "}}"), valeur, page, fixed = TRUE)
  page <- rempl(page, "AVIS", paste0(
    "<!--\n  FICHIER GÉNÉRÉ AUTOMATIQUEMENT — NE PAS MODIFIER À LA MAIN.\n",
    "  Produit par build.R le ", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n",
    "  Contenu : contenu/parcours.yml et contenu/site.yml (bloc cv:)\n",
    "  Structure et styles : modeles/gabarit_cv.html\n-->"))
  page <- rempl(page, "TITRE_ONGLET", esc(txt(cfg$titre_onglet) %||% paste("CV —", txt(site$nom))))
  page <- rempl(page, "NOM", esc(site$nom))
  page <- rempl(page, "CONTACT", contact)
  page <- rempl(page, "PROFIL", profil)
  page <- rempl(page, "CORPS", paste(corps, collapse = "\n"))

  ecrire_fichier(page, file.path(racine, "cv.html"))
  message("cv.html regénéré (version imprimable du parcours).")
  invisible(page)
}

GENERATEURS <- list(
  texte        = gen_texte,
  texte_icones = gen_texte_icones,
  parcours     = gen_parcours,
  documents    = gen_documents,
  publications = gen_publications,
  projets      = gen_projets,
  contact      = gen_contact
)

# --- Construction ----------------------------------------------------------

construire_site <- function(racine = ".") {
  message("Lecture du contenu...")
  donnees <- list(
    site         = lire_yaml(file.path(racine, "contenu", "site.yml")),
    documents    = lire_yaml(file.path(racine, "contenu", "documents.yml")),
    parcours     = lire_yaml(file.path(racine, "contenu", "parcours.yml")),
    publications = lire_yaml(file.path(racine, "contenu", "publications.yml")),
    projets      = lire_yaml(file.path(racine, "contenu", "projets.yml"))
  )
  site <- donnees$site
  if (length(site) == 0) stop("contenu/site.yml est introuvable ou vide.")

  sections <- Filter(function(s) !identical(s$afficher, FALSE), site$sections %||% list())

  html_sections <- character(0)
  html_nav      <- character(0)

  for (s in sections) {
    type <- txt(s$type %||% "texte")
    gen  <- GENERATEURS[[type]]
    if (is.null(gen)) {
      warning("Type de section inconnu : '", type, "' (section '", txt(s$id), "') — ignorée.")
      next
    }
    corps <- gen(s, donnees)
    if (is.null(corps) || !nzchar(corps)) {
      message("  section '", txt(s$id), "' vide -> ignorée")
      next
    }
    id <- txt(s$id)
    html_sections <- c(html_sections,
                       sprintf('        <!-- %s -->\n        <section id="%s">\n%s\n        </section>',
                               esc(s$titre %||% id), esc(id), corps))
    if (!identical(s$menu, FALSE) && nzchar(txt(s$titre))) {
      html_nav <- c(html_nav, sprintf('          <li><a href="#%s"%s>%s</a></li>',
                                      esc(id),
                                      if (length(html_nav) == 0) ' class="active"' else "",
                                      esc(s$titre)))
    }
    message("  + ", type, " : ", txt(s$titre %||% id))
  }

  html_reseaux <- vapply(site$reseaux %||% list(), function(r) sprintf(
    '          <li><a href="%s" class="icon %s"><span class="label">%s</span></a></li>',
    esc(r$url), esc(r$icone), esc(r$nom)), character(1))

  gabarit <- lire_fichier(file.path(racine, "modeles", "gabarit.html"))
  remplacer <- function(page, cle, valeur) {
    gsub(paste0("{{", cle, "}}"), valeur, page, fixed = TRUE)
  }
  page <- sub("<!--GABARIT-DEBUT.*?GABARIT-FIN-->\n", "", gabarit)
  page <- remplacer(page, "AVIS", paste0(
    "<!--\n",
    "  FICHIER GÉNÉRÉ AUTOMATIQUEMENT — NE PAS MODIFIER À LA MAIN.\n",
    "  Produit par build.R le ", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n",
    "  Contenu : contenu/*.yml   |   Structure : modeles/gabarit.html\n",
    "  Design  : Read Only by HTML5 UP (html5up.net) — licence CCA 3.0\n-->"))
  page <- remplacer(page, "TITRE_ONGLET", esc(site$titre_onglet))
  page <- remplacer(page, "META_DESCRIPTION", esc(site$meta_description))
  page <- remplacer(page, "NOM", esc(site$nom))
  page <- remplacer(page, "PHOTO", esc(site$photo))
  page <- remplacer(page, "ACCROCHE",
                    gsub("\n", "<br />\n          ",
                         trimws(esc(paste(site$accroche, collapse = "\n")))))
  page <- remplacer(page, "NAV", paste(html_nav, collapse = "\n"))
  page <- remplacer(page, "RESEAUX", paste(html_reseaux, collapse = "\n"))
  page <- remplacer(page, "SECTIONS", paste(html_sections, collapse = "\n\n"))
  page <- remplacer(page, "ANNEE", format(Sys.Date(), "%Y"))

  restants <- regmatches(page, gregexpr("\\{\\{[A-Z_]+\\}\\}", page))[[1]]
  if (length(restants)) warning("Marqueurs non remplacés : ", paste(unique(restants), collapse = ", "))

  ecrire_fichier(page, file.path(racine, "index.html"))
  gen_cv(donnees, racine)
  message("\nindex.html regénéré (", format(nchar(page), big.mark = " "), " caractères).")
  message("Ouvre-le dans ton navigateur pour vérifier, puis : source(\"publier.R\")")
  invisible(page)
}

# --- Aide : ajouter un PDF sans écrire de YAML à la main --------------------

# Ajoute un document (PDF, PPTX, DOCX, XLSX...) a la section Travaux
# universitaires : copie le fichier dans documents/, ajoute la fiche dans
# contenu/documents.yml, puis regenere le site.
#   ajouter_document("C:/travaux/mon_essai.pdf", "Titre", categorie = "Essai")
ajouter_document <- function(chemin, titre, description = "",
                             categorie = "", date = format(Sys.Date(), "%Y-%m"),
                             racine = ".", chemin_pdf = NULL) {
  if (!is.null(chemin_pdf)) chemin <- chemin_pdf   # ancien nom d'argument
  if (!file.exists(chemin)) stop("Fichier introuvable : ", chemin)
  dir.create(file.path(racine, "documents"), showWarnings = FALSE)
  destination <- file.path("documents", basename(chemin))
  destination <- gsub("\\\\", "/", destination)
  if (normalizePath(chemin, mustWork = FALSE) !=
      normalizePath(file.path(racine, destination), mustWork = FALSE)) {
    file.copy(chemin, file.path(racine, destination), overwrite = TRUE)
  }
  entree <- sprintf(
    '\n- titre: "%s"\n  fichier: "%s"\n  date: "%s"\n  categorie: "%s"\n  description: |\n    %s\n',
    gsub('"', "'", titre), destination, date, gsub('"', "'", categorie),
    gsub("\n", "\n    ", description))
  chemin_yml <- file.path(racine, "contenu", "documents.yml")
  ancien <- lire_fichier(chemin_yml)          # lu et conservé avant l'écriture
  if (!nzchar(ancien)) stop("contenu/documents.yml est vide ou illisible : ",
                            "rien n'a été écrit.", call. = FALSE)
  ecrire_fichier(paste0(ancien, entree), chemin_yml)
  message("Document ajouté : ", destination)
  construire_site(racine)
}

# --- Exécution -------------------------------------------------------------

if (sys.nframe() == 0L || identical(environment(), globalenv())) {
  construire_site()
}
