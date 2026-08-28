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
  con <- file(chemin, open = "wb")
  on.exit(close(con))
  writeBin(charToRaw(enc2utf8(x)), con)
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
md <- function(x, balise = "p") {
  if (is.null(x)) return("")
  s <- paste(as.character(x), collapse = "\n")
  if (!nzchar(trimws(s))) return("")
  s <- gsub("&", "&amp;", s, fixed = TRUE)
  s <- gsub("<", "&lt;",  s, fixed = TRUE)
  s <- gsub(">", "&gt;",  s, fixed = TRUE)
  s <- gsub("\\[([^]]+)\\]\\(([^)]+)\\)", '<a href="\\2">\\1</a>', s)
  s <- gsub("\\*\\*([^*]+)\\*\\*", "<strong>\\1</strong>", s)
  s <- gsub("\\*([^*]+)\\*", "<em>\\1</em>", s)
  paras <- trimws(strsplit(s, "\n[ \t]*\n")[[1]])
  paras <- paras[nzchar(paras)]
  if (length(paras) == 0) return("")
  paste0("<", balise, ">", paras, "</", balise, ">", collapse = "\n")
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
  paste0(bloc, '<div class="container">\n', entete, md(s$texte), "\n</div>")
}

gen_texte_icones <- function(s, ...) {
  faits <- s$faits %||% list()
  li <- vapply(faits, function(f) sprintf('  <li class="icon %s">%s</li>',
                                          esc(f$icone %||% "solid fa-check"),
                                          md_ligne(f$texte)), character(1))
  paste0('<div class="container">\n<h3>', esc(s$titre), "</h3>\n", md(s$texte),
         if (length(li)) paste0('\n<ul class="feature-icons">\n', paste(li, collapse = "\n"),
                                "\n</ul>") else "",
         "\n</div>")
}

gen_parcours <- function(s, donnees, ...) {
  p <- donnees$parcours

  # Garde-fou : une rubrique mal orthographi\u00e9e dans parcours.yml serait
  # sinon ignor\u00e9e en silence, et son contenu dispara\u00eetrait du site.
  RUBRIQUES <- c("experiences", "formation", "formations_connexes")
  connus <- c(RUBRIQUES, paste0("titre_", RUBRIQUES))
  inconnus <- setdiff(names(p), connus)
  if (length(inconnus)) {
    warning("parcours.yml : rubrique(s) ignor\u00e9e(s) car non reconnue(s) : ",
            paste(inconnus, collapse = ", "),
            ".\n  Les rubriques valides sont : ", paste(RUBRIQUES, collapse = ", "),
            ".\n  Pour changer un intitul\u00e9 affich\u00e9, utilise titre_<rubrique>.",
            call. = FALSE)
  }

  etiquette <- function(cle, defaut) {
    v <- txt(p[[paste0("titre_", cle)]])
    if (nzchar(v)) v else defaut
  }

  # Premier champ non vide parmi plusieurs noms possibles : tolère
  # poste / diplome / formation / titre sans que le fichier plante.
  premier <- function(e, noms) {
    for (n in noms) if (nzchar(txt(e[[n]]))) return(e[[n]])
    ""
  }

  bloc <- function(titre, items) {
    if (length(items) == 0) return("")
    items <- items[order(-vapply(items, function(e) cle_date(e$fin, TRUE), numeric(1)),
                         -vapply(items, function(e) cle_date(e$debut), numeric(1)))]
    li <- vapply(items, function(e) {
      intitule <- premier(e, c("poste", "diplome", "formation", "titre"))
      if (!nzchar(txt(intitule))) {
        warning("Une entrée de '", titre, "' n'a pas d'intitulé (poste, diplome ou formation).",
                call. = FALSE)
      }
      sprintf(
        '  <li class="pc-item">\n    <span class="pc-date">%s</span>\n    <h5 class="pc-titre">%s</h5>\n    <span class="pc-org">%s</span>\n    %s\n  </li>',
        periode(e$debut, e$fin),
        esc(intitule),
        paste0(esc(premier(e, c("organisation", "etablissement"))),
               if (nzchar(txt(e$lieu))) paste0(" &middot; ", esc(e$lieu)) else ""),
        md(e$description))
    }, character(1))
    paste0("<h4>", titre, "</h4>\n<ul class=\"pc-liste\">\n",
           paste(li, collapse = "\n"), "\n</ul>\n")
  }

  paste0('<div class="container">\n<h3>', esc(s$titre), "</h3>\n", md(s$texte), "\n",
         bloc(etiquette("experiences", "Expérience professionnelle"), p$experiences %||% list()),
         bloc(etiquette("formation", "Formation"), p$formation %||% list()),
         bloc(etiquette("formations_connexes", "Formations connexes"), p$formations_connexes %||% list()),
         "</div>")
}

gen_documents <- function(s, donnees, ...) {
  docs <- trier_par_date(donnees$documents)
  if (length(docs) == 0) return(NULL)
  li <- vapply(docs, function(d) {
    meta <- paste(Filter(nzchar, c(esc(d$categorie), fmt_date(d$date))), collapse = " &middot; ")
    apercu <- if (nzchar(txt(d$apercu)))
      sprintf('<a href="%s" target="_blank" class="doc-vignette"><img src="%s" alt="Aperçu : %s" /></a>',
              esc(d$fichier), esc(d$apercu), esc(d$titre)) else ""
    sprintf(
paste0('  <li class="doc-item">\n%s',
       '    <div class="doc-corps">\n',
       '      <h4><a href="%s" target="_blank"><span class="icon solid fa-file-pdf"></span> %s</a></h4>\n',
       '      <span class="doc-meta">%s</span>\n%s',
       '      <p><a href="%s" target="_blank" class="button small">Consulter le PDF</a></p>\n',
       '    </div>\n  </li>'),
      if (nzchar(apercu)) paste0("    ", apercu, "\n") else "",
      esc(d$fichier), esc(d$titre), meta,
      if (nzchar(txt(d$description))) paste0("      ", md(d$description), "\n") else "",
      esc(d$fichier))
  }, character(1))
  paste0('<div class="container">\n<h3>', esc(s$titre), "</h3>\n", md(s$texte),
         '\n<ul class="doc-liste">\n', paste(li, collapse = "\n"), "\n</ul>\n</div>")
}

gen_publications <- function(s, donnees, ...) {
  pubs <- trier_par_date(donnees$publications)
  if (length(pubs) == 0) return(NULL)
  li <- vapply(pubs, function(p) {
    lien <- txt(p$url %||% p$fichier)
    titre <- if (nzchar(lien))
      sprintf('<a href="%s" target="_blank">%s</a>', esc(lien), esc(p$titre)) else esc(p$titre)
    ref <- paste(Filter(nzchar, c(esc(p$auteurs), esc(p$editeur), esc(p$numero),
                                  fmt_date(p$date))), collapse = ", ")
    sprintf('  <li class="pub-item">\n    <span class="pub-type">%s</span>\n    <h4 class="pub-titre">%s</h4>\n    <span class="pub-ref">%s</span>\n    %s\n  </li>',
            esc(p$type %||% "Publication"), titre, ref, md(p$resume))
  }, character(1))
  paste0('<div class="container">\n<h3>', esc(s$titre), "</h3>\n", md(s$texte),
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
  paste0('<div class="container">\n<h3>', esc(s$titre), "</h3>\n", md(s$texte),
         '\n<div class="features">\n', paste(art, collapse = "\n"), "\n</div>\n</div>")
}

gen_contact <- function(s, donnees, ...) {
  courriel <- txt(donnees$site$courriel)
  paste0('<div class="container">\n<h3>', esc(s$titre), "</h3>\n", md(s$texte),
         '\n<div class="row gtr-uniform">\n  <div class="col-12">\n',
         sprintf('    <a href="mailto:%s?subject=Prise%%20de%%20contact" class="button primary">Envoyer un courriel</a>\n',
                 esc(courriel)),
         "  </div>\n</div>\n</div>")
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
  message("\nindex.html regénéré (", format(nchar(page), big.mark = " "), " caractères).")
  message("Ouvre-le dans ton navigateur pour vérifier, puis : source(\"publier.R\")")
  invisible(page)
}

# --- Aide : ajouter un PDF sans écrire de YAML à la main --------------------

ajouter_document <- function(chemin_pdf, titre, description = "",
                             categorie = "", date = format(Sys.Date(), "%Y-%m"),
                             racine = ".") {
  if (!file.exists(chemin_pdf)) stop("Fichier introuvable : ", chemin_pdf)
  dir.create(file.path(racine, "documents"), showWarnings = FALSE)
  destination <- file.path("documents", basename(chemin_pdf))
  if (normalizePath(chemin_pdf, mustWork = FALSE) !=
      normalizePath(file.path(racine, destination), mustWork = FALSE)) {
    file.copy(chemin_pdf, file.path(racine, destination), overwrite = TRUE)
  }
  entree <- sprintf(
    '\n- titre: "%s"\n  fichier: "%s"\n  date: "%s"\n  categorie: "%s"\n  description: |\n    %s\n',
    gsub('"', "'", titre), destination, date, gsub('"', "'", categorie),
    gsub("\n", "\n    ", description))
  chemin_yml <- file.path(racine, "contenu", "documents.yml")
  ecrire_fichier(paste0(lire_fichier(chemin_yml), entree), chemin_yml)
  message("Document ajouté : ", destination)
  construire_site(racine)
}

# --- Exécution -------------------------------------------------------------

if (sys.nframe() == 0L || identical(environment(), globalenv())) {
  construire_site()
}
