# ===========================================================================
#  publier.R — Regénère le site puis l'envoie en ligne
#
#  UTILISATION :  source("publier.R")
#
#  Fait, dans l'ordre :
#    1. build.R  -> regénère index.html à partir de contenu/*.yml
#    2. git add / git commit / git push
#  Le site est mis à jour sur williambeaudry.github.io après 1 à 2 minutes.
#
#  Si le push échoue (mot de passe, conflit...), passe par l'onglet Git
#  de RStudio : le index.html est déjà regénéré, il ne reste qu'à pousser.
# ===========================================================================

publier <- function(message_commit = NULL) {

  # 1. Reconstruire le site (build.R est chargé localement, sans polluer
  #    l'environnement global)
  source("build.R", local = TRUE, encoding = "UTF-8")
  construire_site(".")

  # 2. Y a-t-il quelque chose à publier ?
  statut <- suppressWarnings(system2("git", c("status", "--porcelain"), stdout = TRUE))
  if (length(statut) == 0) {
    message("\nRien à publier : aucun changement détecté.")
    return(invisible(FALSE))
  }
  message("\nFichiers modifiés :\n", paste("  ", statut, collapse = "\n"))

  if (is.null(message_commit)) {
    message_commit <- paste("Mise a jour du site -", format(Sys.time(), "%Y-%m-%d %H:%M"))
  }

  # 3. Envoyer sur GitHub
  etape <- function(args, description) {
    if (system2("git", args) != 0) {
      stop("Échec de l'étape : ", description,
           "\nOuvre l'onglet Git de RStudio pour voir le détail.", call. = FALSE)
    }
  }
  etape(c("add", "-A"), "git add")
  etape(c("commit", "-m", shQuote(message_commit)), "git commit")
  etape("push", "git push")

  message("\nPublié. Le site sera à jour dans une minute ou deux :")
  message("  https://williambeaudry.github.io")
  invisible(TRUE)
}

publier()
