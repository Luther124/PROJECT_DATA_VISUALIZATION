# =====================================================================
# 00_installer_packages.R
# Installation des packages à partir du dossier « packages » du projet,
# sans connexion internet.
#
# Ce script est facultatif : app.Rmd utilise déjà directement le dossier
# « packages » au démarrage. Il sert à copier ces packages dans la
# bibliothèque personnelle de R, pour les utiliser dans d'autres projets.
#
# Utilisation : ouvrir ce fichier dans RStudio, puis cliquer sur « Source ».
# Les packages ont été préparés avec R 4.6 sous Windows (packages/VERSION_R.txt).
# =====================================================================

# Le dossier de travail devient celui de ce fichier (dossier Projet_TABORD)
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
}
stopifnot("Le dossier « packages » est introuvable" = dir.exists("packages"))

# Bibliothèque personnelle de R (créée si elle n'existe pas encore)
bibliotheque <- Sys.getenv("R_LIBS_USER")
dir.create(bibliotheque, recursive = TRUE, showWarnings = FALSE)

# Packages fournis qui ne sont pas encore installés
fournis    <- list.dirs("packages", recursive = FALSE, full.names = FALSE)
manquants  <- setdiff(fournis, rownames(installed.packages()))

# Copie des packages manquants depuis le dossier du projet
for (p in manquants) file.copy(file.path("packages", p), bibliotheque, recursive = TRUE)

cat(length(manquants), "package(s) installé(s) dans", bibliotheque, "\n")
cat("Version de R utilisée :", R.version.string, "\n")
