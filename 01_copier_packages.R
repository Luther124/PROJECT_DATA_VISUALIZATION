# =====================================================================
# 01_copier_packages.R
# Copie dans le dossier « packages » tous les packages dont le tableau de
# bord a besoin, avec leurs dépendances. Ce dossier est joint au projet :
# l'utilisateur n'a ainsi aucun package à télécharger.
#
# À lancer UNE SEULE FOIS, sur l'ordinateur où le tableau de bord fonctionne :
#   ouvrir ce fichier dans RStudio, puis cliquer sur « Source ».
# =====================================================================

# Le dossier de travail devient celui de ce fichier (dossier Projet_TABORD)
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
}

# Packages appelés par app.Rmd et par les fichiers du dossier R
pkgs <- c("flexdashboard", "rmarkdown", "knitr", "shiny", "bslib", "plotly", "DT",
          "readxl", "dplyr", "tidyr", "openxlsx", "scales", "officer", "mschart",
          "flextable", "ggplot2", "png", "jpeg", "ellmer")

# Ajout de toutes leurs dépendances
installes <- installed.packages()
deps <- tools::package_dependencies(pkgs, db = installes, recursive = TRUE,
                                    which = c("Depends", "Imports"))

# Les packages livrés avec R sont déjà présents sur tout ordinateur
fournis_avec_R <- rownames(installed.packages(priority = c("base", "recommended")))
a_copier <- setdiff(unique(c(pkgs, unlist(deps))), fournis_avec_R)

# Copie
dir.create("packages", showWarnings = FALSE)
for (p in a_copier) file.copy(find.package(p), "packages", recursive = TRUE)

# Fichier qui indique la version de R utilisée
writeLines(c(paste("Packages copiés avec", R.version.string),
             paste("Système :", Sys.info()[["sysname"]]),
             "Le dossier fonctionne avec la même version de R (deux premiers chiffres)."),
           file.path("packages", "VERSION_R.txt"))

cat("\n", length(a_copier), "packages copiés dans le dossier packages.\n",
    "Taille du dossier :", round(sum(file.size(list.files("packages", recursive = TRUE,
    full.names = TRUE))) / 1e6), "Mo\n", R.version.string, "\n")

