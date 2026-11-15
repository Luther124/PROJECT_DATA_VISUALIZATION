# =====================================================================
# deployer.R : Mise en ligne du dashboard sur Posit Connect Cloud
# ---------------------------------------------------------------------
# Mode d'emploi détaillé : GUIDE_DEPLOIEMENT.txt
# À exécuter LIGNE PAR LIGNE dans RStudio (Ctrl + Entrée), après avoir
# ouvert le projet : Session > Set Working Directory > To Source File Location
# =====================================================================

library(rsconnect)

# ---------------------------------------------------------------------
# ÉTAPE 1 (une seule fois) : relier RStudio à votre compte
#
# OPTION A : Posit Connect Cloud (https://connect.posit.cloud)
#   C'est la nouvelle plateforme de Posit, qui remplace shinyapps.io.
#   Exécuter la ligne ci-dessous : le navigateur s'ouvre, cliquer sur
#   « Authorize », puis revenir dans RStudio.
# ---------------------------------------------------------------------
rsconnect::connectCloudUser()
rsconnect::accounts()          # contrôle : votre compte doit apparaître

# OPTION B : shinyapps.io (seulement si l'option A est refusée)
#   Sur https://www.shinyapps.io : Account > Tokens > Show > Show secret
#   > « Copy to clipboard », coller la ligne copiée dans la console.
# rsconnect::setAccountInfo(name = "votre_compte", token = "XXXXXXXX", secret = "XXXXXXXX")

# ---------------------------------------------------------------------
# ÉTAPE 2 : liste EXACTE des fichiers envoyés au serveur
# Important : l'ancienne application app.R et l'ancien dashboard ne sont
# PAS envoyés. Si app.R était envoyé, le serveur lancerait app.R au
# lieu du flexdashboard.
# ---------------------------------------------------------------------
fichiers <- c(
  "app.Rmd",
  "R/fonctions.R", "R/donnees_rapport.R", "R/commentaires_ia.R",
  "R/exports_office.R", "R/export_pdf.R",
  "rapports/rapport_pdf.Rmd",
  "data/Base_TABORD_harmonisee_2018_2025.xlsx",
  "logo_MINFI.png", "logo_ISSEA.jpg", "rapport_stage_DJAMKENE.pdf"
)
# La clé de l'IA voyage dans .Renviron (fichier privé, jamais publié ailleurs)
if (file.exists(".Renviron")) fichiers <- c(fichiers, ".Renviron")

# Contrôle : tous les fichiers existent-ils ?
stopifnot("Fichier manquant : vérifier le dossier de travail" = all(file.exists(fichiers)))

# ---------------------------------------------------------------------
# ÉTAPE 3 : envoi (5 à 10 minutes la première fois : le serveur installe
# les packages). L'adresse du dashboard s'affiche à la fin et s'ouvre
# dans le navigateur. C'est ce lien qu'il faut transmettre.
# ---------------------------------------------------------------------
rsconnect::deployApp(
  appDir        = ".",
  appFiles      = fichiers,
  appPrimaryDoc = "app.Rmd",                    # document à lancer
  appName       = "tabord-recettes-fiscales",
  appTitle      = "Recettes fiscales - TABORD",
  forceUpdate   = TRUE
)
# Si vous avez relié les DEUX comptes (options A et B), R demande lequel
# utiliser : taper le numéro du compte voulu.

# En cas de problème après la mise en ligne : sur https://connect.posit.cloud,
# ouvrir le contenu « Recettes fiscales - TABORD » puis l'onglet « Logs ».
