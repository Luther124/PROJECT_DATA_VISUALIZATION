# =====================================================================
# donnees_rapport.R : Calculs communs au dashboard, à l'IA et aux exports
# ---------------------------------------------------------------------
# IDÉE CLÉ : tout ce qui est affiché ou exporté part d'UN SEUL objet,
# construit par preparer_rapport(). Cet objet contient, pour chacune des
# quatre sections d'analyse (vue, structure, budget, choc) :
#   - les chiffres clés (KPI),
#   - la DESCRIPTION des graphiques (« spécifications »),
#   - les tableaux,
#   - une liste de « faits » chiffrés, en phrases courtes.
#
# Une spécification de graphique est une simple liste :
#   list(titre, type, data, ylab, couleurs, decimales)
#   - type : "lignes", "barres", "barres_empilees" ou "barres_h"
#   - data : tableau long à 3 colonnes  x (facteur) | serie | valeur
# La même spécification est ensuite dessinée :
#   - en graphique Microsoft natif (mschart)  -> R/exports_office.R
#   - en graphique ggplot2 (image)            -> R/export_pdf.R
# Ainsi Word, PowerPoint et PDF montrent exactement les mêmes chiffres.
#
# Ce fichier suppose que R/fonctions.R a déjà été chargé (source).
# =====================================================================

# ---------------------------------------------------------------------
# 1. Palette et statuts (partagés par le dashboard et les exports)
# ---------------------------------------------------------------------
COUL <- c(
  navy   = "#0B3C5D",   # bleu institutionnel (en-têtes, titres)
  dgi    = "#1D5A96",   # X : impôts et taxes (DGI) / exercice N
  dgd    = "#62AEDF",   # Y : recettes douanières (DGD)
  gris   = "#9AA5B1",   # exercice N-1 / éléments de comparaison
  vert   = "#1E8E3E",   # hausse, surperformance (TR >= 100 %)
  orange = "#E08A1E",   # cible, objectif, vigilance
  rouge  = "#D93025"    # baisse, alerte
)

MOIS_LONG <- c("Janvier", "Février", "Mars", "Avril", "Mai", "Juin", "Juillet",
               "Août", "Septembre", "Octobre", "Novembre", "Décembre")

# Rubriques suivies dans l'analyse d'un choc (tableau trimestriel)
VARS_CHOC <- c("Z", "X", "X1", "X2", "X3", "Y", "Y1", "Y2")

# Statut d'un taux de réalisation : vert >= 100 ; orange >= seuil ; rouge sinon
statut_tr <- function(tr, seuil = 90) {
  dplyr::case_when(is.na(tr)   ~ "gris",
                   tr >= 100   ~ "vert",
                   tr >= seuil ~ "orange",
                   TRUE        ~ "rouge")
}

# Statut d'une variation : vert si hausse, rouge si baisse
statut_variation <- function(x) {
  dplyr::case_when(is.na(x) ~ "gris", x >= 0 ~ "vert", TRUE ~ "rouge")
}

# ---------------------------------------------------------------------
# 2. Calculs élémentaires (fonctions « pures » : mêmes entrées -> mêmes
#    sorties ; elles sont appelées par les reactive() du dashboard)
# ---------------------------------------------------------------------

# Cumul de janvier au mois choisi : année N, année N-1 et objectif annuel
calc_cumul <- function(b, rubrique, annee, mois) {
  s <- serie_rubrique(b, rubrique)
  objectif <- b$bud %>% filter(annee == !!annee, code == rubrique)
  list(
    n        = sum(s$montant[s$annee == annee & s$mois <= mois]),
    n1       = if (any(s$annee == annee - 1))
                 sum(s$montant[s$annee == annee - 1 & s$mois <= mois]) else NA_real_,
    objectif = if (nrow(objectif) > 0) objectif$montant_prevu[1] else NA_real_,
    type     = if (nrow(objectif) > 0) objectif$type_reference[1] else "non précisée"
  )
}

# Taux de réalisation proratisé : cumul / (objectif annuel x mois / 12)
calc_tr_prorata <- function(cumul, mois) {
  if (is.na(cumul$objectif) || cumul$objectif == 0) NA_real_
  else cumul$n / (cumul$objectif * mois / 12) * 100
}

# Performance budgétaire annuelle d'une rubrique.
# Année complète : total publié ; année en cours : somme des mois connus.
calc_tr_rub <- function(b, rubrique, seuil = 90) {
  realisations <- b$real %>%
    filter(code == rubrique) %>%
    group_by(annee) %>%
    summarise(somme_mois = sum(montant), nb_mois = n(), .groups = "drop")
  totaux <- b$tot %>% filter(code == rubrique) %>% select(annee, total_publie)
  prevus <- b$bud %>% filter(code == rubrique) %>%
    select(annee, type_reference, montant_prevu)

  realisations %>%
    left_join(totaux, by = "annee") %>%
    left_join(prevus, by = "annee") %>%
    mutate(realisation = if_else(nb_mois == 12 & !is.na(total_publie), total_publie, somme_mois),
           ecart       = realisation - montant_prevu,
           TR          = realisation / montant_prevu * 100,
           statut      = statut_tr(TR, seuil))
}

# Glissements annuels (GA) mensuels de X, Y, Z pour l'année a
# GA (%) = (valeur du mois en N / valeur du même mois en N-1 - 1) x 100
calc_ga_mensuel <- function(mo, a) {
  mo %>%
    filter(annee %in% c(a - 1, a)) %>%
    select(annee, mois, X, Y, Z) %>%
    pivot_longer(c(X, Y, Z), names_to = "variable", values_to = "valeur") %>%
    mutate(periode = if_else(annee == a, "N", "N_1")) %>%
    select(-annee) %>%
    pivot_wider(names_from = periode, values_from = valeur) %>%
    mutate(GA = (N / N_1 - 1) * 100)
}

# Glissements annuels trimestriels (somme des 3 mois de chaque trimestre)
calc_ga_trim <- function(mo, a, vars = VARS_CHOC) {
  mo %>%
    filter(annee %in% c(a - 1, a)) %>%
    mutate(trimestre = paste0("T", (mois - 1) %/% 3 + 1),
           periode   = if_else(annee == a, "N", "N_1")) %>%
    select(periode, trimestre, all_of(vars)) %>%
    pivot_longer(all_of(vars), names_to = "variable", values_to = "valeur") %>%
    group_by(variable, trimestre, periode) %>%
    summarise(valeur = if (n() == 3) sum(valeur) else NA_real_, .groups = "drop") %>%
    pivot_wider(names_from = periode, values_from = valeur) %>%
    mutate(GA = (N / N_1 - 1) * 100)
}

# ---------------------------------------------------------------------
# 3. Petits outils pour construire les spécifications de graphiques
# ---------------------------------------------------------------------

# Crée une spécification ; renvoie NULL s'il n'y a rien à dessiner
spec <- function(titre, type, data, ylab = "Milliards de FCFA",
                 couleurs = NULL, decimales = 1, ymax = NULL) {
  data <- data %>% filter(!is.na(valeur))
  if (nrow(data) == 0) return(NULL)
  list(titre = titre, type = type, data = data, ylab = ylab,
       couleurs = couleurs, decimales = decimales, ymax = ymax)   # ymax : borne haute de l'axe (facultatif)
}

# Mois sous forme de facteur ordonné (Janv., Févr., ...) pour l'axe des x
f_mois  <- function(m) factor(MOIS_LIB[m], levels = MOIS_LIB)
# Années sous forme de facteur ordonné
f_annee <- function(a) factor(a, levels = sort(unique(a)))

# ---------------------------------------------------------------------
# 4. preparer_rapport() : tout le contenu, pour un jeu de filtres donné
#    f = list(annee, mois, rubrique, seuil, a0, a1, annee_choc)
# ---------------------------------------------------------------------
preparer_rapport <- function(b, an, mo, f) {

  # --- Sécurités sur les filtres --------------------------------------
  if (f$a1 <= f$a0) { f$a0 <- min(an$annee); f$a1 <- max(an$annee) }
  f$lib_rub  <- b$ref$libelle_standard[match(f$rubrique, b$ref$code)]
  f$mois_lib <- MOIS_LONG[f$mois]
  N <- f$annee; M <- f$mois; A <- f$annee_choc

  # ====================================================================
  # SECTION 1 : VUE D'ENSEMBLE (exercice N, cumul à fin du mois M)
  # ====================================================================
  s   <- serie_rubrique(b, f$rubrique)
  cu  <- calc_cumul(b, f$rubrique, N, M)
  trp <- calc_tr_prorata(cu, M)
  ga  <- if (is.na(cu$n1) || cu$n1 == 0) NA_real_ else (cu$n / cu$n1 - 1) * 100
  part_obj <- if (is.na(cu$objectif)) NA_real_ else cu$n / cu$objectif * 100

  # Graphique : saisonnalité comparée N et N-1
  d_saison <- s %>% filter(annee %in% c(N - 1, N)) %>%
    transmute(x = f_mois(mois), serie = as.character(annee), valeur = montant)
  coul_n <- setNames(c(COUL[["gris"]], COUL[["dgi"]]), c(N - 1, N))

  # Graphique : cumul N, N-1 et objectif proratisé
  d_cumul <- s %>% filter(annee %in% c(N - 1, N)) %>%
    arrange(annee, mois) %>% group_by(annee) %>%
    mutate(valeur = cumsum(montant)) %>% ungroup() %>%
    transmute(x = f_mois(mois), serie = as.character(annee), valeur)
  if (!is.na(cu$objectif)) {
    d_cumul <- bind_rows(d_cumul, tibble(x = f_mois(1:12), serie = "Objectif proratisé",
                                         valeur = cu$objectif * (1:12) / 12))
  }

  # Graphiques annuels : niveaux de X et Y, croissance de Z
  d_annuel <- an %>% select(annee, X, Y) %>%
    pivot_longer(c(X, Y), names_to = "v", values_to = "valeur") %>%
    transmute(x = f_annee(annee),
              serie = if_else(v == "X", "X : impôts et taxes (DGI)", "Y : recettes douanières (DGD)"),
              valeur)
  croiss <- an %>% arrange(annee) %>% mutate(g = (Z / lag(Z) - 1) * 100) %>% filter(!is.na(g))
  d_croiss <- croiss %>% transmute(x = f_annee(annee), serie = "Croissance de Z", valeur = g)

  z_deb <- an$Z[which.min(an$annee)]; z_fin <- an$Z[which.max(an$annee)]
  pire_an <- croiss %>% slice_min(g, n = 1); meil_an <- croiss %>% slice_max(g, n = 1)

  vue <- list(
    titre = "Vue d'ensemble",
    sous_titre = paste0(f$lib_rub, ", exercice ", N, ", cumul à fin ", tolower(f$mois_lib)),
    kpi = tibble(
      Indicateur = c(paste0("Cumul recouvré à fin ", tolower(f$mois_lib), " ", N, ", en milliards de FCFA"),
                     paste0("Objectif annuel ", N, " selon la ", sub(" \\(.*\\)$", "", cu$type), ", en milliards de FCFA"),
                     "Part de l'objectif annuel atteinte, en %",
                     paste0("Taux de réalisation proratisé à ", M, "/12, en %"),
                     paste0("Glissement annuel du cumul ", N, "/", N - 1, ", en %")),
      Valeur = c(fmt_n(cu$n), ifelse(is.na(cu$objectif), "n.d.", fmt_n(cu$objectif)),
                 ifelse(is.na(part_obj), "n.d.", fmt_n(part_obj)),
                 ifelse(is.na(trp), "n.d.", fmt_n(trp)), fmt_p(ga))),
    graphiques = list(
      spec(paste0("Saisonnalité comparée : ", N, " et ", N - 1), "lignes", d_saison,
           couleurs = coul_n),
      spec(paste0("Cumul ", N, ", ", N - 1, " et objectif proratisé"), "lignes", d_cumul,
           ylab = "Cumul en milliards de FCFA",
           couleurs = c(coul_n, "Objectif proratisé" = COUL[["orange"]])),
      spec("Évolution annuelle des recettes fiscales : Z = X (DGI) + Y (DGD)",
           "barres_empilees", d_annuel, decimales = 0,
           couleurs = c("X : impôts et taxes (DGI)" = COUL[["dgi"]],
                        "Y : recettes douanières (DGD)" = COUL[["dgd"]])),
      spec("Taux de croissance annuel des recettes fiscales Z, en %", "barres", d_croiss,
           ylab = "%", couleurs = c("Croissance de Z" = COUL[["dgi"]]))),
    tableaux = list(),
    faits = c(
      paste0("Rubrique analysée : ", f$lib_rub, " (code ", f$rubrique, ")."),
      paste0("Cumul recouvré de janvier à ", tolower(f$mois_lib), " ", N, " : ", fmt_n(cu$n), " milliards de FCFA."),
      if (!is.na(cu$n1)) paste0("Cumul sur la même période de ", N - 1, " : ", fmt_n(cu$n1),
                                 " milliards, soit un glissement annuel de ", fmt_p(ga), " %."),
      if (!is.na(cu$objectif)) paste0("Objectif annuel ", N, " (", cu$type, ") : ", fmt_n(cu$objectif),
                                       " milliards ; part déjà atteinte : ", fmt_n(part_obj),
                                       " % pour ", fmt_n(M / 12 * 100), " % de l'année écoulée."),
      if (!is.na(trp)) paste0("Taux de réalisation de l'objectif proratisé (", M, "/12) : ", fmt_n(trp),
                               " % ; seuil d'alerte fixé à ", f$seuil, " %."),
      paste0("Recettes fiscales Z : ", fmt_n(z_deb), " milliards en ", min(an$annee), " et ",
             fmt_n(z_fin), " milliards en ", max(an$annee), "."),
      paste0("Plus forte croissance annuelle de Z : ", fmt_p(meil_an$g), " % en ", meil_an$annee,
             " ; plus faible : ", fmt_p(pire_an$g), " % en ", pire_an$annee, ".")),
    chiffres = list(cumul = cu$n, cumul_n1 = cu$n1, ga = ga, objectif = cu$objectif,
                    part_obj = part_obj, tr = trp, type = cu$type)
  )

  # ====================================================================
  # SECTION 2 : STRUCTURE ET CONTRIBUTIONS (période a0 -> a1)
  # ====================================================================
  a0 <- f$a0; a1 <- f$a1
  per <- an %>% filter(between(annee, a0, a1)) %>% arrange(annee)
  hausse <- (last(per$Z) / first(per$Z) - 1) * 100
  taux_tcam <- tcam(last(per$Z), first(per$Z), a1 - a0) * 100
  c1 <- contribution_cumulee(an, a0, a1, c("X", "Y", "Z"))
  part_x <- c1$contribution[1] / c1$contribution[3] * 100
  poids_x <- mean(per$X / per$Z) * 100

  d_struct <- per %>% transmute(annee, `X (DGI)` = X / Z * 100, `Y (DGD)` = Y / Z * 100) %>%
    pivot_longer(-annee, names_to = "serie", values_to = "valeur") %>%
    transmute(x = f_annee(annee), serie, valeur)

  d_niv1 <- contributions(an, c("X", "Y")) %>% filter(annee > a0, annee <= a1) %>%
    transmute(x = f_annee(annee),
              serie = if_else(variable == "X", "Contribution de X (DGI)", "Contribution de Y (DGD)"),
              valeur = contribution)

  niv2 <- contribution_cumulee(an, a0, a1, c(SOUS_X, SOUS_Y)) %>%
    mutate(libelle = VAR_LIB[variable],
           admin   = if_else(startsWith(variable, "X"), "DGI", "DGD")) %>%
    arrange(contribution)
  d_niv2 <- niv2 %>% transmute(x = factor(libelle, levels = libelle), serie = admin,
                               valeur = contribution)

  t_niv2 <- contribution_cumulee(an, a0, a1, c(SOUS_X, "X", SOUS_Y, "Y", "Z")) %>%
    mutate(part = contribution / contribution[variable == "Z"] * 100,
           Rubrique = case_when(variable == "X" ~ "Sous-total X (DGI)",
                                variable == "Y" ~ "Sous-total Y (DGD)",
                                TRUE ~ unname(VAR_LIB[variable]))) %>%
    transmute(Rubrique, `Contribution en points` = fmt_p(contribution, 2),
              `Part de la hausse en %` = fmt_n(part, 1))
  top3 <- niv2 %>% arrange(desc(contribution)) %>% head(3)
  bas1 <- niv2 %>% head(1)

  structure_ <- list(
    titre = "Structure et contributions à la croissance",
    sous_titre = paste0("Période de décomposition : ", a0, " à ", a1),
    kpi = tibble(
      Indicateur = c(paste0("Hausse cumulée de Z de ", a0, " à ", a1, ", en %"),
                     paste0("Taux de croissance annuel moyen sur ", a1 - a0, " ans, en %"),
                     "Part de la DGI dans la hausse de Z, en %",
                     "Poids moyen de la DGI dans Z, en %"),
      Valeur = c(fmt_p(hausse), fmt_p(taux_tcam), fmt_n(part_x), fmt_n(poids_x))),
    graphiques = list(
      spec("Structure des recettes fiscales, en % de Z", "barres_empilees", d_struct, ylab = "% de Z", ymax = 100,
           couleurs = c("X (DGI)" = COUL[["dgi"]], "Y (DGD)" = COUL[["dgd"]])),
      spec("Décomposition de 1er niveau de la croissance de Z, en points de %", "barres_empilees",
           d_niv1, ylab = "Points de pourcentage", decimales = 2,
           couleurs = c("Contribution de X (DGI)" = COUL[["dgi"]],
                        "Contribution de Y (DGD)" = COUL[["dgd"]])),
      spec(paste0("Décomposition de 2e niveau : contributions cumulées de ", a0, " à ", a1, ", en points de %"),
           "barres_h", d_niv2, ylab = "Points de pourcentage", decimales = 2,
           couleurs = c(DGI = COUL[["dgi"]], DGD = COUL[["dgd"]]))),
    tableaux = list(list(titre = paste0("Contributions cumulées et part dans la hausse, de ", a0, " à ", a1),
                         data = t_niv2)),
    faits = c(
      paste0("Entre ", a0, " et ", a1, ", les recettes fiscales Z passent de ", fmt_n(first(per$Z)),
             " à ", fmt_n(last(per$Z)), " milliards de FCFA, soit ", fmt_p(hausse), " %."),
      paste0("Taux de croissance annuel moyen (TCAM) de Z : ", fmt_p(taux_tcam), " % par an."),
      paste0("La DGI (X) explique ", fmt_n(part_x), " % de la variation de Z ; la DGD (Y) ",
             fmt_n(100 - part_x), " %."),
      paste0("Poids moyen de X dans Z : ", fmt_n(poids_x), " % ; poids de X en ", a0, " : ",
             fmt_n(first(per$X) / first(per$Z) * 100), " % ; en ", a1, " : ",
             fmt_n(last(per$X) / last(per$Z) * 100), " %."),
      paste0("Trois premières contributions cumulées (points de %) : ",
             paste0(top3$libelle, " ", fmt_p(top3$contribution, 2), collapse = " ; "), "."),
      paste0("Plus faible contribution : ", bas1$libelle, " (", fmt_p(bas1$contribution, 2), " point).")),
    chiffres = list(hausse = hausse, tcam = taux_tcam, part_x = part_x, poids_x = poids_x,
                    top = top3, a0 = a0, a1 = a1)
  )

  # ====================================================================
  # SECTION 3 : PERFORMANCE BUDGÉTAIRE (rubrique choisie)
  # ====================================================================
  tr  <- calc_tr_rub(b, f$rubrique, f$seuil)
  trv <- tr %>% filter(!is.na(TR))
  tr_n <- trv$TR[trv$annee == N]; if (length(tr_n) == 0) tr_n <- NA_real_
  ec_n <- trv$ecart[trv$annee == N]; if (length(ec_n) == 0) ec_n <- NA_real_
  tr_moy <- if (nrow(trv)) mean(trv$TR) else NA_real_

  d_tr <- trv %>% transmute(x = f_annee(annee), serie = "Taux de réalisation", valeur = TR)
  d_pr <- trv %>% select(annee, `Prévision` = montant_prevu, `Réalisation` = realisation) %>%
    pivot_longer(-annee, names_to = "serie", values_to = "valeur") %>%
    transmute(x = f_annee(annee), serie, valeur)
  t_bud <- tr %>% transmute(Exercice = as.character(annee), `Référence` = type_reference,
                            `Prévision` = fmt_n(montant_prevu), `Réalisation` = fmt_n(realisation),
                            `Écart` = fmt_p(ecart), `TR en %` = fmt_n(TR),
                            `Mois connus` = as.character(nb_mois))

  budget <- list(
    titre = "Performance budgétaire",
    sous_titre = paste0(f$lib_rub, ", prévisions des lois de finances et réalisations"),
    kpi = tibble(
      Indicateur = c(paste0("Taux de réalisation annuel ", N, ", en %"),
                     paste0("Taux de réalisation moyen de ", min(tr$annee), " à ", max(tr$annee), ", en %"),
                     "Exercices ayant atteint ou dépassé l'objectif",
                     paste0("Exercices sous le seuil d'alerte de ", f$seuil, " %")),
      Valeur = c(ifelse(is.na(tr_n), "n.d.", fmt_n(tr_n)), ifelse(is.na(tr_moy), "n.d.", fmt_n(tr_moy)),
                 paste0(sum(trv$TR >= 100), " / ", nrow(trv)),
                 paste0(sum(trv$TR < f$seuil), " / ", nrow(trv)))),
    graphiques = list(
      spec(paste0("Taux de réalisation annuel de la rubrique ", f$lib_rub, ", en %"), "barres", d_tr, ylab = "%",
           couleurs = c("Taux de réalisation" = COUL[["dgi"]])),
      spec("Prévisions et réalisations, en milliards de FCFA", "barres", d_pr, decimales = 0,
           couleurs = c("Prévision" = COUL[["orange"]], "Réalisation" = COUL[["dgi"]]))),
    tableaux = list(list(titre = paste0("Prévisions, réalisations et écarts de la rubrique ", f$lib_rub,
                                        ", en milliards de FCFA"), data = t_bud)),
    faits = if (nrow(trv) == 0) {
      paste0("Aucune prévision budgétaire n'est disponible pour la rubrique ", f$lib_rub, ".")
    } else c(
      paste0("Rubrique : ", f$lib_rub, "."),
      paste0("Taux de réalisation par exercice (%) : ",
             paste0(trv$annee, " : ", fmt_n(trv$TR), collapse = " ; "), "."),
      paste0("Écarts entre réalisation et prévision, en milliards de FCFA : ",
             paste0(trv$annee, " : ", fmt_p(trv$ecart), collapse = " ; "), "."),
      paste0("L'objectif est atteint ou dépassé ", sum(trv$TR >= 100), " exercice(s) sur ", nrow(trv),
             " ; ", sum(trv$TR < f$seuil), " exercice(s) sous le seuil d'alerte de ", f$seuil, " %."),
      paste0("Taux de réalisation moyen : ", fmt_n(tr_moy), " %."),
      if (!is.na(tr_n)) paste0("Exercice ", N, " : taux de réalisation de ", fmt_n(tr_n),
                                " %, écart de ", fmt_p(ec_n), " milliards de FCFA."),
      "La référence est la loi de finances initiale (LF) ou rectificative (LFR) ; « BG+CC » inclut le CAS COVID-19."),
    chiffres = list(tr_n = tr_n, ecart_n = ec_n, tr_moy = tr_moy, nb = nrow(trv),
                    nb_ok = sum(trv$TR >= 100), nb_alerte = sum(trv$TR < f$seuil))
  )

  # ====================================================================
  # SECTION 4 : CHOC CONJONCTUREL (année A comparée à A-1)
  # ====================================================================
  gam <- calc_ga_mensuel(mo, A)
  gat <- calc_ga_trim(mo, A)
  z2  <- an %>% filter(annee %in% c(A - 1, A, A + 1)) %>% arrange(annee)
  var_z  <- (z2$Z[z2$annee == A] / z2$Z[z2$annee == A - 1] - 1) * 100
  rebond <- if (any(z2$annee == A + 1)) (z2$Z[z2$annee == A + 1] / z2$Z[z2$annee == A] - 1) * 100 else NA_real_
  pire_t <- gat %>% filter(variable == "Z", !is.na(GA)) %>% slice_min(GA, n = 1, with_ties = FALSE)
  pire_m <- gam %>% filter(variable == "Z", !is.na(GA)) %>% slice_min(GA, n = 1, with_ties = FALSE)

  d_profil <- s %>% filter(annee %in% c(A - 1, A, A + 1)) %>%
    transmute(x = f_mois(mois), serie = as.character(annee), valeur = montant)
  coul_choc <- setNames(unname(COUL[c("gris", "rouge", "vert")]), c(A - 1, A, A + 1))
  lib_xyz <- c(X = "X (DGI)", Y = "Y (DGD)", Z = "Z, recettes fiscales")
  d_ga <- gam %>% transmute(x = f_mois(mois), serie = unname(lib_xyz[variable]), valeur = GA)

  annuel_choc <- an %>% filter(annee %in% c(A - 1, A)) %>% select(annee, all_of(VARS_CHOC)) %>%
    pivot_longer(-annee, names_to = "variable", values_to = "valeur") %>%
    group_by(variable) %>%
    summarise(annee_ga = (valeur[annee == A] / valeur[annee == A - 1] - 1) * 100)
  t_num <- gat %>% select(variable, trimestre, GA) %>%
    pivot_wider(names_from = trimestre, values_from = GA) %>%
    left_join(annuel_choc, by = "variable") %>%
    mutate(variable = factor(variable, VARS_CHOC)) %>% arrange(variable)
  t_choc <- t_num %>%
    transmute(Rubrique = unname(VAR_LIB[as.character(variable)]),
              across(starts_with("T"), ~ fmt_p(.x)), `Année` = fmt_p(annee_ga))
  ligne_ga <- function(v) {
    l <- t_num %>% filter(variable == v)
    paste0(VAR_LIB[[v]], " : ", paste0(c("T1", "T2", "T3", "T4"), " ",
           fmt_p(unlist(l[c("T1", "T2", "T3", "T4")])), collapse = ", "),
           " ; année ", fmt_p(l$annee_ga), " %")
  }

  choc <- list(
    titre = "Analyse d'un choc conjoncturel",
    sous_titre = paste0("Année analysée : ", A, ", comparée à ", A - 1),
    kpi = tibble(
      Indicateur = c(paste0("Variation des recettes fiscales Z en ", A, ", en %"),
                     "Trimestre le plus touché, glissement annuel de Z en %",
                     "Mois le plus touché, glissement annuel de Z en %",
                     paste0("Rebond de Z en ", A + 1, ", en %")),
      Valeur = c(fmt_p(var_z), paste0(pire_t$trimestre, " : ", fmt_p(pire_t$GA)),
                 paste0(MOIS_LONG[pire_m$mois], " : ", fmt_p(pire_m$GA)),
                 ifelse(is.na(rebond), "n.d.", fmt_p(rebond)))),
    graphiques = list(
      spec(paste0("Profil mensuel de la rubrique ", f$lib_rub, " : ", A - 1, ", ", A, " et ", A + 1), "lignes",
           d_profil, couleurs = coul_choc),
      spec(paste0("Glissements annuels mensuels de X, Y et Z en ", A, ", en %"), "barres", d_ga,
           ylab = "%", couleurs = setNames(unname(COUL[c("dgi", "dgd", "orange")]), lib_xyz))),
    tableaux = list(list(titre = paste0("Glissements annuels trimestriels et annuels en ", A, ", en %"),
                         data = t_choc)),
    faits = c(
      paste0("Année analysée : ", A, ", comparée à ", A - 1, "."),
      paste0("Variation annuelle des recettes fiscales Z en ", A, " : ", fmt_p(var_z), " %."),
      paste0("Trimestre le plus défavorable pour Z : ", pire_t$trimestre, " (", fmt_p(pire_t$GA), " %)."),
      paste0("Mois le plus défavorable pour Z : ", tolower(MOIS_LONG[pire_m$mois]), " (",
             fmt_p(pire_m$GA), " %)."),
      paste0("Glissements trimestriels en % : ", ligne_ga("Z"), "."),
      paste0("Glissements trimestriels en % : ", ligne_ga("X"), "."),
      paste0("Glissements trimestriels en % : ", ligne_ga("Y"), "."),
      paste0("Glissements trimestriels en % : ", ligne_ga("X1"), "."),
      paste0("Glissements trimestriels en % : ", ligne_ga("Y2"), "."),
      if (!is.na(rebond)) paste0("Variation de Z l'année suivante (", A + 1, ") : ", fmt_p(rebond), " %.")),
    chiffres = list(var_z = var_z, rebond = rebond, pire_t = pire_t, pire_m = pire_m, annee = A)
  )

  # --- Résultat : filtres + quatre sections ----------------------------
  list(filtres = f,
       sections = list(vue = vue, structure = structure_, budget = budget, choc = choc))
}

# Libellés des sections (utilisés par les cases à cocher et les exports)
SECTIONS_LIB <- c(vue = "Vue d'ensemble", structure = "Structure et contributions",
                  budget = "Performance budgétaire", choc = "Choc conjoncturel")

# « Signature » des filtres : sert à savoir si un commentaire IA déjà généré
# correspond encore aux données affichées (sinon il est périmé).
signature_filtres <- function(f, section) {
  cles <- switch(section,
                 vue       = c("annee", "mois", "rubrique", "seuil"),
                 structure = c("a0", "a1"),
                 budget    = c("annee", "rubrique", "seuil"),
                 choc      = c("annee_choc", "rubrique"))
  paste(section, paste(unlist(f[cles]), collapse = "|"), sep = "::")
}
