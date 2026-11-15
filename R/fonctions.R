# =====================================================================
# fonctions.R : Chargement de la base TABORD harmonisée et calculs
# Utilisé par 01_tableaux_graphiques_rapport.R et par app.R
# =====================================================================
suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr)
})

MOIS_LIB <- c("Janv.", "Févr.", "Mars", "Avr.", "Mai", "Juin",
              "Juil.", "Août", "Sept.", "Oct.", "Nov.", "Déc.")

# Variables d'étude : code TABORD (feuille Referentiel) -> variable
VAR_CODES <- c(Z = "RF", X = "RF_X", X1 = "X_TVA", X2 = "X_ISNP", X3 = "X_ACC",
               X4 = "X_ITS", X5 = "X_TSPP", X6 = "X_DET",
               Y = "RF_Y", Y1 = "Y_DDI", Y2 = "Y_TVAI", Y3 = "Y_TI",
               Y4 = "Y_ACCI", Y5 = "Y_DS", Y6_src = "Y_AUT", ISP = "RP_ISP")

VAR_LIB <- c(Z = "Recettes fiscales (Z)",
             X = "Impôts et taxes hors IS pétrolier (X)",
             X1 = "TVA intérieure (X1)", X2 = "IS non pétrolier (X2)",
             X3 = "Droits d'accises (X3)",
             X4 = "Impôts sur traitements et salaires (X4)",
             X5 = "TSPP (X5)", X6 = "Enregistrement et timbre (X6)",
             X7 = "Autres impôts et taxes (X7)",
             Y = "Recettes des douanes (Y)",
             Y1 = "Droits de douane à l'importation (Y1)",
             Y2 = "TVA à l'importation (Y2)", Y3 = "Taxe informatique (Y3)",
             Y4 = "Droits d'accises à l'importation (Y4)",
             Y5 = "Droits de sortie (Y5)", Y6 = "Autres recettes douanières (Y6)")
SOUS_X <- paste0("X", 1:7)
SOUS_Y <- paste0("Y", 1:6)

# ---------------------------------------------------------------------
# 1. Lecture de la base Excel
# ---------------------------------------------------------------------
charger_base <- function(chemin = "data/Base_TABORD_harmonisee_2018_2025.xlsx") {
  ref  <- read_excel(chemin, sheet = "Referentiel") %>%
    filter(!is.na(code), !is.na(type)) %>%
    mutate(niveau = as.integer(niveau))
  real <- read_excel(chemin, sheet = "Realisations") %>%
    select(annee, mois, code, libelle_source, montant) %>%
    filter(!is.na(annee), !is.na(code), !is.na(montant)) %>%
    mutate(annee = as.integer(annee), mois = as.integer(mois),
           date = as.Date(sprintf("%d-%02d-01", annee, mois)))
  bud  <- read_excel(chemin, sheet = "References_budgetaires") %>%
    select(annee, code, type_reference, montant_prevu) %>%
    filter(!is.na(annee), !is.na(code)) %>%
    mutate(annee = as.integer(annee))
  tot  <- read_excel(chemin, sheet = "Totaux_publies", range = cell_cols("A:F")) %>%
    select(annee, code, total_publie) %>%
    filter(!is.na(annee), !is.na(code)) %>%
    mutate(annee = as.integer(annee))
  inconnus <- setdiff(unique(real$code), ref$code)
  if (length(inconnus) > 0)
    warning("Codes absents du Referentiel : ", paste(inconnus, collapse = ", "))
  list(ref = ref, real = real, bud = bud, tot = tot)
}

# ---------------------------------------------------------------------
# 2. Passage des codes TABORD aux variables d'étude (format large)
#    X7 et Y6 sont calculés par différence (additivité garantie)
# ---------------------------------------------------------------------
en_variables <- function(df, cles) {
  inv <- setNames(names(VAR_CODES), VAR_CODES)
  w <- df %>%
    filter(code %in% VAR_CODES) %>%
    mutate(var = inv[code]) %>%
    select(all_of(cles), var, valeur) %>%
    pivot_wider(names_from = var, values_from = valeur)
  for (v in setdiff(names(VAR_CODES), names(w))) w[[v]] <- NA_real_
  w %>%
    mutate(X7 = X - (X1 + X2 + X3 + X4 + X5 + X6),
           Y6 = Y - (Y1 + Y2 + Y3 + Y4 + Y5)) %>%
    select(-Y6_src) %>%
    arrange(across(all_of(cles)))
}

# Annuel : totaux publiés (comme dans le rapport) ; à défaut, somme des mois
annuel_variables <- function(b) {
  somme_mois <- b$real %>% group_by(annee, code) %>%
    summarise(valeur = sum(montant), n_mois = n(), .groups = "drop")
  pub <- b$tot %>% rename(valeur_pub = total_publie)
  df <- somme_mois %>% full_join(pub, by = c("annee", "code")) %>%
    mutate(valeur = coalesce(valeur_pub, valeur))
  en_variables(df, "annee")
}

mensuel_variables <- function(b) {
  df <- b$real %>% rename(valeur = montant)
  en_variables(df, c("annee", "mois"))
}

budget_variables <- function(b) {
  df <- b$bud %>% rename(valeur = montant_prevu)
  out <- en_variables(df, "annee")
  type <- b$bud %>% filter(code == "RF") %>% select(annee, type_reference)
  left_join(out, type, by = "annee")
}

# ---------------------------------------------------------------------
# 3. Indicateurs
# ---------------------------------------------------------------------
tcam <- function(v_fin, v_deb, n) (v_fin / v_deb)^(1 / n) - 1

# Décomposition : contribution (pts) = (V_t - V_{t-1}) / Z_{t-1} * 100
contributions <- function(an, vars = c(SOUS_X, "X", SOUS_Y, "Y", "Z")) {
  an <- arrange(an, annee)
  out <- lapply(vars, function(v) {
    tibble(annee = an$annee, variable = v,
           contribution = (an[[v]] - dplyr::lag(an[[v]])) / dplyr::lag(an$Z) * 100)
  })
  bind_rows(out) %>% filter(!is.na(contribution))
}

contribution_cumulee <- function(an, a0, a1, vars = c(SOUS_X, "X", SOUS_Y, "Y", "Z")) {
  d0 <- filter(an, annee == a0); d1 <- filter(an, annee == a1)
  tibble(variable = vars,
         contribution = sapply(vars, function(v) (d1[[v]] - d0[[v]]) / d0$Z * 100))
}

# Série mensuelle d'une rubrique quelconque (par son code, sans double compte)
serie_rubrique <- function(b, code_rub) {
  b$real %>% filter(code == code_rub) %>%
    group_by(annee, mois) %>% summarise(montant = sum(montant), .groups = "drop")
}

fmt_n <- function(x, d = 1) formatC(x, format = "f", digits = d, big.mark = " ", decimal.mark = ",")
fmt_p <- function(x, d = 1) ifelse(is.na(x), "n.d.",
  paste0(ifelse(x > 0, "+", ""), formatC(x, format = "f", digits = d, decimal.mark = ",")))
