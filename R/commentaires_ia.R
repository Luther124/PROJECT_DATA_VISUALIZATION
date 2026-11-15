# =====================================================================
# commentaires_ia.R : Commentaires d'analyse rédigés par une IA (Gemini)
# ---------------------------------------------------------------------
# PRINCIPE
#   1. preparer_rapport() (R/donnees_rapport.R) fournit, pour chaque
#      section, une liste de « faits » : des phrases courtes contenant les
#      chiffres réellement affichés à l'écran pour les filtres choisis.
#   2. Ces faits sont envoyés à Gemini (package ellmer) avec une consigne
#      stricte : commenter UNIQUEMENT ces chiffres, sans rien inventer.
#   3. Si la clé API est absente, si le quota est dépassé ou si le réseau
#      est coupé, un commentaire « de secours » est rédigé par des règles
#      simples à partir des mêmes chiffres : le dashboard ne plante jamais.
#
# CONFIGURER LA CLÉ API (une seule fois)
#   a) Créer une clé gratuite sur https://aistudio.google.com/apikey
#   b) Créer, à la racine du projet (à côté de app.Rmd), un fichier texte
#      nommé  .Renviron  contenant UNE ligne (sans guillemets, sans espace) :
#         GEMINI_API_KEY=votre_cle_ici
#   c) Redémarrer R (Session > Restart R), puis relancer le dashboard.
#   Le fichier .Renviron est lu au démarrage par app.Rmd ; il est aussi
#   envoyé sur shinyapps.io lors du déploiement (voir deployer.R).
#   NE JAMAIS écrire la clé dans le code ni la publier (GitHub, e-mail...).
#
# CHANGER DE MODÈLE (facultatif)
#   Ajouter dans .Renviron :  GEMINI_MODEL=nom_du_modele
#   Sans cette ligne, ellmer choisit lui-même le modèle Gemini par défaut.
#   Liste des modèles disponibles : ellmer::models_google_gemini()
#
# CHANGER DE FOURNISSEUR : une seule ligne à modifier dans appeler_ia()
#   (ellmer::chat_anthropic() ou ellmer::chat_openai(), avec la clé
#   ANTHROPIC_API_KEY ou OPENAI_API_KEY dans .Renviron).
# =====================================================================

# ---------------------------------------------------------------------
# 1. L'IA est-elle utilisable ? (package installé + clé présente)
# ---------------------------------------------------------------------
cle_ia <- function() {
  cle <- Sys.getenv("GEMINI_API_KEY", unset = "")
  if (cle == "") cle <- Sys.getenv("GOOGLE_API_KEY", unset = "")   # autre nom accepté par ellmer
  if (cle == "") {
    # Si R n'a pas chargé le fichier .Renviron, on le lit directement.
    # readLines() accepte un fichier sans saut de ligne final.
    for (f in c(".Renviron", file.path(getwd(), ".Renviron"))) {
      if (!file.exists(f)) next
      lignes <- trimws(readLines(f, warn = FALSE, encoding = "UTF-8"))
      lignes <- sub("^\ufeff", "", lignes)                         # marque d'encodage éventuelle
      for (l in lignes[grepl("^[A-Z_]+\\s*=", lignes)]) {
        nom <- trimws(sub("=.*$", "", l))
        val <- gsub('^["\']|["\']$', "", trimws(sub("^[^=]*=", "", l)))
        if (nzchar(val)) do.call(Sys.setenv, stats::setNames(list(val), nom))
      }
      cle <- Sys.getenv("GEMINI_API_KEY", unset = "")
      if (cle != "") break
    }
  }
  cle
}
ia_disponible <- function() {
  requireNamespace("ellmer", quietly = TRUE) && nzchar(cle_ia())
}

# ---------------------------------------------------------------------
# 2. Consigne générale donnée à l'IA (« prompt système »)
# ---------------------------------------------------------------------
CONSIGNE_IA <- paste(
  "Tu es un analyste statisticien de la Division de la Prévision du Ministère des Finances",
  "du Cameroun. Tu rédiges le commentaire d'un tableau de bord de suivi des recettes fiscales",
  "(données du TABORD, tableau M-7, montants en milliards de FCFA).",
  "Vocabulaire : Z = recettes fiscales hors impôt sur les sociétés pétrolières ; X = impôts et",
  "taxes gérés par la DGI ; Y = recettes douanières gérées par la DGD ; TR = taux de réalisation",
  "(réalisation / prévision de la loi de finances) ; glissement annuel = variation par rapport",
  "à la même période de l'année précédente.",
  "Règles impératives :",
  "1) Utilise UNIQUEMENT les chiffres fournis dans le message ; n'invente aucun chiffre, aucune",
  "date et aucune cause qui ne figure pas dans les faits fournis.",
  "2) Tu peux qualifier une évolution (hausse, repli, accélération, moins-value...) mais, si tu",
  "évoques une explication économique, présente-la comme une hypothèse à vérifier.",
  "3) Rédige en français un seul paragraphe de 90 à 130 mots, sans titre, sans liste à puces",
  "et sans mise en forme Markdown.",
  "4) Écris les nombres au format français, avec la virgule décimale, et termine par une phrase",
  "simple qui indique le point à surveiller.",
  "5) Style : écris comme un étudiant statisticien qui rédige son rapport de stage. Fais des",
  "phrases courtes et directes, avec des mots simples. N'utilise jamais de tiret long, de",
  "parenthèses ni de point-virgule. Pour un chiffre négatif, écris un recul de 7,5 % plutôt que",
  "-7,5 %. Évite les tournures toutes faites comme il convient de noter, force est de constater,",
  "en somme, par ailleurs, il est important de souligner, ainsi que les adjectifs emphatiques",
  "comme remarquable, crucial ou significatif. Ne commence pas par une phrase d'annonce et ne",
  "termine pas par un résumé de ce qui vient d'être dit."
)

# Question posée pour chaque section
QUESTION_IA <- c(
  vue       = "Commente le niveau de recouvrement de l'exercice, son rythme par rapport à l'objectif et à l'année précédente.",
  structure = "Commente la croissance des recettes fiscales sur la période, le rôle respectif de la DGI et de la DGD et les rubriques motrices.",
  budget    = "Commente la performance budgétaire de la rubrique : exercices au-dessus et en dessous de l'objectif, ampleur des écarts.",
  choc      = "Commente le choc de l'année analysée : ampleur, calendrier infra-annuel, composantes les plus touchées et rebond éventuel."
)

# ---------------------------------------------------------------------
# 3. Appel de l'API Gemini avec ellmer
#    Renvoie le texte rédigé ; déclenche une erreur en cas de problème
#    (elle est interceptée par generer_commentaire() ci-dessous).
#    Si le service est saturé (erreur 503 « high demand »), l'appel est
#    retenté, puis d'autres modèles Gemini sont essayés.
# ---------------------------------------------------------------------
# Une seule tentative, avec un modèle donné ("" = modèle par défaut d'ellmer)
interroger_modele <- function(modele, message_utilisateur) {
  # La clé est lue automatiquement dans GEMINI_API_KEY (ou GOOGLE_API_KEY).
  # Les messages d'information d'ellmer (ex. modèle choisi) sont masqués.
  chat <- suppressMessages(ellmer::chat_google_gemini(   # <- ligne à changer pour un autre fournisseur
    system_prompt = CONSIGNE_IA,
    model         = if (nzchar(modele)) modele else NULL,
    echo          = "none"                    # ne rien afficher dans la console
  ))
  texte <- chat$chat(message_utilisateur, echo = "none")
  texte <- trimws(gsub("[*#`]", "", as.character(texte)))   # retire un éventuel Markdown
  texte <- gsub("\\s*[\u2014\u2013]\\s*", ", ", texte)         # tirets longs éventuels -> virgule
  texte <- gsub("\u2212", "-", texte)                       # signe moins typographique -> tiret simple
  if (!nzchar(texte)) stop("Réponse vide de l'API.")
  texte
}

# Modèles de remplacement, essayés dans cet ordre si le modèle principal est
# saturé. Ce sont des modèles Gemini stables de l'offre gratuite (liste de
# novembre 2026) ; les modèles « lite » sont moins sollicités, donc plus
# souvent disponibles. Pour changer la liste sans toucher au code, écrire
# dans .Renviron :  GEMINI_MODELES_SECOURS=modele_a,modele_b
MODELES_SECOURS <- c("gemini-3.5-flash-lite", "gemini-3.6-flash", "gemini-3.1-flash-lite")

modeles_secours <- function(modele_principal) {
  manuel <- Sys.getenv("GEMINI_MODELES_SECOURS", unset = "")
  liste  <- if (nzchar(manuel)) trimws(strsplit(manuel, ",")[[1]]) else MODELES_SECOURS
  setdiff(liste, modele_principal)
}

# Erreur passagère du service ? (503 = saturé, 429 = quota par minute, 500/504)
erreur_passagere <- function(e) {
  grepl("503|429|500|504|overloaded|high demand|unavailable|timeout|timed out",
        conditionMessage(e), ignore.case = TRUE)
}

appeler_ia <- function(section, faits) {
  message_utilisateur <- paste0(
    QUESTION_IA[[section]], "\n\nFaits chiffrés (seule source autorisée) :\n",
    paste0("- ", faits, collapse = "\n")
  )
  principal <- Sys.getenv("GEMINI_MODEL", unset = "")

  # 1) Modèle principal : jusqu'à 3 tentatives, espacées de 2 puis 5 secondes
  #    (les pics de demande de Gemini durent en général quelques secondes).
  erreur <- NULL
  for (attente in c(0, 2, 5)) {
    Sys.sleep(attente)
    essai <- tryCatch(interroger_modele(principal, message_utilisateur), error = function(e) e)
    if (!inherits(essai, "error")) return(essai)
    erreur <- essai
    if (!erreur_passagere(essai)) stop(essai)      # clé invalide, etc. : inutile d'insister
  }

  # 2) Modèles de remplacement, une tentative chacun
  essayes <- modeles_secours(principal)
  for (modele in essayes) {
    essai <- tryCatch(interroger_modele(modele, message_utilisateur), error = function(e) e)
    if (!inherits(essai, "error")) return(essai)
  }
  # Tout a échoué : generer_commentaire() basculera sur les règles. Le message
  # indique les modèles de remplacement essayés, pour faciliter le diagnostic.
  stop(paste0("modèles de remplacement essayés sans succès : ", paste(essayes, collapse = ", "),
              ". ", conditionMessage(erreur)))
}

# ---------------------------------------------------------------------
# 4. Commentaire de secours, rédigé par règles (sans IA)
#    Utilise les chiffres de r$sections[[section]]$chiffres.
# ---------------------------------------------------------------------
commentaire_regles <- function(section, sec, f) {
  k <- sec$chiffres
  hausse_ou_baisse <- function(x, plus, moins) if (x >= 0) plus else moins
  sans_code <- function(x) sub(" \\([A-Z][0-9]?\\)$", "", x)        # « TVA intérieure (X1) » -> « TVA intérieure »

  switch(section,

    vue = {
      p1 <- paste0("À fin ", tolower(f$mois_lib), " ", f$annee, ", les recouvrements de la rubrique ", f$lib_rub,
                   " atteignent ", fmt_n(k$cumul), " milliards de FCFA.")
      if (!is.na(k$ga)) p1 <- paste0(p1, " C'est ", fmt_n(abs(k$ga)), " % de ",
                                     hausse_ou_baisse(k$ga, "plus", "moins"), " qu'à la même période de ",
                                     f$annee - 1, ".")
      p2 <- if (is.na(k$tr)) "Aucun objectif budgétaire n'est disponible pour cette rubrique." else
        paste0("L'objectif annuel fixé par la ", sub(" \\(.*\\)$", "", k$type), " est de ", fmt_n(k$objectif), " milliards. Le cumul en ",
               "représente ", fmt_n(k$part_obj), " % et le taux de réalisation, calculé sur l'objectif proratisé, est de ",
               fmt_n(k$tr), " %. ",
               if (k$tr >= 100) "Le rythme de recouvrement suit donc la trajectoire attendue."
               else if (k$tr >= f$seuil) "Le rythme de recouvrement est un peu en dessous de la trajectoire attendue et doit être surveillé."
               else paste0("Ce taux est inférieur au seuil d'alerte de ", f$seuil,
                           " %. Une moins-value en fin d'exercice est à craindre."))
      paste(p1, p2)
    },

    structure = paste0(
      "Entre ", k$a0, " et ", k$a1, ", les recettes fiscales ",
      hausse_ou_baisse(k$hausse, "augmentent", "diminuent"), " de ", fmt_n(abs(k$hausse)), " %, soit ",
      fmt_n(abs(k$tcam)), " % par an en moyenne. La DGI explique ", fmt_n(k$part_x),
      " % de cette évolution, alors qu'elle représente en moyenne ", fmt_n(k$poids_x),
      " % des recettes. Les trois rubriques qui contribuent le plus sont les suivantes : ",
      paste0(sans_code(k$top$libelle), " avec ", fmt_n(k$top$contribution, 2), " points", collapse = ", "),
      ". La croissance repose donc sur un petit nombre de rubriques, qu'il faut suivre en priorité."),

    budget = if (k$nb == 0) {
      "Aucune prévision budgétaire n'est disponible pour cette rubrique. Sa performance budgétaire ne peut donc pas être évaluée."
    } else paste0(
      "Sur les ", k$nb, " exercices pour lesquels une prévision existe, la rubrique ", f$lib_rub,
      " atteint ou dépasse son objectif ", k$nb_ok, " fois. Le taux de réalisation moyen est de ",
      fmt_n(k$tr_moy), " %. ",
      if (k$nb_alerte == 0) paste0("Aucun exercice ne passe sous le seuil d'alerte de ", f$seuil, " %. ")
      else if (k$nb_alerte == 1) paste0("Un exercice passe sous le seuil d'alerte de ", f$seuil, " %. ")
      else paste0(k$nb_alerte, " exercices passent sous le seuil d'alerte de ", f$seuil, " %. "),
      if (is.na(k$tr_n)) "" else paste0("En ", f$annee, ", le taux de réalisation est de ", fmt_n(k$tr_n),
        " %, ce qui correspond à une ", hausse_ou_baisse(k$ecart_n, "plus-value", "moins-value"), " de ",
        fmt_n(abs(k$ecart_n)), " milliards de FCFA. "),
      "Ces écarts dépendent aussi de la loi de finances retenue comme référence, initiale ou rectificative."),

    choc = {
      trimestres <- c(T1 = "premier", T2 = "deuxième", T3 = "troisième", T4 = "quatrième")
      paste0(
        "En ", k$annee, ", les recettes fiscales ", hausse_ou_baisse(k$var_z, "augmentent", "reculent"), " de ",
        fmt_n(abs(k$var_z)), " % par rapport à ", k$annee - 1, ". Le ", trimestres[[k$pire_t$trimestre]],
        " trimestre est le moins favorable, avec un glissement annuel de ", fmt_p(k$pire_t$GA),
        " %. Le mois le plus difficile est ", tolower(MOIS_LONG[k$pire_m$mois]), ", avec ", fmt_p(k$pire_m$GA), " %. ",
        if (is.na(k$rebond)) "" else paste0("En ", k$annee + 1, ", les recettes ",
          if (k$rebond >= 0 && k$var_z < 0) "repartent à la hausse de "
          else if (k$rebond >= 0) "augmentent de "
          else if (k$var_z < 0) "reculent encore de " else "reculent de ", fmt_n(abs(k$rebond)), " %. "),
        "Le tableau des glissements trimestriels montre par quelles rubriques cette variation est passée.")
    }
  )
}

# ---------------------------------------------------------------------
# 5. Fonction principale appelée par le dashboard et par les exports
#    Renvoie une liste : texte, source ("IA" ou "regles"), note (message
#    d'information éventuel), signature (filtres correspondants), heure.
# ---------------------------------------------------------------------
generer_commentaire <- function(section, r, utiliser_ia = TRUE) {
  sec <- r$sections[[section]]
  f   <- r$filtres
  resultat <- list(texte = NULL, source = "regles", note = NULL,
                   signature = signature_filtres(f, section), heure = Sys.time())

  if (utiliser_ia && ia_disponible()) {
    # tryCatch : toute erreur (réseau, quota, clé invalide) bascule sur les règles
    essai <- tryCatch(appeler_ia(section, sec$faits), error = function(e) e)
    if (inherits(essai, "error")) {
      resultat$note <- paste0("L'appel à l'IA n'a pas abouti, le commentaire a donc été rédigé par règles. Détail : ",
                              substr(conditionMessage(essai), 1, 260))
    } else {
      resultat$texte  <- essai
      resultat$source <- "IA"
    }
  } else if (utiliser_ia) {
    resultat$note <- if (!requireNamespace("ellmer", quietly = TRUE)) {
      "Le package ellmer n'est pas disponible. Le commentaire a été rédigé par règles."
    } else {
      paste0("La clé GEMINI_API_KEY est introuvable : le fichier .Renviron est absent du dossier ",
             normalizePath(getwd()), ". Le commentaire a été rédigé par règles.")
    }
  }

  if (is.null(resultat$texte)) resultat$texte <- commentaire_regles(section, sec, f)
  resultat
}

# Mention de source affichée sous chaque commentaire (écran et exports)
mention_source <- function(com) {
  if (identical(com$source, "IA")) "Commentaire rédigé par l'IA Gemini à partir des chiffres affichés, à relire avant diffusion."
  else "Commentaire automatique rédigé par règles de calcul, sans IA."
}
