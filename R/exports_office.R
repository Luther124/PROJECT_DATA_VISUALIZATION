# =====================================================================
# exports_office.R : Exports Word (.docx) et PowerPoint (.pptx)
# ---------------------------------------------------------------------
# EXIGENCE : les graphiques doivent être des graphiques Microsoft NATIFS
# (« MS Chart »), c'est-à-dire modifiables dans Word / PowerPoint
# (clic droit > Modifier les données), et non des images.
#   - mschart  : fabrique les graphiques natifs (ms_barchart, ms_linechart)
#   - officer  : crée le document et y insère textes, logos et graphiques
#   - flextable: met en forme les tableaux
#
# Les graphiques ne sont pas « redessinés » ici : on convertit les
# spécifications produites par preparer_rapport() (R/donnees_rapport.R)
# avec spec_vers_mschart(). Le PDF utilise les mêmes spécifications
# (R/export_pdf.R), donc les trois formats sont cohérents.
#
# Police : Times New Roman partout (texte, tableaux, graphiques).
# =====================================================================

POLICE <- "Times New Roman"

# Informations de la page de garde (modifiables ici, en un seul endroit)
RAPPORT_INFOS <- list(
  titre      = paste("Analyse de la dynamique des recettes fiscales de 2018 à 2025 et conception",
                     "d’un outil automatisé de suivi conjoncturel à partir des données du TABORD"),
  sous_titre = "Projet de Data Visualisation - ISSEA",
  auteur     = "DJAMKENE SOUGNABE Luther",
  source     = "Sources : MINFI, TABORD (tableau M-7) ; calculs de l'auteur.",
  logo_minfi = "logo_MINFI.png",
  logo_issea = "logo_ISSEA.jpg"
)

# Texte d'introduction commun à Word, PowerPoint et PDF
texte_introduction <- function() c(
  paste("Ce document est produit par le tableau de bord de suivi des recettes fiscales, à partir",
        "des filtres choisis à l'écran. Les données viennent du tableau M-7 du TABORD, le Tableau de",
        "bord des finances publiques tenu par la Division de la Prévision du Ministère des Finances.",
        "Elles couvrent les exercices 2018 à 2025 et sont exprimées en milliards de FCFA."),
  paste("Les recettes fiscales, notées Z, additionnent deux composantes. La première, notée X,",
        "regroupe les impôts et taxes hors impôt sur les sociétés pétrolières, gérés par la Direction",
        "Générale des Impôts. La seconde, notée Y, correspond aux recettes des douanes, gérées par la",
        "Direction Générale des Douanes.")
)

texte_methode <- function() c(
  "Glissement annuel en % = (valeur N / valeur N-1 - 1) × 100, calculé sur un même mois, un même trimestre ou un même cumul.",
  "TCAM en % = [(valeur finale / valeur initiale)^(1/n) - 1] × 100, où n est le nombre d'années.",
  "Contribution à la croissance en points de % = (V t - V t-1) / Z t-1 × 100.",
  "Taux de réalisation proratisé en % = cumul recouvré / (objectif annuel × mois / 12) × 100.",
  "Taux de réalisation annuel en % = réalisation annuelle / prévision de la loi de finances × 100."
)

# Raccourci : propriétés de texte en Times New Roman
txt <- function(taille = 12, gras = FALSE, italique = FALSE, couleur = "black") {
  officer::fp_text(font.family = POLICE, font.size = taille, bold = gras,
                   italic = italique, color = couleur)
}

# Résumé des filtres actifs (rappelé dans chaque export)
resume_filtres <- function(f) {
  tibble::tibble(
    `Paramètre` = c("Exercice", "Cumul à fin de", "Rubrique analysée", "Seuil d'alerte du TR",
                    "Période de décomposition", "Année de choc analysée"),
    Valeur = c(as.character(f$annee), f$mois_lib, paste0(f$lib_rub, ", code ", f$rubrique),
               paste0(f$seuil, " %"), paste0(f$a0, " à ", f$a1), as.character(f$annee_choc))
  )
}

# ---------------------------------------------------------------------
# 1. Thème commun des graphiques natifs
# ---------------------------------------------------------------------
THEME_MS <- mschart::mschart_theme(
  main_title   = txt(13, gras = TRUE, couleur = "#0B3C5D"),
  axis_title   = txt(11, gras = TRUE),
  axis_text    = txt(10),
  legend_text  = txt(10),
  grid_major_line_x = officer::fp_border(width = 0),                 # pas de grille verticale
  grid_major_line_y = officer::fp_border(color = "#D9DEE5", width = 0.5),
  legend_position   = "b"                                            # légende en bas
)

# ---------------------------------------------------------------------
# 2. Conversion d'une spécification en graphique Microsoft natif
#    sp : list(titre, type, data(x, serie, valeur), ylab, couleurs, decimales)
# ---------------------------------------------------------------------
spec_vers_mschart <- function(sp, avec_titre = FALSE) {
  d <- as.data.frame(sp$data)
  series   <- intersect(names(sp$couleurs), unique(d$serie))   # séries réellement présentes
  d$serie  <- factor(d$serie, levels = series)
  couleurs <- sp$couleurs[series]
  format_nombre <- if (sp$decimales == 0) "#,##0" else paste0("#,##0.", strrep("0", sp$decimales))

  if (sp$type == "lignes") {
    # --- Courbes avec marqueurs --------------------------------------
    g <- mschart::ms_linechart(d, x = "x", y = "valeur", group = "serie")
    g <- mschart::chart_settings(g, style = "lineMarker")
    g <- mschart::chart_data_symbol(g, values = setNames(rep("circle", length(series)), series))
    g <- mschart::chart_data_size(g, values = setNames(rep(5, length(series)), series))
    g <- mschart::chart_data_line_width(g, values = setNames(rep(2.25, length(series)), series))
    g <- mschart::chart_data_smooth(g, values = setNames(rep(0, length(series)), series))   # segments droits
    # la série « Objectif proratisé » est tracée en pointillés
    styles <- setNames(ifelse(series == "Objectif proratisé", "dashed", "solid"), series)
    g <- mschart::chart_data_line_style(g, values = styles)
  } else {
    # --- Barres : groupées, empilées ou horizontales ------------------
    g <- mschart::ms_barchart(d, x = "x", y = "valeur", group = "serie")
    g <- switch(sp$type,
      barres          = mschart::chart_settings(g, grouping = "clustered", gap_width = 60, overlap = 0),
      barres_empilees = mschart::chart_settings(g, grouping = "stacked", gap_width = 60, overlap = 100),
      barres_h        = mschart::chart_settings(g, grouping = "stacked", gap_width = 40, overlap = 100,
                                                dir = "horizontal"))
    # étiquettes de valeurs quand le graphique reste lisible
    if (sp$type == "barres_h" || (sp$type == "barres" && length(series) == 1)) {
      g <- mschart::chart_data_labels(g, show_val = TRUE, num_fmt = format_nombre,
                                      position = if (sp$type == "barres_h") "ctr" else "outEnd")
      g <- mschart::chart_labels_text(g, values = txt(9, gras = sp$type == "barres_h",
                                                      couleur = if (sp$type == "barres_h") "white" else "black"))
    }
  }

  g <- mschart::chart_data_fill(g, values = couleurs)
  g <- mschart::chart_data_stroke(g, values = couleurs)
  g <- mschart::chart_labels(g, title = if (avec_titre) sp$titre else NULL,
                             xlab = NULL, ylab = sp$ylab)
  g <- if (is.null(sp$ymax)) mschart::chart_ax_y(g, num_fmt = format_nombre)
       else mschart::chart_ax_y(g, num_fmt = format_nombre, limit_min = 0, limit_max = sp$ymax)
  g <- mschart::chart_ax_x(g, tick_label_pos = "low")   # libellés en bas même si valeurs négatives
  mschart::set_theme(g, THEME_MS)
}

# ---------------------------------------------------------------------
# 3. Tableau mis en forme (flextable), commun à Word et PowerPoint
# ---------------------------------------------------------------------
tableau_ft <- function(d, taille = 10, largeur = 6.3) {
  ft <- flextable::flextable(as.data.frame(d, check.names = FALSE))
  ft <- flextable::font(ft, fontname = POLICE, part = "all")
  ft <- flextable::fontsize(ft, size = taille, part = "all")
  ft <- flextable::bold(ft, part = "header")
  ft <- flextable::bg(ft, bg = "#0B3C5D", part = "header")
  ft <- flextable::color(ft, color = "white", part = "header")
  ft <- flextable::border_remove(ft)
  ft <- flextable::hline(ft, border = officer::fp_border(color = "#CBD2D9", width = 0.5), part = "body")
  ft <- flextable::hline_bottom(ft, border = officer::fp_border(color = "#0B3C5D", width = 1.2), part = "body")
  ft <- flextable::align(ft, j = seq_len(ncol(d))[-1], align = "right", part = "all")
  ft <- flextable::padding(ft, padding.top = 2, padding.bottom = 2, part = "all")
  ft <- flextable::autofit(ft)
  flextable::fit_to_width(ft, max_width = largeur)
}

# Petite fonction : logo -> image officer avec une hauteur donnée (pouces)
logo_img <- function(chemin, hauteur = 1.1) {
  if (!file.exists(chemin)) return(NULL)
  officer::external_img(chemin, width = hauteur, height = hauteur)   # logos carrés
}

# Correction d'interligne du fichier Word produit.
# officer écrit l'interligne sans préciser sa nature ; certains logiciels
# (LibreOffice, visionneuses en ligne) le lisent alors comme une hauteur
# FIXE, ce qui écrase les grands titres et les logos. On précise donc
# « interligne proportionnel » (lineRule = auto) dans le XML du document.
corriger_interlignes <- function(fichier) {
  tryCatch({
    dossier <- tempfile("docx_")
    officer::unpack_folder(fichier, dossier)
    xml <- file.path(dossier, "word", "document.xml")
    contenu <- readLines(xml, encoding = "UTF-8", warn = FALSE)
    contenu <- gsub('(<w:spacing [^>]*w:line="[0-9]+")/>', '\\1 w:lineRule="auto"/>', contenu, useBytes = TRUE)
    writeLines(contenu, xml, useBytes = TRUE)
    officer::pack_folder(dossier, fichier)
    unlink(dossier, recursive = TRUE)
  }, error = function(e) NULL)   # en cas d'échec, le fichier d'origine est conservé
  invisible(fichier)
}

# =====================================================================
# 4. EXPORT WORD
#    fichier      : chemin du .docx à créer
#    r            : résultat de preparer_rapport()
#    commentaires : liste nommée par section (résultats de generer_commentaire())
#    sections     : codes des sections à inclure ("vue", "structure", ...)
# =====================================================================
exporter_word <- function(fichier, r, commentaires = list(),
                          sections = names(r$sections), infos = RAPPORT_INFOS) {
  f <- r$filtres
  centre  <- officer::fp_par(text.align = "center", padding.bottom = 6)
  justifie <- officer::fp_par(text.align = "justify", padding.bottom = 6, line_spacing = 1.15)

  # Paragraphe simple (Times New Roman 12, justifié)
  par_txt <- function(doc, texte, prop = txt(12), fp = justifie) {
    officer::body_add_fpar(doc, officer::fpar(officer::ftext(texte, prop), fp_p = fp))
  }
  # Titre : style « heading » de Word (pour le sommaire) + police Times New Roman
  titre_n <- function(doc, texte, niveau = 1) {
    officer::body_add_fpar(doc, officer::fpar(
      officer::ftext(texte, txt(if (niveau == 1) 16 else 13, gras = TRUE, couleur = "#0B3C5D"))),
      style = paste("heading", niveau))
  }
  # légende : reste sur la même page que le graphique ou le tableau qui suit
  centre_lie <- officer::fp_par(text.align = "center", padding.bottom = 6, padding.top = 8,
                                keep_with_next = TRUE)
  legende <- function(doc, texte) par_txt(doc, texte, txt(11, gras = TRUE), centre_lie)
  source_ <- function(doc) par_txt(doc, infos$source, txt(9, italique = TRUE), centre)

  doc <- officer::read_docx()

  # ---------------- Page de garde ------------------------------------
  logos <- Filter(Negate(is.null), list(logo_img(infos$logo_minfi), logo_img(infos$logo_issea)))
  if (length(logos) == 2) {
    doc <- officer::body_add_fpar(doc, officer::fpar(logos[[1]], officer::ftext("          ", txt(12)),
                                                     logos[[2]], fp_p = centre))
  } else if (length(logos) == 1) {
    doc <- officer::body_add_fpar(doc, officer::fpar(logos[[1]], fp_p = centre))
  }
  doc <- par_txt(doc, "Ministère des Finances - Division de la Prévision", txt(12, gras = TRUE), centre)
  doc <- par_txt(doc, "Institut Sous-régional de Statistique et d’Économie Appliquée", txt(12, gras = TRUE), centre)
  doc <- par_txt(doc, "", fp = centre)
  doc <- par_txt(doc, "", fp = centre)
  doc <- par_txt(doc, infos$titre, txt(20, gras = TRUE, couleur = "#0B3C5D"), centre)
  doc <- par_txt(doc, infos$sous_titre, txt(15, italique = TRUE), centre)
  doc <- par_txt(doc, "", fp = centre)
  doc <- par_txt(doc, paste0("Rédigé par : ", infos$auteur), txt(13, gras = TRUE), centre)
  doc <- par_txt(doc, paste0("Document généré le ", format(Sys.Date(), "%d/%m/%Y")), txt(12), centre)
  doc <- par_txt(doc, "", fp = centre)
  doc <- par_txt(doc, "Paramètres de l'édition", txt(12, gras = TRUE), centre)
  doc <- flextable::body_add_flextable(doc, tableau_ft(resume_filtres(f), largeur = 5), align = "center")
  doc <- officer::body_add_break(doc)

  # ---------------- Sommaire -----------------------------------------
  doc <- par_txt(doc, "SOMMAIRE", txt(16, gras = TRUE, couleur = "#0B3C5D"), centre)
  doc <- officer::body_add_toc(doc, level = 2)
  doc <- par_txt(doc, "Dans Word, faire un clic droit sur le sommaire puis choisir Mettre à jour les champs.",
                 txt(9, italique = TRUE), centre)
  doc <- officer::body_add_break(doc)

  # ---------------- Introduction -------------------------------------
  doc <- titre_n(doc, "Introduction")
  for (p in texte_introduction()) doc <- par_txt(doc, p)

  # ---------------- Sections d'analyse -------------------------------
  n_graph <- 0; n_tab <- 0
  for (code in intersect(names(r$sections), sections)) {
    sec <- r$sections[[code]]
    doc <- titre_n(doc, sec$titre)
    doc <- par_txt(doc, sec$sous_titre, txt(12, italique = TRUE))

    # Chiffres clés
    n_tab <- n_tab + 1
    doc <- legende(doc, paste0("Tableau ", n_tab, " : Chiffres clés de la section ", sec$titre))
    doc <- flextable::body_add_flextable(doc, tableau_ft(sec$kpi), align = "center")
    doc <- source_(doc)

    # Graphiques Microsoft natifs
    for (sp in Filter(Negate(is.null), sec$graphiques)) {
      n_graph <- n_graph + 1
      doc <- legende(doc, paste0("Graphique ", n_graph, " : ", sp$titre))
      doc <- mschart::body_add_chart(doc, chart = spec_vers_mschart(sp), style = "centered",
                                     width = 6.3, height = if (sp$type == "barres_h") 4.6 else 3.6)
      doc <- source_(doc)
    }

    # Tableaux détaillés
    for (tb in sec$tableaux) {
      n_tab <- n_tab + 1
      doc <- legende(doc, paste0("Tableau ", n_tab, " : ", tb$titre))
      doc <- flextable::body_add_flextable(doc, tableau_ft(tb$data, taille = 9), align = "center")
      doc <- source_(doc)
    }

    # Commentaire d'analyse
    com <- commentaires[[code]]
    if (!is.null(com)) {
      doc <- titre_n(doc, "Commentaire d'analyse", niveau = 2)
      doc <- par_txt(doc, com$texte)
      doc <- par_txt(doc, mention_source(com), txt(9, italique = TRUE, couleur = "#5F6B7A"))
    }
    doc <- officer::body_add_break(doc)
  }

  # ---------------- Note méthodologique ------------------------------
  doc <- titre_n(doc, "Note méthodologique")
  for (p in texte_methode()) doc <- par_txt(doc, p)

  print(doc, target = fichier)
  corriger_interlignes(fichier)
}

# =====================================================================
# 5. EXPORT POWERPOINT (mêmes arguments que exporter_word)
#    Modèle par défaut d'officer : diapositives 4:3 (10 x 7,5 pouces).
# =====================================================================
exporter_pptx <- function(fichier, r, commentaires = list(),
                          sections = names(r$sections), infos = RAPPORT_INFOS) {
  f <- r$filtres
  gauche <- officer::fp_par(text.align = "left", padding.bottom = 6)
  centre <- officer::fp_par(text.align = "center")

  # Emplacements (en pouces) sur la diapositive
  zone       <- function(l, t, w, h) officer::ph_location(left = l, top = t, width = w, height = h)
  ZONE_CORPS <- zone(0.5, 1.45, 9, 5.2)
  ZONE_PIED  <- zone(0.5, 6.85, 9, 0.4)

  # Nouvelle diapositive « titre seul » avec un titre en Times New Roman
  diapo <- function(ppt, titre) {
    ppt <- officer::add_slide(ppt, layout = "Title Only", master = "Office Theme")
    officer::ph_with(ppt, officer::fpar(officer::ftext(titre, txt(24, gras = TRUE, couleur = "#0B3C5D"))),
                     location = officer::ph_location_type("title"))
  }
  pied <- function(ppt, texte = infos$source) {
    officer::ph_with(ppt, officer::fpar(officer::ftext(texte, txt(10, italique = TRUE, couleur = "#5F6B7A")),
                                        fp_p = centre), location = ZONE_PIED)
  }
  bloc <- function(lignes, taille = 16, puce = "") {
    do.call(officer::block_list, lapply(lignes, function(l)
      officer::fpar(officer::ftext(paste0(puce, l), txt(taille)), fp_p = gauche)))
  }

  ppt <- officer::read_pptx()

  # ---------------- Diapositive de titre -----------------------------
  ppt <- officer::add_slide(ppt, layout = "Blank", master = "Office Theme")
  if (file.exists(infos$logo_minfi))
    ppt <- officer::ph_with(ppt, officer::external_img(infos$logo_minfi), location = zone(3.3, 0.4, 1.3, 1.3))
  if (file.exists(infos$logo_issea))
    ppt <- officer::ph_with(ppt, officer::external_img(infos$logo_issea), location = zone(5.4, 0.4, 1.3, 1.3))
  ppt <- officer::ph_with(ppt, officer::block_list(
    officer::fpar(officer::ftext(infos$titre, txt(26, gras = TRUE, couleur = "#0B3C5D")), fp_p = centre),
    officer::fpar(officer::ftext(" ", txt(10)), fp_p = centre),
    officer::fpar(officer::ftext(infos$sous_titre, txt(18, italique = TRUE)), fp_p = centre),
    officer::fpar(officer::ftext(" ", txt(10)), fp_p = centre),
    officer::fpar(officer::ftext(infos$auteur, txt(18, gras = TRUE)), fp_p = centre),
    officer::fpar(officer::ftext(format(Sys.Date(), "%d/%m/%Y"), txt(14)), fp_p = centre)),
    location = zone(0.6, 2.0, 8.8, 4.8))

  # ---------------- Objet, source et paramètres ----------------------
  ppt <- diapo(ppt, "Objet, source et paramètres de l'édition")
  ppt <- officer::ph_with(ppt, bloc(texte_introduction(), taille = 14), location = zone(0.5, 1.45, 9, 2.7))
  ppt <- officer::ph_with(ppt, tableau_ft(resume_filtres(f), taille = 12, largeur = 7),
                          location = zone(1.5, 4.3, 7, 2.3))

  # ---------------- Sections d'analyse -------------------------------
  for (code in intersect(names(r$sections), sections)) {
    sec <- r$sections[[code]]
    com <- commentaires[[code]]

    # a) Chiffres clés + commentaire d'analyse
    ppt <- diapo(ppt, sec$titre)
    ppt <- officer::ph_with(ppt, officer::fpar(officer::ftext(sec$sous_titre, txt(14, italique = TRUE))),
                            location = zone(0.5, 1.35, 9, 0.4))
    ppt <- officer::ph_with(ppt, tableau_ft(sec$kpi, taille = 12, largeur = 8), location = zone(1, 1.85, 8, 2.2))
    if (!is.null(com)) {
      ppt <- officer::ph_with(ppt, officer::block_list(
        officer::fpar(officer::ftext("Commentaire d'analyse", txt(14, gras = TRUE, couleur = "#0B3C5D")), fp_p = gauche),
        officer::fpar(officer::ftext(com$texte, txt(12)), fp_p = officer::fp_par(text.align = "justify"))),
        location = zone(0.5, 4.15, 9, 2.6))
      ppt <- pied(ppt, mention_source(com))
    }

    # b) Une diapositive par graphique Microsoft natif (modifiable)
    for (sp in Filter(Negate(is.null), sec$graphiques)) {
      ppt <- diapo(ppt, sp$titre)
      ppt <- officer::ph_with(ppt, spec_vers_mschart(sp), location = ZONE_CORPS)
      ppt <- pied(ppt)
    }

    # c) Une diapositive par tableau détaillé
    for (tb in sec$tableaux) {
      ppt <- diapo(ppt, tb$titre)
      long <- nrow(tb$data) > 12                       # tableau long : police réduite, sans pied
      ppt <- officer::ph_with(ppt, tableau_ft(tb$data, taille = if (long) 8 else 10, largeur = 9),
                              location = ZONE_CORPS)
      if (!long) ppt <- pied(ppt)
    }
  }

  # ---------------- Méthode + fin ------------------------------------
  ppt <- diapo(ppt, "Note méthodologique")
  ppt <- officer::ph_with(ppt, bloc(texte_methode(), taille = 14, puce = "• "), location = ZONE_CORPS)

  ppt <- officer::add_slide(ppt, layout = "Blank", master = "Office Theme")
  ppt <- officer::ph_with(ppt, officer::fpar(
    officer::ftext("Merci de votre attention", txt(30, gras = TRUE, couleur = "#0B3C5D")), fp_p = centre),
    location = zone(0.5, 3.1, 9, 1.2))

  print(ppt, target = fichier)
  invisible(fichier)
}
