# =====================================================================
# export_pdf.R : Export PDF du rapport
# ---------------------------------------------------------------------
# Approche « classique » : graphiques ggplot2 (images) + gabarit
# R Markdown  rapports/rapport_pdf.Rmd  compilé par rmarkdown::render().
#
# La compilation PDF passe par LaTeX :
#   - sur votre ordinateur : installer TinyTeX UNE fois
#         install.packages("tinytex") ; tinytex::install_tinytex()
#   - sur shinyapps.io : LaTeX est déjà présent sur le serveur.
# Si LaTeX est absent ou échoue (cas de certains serveurs d'hébergement),
# exporter_pdf() bascule automatiquement sur un PDF mis en page
# directement par R, sans LaTeX, avec le même contenu : le bouton de
# téléchargement fonctionne donc dans tous les cas.
#
# Suppose chargés : R/fonctions.R, R/donnees_rapport.R,
#                   R/commentaires_ia.R, R/exports_office.R
# =====================================================================

# ---------------------------------------------------------------------
# 1. Conversion d'une spécification de graphique en ggplot2
#    (même spécification que pour les graphiques Microsoft natifs)
# ---------------------------------------------------------------------
spec_vers_ggplot <- function(sp) {
  d <- sp$data
  series   <- intersect(names(sp$couleurs), unique(d$serie))
  d$serie  <- factor(d$serie, levels = series)
  couleurs <- sp$couleurs[series]
  fmt_axe  <- scales::label_number(accuracy = 10^(-sp$decimales), big.mark = " ", decimal.mark = ",")

  p <- ggplot2::ggplot(d, ggplot2::aes(x = x, y = valeur))

  if (sp$type == "lignes") {
    pointilles <- setNames(ifelse(series == "Objectif proratisé", "dashed", "solid"), series)
    p <- p +
      ggplot2::geom_line(ggplot2::aes(colour = serie, linetype = serie, group = serie), linewidth = 0.9) +
      ggplot2::geom_point(ggplot2::aes(colour = serie), size = 1.8) +
      ggplot2::scale_colour_manual(values = couleurs) +
      ggplot2::scale_linetype_manual(values = pointilles)
  } else {
    position <- if (sp$type == "barres") ggplot2::position_dodge(width = 0.8)
                else ggplot2::position_stack(reverse = TRUE)   # 1re série en bas, comme dans Office
    p <- p +
      ggplot2::geom_col(ggplot2::aes(fill = serie), position = position, width = 0.72) +
      ggplot2::scale_fill_manual(values = couleurs)
    # étiquettes de valeurs (mêmes cas que dans les graphiques natifs)
    if (sp$type == "barres_h" || (sp$type == "barres" && length(series) == 1)) {
      p <- p + ggplot2::geom_text(
        ggplot2::aes(label = formatC(valeur, format = "f", digits = sp$decimales, decimal.mark = ","),
                     hjust = if (sp$type == "barres_h") -0.15 else 0.5,
                     vjust = if (sp$type == "barres_h") 0.5 else ifelse(valeur >= 0, -0.5, 1.4)),
        size = 3, family = "serif")
    }
    if (sp$type == "barres_h") p <- p + ggplot2::coord_flip()
  }

  # borne haute imposée (ex. 100 %) : on « zoome » sans supprimer de données
  if (!is.null(sp$ymax) && sp$type != "barres_h") {
    p <- p + ggplot2::coord_cartesian(ylim = c(0, sp$ymax), expand = FALSE)
  }
  p +
    ggplot2::scale_y_continuous(labels = fmt_axe,
                                expand = ggplot2::expansion(mult = c(0.04, 0.10))) +
    ggplot2::labs(x = NULL, y = sp$ylab, colour = NULL, fill = NULL, linetype = NULL) +
    ggplot2::theme_minimal(base_family = "serif", base_size = 11) +   # « serif » = police de type Times
    ggplot2::theme(legend.position = "bottom",
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_blank(),
                   axis.title = ggplot2::element_text(face = "bold"))
}

# Hauteur (pouces) d'un graphique selon son type
hauteur_graphique <- function(sp) if (sp$type == "barres_h") 4.4 else 3.3

# ---------------------------------------------------------------------
# 2. PDF SANS LaTeX : rapport complet mis en page directement par R
#    (package grid). Utilisé automatiquement quand LaTeX est absent, ce
#    qui est le cas sur certains serveurs d'hébergement.
#    Contenu identique au gabarit LaTeX : page de garde avec logos,
#    chiffres clés, graphiques, tableaux et commentaires.
#    Principe : on écrit de haut en bas ; « y » est la position courante
#    (en pouces depuis le haut de la page A4) et une nouvelle page est
#    ouverte dès que l'élément suivant ne tient plus.
# ---------------------------------------------------------------------
exporter_pdf_secours <- function(fichier, r, commentaires = list(),
                                 sections = names(r$sections), infos = RAPPORT_INFOS) {
  HAUT <- 11.69; LARG <- 8.27; MARGE <- 1; UTILE <- LARG - 2 * MARGE   # format A4, en pouces

  # cairo_pdf gère correctement les accents ; à défaut, pdf() classique
  if (isTRUE(capabilities("cairo"))) grDevices::cairo_pdf(fichier, width = LARG, height = HAUT, onefile = TRUE, family = "serif")
  else grDevices::pdf(fichier, width = LARG, height = HAUT, family = "Times")
  on.exit(grDevices::dev.off(), add = TRUE)

  etat <- new.env(); etat$y <- MARGE; etat$page <- 0; etat$graph <- 0; etat$tab <- 0
  pouce <- function(v) grid::unit(v, "in")

  # --- Briques de mise en page ---------------------------------------
  nouvelle_page <- function() {
    grid::grid.newpage(); etat$page <- etat$page + 1; etat$y <- MARGE
    if (etat$page > 1) grid::grid.text(etat$page, x = 0.5, y = pouce(0.5),
                                       gp = grid::gpar(fontsize = 10, fontfamily = "serif"))
  }
  besoin  <- function(h) if (etat$y + h > HAUT - MARGE) nouvelle_page()   # saut de page si nécessaire
  espace  <- function(h) etat$y <- etat$y + h

  # Paragraphe : découpé en lignes, écrit ligne par ligne
  texte <- function(x, taille = 11, style = "plain", couleur = "black", centre = FALSE, apres = 0.10) {
    for (paragraphe in x) {
      lignes <- strwrap(paragraphe, width = floor(UTILE * 72 / (taille * 0.455)))
      if (length(lignes) == 0) lignes <- ""
      for (l in lignes) {
        h <- taille * 1.32 / 72
        besoin(h)
        grid::grid.text(l, x = if (centre) 0.5 else pouce(MARGE), y = pouce(HAUT - etat$y - h * 0.75),
                        just = if (centre) "centre" else "left",
                        gp = grid::gpar(fontsize = taille, fontface = style, col = couleur, fontfamily = "serif"))
        espace(h)
      }
      espace(apres)
    }
  }

  # Tableau : en-tête bleu, lignes séparées par un filet, coupé entre deux pages si besoin
  tableau <- function(d, titre, taille = 9) {
    d <- as.data.frame(lapply(d, as.character), check.names = FALSE, stringsAsFactors = FALSE)
    etat$tab <- etat$tab + 1
    police <- grid::gpar(fontsize = taille, fontfamily = "serif")
    largeur_txt <- function(v) max(grid::convertWidth(grid::stringWidth(v), "in", valueOnly = TRUE))
    grid::pushViewport(grid::viewport(gp = police))
    largeurs <- sapply(seq_along(d), function(j) largeur_txt(c(names(d)[j], d[[j]]))) + 0.22
    grid::popViewport()
    if (sum(largeurs) > UTILE) largeurs <- largeurs * UTILE / sum(largeurs)
    gauche <- MARGE + (UTILE - sum(largeurs)) / 2          # tableau centré
    x0 <- gauche + c(0, cumsum(largeurs)[-length(largeurs)])
    hl <- 0.25                                             # hauteur d'une ligne

    ligne <- function(valeurs, entete = FALSE) {
      if (entete) grid::grid.rect(x = pouce(gauche), y = pouce(HAUT - etat$y), width = pouce(sum(largeurs)),
                                  height = pouce(hl), just = c("left", "top"),
                                  gp = grid::gpar(fill = "#0B3C5D", col = NA))
      for (j in seq_along(valeurs)) {
        droite <- j > 1                                    # 1re colonne à gauche, les autres à droite
        grid::grid.text(valeurs[j], x = pouce(if (droite) x0[j] + largeurs[j] - 0.09 else x0[j] + 0.09),
                        y = pouce(HAUT - etat$y - hl / 2), just = if (droite) "right" else "left",
                        gp = grid::gpar(fontsize = taille, fontfamily = "serif",
                                        fontface = if (entete) "bold" else "plain",
                                        col = if (entete) "white" else "black"))
      }
      espace(hl)
      if (!entete) grid::grid.lines(x = pouce(c(gauche, gauche + sum(largeurs))), y = pouce(HAUT - etat$y),
                                    gp = grid::gpar(col = "#CBD2D9", lwd = 0.5))
    }
    besoin(0.35 + hl * min(nrow(d) + 1, 5))                # la légende n'est pas séparée du tableau
    texte(paste0("Tableau ", etat$tab, " : ", titre), 10.5, "bold", centre = TRUE, apres = 0.05)
    ligne(names(d), entete = TRUE)
    for (i in seq_len(nrow(d))) {
      if (etat$y + hl > HAUT - MARGE) { nouvelle_page(); ligne(names(d), entete = TRUE) }
      ligne(unlist(d[i, ]))
    }
    espace(0.05)
    texte(infos$source, 8.5, "italic", centre = TRUE, apres = 0.18)
  }

  # Graphique ggplot2 placé à la position courante
  graphique <- function(sp) {
    h <- hauteur_graphique(sp)
    besoin(h + 0.75)
    etat$graph <- etat$graph + 1
    texte(paste0("Graphique ", etat$graph, " : ", sp$titre), 10.5, "bold", centre = TRUE, apres = 0.05)
    print(spec_vers_ggplot(sp),
          vp = grid::viewport(x = pouce(MARGE), y = pouce(HAUT - etat$y), width = pouce(UTILE),
                              height = pouce(h), just = c("left", "top")))
    espace(h + 0.03)
    texte(infos$source, 8.5, "italic", centre = TRUE, apres = 0.18)
  }

  # Logos côte à côte (packages png et jpeg ; ignorés s'ils sont absents)
  lire_image <- function(chemin) {
    if (!file.exists(chemin)) return(NULL)
    tryCatch(if (grepl("\\.png$", chemin, ignore.case = TRUE)) png::readPNG(chemin) else jpeg::readJPEG(chemin),
             error = function(e) NULL)
  }
  logos <- function(taille = 1.25) {
    images <- Filter(Negate(is.null), list(lire_image(infos$logo_minfi), lire_image(infos$logo_issea)))
    if (length(images) == 0) return(invisible())
    ecart <- 0.6; total <- length(images) * taille + (length(images) - 1) * ecart
    for (k in seq_along(images)) {
      grid::grid.raster(images[[k]], x = pouce((LARG - total) / 2 + (k - 1) * (taille + ecart)),
                        y = pouce(HAUT - etat$y), width = pouce(taille), height = pouce(taille),
                        just = c("left", "top"))
    }
    espace(taille + 0.2)
  }

  # --- Page de garde ---------------------------------------------------
  f <- r$filtres
  nouvelle_page()
  logos()
  texte("Ministère des Finances - Division de la Prévision", 12, "bold", centre = TRUE, apres = 0.02)
  texte("Institut Sous-régional de Statistique et d’Économie Appliquée", 12, "bold", centre = TRUE)
  espace(1.0)
  texte(infos$titre, 19, "bold", "#0B3C5D", centre = TRUE, apres = 0.15)
  texte(infos$sous_titre, 14, "italic", centre = TRUE)
  espace(0.7)
  texte(paste0("Rédigé par : ", infos$auteur), 13, "bold", centre = TRUE, apres = 0.05)
  texte(paste0("Document généré le ", format(Sys.Date(), "%d/%m/%Y")), 12, centre = TRUE)
  espace(0.6)
  tableau(resume_filtres(f), "Paramètres de l'édition", taille = 10)

  # --- Introduction ----------------------------------------------------
  nouvelle_page()
  texte("Introduction", 16, "bold", "#0B3C5D", apres = 0.15)
  texte(texte_introduction(), 11.5)

  # --- Sections d'analyse ----------------------------------------------
  for (code in intersect(names(r$sections), sections)) {
    sec <- r$sections[[code]]; com <- commentaires[[code]]
    nouvelle_page()
    texte(sec$titre, 16, "bold", "#0B3C5D", apres = 0.05)
    texte(sec$sous_titre, 11, "italic", apres = 0.2)
    tableau(sec$kpi, paste0("Chiffres clés de la section ", sec$titre), taille = 10)
    for (sp in Filter(Negate(is.null), sec$graphiques)) graphique(sp)
    for (tb in sec$tableaux) tableau(tb$data, tb$titre)
    if (!is.null(com)) {
      besoin(1.2)
      texte("Commentaire d'analyse", 13, "bold", "#0B3C5D", apres = 0.08)
      texte(com$texte, 11.5)
      texte(mention_source(com), 9, "italic", "#5F6B7A")
    }
  }

  # --- Note méthodologique ---------------------------------------------
  nouvelle_page()
  texte("Note méthodologique", 16, "bold", "#0B3C5D", apres = 0.15)
  texte(paste0("• ", texte_methode()), 11)
  invisible(fichier)
}

# ---------------------------------------------------------------------
# 3. Export PDF principal (mêmes arguments que exporter_word)
#    Renvoie "latex" ou "secours" selon la méthode réellement utilisée.
# ---------------------------------------------------------------------
exporter_pdf <- function(fichier, r, commentaires = list(), sections = names(r$sections),
                         infos = RAPPORT_INFOS, gabarit = "rapports/rapport_pdf.Rmd") {

  # Tout se passe dans un dossier temporaire : sur un serveur, le dossier
  # de l'application n'est pas forcément accessible en écriture.
  dossier <- tempfile("pdf_"); dir.create(dossier)
  on.exit(unlink(dossier, recursive = TRUE), add = TRUE)
  file.copy(gabarit, file.path(dossier, "rapport_pdf.Rmd"))
  logos <- c(infos$logo_minfi, infos$logo_issea)
  file.copy(logos[file.exists(logos)], dossier)

  # Les objets sont transmis au gabarit par un environnement dédié :
  # le gabarit y trouve r, commentaires, sections, infos, ainsi que toutes
  # les fonctions du projet (environnement parent).
  env <- new.env(parent = environment(exporter_pdf))
  env$r <- r; env$commentaires <- commentaires; env$sections <- sections; env$infos <- infos

  essai <- tryCatch(
    rmarkdown::render(file.path(dossier, "rapport_pdf.Rmd"), output_file = "rapport.pdf",
                      output_dir = dossier, intermediates_dir = dossier, envir = env, quiet = TRUE),
    error = function(e) e)

  if (inherits(essai, "error") || !file.exists(file.path(dossier, "rapport.pdf"))) {
    message("Compilation LaTeX impossible : PDF de secours. Détail : ",
            if (inherits(essai, "error")) conditionMessage(essai) else "fichier absent")
    exporter_pdf_secours(fichier, r, commentaires, sections, infos)
    return(invisible("secours"))
  }
  file.copy(file.path(dossier, "rapport.pdf"), fichier, overwrite = TRUE)
  invisible("latex")
}
