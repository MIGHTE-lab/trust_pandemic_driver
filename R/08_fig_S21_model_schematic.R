# =============================================================================
# 08_fig_S21_model_schematic.R
# Fig. S21: proposed trust-stratified, behavior-coupled SEIR(+V) model
# (stock-and-flow schematic for SI Text S1). Drawn at its final size on the
# landscape SI page (8.2 x 6.2 in). Uses no data.
#
# Output: output/figures/FigS21_model_schematic.{pdf,png,tiff}
# Original script: pnas_figures/fig_S21_trust_stratified_SEIR_PNAS.R
#   (drawing code unchanged)
#
# FORM
#   Adapted stock-flow diagram. Stocks (S, E, I, R, V) are boxes; flows are
#   arrows carrying rate labels; auxiliary variables feed the rates from above.
#   The population is stratified by institutional trust into K strata indexed
#   by tau. Three are drawn for illustration; an ellipsis row indicates that K
#   is set by the response categories of the survey instrument, not by the
#   framework.
#
# WHAT THE FIGURE ARGUES
#   Trust does not act on transmission directly. It acts through protective
#   behavior (-> contact reduction -> beta -> force of infection) and through
#   vaccination (-> movement out of the susceptible compartment). Those two
#   behavioral quantities are what this study measures; everything downstream
#   is proposed. Trust strata are themselves dynamic: individuals move between
#   them at time-varying rates, which is what distinguishes trust from a fixed
#   stratifier such as geography.
#
# ENCODING (greyscale-safe; colour is decorative only)
#   SOLID outline / SOLID arrow   = MEASURED IN THIS STUDY
#   DASHED outline / DASHED arrow = ASSUMED OR PROPOSED by the framework
#   No empirical values appear anywhere. Stratum boxes are equal-sized by
#   design: survey proportions vary by wave (Fig. S19), so no single scaling
#   would be defensible.
#
# OUTPUT  PDF (primary), 600-dpi PNG and TIFF at 8.2 x 6.2 in (SI page).
#
# Self-contained; no external data dependency.
# =============================================================================

if (!exists("REPO_DIR")) {
    .f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
    .f <- c(gsub("~+~", " ", .f, fixed = TRUE),  # Rscript writes spaces as ~+~
            unlist(lapply(sys.frames(), function(e) e$ofile)))
    REPO_DIR <- if (length(.f)) normalizePath(file.path(dirname(.f[length(.f)]), ".."),
                                              winslash = "/") else getwd()
}
source(file.path(REPO_DIR, "R", "00_setup.R"))
use_packages(c("ggplot2", "ragg", "systemfonts"))
source(file.path(REPO_DIR, "R", "figure_theme.R"))     # PNAS_FONT, save_pnas()
FIG_DIR <- OUT_FIG
FIG_W <- 8.2
FIG_H <- 6.2
FF <- PNAS_FONT

# Text sizes (mm; x 2.845 = pt).  Original values in comments.
S_TXT   <- 2.85   # 2.5  plain box text            -> 8.1 pt
S_STOCK <- 3.35   # 3.0  stock names (S_H ...)      -> 9.5 pt, subscripts 6.7 pt
S_FLOW  <- 3.05   # 2.5  lambda/sigma/gamma labels  -> 8.7 pt, subscripts 6.1 pt
S_VFLOW <- 3.05   # 2.3  nu/omega/(1-eps)lambda     -> 8.7 pt, subscripts 6.1 pt
S_NOTE  <- 2.65   # 2.3  side notes                 -> 7.5 pt
S_HEAD  <- 3.5    # 3.3  main header                -> 10 pt
S_SUB   <- 3.15   # 2.9  "Measured in this study"   -> 9 pt
S_TRU   <- 2.95   # 2.7  trust-strata header        -> 8.4 pt

C_MEAS <- "#1F3A93"   # navy   - measured in this study
C_PROP <- "#6E6E6E"   # grey   - proposed / assumed
C_TRU  <- "#F39C12"   # orange - trust strata (manuscript palette)
C_MIX  <- "#9A9A9A"   # lighter grey for mixing, to sit behind the flows
TXT    <- "grey15"

# ---- geometry ---------------------------------------------------------------
bands   <- c(High = 6.15, Moderate = 4.35, Low = 2.55)
y_ell   <- 0.95            # ellipsis row: "more strata as the instrument allows"
x_tr    <- 1.00            # trust stratum boxes
x_S <- 3.00; x_E <- 4.50; x_I <- 6.00; x_R <- 7.50
y_Voff  <- 0.85            # V sits below S
bw <- 0.72; bh <- 0.50
xs <- c(x_S, x_E, x_I, x_R)

# pl = TRUE marks labels that are plotmath expressions (the stock names).
# Everything else is plain text. This flag replaces the earlier newline
# heuristic, which mis-classified any single-line auxiliary label and caused
# parse() to fail on ordinary prose.
rect_df <- function(x, y, lab, status, w = bw, h = bh, col = NULL, pl = FALSE) {
    data.frame(x = x, y = y, lab = lab, status = status,
               xmin = x - w/2, xmax = x + w/2,
               ymin = y - h/2, ymax = y + h/2,
               col = if (is.null(col)) ifelse(status == "meas", C_MEAS, C_PROP)
                     else col,
               lty = ifelse(status == "meas", "solid", "dashed"),
               parse_lab = pl,
               stringsAsFactors = FALSE)
}

# ---- stocks: S E I R + V, per stratum (all proposed) ------------------------
stocks <- do.call(rbind, lapply(seq_along(bands), function(i) {
    y <- bands[i]; s <- substr(names(bands)[i], 1, 1)
    rbind(rect_df(x_S, y, paste0("S[", s, "]"), "prop", pl = TRUE),
          rect_df(x_E, y, paste0("E[", s, "]"), "prop", pl = TRUE),
          rect_df(x_I, y, paste0("I[", s, "]"), "prop", pl = TRUE),
          rect_df(x_R, y, paste0("R[", s, "]"), "prop", pl = TRUE),
          rect_df(x_S, y - y_Voff, paste0("V[", s, "]"), "prop", pl = TRUE))
}))

# ---- trust strata (measured) ------------------------------------------------
trust <- do.call(rbind, lapply(seq_along(bands), function(i)
    rect_df(x_tr, bands[i], names(bands)[i], "meas", w = 1.5, h = 0.62,
            col = C_TRU)))
trust$lty <- "solid"

# ---- auxiliaries measured in this study -------------------------------------
aux <- rbind(
    rect_df(4.00, 8.40, "Protective behavior\nby trust stratum", "meas",
            w = 2.7, h = 0.85),
    rect_df(6.95, 8.40, "Vaccination status (self-reported)\nby trust stratum",
            "meas", w = 3.1, h = 0.85))

# ---- proposed rate auxiliaries ----------------------------------------------
rates <- rbind(
    # Labels are blank here and drawn below as plotmath annotations, so that
    # Greek symbols match the typesetting used in the model structure.
    rect_df(4.00, 7.35, "", "prop", w = 2.7, h = 0.75),
    rect_df(6.95, 7.35, "", "prop", w = 3.1, h = 0.75))

boxes <- rbind(stocks, trust, aux, rates)

# ---- SEIR flows --------------------------------------------------------------
flow <- function(x1, x2, y, lab) data.frame(
    x = x1 + bw/2, xend = x2 - bw/2, y = y, yend = y,
    lab = lab, lx = (x1 + x2)/2, ly = y + 0.20, stringsAsFactors = FALSE)

flows <- do.call(rbind, lapply(seq_along(bands), function(i) {
    y <- bands[i]; s <- substr(names(bands)[i], 1, 1)
    rbind(flow(x_S, x_E, y, paste0("lambda[", s, "]")),
          flow(x_E, x_I, y, "sigma"),
          flow(x_I, x_R, y, "gamma"))
}))

# ---- vaccination and immunity flows around V --------------------------------
# S -> V (vaccination, rate nu); V -> S (waning, omega); V -> E (breakthrough
# infection at reduced susceptibility).  Together these represent vaccinated
# individuals as still participating in transmission, but structurally
# differently from the unvaccinated susceptible pool.
vflows <- do.call(rbind, lapply(seq_along(bands), function(i) {
    y <- bands[i]; yv <- y - y_Voff; s <- substr(names(bands)[i], 1, 1)
    rbind(
        # S -> V, down the left edge
        data.frame(x = x_S - 0.18, xend = x_S - 0.18, y = y - bh/2,
                   yend = yv + bh/2, lab = paste0("nu[", s, "]"),
                   lx = x_S - 0.52, ly = (y + yv)/2, stringsAsFactors = FALSE),
        # V -> S, up the right edge (waning immunity)
        data.frame(x = x_S + 0.18, xend = x_S + 0.18, y = yv + bh/2,
                   yend = y - bh/2, lab = "omega",
                   lx = x_S + 0.46, ly = (y + yv)/2, stringsAsFactors = FALSE),
        # V -> E, breakthrough at reduced susceptibility
        data.frame(x = x_S + bw/2, xend = x_E - bw/2 + 0.02, y = yv,
                   yend = y - bh/2 - 0.04,
                   lab = "(1-epsilon)*lambda", lx = (x_S + x_E)/2 + 0.15,
                   ly = yv - 0.22, stringsAsFactors = FALSE))
}))

# ---- assortative mixing: acts across S, E, I and R ---------------------------
mixing <- do.call(rbind, lapply(xs, function(xx)
    data.frame(x = xx, xend = xx,
               y    = c(bands[1] - bh/2 - 0.02, bands[2] - bh/2 - 0.02),
               yend = c(bands[2] + bh/2 + 0.02, bands[3] + bh/2 + 0.02),
               stringsAsFactors = FALSE)))

# ---- time-varying transitions between trust strata --------------------------
transit <- data.frame(
    x = x_tr - 0.95, xend = x_tr - 0.95,
    y    = c(bands[1] - 0.32, bands[2] - 0.32),
    yend = c(bands[2] + 0.32, bands[3] + 0.32))

# ---- ellipsis row: K is set by the survey instrument ------------------------
ell <- data.frame(x = c(x_tr, xs), y = y_ell)

# ---- plot --------------------------------------------------------------------
p <- ggplot() +
    # mixing drawn first so flows sit on top
    geom_segment(data = mixing, aes(x = x, y = y, xend = xend, yend = yend),
                 colour = C_MIX, linetype = "dashed", linewidth = 0.38,
                 arrow = arrow(length = unit(0.10, "cm"), ends = "both",
                               type = "closed")) +

    geom_rect(data = boxes,
              aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              fill = "white", colour = boxes$col, linetype = boxes$lty,
              linewidth = 0.55) +
    # plain-text labels (trust strata, measured auxiliaries, rate auxiliaries)
    geom_text(data = boxes[!boxes$parse_lab, ],
              aes(x = x, y = y, label = lab), size = S_TXT,
              lineheight = 0.95, colour = TXT, family = FF) +
    # rate-auxiliary labels, as plotmath so tau / beta / nu render as symbols
    annotate("text", x = 4.00, y = 7.35, size = S_TXT, colour = TXT, parse = TRUE, family = FF,
             label = "atop(\"Contact reduction  C(t, \"*tau*\")\", \"modulates transmission rate  \"*beta*\"(t, \"*tau*\")\")") +
    annotate("text", x = 6.95, y = 7.35, size = S_TXT, colour = TXT, parse = TRUE, family = FF,
             label = "paste(\"Vaccination rate  \", nu, \"(t, \" *tau*\")\")") +

    # plotmath labels (stock names only)
    geom_text(data = boxes[boxes$parse_lab, ],
              aes(x = x, y = y, label = lab), size = S_STOCK, colour = TXT,
              parse = TRUE, family = FF) +

    # SEIR flows
    geom_segment(data = flows, aes(x = x, y = y, xend = xend, yend = yend),
                 colour = C_PROP, linetype = "dashed", linewidth = 0.5,
                 arrow = arrow(length = unit(0.15, "cm"), type = "closed")) +
    geom_text(data = flows, aes(x = lx, y = ly, label = lab), size = S_FLOW,
              colour = C_PROP, parse = TRUE, family = FF) +

    # V flows
    geom_segment(data = vflows, aes(x = x, y = y, xend = xend, yend = yend),
                 colour = C_PROP, linetype = "dashed", linewidth = 0.45,
                 arrow = arrow(length = unit(0.13, "cm"), type = "closed")) +
    geom_text(data = vflows, aes(x = lx, y = ly, label = lab), size = S_VFLOW,
              colour = C_PROP, parse = TRUE, family = FF) +

    # trust stratum -> its SEIR row
    geom_segment(data = data.frame(x = x_tr + 0.75, xend = x_S - bw/2 - 0.24,
                                   y = bands, yend = bands),
                 aes(x = x, y = y, xend = xend, yend = yend),
                 colour = C_PROP, linetype = "dashed", linewidth = 0.45,
                 arrow = arrow(length = unit(0.13, "cm"), type = "closed")) +

    # behavior -> rate, and rate -> model
    geom_segment(data = data.frame(
                     x    = c(4.00, 6.95, 4.00, 6.95),
                     y    = c(7.97, 7.97, 6.97, 6.97),
                     xend = c(4.00, 6.95, 3.90, 3.35),
                     yend = c(7.74, 7.74, 6.55, 6.05)),
                 aes(x = x, y = y, xend = xend, yend = yend),
                 colour = C_PROP, linetype = "dashed", linewidth = 0.45,
                 arrow = arrow(length = unit(0.13, "cm"), type = "closed")) +

    # time-varying trust transitions
    geom_segment(data = transit, aes(x = x, y = y, xend = xend, yend = yend),
                 colour = C_PROP, linetype = "dashed", linewidth = 0.5,
                 arrow = arrow(length = unit(0.13, "cm"), ends = "both",
                               type = "closed")) +
    annotate("text", x = x_tr - 1.32, y = mean(bands), angle = 90,
             label = "atop(\"Time-varying transitions between strata\", paste(theta, \"(t)\"))",
             size = S_NOTE, colour = C_PROP, parse = TRUE, family = FF) +

    # ellipsis row
    # vertical ellipsis drawn as rotated ASCII dots: U+22EE does not embed
    # reliably in EPS output
    geom_text(data = ell, aes(x = x, y = y), label = "...", angle = 90,
              size = 4.6, colour = C_PROP, family = FF) +
    annotate("text", x = 8.55, y = y_ell, hjust = 0, size = S_NOTE,
             colour = C_PROP, lineheight = 0.95, family = FF,
             label = "K strata, set by the response\ncategories of the instrument") +

    # mixing label
    annotate("text", x = 8.55, y = mean(bands[2:3]) + 0.35, hjust = 0,
             size = S_NOTE, colour = C_MIX, lineheight = 0.95, family = FF,
             label = "Assortative mixing\nbetween strata") +

    # headers
    annotate("text", x = 4.6, y = 9.62,
             label = "Epidemiological model structure (proposed)",
             size = S_HEAD, fontface = "bold", colour = C_PROP, family = FF) +
    annotate("text", x = 5.4, y = 9.05, label = "Measured in this study",
             size = S_SUB, fontface = "bold", colour = C_MEAS, family = FF) +
    annotate("text", x = x_tr, y = 7.40, size = S_TRU, colour = C_TRU,
             parse = TRUE, family = FF,
             label = "atop(bold(\"Institutional trust strata\"), bold(paste(\"indexed by \", tau, \"  (\", tau, \" = 1 ... K)\")))") +

    # legend
    annotate("segment", x = 1.30, xend = 1.95, y = 0.10, yend = 0.10,
             colour = C_MEAS, linetype = "solid", linewidth = 0.6) +
    annotate("text", x = 2.05, y = 0.10, hjust = 0, size = S_TXT, colour = TXT,
             label = "measured in this study", family = FF) +
    annotate("segment", x = 4.85, xend = 5.50, y = 0.10, yend = 0.10,
             colour = C_PROP, linetype = "dashed", linewidth = 0.6) +
    annotate("text", x = 5.60, y = 0.10, hjust = 0, size = S_TXT, colour = TXT,
             label = "assumed or proposed by the framework", family = FF) +

    coord_cartesian(xlim = c(-0.55, 10.6), ylim = c(-0.15, 9.95),
                    expand = FALSE) +
    theme_void(base_family = FF) +
    theme(plot.margin = margin(4, 6, 4, 6))

save_pnas(p, file.path(FIG_DIR, "FigS21_model_schematic"), FIG_W, FIG_H)
log_msg("Done: 08_fig_S21_model_schematic.R")
