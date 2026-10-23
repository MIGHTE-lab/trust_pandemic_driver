## ============================================================================ #
## figure_theme.R   (original name: pnas_figure_theme.R)
##
## Shared settings for PNAS-ready figures that are rendered at their FINAL
## printed size, so the point sizes set here are the point sizes on the page.
##
## PNAS digital-art guidance (2026 author instructions):
##   * >= 300 dpi (we write 600 dpi), RGB colour, fonts embedded
##   * final size: ~9 x 6 cm (small), ~11 x 11 cm (medium), ~18 x 22 cm (large)
##     column widths: 8.7 cm (single), 11.4 cm (1.5 col), 17.8 cm (double)
##   * lettering 6-12 pt after reduction, consistent within a figure
##
## Usage: sourced by R/07_figures.R and R/08_fig_S21_model_schematic.R.
## Author: AJZ / revision support   Date: 2026-10-13
## ============================================================================ #

suppressPackageStartupMessages({
    library(ggplot2)
    library(scales)
})

# ---- Page geometry (inches) -------------------------------------------------
CM <- 1 / 2.54
PNAS_W_SINGLE <- 8.7 * CM     # 3.425 in
PNAS_W_ONEHALF <- 11.4 * CM   # 4.488 in
PNAS_W_DOUBLE <- 17.8 * CM    # 7.008 in
SI_TEXT_W <- 9.0              # SI figure pages: landscape US Letter, 1-in margins
SI_TEXT_H <- 6.5

PNAS_DPI <- 600

# ---- Type scale (points at final size) --------------------------------------
PT_MIN <- 6.0       # PNAS floor; nothing in any figure is set below this
PT_SMALL <- 6.5     # tick labels, legend text, milestone labels
PT_BASE <- 7.0      # axis titles, strip text
PT_TITLE <- 8.0     # panel titles

# ggplot2 geom_text()/annotate() sizes are in mm; convert from points
pt2mm <- function(pt) pt / ggplot2::.pt

# ---- Font ---------------------------------------------------------------------
# Arial where installed (Windows/macOS); otherwise Liberation Sans, which is
# metrically identical to Arial, so line breaks and layout do not change.
.pnas_pick_font <- function() {
    cand <- c("Arial", "Liberation Sans", "Helvetica", "TeX Gyre Heros")
    if (requireNamespace("systemfonts", quietly = TRUE)) {
        fams <- unique(systemfonts::system_fonts()$family)
        hit <- cand[cand %in% fams]
        if (length(hit) > 0) return(hit[1])
    }
    "sans"
}
PNAS_FONT <- .pnas_pick_font()

#' Text width in points for a given string at a given size (used to choose
#' how many legend rows fit a panel).
text_width_pt <- function(x, size_pt = PT_SMALL, family = PNAS_FONT) {
    if (requireNamespace("systemfonts", quietly = TRUE)) {
        w <- tryCatch(
            systemfonts::string_width(x, family = family, size = size_pt, res = 72),
            error = function(e) NULL)
        if (!is.null(w)) return(as.numeric(w))
    }
    nchar(x) * size_pt * 0.55
}

#' Width (pt) of a horizontal ggplot legend with these labels laid out in
#' `nrow` rows, measured from the actual legend grob (so it honours the theme's
#' key sizes, spacing and the exact font metrics).
.legend_width_cache <- new.env()
legend_width_pt <- function(labels, nrow, theme_fun = theme_pnas) {
    key <- paste(c(labels, nrow), collapse = "\r")
    if (!is.null(.legend_width_cache[[key]])) return(.legend_width_cache[[key]])
    df <- data.frame(x = seq_along(labels), y = 1, g = factor(labels, levels = labels))
    p <- ggplot(df, aes(x, y, colour = g)) + geom_point() +
        scale_colour_discrete(name = NULL) +
        guides(colour = guide_legend(nrow = nrow, byrow = TRUE)) +
        theme_fun() + theme(legend.position = "bottom")
    # measure on a Cairo device so the real font metrics are used (the default
    # pdf() device only knows the PostScript base fonts)
    tmp <- tempfile(fileext = ".pdf")
    grDevices::cairo_pdf(tmp, width = 7, height = 7)
    on.exit({ grDevices::dev.off(); unlink(tmp) }, add = TRUE)
    g <- ggplotGrob(p)
    i <- which(vapply(g$grobs, function(x) grepl("guide-box", x$name), logical(1)))
    w <- grid::convertWidth(sum(g$grobs[[i[1]]]$widths), "pt", valueOnly = TRUE)
    assign(key, w, envir = .legend_width_cache)
    w
}

#' Smallest number of legend rows for which the legend fits in `avail_pt`.
legend_rows_needed <- function(labels, avail_pt, ...) {
    for (nr in seq_along(labels)) {
        if (legend_width_pt(labels, nr) <= avail_pt) return(nr)
    }
    length(labels)
}

# ---- Theme ------------------------------------------------------------------
theme_pnas <- function(base_size = PT_BASE, base_family = PNAS_FONT) {
    theme_minimal(base_size = base_size, base_family = base_family) +
        theme(
            text = element_text(colour = "grey10"),
            plot.title = element_text(size = PT_TITLE, face = "bold", hjust = 0,
                                      margin = margin(b = 2)),
            plot.title.position = "panel",
            axis.title = element_text(size = base_size),
            axis.title.y = element_text(margin = margin(r = 2)),
            axis.title.x = element_text(margin = margin(t = 2)),
            axis.text = element_text(size = PT_SMALL, colour = "grey25"),
            axis.ticks = element_line(colour = "grey55", linewidth = 0.2),
            axis.ticks.length = unit(1.5, "pt"),
            panel.grid.major = element_line(colour = "grey90", linewidth = 0.2),
            panel.grid.minor = element_blank(),
            strip.text = element_text(size = base_size, face = "bold",
                                      margin = margin(1, 1, 1, 1)),
            legend.title = element_text(size = base_size, face = "bold"),
            legend.text = element_text(size = PT_SMALL, margin = margin(l = 1.5, r = 3)),
            legend.key.height = unit(7, "pt"),
            legend.key.width = unit(9, "pt"),
            legend.spacing.x = unit(2, "pt"),
            legend.spacing.y = unit(0, "pt"),
            legend.margin = margin(0, 0, 0, 0),
            legend.box.margin = margin(0, 0, 0, 0),
            legend.box.spacing = unit(3, "pt"),
            plot.margin = margin(3, 4, 2, 2)
        )
}

# ---- Pandemic milestones (dates/colours from unified_palettes.R) ------------
# Label placement for the publication panels: `side` puts the label to the
# right (0) or left (1) of its dashed line; `tier` stacks labels that would
# otherwise collide (1 = top row of the label band, 2 = second row).
PNAS_MILESTONES <- data.frame(
    date = as.Date(c("2020-05-27", "2020-07-01", "2021-04-01",
                     "2021-08-02", "2022-01-01", "2022-01-15")),
    label = c("Death toll\nexceeds 100k", "CDC universal\nmask guidance",
              "Vaccine\navailability", "Vaccination\ncoverage 70%",
              "Omicron\ndominant", "At-home test\nkits available"),
    colour = c("#D32F2F", "#0D47A1", "#2E7D32", "#2E7D32", "#6A1B9A", "#795548"),
    side = c(0, 0, 1, 0, 1, 0),
    tier = c(1, 2, 1, 1, 2, 2),
    stringsAsFactors = FALSE
)

#' Add milestone lines (and optionally labels) to a date-axis panel.
#' Labels are drawn in a band above the data (y between `y_band[1]` and
#' `y_band[2]` in data units); lines span the full panel.  `layout = "narrow"`
#' (panels < ~3.3 in wide) uses three label rows instead of two.
milestone_band <- function(layout = c("wide", "narrow")) {
    layout <- match.arg(layout)
    if (layout == "wide") c(101, 119) else c(101, 125)
}
add_milestones_pnas <- function(p, x_range, labels = TRUE, layout = c("wide", "narrow"),
                                label_pt = PT_MIN + 0.2, line_width = 0.25,
                                line_alpha = 0.65, nudge_days = 6) {
    layout <- match.arg(layout)
    ms <- PNAS_MILESTONES
    if (layout == "narrow") ms$tier <- c(1, 2, 3, 1, 2, 2)
    ms <- ms[ms$date >= x_range[1] & ms$date <= x_range[2], , drop = FALSE]
    if (nrow(ms) == 0) return(p)
    p <- p + geom_vline(data = ms, aes(xintercept = date), colour = ms$colour,
                        linetype = "22", linewidth = line_width, alpha = line_alpha)
    if (!labels) return(p)
    y_band <- milestone_band(layout)
    n_tier <- if (layout == "wide") 2 else 3
    tier_h <- diff(y_band) / n_tier
    ms$y <- y_band[2] - (ms$tier - 1) * tier_h
    ms$x <- ms$date + ifelse(ms$side == 0, nudge_days, -nudge_days)
    p + geom_label(data = ms, aes(x = x, y = y, label = label), colour = ms$colour,
                   fill = "white", label.size = 0, label.r = unit(0, "pt"),
                   label.padding = unit(0.6, "pt"),
                   hjust = ms$side, vjust = 1, size = pt2mm(label_pt),
                   lineheight = 0.88, family = PNAS_FONT, inherit.aes = FALSE)
}

# ---- Saving -----------------------------------------------------------------
#' Write one figure at final size as vector PDF (cairo: fonts embedded),
#' 600-dpi PNG and 600-dpi LZW-compressed TIFF (both RGB, white background).
save_pnas <- function(plot, stem, width, height, formats = c("pdf", "png", "tiff")) {
    dir.create(dirname(stem), recursive = TRUE, showWarnings = FALSE)
    if ("pdf" %in% formats) {
        # Greek/maths glyphs (plotmath) from the same typeface as the text
        pdf_dev <- function(filename, ...) grDevices::cairo_pdf(
            filename, ..., symbolfamily = grDevices::cairoSymbolFont(PNAS_FONT, usePUA = FALSE))
        ggsave(paste0(stem, ".pdf"), plot, width = width, height = height,
               units = "in", device = pdf_dev, bg = "white")
    }
    if ("png" %in% formats)
        ggsave(paste0(stem, ".png"), plot, width = width, height = height,
               units = "in", dpi = PNAS_DPI, device = ragg::agg_png, bg = "white")
    if ("tiff" %in% formats)
        ggsave(paste0(stem, ".tiff"), plot, width = width, height = height,
               units = "in", dpi = PNAS_DPI, device = ragg::agg_tiff,
               compression = "lzw", bg = "white")
    invisible(stem)
}

message("figure_theme.R loaded (font: ", PNAS_FONT, ").")
