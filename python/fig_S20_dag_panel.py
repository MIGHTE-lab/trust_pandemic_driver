"""Fig. S20: four-panel conceptual DAG figure (uses no data).

The four panels contrast political identity as a candidate confounder,
hypothetical mediator, conditioned collider, and a trust measurement-error
structure. Drawn at its final size on the landscape SI page (8.6 x 6.2 in).

Output: output/figures/FigS20_dag_panel.{svg,pdf,png,tiff}
Run from the repository root:  python python/fig_S20_dag_panel.py
Requires: matplotlib, Pillow.
Original script: pnas_figures/build_DAG_4_scenarios_panel_PNAS.py (drawing code unchanged)
"""

import os
from pathlib import Path

import matplotlib as mpl

mpl.use("Agg")

import matplotlib.pyplot as plt
from matplotlib.path import Path as MplPath
from matplotlib.patches import FancyArrowPatch, FancyBboxPatch


ROOT = Path(__file__).resolve().parent.parent  # repository root
OUT = Path(os.environ.get("C50_OUTPUT_DIR") or ROOT / "output")  # as in R/00_setup.R
STEM = OUT / "figures" / "FigS20_dag_panel"

INK = "#173042"
ARROW = "#334A5A"
ACCENT = "#277487"
CONDITION_FILL = "#EAF4F6"
SECONDARY = "#526874"
PANEL_EDGE = "#D5E1E5"
FOOTER_FILL = "#F7FAFB"


mpl.rcParams.update(
    {
        "font.family": "sans-serif",
        "font.sans-serif": ["Arial", "Liberation Sans", "DejaVu Sans"],
        "mathtext.fontset": "stixsans",
        "pdf.fonttype": 42,
        "ps.fonttype": 42,
        "svg.fonttype": "none",
        "axes.unicode_minus": False,
    }
)


def _bezier_path(points):
    return MplPath(
        points,
        [MplPath.MOVETO, MplPath.CURVE4, MplPath.CURVE4, MplPath.CURVE4],
    )


def bezier_arrow(
    ax,
    points,
    *,
    lw=1.0,
    color=ARROW,
    linestyle="solid",
    halo_width=None,
    zorder=2,
):
    """Draw a cubic Bezier arrow, optionally with a white crossing bridge."""
    path = _bezier_path(points)
    if halo_width is not None:
        halo = FancyArrowPatch(
            path=path,
            arrowstyle="-",
            linewidth=halo_width,
            edgecolor="white",
            facecolor="none",
            capstyle="round",
            joinstyle="round",
            zorder=zorder - 0.1,
        )
        ax.add_patch(halo)
    patch = FancyArrowPatch(
        path=path,
        arrowstyle="-|>",
        mutation_scale=9.0,
        linewidth=lw,
        linestyle=linestyle,
        edgecolor=color,
        facecolor=color,
        capstyle="round",
        joinstyle="round",
        zorder=zorder,
    )
    ax.add_patch(patch)


def line_arrow(
    ax,
    start,
    end,
    *,
    lw=1.05,
    color=ARROW,
    linestyle="solid",
    zorder=2,
):
    """Draw a straight arrow between two node-boundary points."""
    patch = FancyArrowPatch(
        start,
        end,
        arrowstyle="-|>",
        mutation_scale=9.0,
        linewidth=lw,
        linestyle=linestyle,
        edgecolor=color,
        facecolor=color,
        capstyle="round",
        joinstyle="round",
        shrinkA=0,
        shrinkB=0,
        zorder=zorder,
    )
    ax.add_patch(patch)


def conditioned_node(ax, center, label, *, width=1.15, height=0.72):
    """Add a boxed node to denote conditioning in the adjusted model."""
    x, y = center
    patch = FancyBboxPatch(
        (x - width / 2, y - height / 2),
        width,
        height,
        boxstyle="round,pad=0.018,rounding_size=0.075",
        facecolor=CONDITION_FILL,
        edgecolor=ACCENT,
        linewidth=1.2,
        zorder=4,
    )
    ax.add_patch(patch)
    ax.text(
        x,
        y,
        label,
        ha="center",
        va="center",
        fontsize=13.0,
        color=INK,
        zorder=5,
    )


def text_node(ax, center, label, *, fontsize=14.0):
    """Add an unboxed notation-only node."""
    ax.text(
        center[0],
        center[1],
        label,
        ha="center",
        va="center",
        fontsize=fontsize,
        color=INK,
        zorder=5,
    )


def setup_panel(ax, title):
    ax.set_xlim(0, 10)
    ax.set_ylim(0, 6.25)
    ax.set_aspect("equal")
    ax.axis("off")
    border = FancyBboxPatch(
        (-0.025, 0.02),
        1.05,
        0.96,
        transform=ax.transAxes,
        boxstyle="round,pad=0.004,rounding_size=0.015",
        facecolor="white",
        edgecolor=PANEL_EDGE,
        linewidth=0.85,
        clip_on=False,
        zorder=0,
    )
    ax.add_patch(border)
    ax.set_title(title, loc="left", fontsize=9.5, fontweight="bold", pad=4, color=INK)


def panel_note(ax, text):
    ax.text(
        5.0,
        0.20,
        text,
        ha="center",
        va="bottom",
        fontsize=8.6,
        color=SECONDARY,
        style="italic",
        zorder=5,
    )


def draw_reference_panel(ax):
    """Panel (i): the complete reference structure with P as confounder."""
    setup_panel(ax, r"(i)  $P$ as a candidate confounder (reference)")

    line_arrow(ax, (1.18, 4.74), (1.18, 3.62))  # X -> P
    bezier_arrow(ax, [(1.75, 5.00), (2.70, 4.92), (3.80, 4.48), (4.24, 4.30)])  # X -> T
    bezier_arrow(ax, [(1.75, 3.38), (2.75, 3.47), (3.82, 3.78), (4.24, 3.98)])  # P -> T
    bezier_arrow(ax, [(1.62, 5.42), (3.20, 6.12), (7.05, 5.96), (8.30, 4.49)])  # X -> B
    bezier_arrow(ax, [(1.75, 3.08), (3.80, 2.66), (6.82, 3.00), (8.25, 3.86)])  # P -> B
    line_arrow(
        ax,
        (5.16, 4.12),
        (8.02, 4.12),
        lw=1.35,
        color=ACCENT,
        linestyle=(0, (5.0, 3.0)),
    )  # T -> B observed association
    bezier_arrow(ax, [(0.78, 4.80), (0.02, 3.38), (1.03, 1.02), (4.40, 1.38)])  # X -> U
    bezier_arrow(ax, [(1.44, 2.86), (2.38, 2.28), (3.76, 1.86), (4.48, 1.68)])  # P -> U
    bezier_arrow(
        ax,
        [(4.96, 1.86), (4.94, 2.43), (4.76, 3.34), (4.68, 3.75)],
        halo_width=4.2,
        zorder=2.7,
    )  # U -> T, bridged over P -> B
    bezier_arrow(ax, [(5.48, 1.54), (6.76, 0.98), (8.52, 1.69), (8.49, 3.62)])  # U -> B

    conditioned_node(ax, (1.18, 5.10), r"$X$")
    conditioned_node(ax, (1.18, 3.25), r"$P$")
    text_node(ax, (4.70, 4.12), r"$T_w$")
    text_node(ax, (4.92, 1.52), r"$U_{\mathrm{sec}}$")
    text_node(ax, (8.67, 4.12), r"$B_{w,k}$")
    panel_note(ax, r"$P$ is a hypothesized common cause of $T_w$ and $B_{w,k}$.")


def draw_mediator_panel(ax):
    """Panel (ii): T_w -> P -> B_wk, with P conditioned on."""
    setup_panel(ax, r"(ii)  $P$ as a hypothetical mediator")

    bezier_arrow(ax, [(1.66, 5.27), (2.95, 5.86), (4.75, 5.12), (5.48, 4.18)])  # X -> P
    bezier_arrow(ax, [(0.72, 4.82), (-0.04, 3.32), (1.04, 1.00), (4.03, 1.37)])  # X -> U
    bezier_arrow(ax, [(1.66, 4.90), (2.35, 4.65), (2.93, 4.13), (3.34, 3.98)])  # X -> T
    bezier_arrow(ax, [(1.58, 5.42), (3.50, 6.12), (7.35, 5.96), (8.43, 4.28)])  # X -> B
    bezier_arrow(ax, [(4.45, 1.85), (4.20, 2.45), (3.98, 3.18), (3.90, 3.53)])  # U -> T
    bezier_arrow(ax, [(5.14, 1.52), (6.20, 0.96), (8.38, 1.35), (8.54, 3.45)])  # U -> B
    line_arrow(ax, (4.38, 3.90), (5.45, 3.90))  # T -> P
    line_arrow(ax, (6.63, 3.90), (8.02, 3.90))  # P -> B
    bezier_arrow(
        ax,
        [(4.22, 3.58), (5.18, 2.98), (7.35, 2.98), (8.14, 3.56)],
        lw=1.35,
        color=ACCENT,
        linestyle=(0, (5.0, 3.0)),
    )  # T -> B observed association

    conditioned_node(ax, (1.10, 5.10), r"$X$")
    conditioned_node(ax, (6.04, 3.90), r"$P$")
    text_node(ax, (3.88, 3.90), r"$T_w$")
    text_node(ax, (4.62, 1.50), r"$U_{\mathrm{sec}}$")
    text_node(ax, (8.68, 3.90), r"$B_{w,k}$")
    panel_note(ax, r"Conditioning may block part of the $T_w \rightarrow P \rightarrow B_{w,k}$ pathway.")


def draw_collider_panel(ax):
    """Panel (iii): conditioned P as the collider T_w -> P <- U_sec."""
    setup_panel(ax, r"(iii)  Conditioned $P$ as a collider")

    bezier_arrow(ax, [(1.67, 5.02), (2.75, 4.92), (4.72, 4.73), (5.61, 4.48)])  # X -> P
    bezier_arrow(ax, [(1.62, 5.32), (3.20, 6.02), (5.04, 5.86), (5.63, 5.56)])  # X -> U
    bezier_arrow(ax, [(1.66, 4.78), (2.42, 4.42), (3.18, 3.70), (3.52, 3.48)])  # X -> T
    bezier_arrow(ax, [(1.46, 4.65), (2.35, 1.72), (6.95, 1.70), (8.38, 3.05)])  # X -> B
    line_arrow(ax, (6.20, 5.18), (6.20, 4.64))  # U -> P
    bezier_arrow(ax, [(6.68, 5.45), (7.62, 5.14), (8.43, 4.17), (8.58, 3.72)])  # U -> B
    bezier_arrow(ax, [(4.42, 3.55), (4.96, 3.80), (5.35, 4.00), (5.61, 4.05)])  # T -> P
    line_arrow(
        ax,
        (4.48, 3.35),
        (8.14, 3.35),
        lw=1.35,
        color=ACCENT,
        linestyle=(0, (5.0, 3.0)),
    )  # T -> B observed association

    conditioned_node(ax, (1.10, 5.00), r"$X$")
    conditioned_node(ax, (6.20, 4.28), r"$P$")
    text_node(ax, (4.00, 3.35), r"$T_w$")
    text_node(ax, (6.20, 5.55), r"$U_{\mathrm{sec}}$")
    text_node(ax, (8.72, 3.35), r"$B_{w,k}$")
    panel_note(ax, r"Conditioning may open the $T_w \leftrightarrow U_{\mathrm{sec}} \rightarrow B_{w,k}$ path.")


def draw_measurement_panel(ax):
    """Panel (iv): latent/true T and its imperfect measured proxy T_m."""
    setup_panel(ax, r"(iv)  Potential measurement error in trust")

    line_arrow(ax, (1.06, 4.73), (1.06, 3.72))  # X -> P
    bezier_arrow(ax, [(1.63, 5.00), (2.38, 4.90), (2.94, 4.42), (3.20, 4.25)])  # X -> T
    bezier_arrow(ax, [(1.63, 3.48), (2.36, 3.56), (2.90, 3.90), (3.20, 4.05)])  # P -> T
    bezier_arrow(ax, [(1.52, 5.40), (3.20, 6.08), (7.40, 5.98), (8.45, 4.58)])  # X -> B
    bezier_arrow(ax, [(1.63, 3.20), (3.55, 2.70), (7.12, 3.02), (8.30, 3.86)])  # P -> B
    bezier_arrow(ax, [(0.68, 4.78), (-0.04, 3.25), (0.94, 1.02), (4.02, 1.36)])  # X -> U
    bezier_arrow(ax, [(1.33, 2.98), (2.20, 2.31), (3.48, 1.82), (4.15, 1.66)])  # P -> U
    bezier_arrow(
        ax,
        [(4.64, 1.86), (4.60, 2.47), (4.12, 3.38), (3.76, 3.79)],
        halo_width=4.2,
        zorder=2.7,
    )  # U -> T, bridged over P -> B
    bezier_arrow(ax, [(5.13, 1.51), (6.36, 0.98), (8.45, 1.64), (8.53, 3.63)])  # U -> B
    bezier_arrow(ax, [(3.92, 4.48), (4.83, 5.24), (7.28, 5.02), (8.20, 4.34)])  # T -> B underlying relation
    line_arrow(ax, (4.08, 4.15), (5.02, 4.15))  # T -> T_m
    line_arrow(
        ax,
        (6.17, 4.15),
        (8.12, 4.15),
        lw=1.35,
        color=ACCENT,
        linestyle=(0, (5.0, 3.0)),
    )  # T_m -> B observed association

    conditioned_node(ax, (1.06, 5.09), r"$X$")
    conditioned_node(ax, (1.06, 3.35), r"$P$")
    text_node(ax, (3.64, 4.15), r"$T$")
    text_node(ax, (5.59, 4.15), r"$T_m$")
    text_node(ax, (4.58, 1.50), r"$U_{\mathrm{sec}}$")
    text_node(ax, (8.72, 4.15), r"$B_{w,k}$")
    ax.text(
        4.62,
        4.48,
        "imperfect measurement",
        ha="center",
        va="bottom",
        fontsize=7.5,
        color=SECONDARY,
        zorder=5,
        bbox=dict(facecolor="white", edgecolor="none", pad=0.6),
    )
    panel_note(ax, r"$T$ is latent/true trust; $T_m$ is the trust measure used in the fitted models.")


def build_figure():
    fig = plt.figure(figsize=(8.6, 6.2), facecolor="white")
    grid = fig.add_gridspec(
        3,
        2,
        height_ratios=[1.0, 1.0, 0.36],
        left=0.012,
        right=0.988,
        top=0.955,
        bottom=0.012,
        wspace=0.04,
        hspace=0.15,
    )

    axes = [
        fig.add_subplot(grid[0, 0]),
        fig.add_subplot(grid[0, 1]),
        fig.add_subplot(grid[1, 0]),
        fig.add_subplot(grid[1, 1]),
    ]
    draw_reference_panel(axes[0])
    draw_mediator_panel(axes[1])
    draw_collider_panel(axes[2])
    draw_measurement_panel(axes[3])

    # Figure title omitted: it is identical to the caption title in the SI.

    footer = fig.add_subplot(grid[2, :])
    footer.set_xlim(0, 1)
    footer.set_ylim(0, 1)
    footer.axis("off")
    footer_box = FancyBboxPatch(
        (0.01, 0.06),
        0.98,
        0.88,
        transform=footer.transAxes,
        boxstyle="round,pad=0.006,rounding_size=0.02",
        facecolor=FOOTER_FILL,
        edgecolor=PANEL_EDGE,
        linewidth=0.8,
        zorder=0,
    )
    footer.add_patch(footer_box)
    legend = (
        r"$T_w$ = institutional trust in survey wave $w$; $B_{w,k}$ = protective behavior $k$ in wave $w$; "
        r"$P$ = political leaning and ideology."
        "\n"
        r"$X \subseteq C$ = measured baseline covariates; $U_{\mathrm{sec}}$ = unmeasured confounders (e.g., pandemic fatigue)."
        "\n"
        r"In panel (iv), $T$ is latent/true trust and $T_m$ is its imperfect single-item measurement; no direction of measurement-error bias is assumed."
        "\n"
        "The panels are alternatives the study cannot distinguish. All DAGs represent statistical associations;"
        "\n"
        "absence of arrows does not establish independence or causal effects."
    )
    footer.text(
        0.5,
        0.50,
        legend,
        ha="center",
        va="center",
        fontsize=8.6,
        color=SECONDARY,
        linespacing=1.12,
        zorder=5,
    )

    return fig


def main():
    STEM.parent.mkdir(parents=True, exist_ok=True)
    fig = build_figure()
    metadata = {
        "Title": "Alternative conceptual structures for political identity and trust measurement",
        "Subject": "Four noncausal conceptual DAG scenarios for adjusted trust-behavior associations",
        "Keywords": "institutional trust, political identity, confounder, mediator, collider, measurement error",
    }
    fig.savefig(
        f"{STEM}.svg",
        format="svg",
        facecolor="white",
        metadata={"Title": metadata["Title"], "Description": metadata["Subject"]},
    )
    fig.savefig(f"{STEM}.pdf", format="pdf", facecolor="white", metadata=metadata)
    fig.savefig(
        f"{STEM}.png",
        format="png",
        dpi=600,
        facecolor="white",
        metadata={"Title": metadata["Title"]},
    )
    plt.close(fig)
    # PNAS asks for RGB (Agg writes RGBA): flatten onto white, write PNG + LZW TIFF
    from PIL import Image

    Image.MAX_IMAGE_PIXELS = None
    rgba = Image.open(f"{STEM}.png")
    rgb = Image.new("RGB", rgba.size, "white")
    rgb.paste(rgba, mask=rgba.split()[-1])
    rgb.save(f"{STEM}.png", dpi=(600, 600))
    rgb.save(f"{STEM}.tiff", dpi=(600, 600), compression="tiff_lzw")


if __name__ == "__main__":
    main()
