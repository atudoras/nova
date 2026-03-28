"""
NOVA R Package — Comprehensive PDF User Guide Generator
Produces docs/NOVA-User-Guide.pdf
"""

import os, textwrap
from pathlib import Path
from PIL import Image as PILImage
from reportlab.lib.pagesizes import A4
from reportlab.lib import colors
from reportlab.lib.units import cm, mm
from reportlab.lib.styles import getSampleStyleSheet, ParagraphStyle
from reportlab.lib.enums import TA_LEFT, TA_CENTER, TA_JUSTIFY
from reportlab.platypus import (
    BaseDocTemplate, PageTemplate, Frame, Paragraph, Spacer,
    Table, TableStyle, Image, PageBreak, HRFlowable, KeepTogether,
    NextPageTemplate, FrameBreak
)
from reportlab.platypus.tableofcontents import TableOfContents
from reportlab.pdfgen import canvas
from reportlab.lib.utils import ImageReader

# ── Paths ──────────────────────────────────────────────────────────────────────
BASE = Path("/Users/alextudoras/My Documents (change name)/Project_NOVA/NOVA copy 2")
FIGS = BASE / "docs/user-guide/figures"
OUT  = BASE / "docs/NOVA-User-Guide.pdf"

# ── Color palette ──────────────────────────────────────────────────────────────
NAVY    = colors.HexColor("#0F2D4A")
BLUE    = colors.HexColor("#1F78B4")
LBLUE   = colors.HexColor("#A6CEE3")
GREEN   = colors.HexColor("#33A02C")
RED     = colors.HexColor("#E31A1C")
ORANGE  = colors.HexColor("#FF7F00")
GRAY    = colors.HexColor("#555555")
LGRAY   = colors.HexColor("#F4F6F8")
CODEBG  = colors.HexColor("#F0F4F8")
CODEFG  = colors.HexColor("#1A2332")
WHITE   = colors.white
BLACK   = colors.black
TEAL    = colors.HexColor("#2C7BB6")

PAGE_W, PAGE_H = A4
MARGIN = 2.2 * cm

# ── Style sheet ────────────────────────────────────────────────────────────────
def build_styles():
    s = getSampleStyleSheet()

    def add(name, **kw):
        base = kw.pop("parent", "Normal")
        s.add(ParagraphStyle(name=name, parent=s[base], **kw))

    # Body
    add("Body", fontSize=10.5, leading=16, textColor=colors.HexColor("#222222"),
        spaceAfter=6, alignment=TA_JUSTIFY)
    add("BodyLeft", parent="Body", alignment=TA_LEFT)

    # Headings
    add("ChapterNum", fontSize=11, leading=14, textColor=BLUE, fontName="Helvetica-Bold",
        spaceBefore=0, spaceAfter=2)
    add("ChapterTitle", fontSize=22, leading=28, textColor=NAVY, fontName="Helvetica-Bold",
        spaceBefore=0, spaceAfter=14)
    add("H2", fontSize=14, leading=18, textColor=NAVY, fontName="Helvetica-Bold",
        spaceBefore=16, spaceAfter=6, borderPad=0, keepWithNext=1)
    add("H3", fontSize=11.5, leading=15, textColor=BLUE, fontName="Helvetica-Bold",
        spaceBefore=12, spaceAfter=4, keepWithNext=1)
    add("H4", fontSize=10.5, leading=14, textColor=GRAY, fontName="Helvetica-BoldOblique",
        spaceBefore=8, spaceAfter=3)

    # Code
    add("CodeBlock", fontSize=8.8, leading=13, fontName="Courier",
        textColor=CODEFG, backColor=CODEBG,
        leftIndent=10, rightIndent=10,
        spaceBefore=4, spaceAfter=4,
        borderWidth=0, borderColor=CODEBG,
        borderPad=8)
    add("CodeInline", fontSize=9, fontName="Courier", textColor=BLUE, parent="Body")

    # Callout boxes
    add("TipBox", fontSize=10, leading=15, textColor=colors.HexColor("#155724"),
        leftIndent=12, rightIndent=12, spaceBefore=6, spaceAfter=6)
    add("NoteBox", fontSize=10, leading=15, textColor=colors.HexColor("#0c5460"),
        leftIndent=12, rightIndent=12, spaceBefore=6, spaceAfter=6)
    add("WarnBox", fontSize=10, leading=15, textColor=colors.HexColor("#721c24"),
        leftIndent=12, rightIndent=12, spaceBefore=6, spaceAfter=6)

    # Caption
    add("Caption", fontSize=9, leading=13, textColor=GRAY, fontName="Helvetica-Oblique",
        alignment=TA_CENTER, spaceBefore=4, spaceAfter=12)

    # TOC
    add("TOCEntry1", fontSize=11, leading=16, fontName="Helvetica-Bold", textColor=NAVY,
        leftIndent=0, spaceBefore=6)
    add("TOCEntry2", fontSize=10, leading=14, textColor=GRAY, leftIndent=16, spaceBefore=2)

    # Bullet
    add("BulletItem", fontSize=10.5, leading=16, leftIndent=18, firstLineIndent=-12,
        textColor=colors.HexColor("#222222"), spaceAfter=3, alignment=TA_LEFT)

    # Table header
    add("TH", fontSize=9.5, leading=13, fontName="Helvetica-Bold",
        textColor=WHITE, alignment=TA_CENTER)
    add("TD", fontSize=9, leading=13, textColor=colors.HexColor("#222222"),
        alignment=TA_LEFT)
    add("TDCode", fontSize=8.5, leading=12, fontName="Courier",
        textColor=BLUE, alignment=TA_LEFT)

    return s

SS = build_styles()

# ── Helper flowables ───────────────────────────────────────────────────────────
def P(text, style="Body"):          return Paragraph(text, SS[style])
def SP(n=6):                        return Spacer(1, n)
def HR(color=LBLUE, width=1):       return HRFlowable(width="100%", thickness=width,
                                                       color=color, spaceAfter=6, spaceBefore=6)

def code_block(code_text):
    """Render a code block as a table with colored background."""
    lines = code_text.strip().split("\n")
    formatted = "<br/>".join(
        line.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")
        for line in lines
    )
    para = Paragraph(formatted, SS["CodeBlock"])
    t = Table([[para]], colWidths=[PAGE_W - 2 * MARGIN])
    t.setStyle(TableStyle([
        ("BACKGROUND", (0, 0), (-1, -1), CODEBG),
        ("ROUNDEDCORNERS", [4]),
        ("LEFTPADDING",  (0, 0), (-1, -1), 12),
        ("RIGHTPADDING", (0, 0), (-1, -1), 12),
        ("TOPPADDING",   (0, 0), (-1, -1), 10),
        ("BOTTOMPADDING",(0, 0), (-1, -1), 10),
        ("BOX", (0, 0), (-1, -1), 0.5, colors.HexColor("#C9D6E3")),
    ]))
    return t

def callout(text, kind="note"):
    colors_map = {
        "tip":  (colors.HexColor("#D4EDDA"), colors.HexColor("#28A745"), "TipBox",  "Tip"),
        "note": (colors.HexColor("#D1ECF1"), colors.HexColor("#17A2B8"), "NoteBox", "Note"),
        "warn": (colors.HexColor("#F8D7DA"), colors.HexColor("#DC3545"), "WarnBox", "Warning"),
    }
    bg, border, style, label = colors_map[kind]
    content = Paragraph(f"<b>{label}:</b> {text}", SS[style])
    t = Table([[content]], colWidths=[PAGE_W - 2 * MARGIN])
    t.setStyle(TableStyle([
        ("BACKGROUND", (0, 0), (-1, -1), bg),
        ("LEFTPADDING",  (0, 0), (-1, -1), 14),
        ("RIGHTPADDING", (0, 0), (-1, -1), 14),
        ("TOPPADDING",   (0, 0), (-1, -1), 8),
        ("BOTTOMPADDING",(0, 0), (-1, -1), 8),
        ("BOX", (0, 0), (-1, -1), 1.5, border),
        ("LEFTBORDER", (0, 0), (-1, -1), 4, border),
    ]))
    return t

def fig(filename, width_cm=13, caption=None):
    path = FIGS / filename
    if not path.exists():
        return [P(f"[Figure: {filename} — not found]", "Caption")]
    # Read actual pixel dimensions to preserve aspect ratio
    with PILImage.open(str(path)) as im:
        px_w, px_h = im.size
    ratio = px_h / px_w
    max_w = min(width_cm * cm, PAGE_W - 2 * MARGIN)
    w = max_w
    h = w * ratio
    # If height would overflow a page frame, scale down
    max_h = PAGE_H - 5 * cm
    if h > max_h:
        h = max_h
        w = h / ratio
    img = Image(str(path), width=w, height=h)
    img.hAlign = "CENTER"
    flows = [img]
    if caption:
        flows.append(P(caption, "Caption"))
    return flows

def chapter_header(num, title):
    return [
        SP(8),
        P(f"Chapter {num}", "ChapterNum"),
        P(title, "ChapterTitle"),
        HR(BLUE, 1.5),
        SP(6),
    ]

def h2(text):  return [SP(4), P(text, "H2"), HR(LBLUE, 0.5)]
def h3(text):  return [P(text, "H3")]
def h4(text):  return [P(text, "H4")]

def param_table(rows):
    """Render a parameter reference table."""
    header = [P("Parameter", "TH"), P("Default", "TH"), P("Description", "TH")]
    data = [header]
    for param, default, desc in rows:
        data.append([
            P(f"<font name='Courier'>{param}</font>", "TD"),
            P(f"<font name='Courier' color='#1F78B4'>{default}</font>", "TD"),
            P(desc, "TD"),
        ])
    cw = [4.5 * cm, 3.2 * cm, PAGE_W - 2 * MARGIN - 7.7 * cm]
    t = Table(data, colWidths=cw, repeatRows=1)
    t.setStyle(TableStyle([
        ("BACKGROUND",   (0, 0), (-1, 0),  NAVY),
        ("BACKGROUND",   (0, 1), (-1, -1), WHITE),
        ("ROWBACKGROUNDS",(0, 1), (-1, -1), [WHITE, LGRAY]),
        ("GRID",         (0, 0), (-1, -1), 0.4, colors.HexColor("#CCCCCC")),
        ("TOPPADDING",   (0, 0), (-1, -1), 6),
        ("BOTTOMPADDING",(0, 0), (-1, -1), 6),
        ("LEFTPADDING",  (0, 0), (-1, -1), 8),
        ("RIGHTPADDING", (0, 0), (-1, -1), 8),
        ("VALIGN",       (0, 0), (-1, -1), "TOP"),
    ]))
    return t

# ── Page templates ─────────────────────────────────────────────────────────────
def cover_page_canvas(c, doc):
    c.saveState()
    # Navy background
    c.setFillColor(NAVY)
    c.rect(0, 0, PAGE_W, PAGE_H, fill=1, stroke=0)
    # Blue accent strip
    c.setFillColor(BLUE)
    c.rect(0, PAGE_H * 0.38, PAGE_W, 4, fill=1, stroke=0)
    # Light blue stripe
    c.setFillColor(LBLUE)
    c.rect(0, PAGE_H * 0.38 - 3, PAGE_W, 3, fill=1, stroke=0)
    # Title
    c.setFillColor(WHITE)
    c.setFont("Helvetica-Bold", 46)
    c.drawCentredString(PAGE_W / 2, PAGE_H * 0.62, "NOVA")
    c.setFont("Helvetica", 20)
    c.drawCentredString(PAGE_W / 2, PAGE_H * 0.55, "Neural Output Visualization")
    c.drawCentredString(PAGE_W / 2, PAGE_H * 0.50, "& Analysis")
    # Subtitle
    c.setFillColor(LBLUE)
    c.setFont("Helvetica-BoldOblique", 14)
    c.drawCentredString(PAGE_W / 2, PAGE_H * 0.43, "Complete User Guide  •  v0.1.1")
    # Bottom info
    c.setFillColor(colors.HexColor("#7FAACC"))
    c.setFont("Helvetica", 10)
    c.drawCentredString(PAGE_W / 2, PAGE_H * 0.18,
                        "Alex Tudoras  •  UCSF  •  alex.tudorasmiravet@ucsf.edu")
    c.drawCentredString(PAGE_W / 2, PAGE_H * 0.14,
                        "github.com/atudoras/nova")
    # Small decorative dots
    c.setFillColor(BLUE)
    for i, x in enumerate([0.2, 0.35, 0.5, 0.65, 0.8]):
        c.circle(PAGE_W * x, PAGE_H * 0.28, 4 + i * 1.5, fill=1, stroke=0)
    c.restoreState()

def normal_page_canvas(c, doc):
    c.saveState()
    # Header bar
    c.setFillColor(NAVY)
    c.rect(0, PAGE_H - 1.4 * cm, PAGE_W, 1.4 * cm, fill=1, stroke=0)
    c.setFillColor(WHITE)
    c.setFont("Helvetica-Bold", 9)
    c.drawString(MARGIN, PAGE_H - 0.9 * cm, "NOVA User Guide  v0.1.1")
    c.setFont("Helvetica", 9)
    c.drawRightString(PAGE_W - MARGIN, PAGE_H - 0.9 * cm, "github.com/atudoras/nova")
    # Footer
    c.setFillColor(LGRAY)
    c.rect(0, 0, PAGE_W, 1.2 * cm, fill=1, stroke=0)
    c.setFillColor(GRAY)
    c.setFont("Helvetica", 8.5)
    c.drawCentredString(PAGE_W / 2, 0.45 * cm, f"Page {doc.page}")
    c.setFillColor(BLUE)
    c.rect(MARGIN, 1.2 * cm, PAGE_W - 2 * MARGIN, 1, fill=1, stroke=0)
    c.restoreState()

# ── Document class with TOC ────────────────────────────────────────────────────
class NOVADoc(BaseDocTemplate):
    def __init__(self, filename):
        super().__init__(
            filename,
            pagesize=A4,
            leftMargin=MARGIN, rightMargin=MARGIN,
            topMargin=1.8 * cm, bottomMargin=1.6 * cm,
            title="NOVA User Guide",
            author="Alex Tudoras",
            subject="MEA Neuronal Data Analysis",
        )
        self.toc = TableOfContents()
        self.toc.levelStyles = [SS["TOCEntry1"], SS["TOCEntry2"]]

        cover_frame = Frame(0, 0, PAGE_W, PAGE_H, leftPadding=0, rightPadding=0,
                            topPadding=0, bottomPadding=0)
        body_frame  = Frame(MARGIN, 1.4 * cm, PAGE_W - 2 * MARGIN,
                            PAGE_H - 3.4 * cm, id="body")

        self.addPageTemplates([
            PageTemplate(id="cover", frames=cover_frame,
                         onPage=cover_page_canvas),
            PageTemplate(id="normal", frames=body_frame,
                         onPage=normal_page_canvas),
        ])

    def afterFlowable(self, flowable):
        # Register headings with TOC
        if isinstance(flowable, Paragraph):
            style = flowable.style.name
            text  = flowable.getPlainText()
            if style == "ChapterTitle":
                self.notify("TOCEntry", (0, text, self.page))
            elif style == "H2":
                self.notify("TOCEntry", (1, text, self.page))


# ── Content builder ────────────────────────────────────────────────────────────
def build_story():
    story = []

    # ── Cover (blank page, canvas does the drawing) ────────────────────────────
    story.append(NextPageTemplate("cover"))
    story.append(PageBreak())

    # ── TOC page ───────────────────────────────────────────────────────────────
    story.append(NextPageTemplate("normal"))
    story.append(PageBreak())
    story += [
        SP(20),
        P("Table of Contents", "ChapterTitle"),
        HR(BLUE, 1.5),
        SP(8),
    ]
    story.append(TableOfContents())
    story.append(PageBreak())

    # ══════════════════════════════════════════════════════════════════════════
    # Chapter 1 — Introduction
    # ══════════════════════════════════════════════════════════════════════════
    story += chapter_header(1, "Introduction")
    story += [
        P("NOVA (<b>N</b>eural <b>O</b>utput <b>V</b>isualization and <b>A</b>nalysis) "
          "is an R package for analysing Multi-Electrode Array (MEA) neuronal recordings. "
          "It transforms raw electrode CSV files into publication-ready figures with a "
          "single, reproducible workflow — no programming experience required beyond "
          "changing one file path.", "Body"),
        SP(8),
    ]
    story += h2("What NOVA Does")
    story += [
        P("MEA experiments record the electrical activity of neuronal networks across "
          "dozens of wells, timepoints, treatments and genotypes. The resulting data "
          "is high-dimensional: many metrics × many wells × many timepoints. NOVA "
          "addresses three core analytical needs:", "Body"),
        SP(4),
        P("<b>1. Dimensionality reduction</b> — PCA collapses all electrode metrics "
          "into interpretable axes, revealing which treatments shift network activity "
          "and how genotype modifies the response.", "BulletItem"),
        P("<b>2. Temporal trajectory analysis</b> — connecting PCA scores across "
          "timepoints reveals the <i>path</i> each treatment group takes through "
          "network-state space, distinguishing fast responders from slow.", "BulletItem"),
        P("<b>3. Per-metric visualisation</b> — bar, box and violin plots for any "
          "individual electrode metric (firing rate, burst frequency, synchrony…) "
          "across groups and timepoints.", "BulletItem"),
        SP(10),
    ]
    story += h2("The Four-Step Pipeline")
    pipeline = [
        [P("Step", "TH"), P("Function", "TH"), P("What it does", "TH")],
        [P("1. Discover", "TD"), P("discover_mea_structure()", "TDCode"),
         P("Scans your data folder, detects experiments, timepoints, well counts and treatment metadata automatically.", "TD")],
        [P("2. Process", "TD"), P("process_mea_flexible()", "TDCode"),
         P("Reads all CSVs, assigns Treatment/Genotype from filenames, baseline-normalises values, returns tidy long-format data.", "TD")],
        [P("3. Analyse", "TD"), P("pca_analysis_enhanced()", "TDCode"),
         P("Runs PCA on normalised electrode metrics, computes variance explained, returns scores and loadings.", "TD")],
        [P("4. Visualise", "TD"), P("plot_pca_trajectories_general()\ncreate_mea_heatmaps_enhanced()\nplot_mea_metric()", "TDCode"),
         P("Creates trajectory plots, heatmaps and per-metric figures. All functions return ggplot/pheatmap objects and optionally save to disk.", "TD")],
    ]
    t = Table(pipeline, colWidths=[2.8*cm, 5.2*cm, PAGE_W - 2*MARGIN - 8*cm], repeatRows=1)
    t.setStyle(TableStyle([
        ("BACKGROUND",    (0, 0), (-1, 0),  NAVY),
        ("ROWBACKGROUNDS",(0, 1), (-1, -1), [WHITE, LGRAY]),
        ("GRID",          (0, 0), (-1, -1), 0.4, colors.HexColor("#CCCCCC")),
        ("TOPPADDING",    (0, 0), (-1, -1), 7),
        ("BOTTOMPADDING", (0, 0), (-1, -1), 7),
        ("LEFTPADDING",   (0, 0), (-1, -1), 8),
        ("VALIGN",        (0, 0), (-1, -1), "TOP"),
    ]))
    story += [t, SP(12)]

    story += [PageBreak()]

    # ══════════════════════════════════════════════════════════════════════════
    # Chapter 2 — Installation & Quick Start
    # ══════════════════════════════════════════════════════════════════════════
    story += chapter_header(2, "Installation & Quick Start")
    story += h2("Installing NOVA")
    story += [
        P("NOVA is available from GitHub. Install it in R with:", "Body"),
    ]
    story += [code_block('# Recommended\nremotes::install_github("atudoras/nova")\n\n'
                         '# Or using devtools\ndevtools::install_github("atudoras/nova")'), SP(6)]
    story += [
        callout('If you do not have <font name="Courier">remotes</font> installed yet, '
                'run <font name="Courier">install.packages("remotes")</font> first.', "tip"),
        SP(6),
        P("CRAN submission is pending. Once accepted, you will be able to install via:", "Body"),
    ]
    story += [code_block('install.packages("NOVA")'), SP(10)]

    story += h2("Zero-Code Quick Start")
    story += [
        P("The fastest way to run a full NOVA analysis is the included quickstart script. "
          "It requires only one line to be edited:", "Body"),
    ]
    story += [code_block(
        '# 1. Open: Example/nova_quickstart.R\n'
        '# 2. Change this one line to point to your MEA data folder:\n'
        'DATA_DIR <- "/path/to/your/MEA/data/folder"\n\n'
        '# 3. Source the script — all figures save to DATA_DIR/nova_output/\n'
        'source("Example/nova_quickstart.R")'
    ), SP(6)]
    story += [
        callout("The quickstart expects subfolders named MEA001, MEA002… (or any folder "
                "matching MEA followed by digits). Each subfolder should contain CSV files "
                "named with the timepoint (e.g. baseline.csv, 15min.csv).", "note"),
        SP(10),
    ]

    story += h2("Loading the Package")
    story += [code_block('library(NOVA)'), SP(6),
              P("All functions are available after loading. No additional setup is needed.", "Body"),
              PageBreak()]

    # ══════════════════════════════════════════════════════════════════════════
    # Chapter 3 — Data Requirements
    # ══════════════════════════════════════════════════════════════════════════
    story += chapter_header(3, "Data Requirements & Directory Structure")
    story += h2("Expected Folder Layout")
    story += [code_block(
        'MEA_Data/\n'
        '  MEA022b/\n'
        '    baseline.csv\n'
        '    15min.csv\n'
        '    30min.csv\n'
        '    1h.csv\n'
        '    1h30min.csv\n'
        '    2h.csv\n'
        '  MEA022c/\n'
        '    baseline.csv\n'
        '    ...'
    ), SP(6)]
    story += [
        P("<b>Rules:</b>", "BodyLeft"),
        P("• Each experiment is a subfolder (MEA followed by any identifier).", "BulletItem"),
        P("• Each timepoint is a separate CSV file — the filename becomes the timepoint label.", "BulletItem"),
        P("• Folder names and CSV names are used verbatim as labels in all figures.", "BulletItem"),
        SP(8),
    ]

    story += h2("CSV Format")
    story += [
        P("NOVA auto-detects the metadata region using a label scan — "
          "no hardcoded row numbers. The expected format is:", "Body"),
        code_block(
            '# Rows 1–120: plate map / instrument header (auto-skipped)\n'
            '# Row 121+:   data region\n'
            '#\n'
            '# Column A: Well label (A1, B1, C1...)\n'
            '# Column B: Treatment assignment  <- detected by label scan\n'
            '# Column C: Genotype assignment   <- detected by label scan\n'
            '# Columns D+: one column per MEA metric (Firing Rate, etc.)'
        ),
        SP(6),
        callout("If your CSVs do not have Treatment/Genotype columns, NOVA will still "
                "process the data and use the experiment folder name as the group label. "
                "You can assign metadata manually via the config argument.", "tip"),
        SP(10),
    ]

    story += h2("Supported Timepoint Naming")
    story += [
        P("NOVA recognises these timepoint naming conventions and auto-sorts them chronologically:", "Body"),
        code_block(
            '# Supported patterns (case-insensitive):\n'
            'baseline, bl, base\n'
            '15min, 15m, 15\n'
            '30min, 30m, 30\n'
            '1h, 1hour, 60min\n'
            '1h30min, 1h30m, 90min\n'
            '2h, 2hour, 120min\n'
            '1day, 24h, 1d\n\n'
            '# Custom names also work — just specify timepoint_order manually'
        ),
        PageBreak(),
    ]

    # ══════════════════════════════════════════════════════════════════════════
    # Chapter 4 — Core Workflow
    # ══════════════════════════════════════════════════════════════════════════
    story += chapter_header(4, "Core Workflow: Step by Step")
    story += h2("Step 1 — Discover Your Data")
    story += [
        P("Start by scanning the data folder. This builds the structure map that all "
          "downstream functions use.", "Body"),
        code_block(
            'library(NOVA)\n\n'
            'structure <- discover_mea_structure(\n'
            '  main_dir = "/path/to/MEA_Data",\n'
            '  verbose  = TRUE          # prints a summary table\n'
            ')\n\n'
            '# Returns a list with:\n'
            '#   $experiments  — experiment names found\n'
            '#   $timepoints   — timepoints per experiment\n'
            '#   $n_wells      — well counts\n'
            '#   $treatments   — treatment labels detected'
        ),
        SP(6),
    ]
    story += h2("Step 2 — Process the Data")
    story += [
        P("Load all CSVs, assign metadata, and normalise to baseline.", "Body"),
        code_block(
            'result <- process_mea_flexible(\n'
            '  main_dir              = "/path/to/MEA_Data",\n'
            '  grouping_variables    = c("Treatment", "Genotype"),\n'
            '  baseline_timepoint    = "baseline",   # NULL = no normalisation\n'
            '  exclude_std_variables = TRUE,         # remove SD columns\n'
            '  verbose               = TRUE\n'
            ')\n\n'
            '# result$normalized_data  — long-format tidy data frame\n'
            '# result$metadata         — well-level treatment/genotype table\n'
            '# result$summary          — processing summary'
        ),
        SP(10),
    ]
    story += h2("Step 3 — Run PCA")
    story += [
        P("Reduce the multi-metric data to principal components.", "Body"),
        code_block(
            'pca_out <- pca_analysis_enhanced(\n'
            '  processing_result  = result,\n'
            '  n_components       = 10,      # number of PCs to retain\n'
            '  variance_cutoff    = 0.95,    # retain PCs up to 95% variance\n'
            '  scale_data         = TRUE,    # z-score before PCA (recommended)\n'
            '  impute             = TRUE,    # impute missing wells\n'
            '  verbose            = TRUE\n'
            ')\n\n'
            '# pca_out$plot_data  — PC scores with Treatment/Genotype/Timepoint\n'
            '# pca_out$pca_result — prcomp object\n'
            '# pca_out$variance   — variance explained per PC'
        ),
        SP(6),
        callout("PC1 and PC2 together typically explain 50–70% of variance in MEA data. "
                "If less than 50%, consider filtering out low-variance metrics with "
                "min_var before running PCA.", "tip"),
        PageBreak(),
    ]

    # ══════════════════════════════════════════════════════════════════════════
    # Chapter 5 — PCA Trajectory Analysis
    # ══════════════════════════════════════════════════════════════════════════
    story += chapter_header(5, "PCA Trajectory Analysis")
    story += [
        P("Trajectory plots connect each group's PCA centroid across timepoints, "
          "revealing the <i>path</i> each treatment takes through network-state "
          "space. NOVA generates individual-well trajectories, group averages with "
          "error bars, and colour-coded multi-group overlays.", "Body"),
        SP(8),
    ]

    # Show the README trajectory figure
    if (FIGS / "readme_trajectory.png").exists():
        story += fig("readme_trajectory.png", width_cm=13,
                     caption="Figure 5.1 — Group-average PCA trajectories for four treatment groups "
                             "(PBS, KA, Gabazine, NMDA) over 7 timepoints. Open diamond = baseline; "
                             "filled circle = 2 h endpoint. Generated from the MEA Neuronal Agonists dataset.")
        story += [SP(4)]

    story += h2("Basic Trajectory Plot")
    story += [code_block(
        'plots <- plot_pca_trajectories_general(\n'
        '  pca_results         = pca_out,\n'
        '  trajectory_grouping = c("Treatment", "Genotype"),\n'
        '  timepoint_var       = "Timepoint",\n'
        '  timepoint_order     = c("baseline", "15min", "30min", "1h", "1h30min", "2h"),\n'
        '  line_size           = 2,\n'
        '  alpha               = 0.8,\n'
        '  return_list         = TRUE,\n'
        '  save_plots          = FALSE\n'
        ')\n\n'
        '# Access individual plots:\n'
        'plots$combined_avg        # all groups, group averages\n'
        'plots$combined_all        # all individual wells\n'
        'plots$KA_avg              # KA group average only'
    ), SP(6)]

    story += h2("Color by Treatment (New in v0.1.1)")
    story += [
        P("The new <font name='Courier' color='#1F78B4'>color_by</font> parameter "
          "colours each trajectory by a single variable rather than a group combination. "
          "This is ideal for README figures and presentations.", "Body"),
        code_block(
            'plots <- plot_pca_trajectories_general(\n'
            '  pca_results         = pca_out,\n'
            '  color_by            = "Treatment",   # NEW: colour by Treatment only\n'
            '  trajectory_grouping = c("Treatment"),\n'
            '  timepoint_order     = c("baseline", "15min", "30min",\n'
            '                          "1h", "1h30min", "2h")\n'
            ')'
        ),
        SP(8),
    ]

    if (FIGS / "traj_color_by_treatment.png").exists():
        story += fig("traj_color_by_treatment.png", width_cm=12,
                     caption="Figure 5.2 — Trajectories coloured by Treatment. "
                             "Genotype labels appear at the 2 h endpoint via ggrepel.")
        story += [SP(4)]

    story += h2("Saving Trajectory Plots")
    story += [code_block(
        'plot_pca_trajectories_general(\n'
        '  pca_results  = pca_out,\n'
        '  save_plots   = TRUE,\n'
        '  output_dir   = "/path/to/output/",\n'
        '  plot_prefix  = "my_experiment",\n'
        '  width        = 10,\n'
        '  height       = 8,\n'
        '  dpi          = 300\n'
        ')'
    ), SP(6)]

    story += h2("Trajectory Plot Parameters")
    story += [
        param_table([
            ("pca_results",         "required",     "PCA output from pca_analysis_enhanced(), or a data frame with PC1/PC2/Timepoint columns."),
            ("color_by",            "\"group\"",    "Variable to colour trajectories by. Options: \"group\" (Treatment+Genotype combination), \"Treatment\", or any column in plot_data."),
            ("trajectory_grouping", "NULL",         "Character vector of grouping variables. NULL = auto-detect Treatment and Genotype columns."),
            ("timepoint_order",     "NULL",         "Character vector giving chronological order of timepoints. NULL = auto-detect."),
            ("line_size",           "2",            "Line width for trajectory paths."),
            ("alpha",               "0.7",          "Transparency for individual-well trajectories (0=transparent, 1=opaque)."),
            ("smooth_lines",        "FALSE",        "Apply spline smoothing to trajectory curves."),
            ("color_palette",       "NULL",         "Named character vector of hex colours. NULL = auto-generate from built-in scientific palette."),
            ("save_plots",          "FALSE",        "If TRUE, save all plots to output_dir as PNG files."),
            ("dpi",                 "150",          "Resolution for saved plots. Use 300 for publication."),
        ]),
        PageBreak(),
    ]

    # ══════════════════════════════════════════════════════════════════════════
    # Chapter 6 — Heatmap Analysis
    # ══════════════════════════════════════════════════════════════════════════
    story += chapter_header(6, "Heatmap Analysis")
    story += [
        P("NOVA's heatmap function renders the multi-metric MEA data as a clustered "
          "colour grid. Three modes are available: by Treatment, by Genotype, and the "
          "new <b>combination</b> mode (Treatment x Genotype) added in v0.1.1.", "Body"),
        SP(8),
    ]

    if (FIGS / "heatmap_combination.png").exists():
        story += fig("heatmap_combination.png", width_cm=13,
                     caption="Figure 6.1 — Treatment x Genotype combination heatmap with Z-score scaling "
                             "(blue = below mean, red = above mean, capped at +-3 SD). "
                             "Dual annotation strips show Treatment (top) and Genotype (right).")
        story += [SP(4)]

    story += h2("Combination Heatmap (Treatment x Genotype)")
    story += [
        P("The combination heatmap is the recommended default — it shows both experimental "
          "variables simultaneously and uses Z-score normalisation to make all metrics "
          "visually comparable regardless of scale.", "Body"),
        code_block(
            'heatmaps <- create_mea_heatmaps_enhanced(\n'
            '  processing_result = result,\n'
            '  split_by          = "combination",   # Treatment x Genotype\n'
            '  scale_method      = "z_score",\n'
            '  cluster_rows      = TRUE,\n'
            '  cluster_cols      = TRUE,\n'
            '  save_plots        = FALSE\n'
            ')\n\n'
            '# Access:\n'
            'heatmaps$combination_result  # pheatmap object'
        ),
        SP(6),
    ]

    story += h2("Treatment-Only and Genotype-Only Heatmaps")
    story += [code_block(
        '# Treatment only\n'
        'create_mea_heatmaps_enhanced(\n'
        '  processing_result = result,\n'
        '  split_by          = "Treatment"\n'
        ')\n\n'
        '# Genotype only\n'
        'create_mea_heatmaps_enhanced(\n'
        '  processing_result = result,\n'
        '  split_by          = "Genotype"\n'
        ')'
    ), SP(8)]

    if (FIGS / "heatmap_treatment.png").exists():
        story += fig("heatmap_treatment.png", width_cm=11,
                     caption="Figure 6.2 — Treatment-only heatmap, wells averaged per treatment group.")
        story += [SP(4)]

    story += h2("Filtering Data Before Heatmap")
    story += [code_block(
        '# Show only KA and PBS at timepoints 1h and 2h\n'
        'create_mea_heatmaps_enhanced(\n'
        '  processing_result  = result,\n'
        '  filter_treatments  = c("KA", "PBS"),\n'
        '  filter_timepoints  = c("1h", "2h"),\n'
        '  split_by           = "combination"\n'
        ')'
    ), SP(10)]

    story += h2("Heatmap Parameters")
    story += [
        param_table([
            ("split_by",           "NULL",          "How to split wells into heatmap annotation columns. Options: NULL, \"Treatment\", \"Genotype\", \"combination\"."),
            ("scale_method",       "\"z_score\"",   "Normalisation before colour mapping. Options: \"z_score\", \"none\", \"row\", \"column\"."),
            ("cluster_rows",       "TRUE",          "Hierarchical clustering of metrics (rows)."),
            ("cluster_cols",       "TRUE",          "Hierarchical clustering of wells (columns)."),
            ("filter_timepoints",  "NULL",          "Character vector — include only these timepoints. NULL = all."),
            ("filter_treatments",  "NULL",          "Character vector — include only these treatments. NULL = all."),
            ("filter_genotypes",   "NULL",          "Character vector — include only these genotypes. NULL = all."),
            ("use_raw",            "FALSE",         "If TRUE, use raw electrode values instead of normalised values."),
            ("fontsize",           "10",            "Base font size for row/column labels."),
            ("dpi",                "300",           "Resolution for saved plots."),
        ]),
        PageBreak(),
    ]

    # ══════════════════════════════════════════════════════════════════════════
    # Chapter 7 — Per-Metric Visualisation
    # ══════════════════════════════════════════════════════════════════════════
    story += chapter_header(7, "Per-Metric Visualisation")
    story += [
        P("While PCA and heatmaps show the whole dataset, <font name='Courier' color='#1F78B4'>plot_mea_metric()</font> "
          "focuses on a <i>single</i> electrode metric at a time — letting you inspect "
          "firing rate, burst frequency, synchrony, or any other metric in detail "
          "with bar, box, violin or line charts.", "Body"),
        SP(8),
    ]

    story += h2("Bar Chart with SEM Error Bars")
    story += [code_block(
        'plot_mea_metric(\n'
        '  data            = result$normalized_data,\n'
        '  metric          = "Firing Rate",\n'
        '  x_var           = "Timepoint",\n'
        '  group_by        = "Treatment",\n'
        '  plot_type       = "bar",\n'
        '  error_type      = "sem",     # "sem", "sd", or "ci95"\n'
        '  show_points     = TRUE,\n'
        '  point_alpha     = 0.5\n'
        ')'
    ), SP(6)]

    # Show metric figures side by side
    bar_exists    = (FIGS / "metric_bar_firing_rate.png").exists()
    box_exists    = (FIGS / "metric_box_firing_rate.png").exists()
    violin_exists = (FIGS / "metric_violin_bursts.png").exists()

    if bar_exists and box_exists:
        img_w = (PAGE_W - 2 * MARGIN - 0.5 * cm) / 2
        row = [[
            Image(str(FIGS / "metric_bar_firing_rate.png"), width=img_w, height=img_w * 0.8),
            Image(str(FIGS / "metric_box_firing_rate.png"), width=img_w, height=img_w * 0.8),
        ]]
        t = Table(row, colWidths=[img_w, img_w])
        t.setStyle(TableStyle([("VALIGN", (0, 0), (-1, -1), "TOP"),
                                ("ALIGN",  (0, 0), (-1, -1), "CENTER")]))
        story += [t, P("Figure 7.1 — Bar chart with SEM (left) and box plot (right) for "
                        "Firing Rate across treatments over time.", "Caption"), SP(6)]

    story += h2("Box and Violin Plots")
    story += [code_block(
        '# Box plot\n'
        'plot_mea_metric(data=result$normalized_data, metric="Firing Rate",\n'
        '               plot_type="box", group_by="Treatment")\n\n'
        '# Violin plot with individual points\n'
        'plot_mea_metric(data=result$normalized_data, metric="Burst Frequency",\n'
        '               plot_type="violin", show_points=TRUE, point_alpha=0.4)'
    ), SP(6)]

    if violin_exists:
        story += fig("metric_violin_bursts.png", width_cm=11,
                     caption="Figure 7.2 — Violin plot of Burst Frequency across treatment groups, "
                             "with individual well data points overlaid.")
        story += [SP(4)]

    story += h2("Faceting by Genotype")
    story += [code_block(
        '# Split into panels by Genotype\n'
        'plot_mea_metric(\n'
        '  data       = result$normalized_data,\n'
        '  metric     = "Synchrony Index",\n'
        '  x_var      = "Timepoint",\n'
        '  group_by   = "Treatment",\n'
        '  facet_by   = "Genotype",    # one panel per genotype\n'
        '  plot_type  = "bar"\n'
        ')'
    ), SP(6)]

    story += h2("plot_mea_metric Parameters")
    story += [
        param_table([
            ("data",               "required",      "Long-format data frame — typically result$normalized_data from process_mea_flexible()."),
            ("metric",             "required",      "Exact string matching a value in the Variable column (e.g. \"Firing Rate\", \"Burst Frequency\")."),
            ("x_var",              "\"Timepoint\"", "Column to use as the x-axis."),
            ("group_by",           "\"Treatment\"", "Grouping variable for colour and fill."),
            ("facet_by",           "NULL",          "Column to facet (split into separate panels). NULL = no faceting."),
            ("plot_type",          "\"bar\"",       "Chart type: \"bar\", \"box\", \"violin\", or \"line\"."),
            ("error_type",         "\"sem\"",       "Error bar type: \"sem\" (standard error), \"sd\" (standard deviation), \"ci95\" (95% CI)."),
            ("show_points",        "TRUE",          "Overlay individual data points on bar/violin plots."),
            ("filter_treatments",  "NULL",          "Subset of treatments to include. NULL = all."),
            ("filter_timepoints",  "NULL",          "Subset of timepoints to include. NULL = all."),
        ]),
        PageBreak(),
    ]

    # ══════════════════════════════════════════════════════════════════════════
    # Chapter 8 — Customisation
    # ══════════════════════════════════════════════════════════════════════════
    story += chapter_header(8, "Customising Figures")
    story += h2("Custom Colour Palettes")
    story += [
        P("All NOVA plotting functions accept a <font name='Courier' color='#1F78B4'>color_palette</font> "
          "argument — a named character vector mapping group labels to hex colours.", "Body"),
        code_block(
            '# Define your own palette\n'
            'my_palette <- c(\n'
            '  "PBS"      = "#6B7280",   # gray  — control\n'
            '  "KA"       = "#EF4444",   # red   — seizure\n'
            '  "Gabazine" = "#3B82F6",   # blue  — disinhibition\n'
            '  "NMDA"     = "#10B981"    # green — excitatory\n'
            ')\n\n'
            'plot_pca_trajectories_general(\n'
            '  pca_results    = pca_out,\n'
            '  color_palette  = my_palette\n'
            ')'
        ),
        SP(8),
    ]

    if (FIGS / "s3_custom_color.png").exists():
        story += fig("s3_custom_color.png", width_cm=11,
                     caption="Figure 8.1 — PCA scatter with a custom four-colour palette.")
        story += [SP(4)]

    story += h2("Saving All Figures at Once")
    story += [code_block(
        '# Save trajectory plots at publication quality\n'
        'plot_pca_trajectories_general(\n'
        '  pca_results = pca_out,\n'
        '  save_plots  = TRUE,\n'
        '  output_dir  = "figures/trajectories/",\n'
        '  dpi         = 300,\n'
        '  width       = 10,\n'
        '  height      = 8\n'
        ')\n\n'
        '# Save heatmaps\n'
        'create_mea_heatmaps_enhanced(\n'
        '  processing_result = result,\n'
        '  save_plots        = TRUE,\n'
        '  output_dir        = "figures/heatmaps/",\n'
        '  plot_format       = "png",   # or "pdf", "svg"\n'
        '  dpi               = 300\n'
        ')'
    ), SP(8)]

    story += h2("Changing PCA Components")
    story += [code_block(
        '# Plot PC3 vs PC4 instead of PC1 vs PC2\n'
        'plot_pca_trajectories_general(\n'
        '  pca_results = pca_out,\n'
        '  pc_x        = "PC3",\n'
        '  pc_y        = "PC4"\n'
        ')'
    ), PageBreak()]

    # ══════════════════════════════════════════════════════════════════════════
    # Chapter 9 — Complete Function Reference
    # ══════════════════════════════════════════════════════════════════════════
    story += chapter_header(9, "Complete Function Reference")
    story += [
        P("All exported NOVA functions are listed below with their full signatures. "
          "Access in-line help with <font name='Courier' color='#1F78B4'>?function_name</font>.", "Body"),
        SP(6),
    ]

    funcs = [
        ("discover_mea_structure()", [
            ("main_dir",            "required",   "Path to the folder containing MEA experiment subfolders."),
            ("experiment_pattern",  '"MEA\\\\d+"', "Regex matching experiment subfolder names."),
            ("file_pattern",        '"\\\\.csv$"', "Regex matching CSV filenames to include."),
            ("verbose",             "TRUE",        "Print a discovery summary table."),
        ]),
        ("process_mea_flexible()", [
            ("main_dir",              "required",             "Path to MEA data folder."),
            ("selected_experiments",  "NULL",                 "Character vector — process only these experiments. NULL = all."),
            ("selected_timepoints",   "NULL",                 "Character vector — include only these timepoints. NULL = all."),
            ("grouping_variables",    'c("Treatment","Genotype")', "Column names to use as grouping factors."),
            ("baseline_timepoint",    "NULL",                 "Timepoint name to use as baseline (values divided by baseline mean). NULL = no normalisation."),
            ("exclude_std_variables", "TRUE",                 "Drop standard-deviation metric columns."),
            ("verbose",               "TRUE",                 "Print processing log."),
        ]),
        ("pca_analysis_enhanced()", [
            ("processing_result", "NULL",   "Output of process_mea_flexible(). Required if normalized_data is NULL."),
            ("n_components",      "10",     "Number of principal components to compute."),
            ("variance_cutoff",   "0.95",   "Retain components explaining this cumulative variance."),
            ("scale_data",        "TRUE",   "Z-score metrics before PCA (strongly recommended)."),
            ("impute",            "TRUE",   "Impute missing wells using column mean."),
            ("min_var",           "NULL",   "Minimum variance threshold; metrics below this are excluded."),
            ("value_column",      '"Normalized_Value"', "Column containing metric values."),
            ("verbose",           "TRUE",   "Print PCA summary."),
        ]),
        ("plot_pca_trajectories_general()", [
            ("pca_results",         "required",    "PCA output object or data frame with PC columns."),
            ("color_by",            '"group"',     "Colour variable: \"group\", \"Treatment\", or any column name."),
            ("trajectory_grouping", "NULL",        "Grouping variables for trajectory lines. NULL = auto-detect."),
            ("timepoint_var",       '"Timepoint"', "Column identifying timepoints."),
            ("timepoint_order",     "NULL",        "Chronological order of timepoints. NULL = auto-sort."),
            ("individual_var",      '"Experiment"',"Variable identifying individual wells/experiments."),
            ("point_size",          "3",           "Size of timepoint dots."),
            ("line_size",           "2",           "Line width for trajectory paths."),
            ("alpha",               "0.7",         "Transparency for individual traces."),
            ("smooth_lines",        "FALSE",       "Apply spline smoothing."),
            ("color_palette",       "NULL",        "Named colour vector. NULL = built-in palette."),
            ("save_plots",          "FALSE",       "Save to output_dir."),
            ("dpi",                 "150",         "Resolution for saved files."),
            ("return_list",         "TRUE",        "Return list of all plot objects."),
        ]),
        ("create_mea_heatmaps_enhanced()", [
            ("processing_result",    "NULL",          "Output of process_mea_flexible()."),
            ("split_by",             "NULL",          "NULL, \"Treatment\", \"Genotype\", or \"combination\"."),
            ("scale_method",         '"z_score"',     "\"z_score\", \"none\", \"row\", or \"column\"."),
            ("cluster_rows",         "TRUE",          "Cluster metrics (rows)."),
            ("cluster_cols",         "TRUE",          "Cluster wells (columns)."),
            ("filter_timepoints",    "NULL",          "Include only these timepoints."),
            ("filter_treatments",    "NULL",          "Include only these treatments."),
            ("filter_genotypes",     "NULL",          "Include only these genotypes."),
            ("use_raw",              "FALSE",         "Use raw values instead of normalised values."),
            ("quality_threshold",    "0.8",           "Minimum data completeness (0–1) for a well to be included."),
            ("min_observations",     "3",             "Minimum number of observations per well."),
            ("save_plots",           "FALSE",         "Save to output_dir."),
            ("dpi",                  "300",           "Resolution."),
        ]),
        ("plot_mea_metric()", [
            ("data",               "required",      "Long-format data frame (result$normalized_data)."),
            ("metric",             "required",      "Metric name string matching Variable column."),
            ("x_var",              '"Timepoint"',   "X-axis variable."),
            ("group_by",           '"Treatment"',   "Colour/fill grouping variable."),
            ("facet_by",           "NULL",          "Faceting variable. NULL = no facets."),
            ("plot_type",          '"bar"',         "\"bar\", \"box\", \"violin\", or \"line\"."),
            ("error_type",         '"sem"',         "\"sem\", \"sd\", or \"ci95\"."),
            ("show_points",        "TRUE",          "Overlay individual data points."),
            ("point_alpha",        "0.6",           "Transparency for data points."),
            ("colors",             "NULL",          "Named colour vector. NULL = auto."),
        ]),
    ]

    for fname, params in funcs:
        story += h3(fname)
        story += [param_table(params), SP(8)]

    story.append(PageBreak())

    # ══════════════════════════════════════════════════════════════════════════
    # Chapter 10 — Troubleshooting
    # ══════════════════════════════════════════════════════════════════════════
    story += chapter_header(10, "Troubleshooting")

    issues = [
        ("discover_mea_structure() finds 0 experiments",
         'Check that your folder contains subfolders matching the pattern '
         '"MEA" followed by digits (e.g. MEA001, MEA022b). '
         'If your folders are named differently, set experiment_pattern to a '
         'regex that matches them, e.g. experiment_pattern = "Plate\\\\d+".',
         "tip"),
        ("CSV files are read but all values are NA",
         'NOVA auto-detects the data region by scanning for the label row. '
         'If your CSV header is at a non-standard position, the scan may fail. '
         'Check your CSV structure and ensure the column containing well labels '
         '(A1, B1...) is present.', "warn"),
        ("PCA plot shows only one cluster / no separation",
         'This usually means baseline normalisation collapsed the variance. '
         'Try setting baseline_timepoint = NULL in process_mea_flexible() '
         'to use raw values, or check that your baseline CSV has sufficient coverage.', "note"),
        ("Trajectory lines look random / cross unexpectedly",
         'Check timepoint_order — if timepoints are not in the correct chronological '
         'order, trajectories will zigzag. Always specify timepoint_order explicitly '
         'when using non-standard timepoint names.', "warn"),
        ("Heatmap shows only one colour (all red or all blue)",
         'This indicates extreme outlier wells are dominating the colour scale. '
         'The combination heatmap already caps Z-scores at +-3 SD, but if using '
         'scale_method = "none", try switching to "z_score".', "note"),
        ("Error: package ggrepel not found",
         'Install ggrepel: install.packages("ggrepel"). '
         'It is required for trajectory end-point labels.', "tip"),
        ("Saved figures are blurry",
         'Increase dpi to 300 (publication standard) in save_plots calls. '
         'The default dpi = 150 is suitable for screen preview only.', "tip"),
    ]

    for title, text, _ in issues:
        story += h3(title)
        story += [P(text, "Body"), SP(6)]

    story += [
        SP(10), HR(BLUE),
        P("For additional help, open an issue at "
          "<b>github.com/atudoras/nova/issues</b> or email "
          "alex.tudorasmiravet@ucsf.edu.", "Body"),
        SP(20),
        P("NOVA v0.1.1  •  GPL (>= 3)  •  Alex Tudoras, UCSF  •  2024–2026", "Caption"),
    ]

    return story


# ── Build ──────────────────────────────────────────────────────────────────────
if __name__ == "__main__":
    OUT.parent.mkdir(parents=True, exist_ok=True)
    doc   = NOVADoc(str(OUT))
    story = build_story()
    doc.multiBuild(story)   # multiBuild needed for TOC page numbers
    print(f"\nPDF written: {OUT}  ({OUT.stat().st_size // 1024} KB)")
