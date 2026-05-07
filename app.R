
# ============================================================
# SIMplyBee Educational Shiny App
# Corrected version: same teaching structure, but with
# session-safe simulation state and package-faithful calculations.
# Merged final version from three app variants.
# ============================================================

library(shiny)
library(SIMplyBee)
library(AlphaSimR)
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)

# ============================================================
# Helper palette & theme
# ============================================================
bee_colors <- c(
  gold       = "#F5A623",
  dark_gold  = "#C8861B",
  black      = "#1A1A1A",
  honey      = "#FFC857",
  cream      = "#FFF8E7",
  brown      = "#8B5E3C",
  green      = "#4CAF50",
  blue       = "#2196F3",
  purple     = "#9C27B0",
  red        = "#F44336",
  grey       = "#757575"
)

theme_bee <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.background   = element_rect(fill = "#FFF8E7", color = NA),
      panel.background  = element_rect(fill = "#FFF8E7", color = NA),
      panel.grid.major  = element_line(color = "#E0C97A", linewidth = 0.3),
      panel.grid.minor  = element_blank(),
      plot.title        = element_text(face = "bold", color = "#8B5E3C", size = 15),
      plot.subtitle     = element_text(color = "#8B5E3C", size = 11),
      axis.title        = element_text(color = "#1A1A1A", face = "bold"),
      legend.background = element_rect(fill = "#FFF8E7", color = NA),
      strip.background  = element_rect(fill = "#F5A623", color = NA),
      strip.text        = element_text(color = "white", face = "bold")
    )
}

DEFAULT_APP_SEED <- 1234L

# ============================================================
# UI
# ============================================================
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { background-color: #FFF8E7; font-family: 'Segoe UI', sans-serif; }

      /* ---- Navbar ---- */
      .navbar { background-color: #F5A623 !important; border: none; }
      .navbar-brand { color: #1A1A1A !important; font-weight: bold; font-size: 20px; }
      .navbar-nav > li > a { color: #1A1A1A !important; font-weight: 600; }
      .navbar-nav > li > a:hover { background-color: #C8861B !important; color: white !important; }
      .navbar-nav > .active > a,
      .navbar-nav > .active > a:focus,
      .navbar-nav > .active > a:hover { background-color: #8B5E3C !important; color: white !important; }

      /* ---- Cards / boxes ---- */
      .bee-card {
        background: white;
        border-radius: 10px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.10);
        padding: 20px;
        margin-bottom: 18px;
        border-left: 5px solid #F5A623;
      }
      .bee-card h4 { color: #8B5E3C; font-weight: bold; margin-top: 0; }

      /* ---- Info boxes ---- */
      .info-box {
        background: #FFF3CD;
        border: 1px solid #F5A623;
        border-radius: 8px;
        padding: 14px 18px;
        margin-bottom: 14px;
      }
      .info-box .title { font-weight: bold; color: #8B5E3C; }

      /* ---- Metric boxes ---- */
      .metric-box {
        background: #F5A623;
        border-radius: 10px;
        padding: 18px;
        text-align: center;
        color: white;
        margin: 6px;
        box-shadow: 0 2px 6px rgba(0,0,0,0.15);
      }
      .metric-box .metric-value { font-size: 2.4em; font-weight: bold; line-height: 1; }
      .metric-box .metric-label { font-size: 0.85em; margin-top: 4px; opacity: 0.9; }
      .metric-box.queen-box   { background: #9C27B0; }
      .metric-box.worker-box  { background: #F5A623; }
      .metric-box.drone-box   { background: #2196F3; }
      .metric-box.vq-box      { background: #4CAF50; }
      .metric-box.father-box  { background: #8B5E3C; }

      /* ---- Buttons ---- */
      .btn-bee {
        background-color: #F5A623; color: white;
        border: none; border-radius: 6px;
        font-weight: 600; padding: 8px 20px;
      }
      .btn-bee:hover { background-color: #C8861B; color: white; }
      .btn-run { background-color: #4CAF50; color: white; border: none;
                 border-radius: 6px; font-weight: 600; padding: 10px 24px; }
      .btn-run:hover { background-color: #388E3C; color: white; }

      /* ---- Code blocks ---- */
      .r-code {
        background: #1A1A1A; color: #F5A623;
        border-radius: 8px; padding: 14px 18px;
        font-family: 'Courier New', monospace;
        font-size: 0.88em; margin: 10px 0;
        white-space: pre-wrap; word-wrap: break-word;
      }

      /* ---- Tabs inside panels ---- */
      .nav-tabs > li > a { color: #8B5E3C; }
      .nav-tabs > li.active > a { background: #F5A623; color: white; border-color: #F5A623; }

      /* ---- Hero banner ---- */
      .hero-banner {
        background: linear-gradient(135deg, #F5A623 0%, #8B5E3C 100%);
        color: white; border-radius: 12px;
        padding: 30px 36px; margin-bottom: 24px;
      }
      .hero-banner h2 { margin-top: 0; font-size: 28px; }

      /* ---- Section divider ---- */
      hr.bee-hr { border-color: #F5A623; margin: 20px 0; }

      /* ---- Status text ---- */
      .status-ok   { color: #4CAF50; font-weight: bold; }
      .status-warn { color: #F44336; font-weight: bold; }
    "))
  ),

  navbarPage(
    title = "🐝 SIMplyBee Explorer",
    id    = "main_tabs",

    # ===========================================================
    # TAB 1 – About
    # ===========================================================
    tabPanel("About",
      br(),
      div(class = "hero-banner",
        h2("🐝 SIMplyBee Explorer"),
        p("An interactive Shiny application to learn and demonstrate the",
          tags$strong("SIMplyBee"), "R package — a stochastic simulator of",
          "honeybee populations and breeding programmes, built on top of",
          tags$strong("AlphaSimR"), "."),
        p(tags$b("S. Biffani"),
          " — IBBA-CNR & Dipartimento di Scienze Medico-Veterinarie, UNIPR")
      ),
      fluidRow(
        column(4,
          div(class = "bee-card",
            h4("📦 What is SIMplyBee?"),
            p("SIMplyBee simulates individual bees that form a colony including:"),
            tags$ul(
              tags$li("👑 A ", tags$b("queen")),
              tags$li("🐝 ", tags$b("Workers"), " (fertilised females)"),
              tags$li("🐝 ", tags$b("Drones"), " (haploid males)"),
              tags$li("🌸 ", tags$b("Virgin queens")),
              tags$li("💑 ", tags$b("Fathers"), " (mating drones stored with the queen)")
            ),
            p("Multiple colonies can be studied together as an apiary-level simulation."),
            p(tags$small("Teaching tip: with one father, CSD risk may still be 0% under open mating; use the inbred brother-mating option in Colony Setup for a stronger classroom demo."))
          )
        ),
        column(4,
          div(class = "bee-card",
            h4("🔬 Key Features"),
            tags$ul(
              tags$li("Haplo-diploid inheritance"),
              tags$li("Complementary sex determiner (csd) locus"),
              tags$li("Colony events: swarming, supersedure, collapse"),
              tags$li("Quantitative genetics & colony values"),
              tags$li("Genomic information & pedigree-based parentage"),
              tags$li("Full AlphaSimR compatibility")
            )
          )
        ),
        column(4,
          div(class = "bee-card",
            h4("🗺️ App Navigation"),
            tags$ol(
              tags$li(tags$b("Colony Setup"), " – Found genomes and build a colony"),
              tags$li(tags$b("Colony Structure"), " – Visualise castes, CSD, and parentage"),
              tags$li(tags$b("Colony Events"), " – Simulate swarm / supersede / collapse"),
              tags$li(tags$b("Quant. Genetics"), " – Define traits and simulate colony values"),
              tags$li(tags$b("Multi-Colony"), " – Simulate an apiary")
            )
          )
        )
      ),
      fluidRow(
        column(12,
          div(class = "bee-card",
            h4("⚡ Minimal Working Example"),
            div(class = "r-code",
"library(SIMplyBee)
library(AlphaSimR)

# 1. Create founder genomes
founderGenomes <- quickHaplo(nInd = 6, nChr = 1, segSites = 100)

# 2. Set up global simulation parameters
SP <- SimParamBee$new(founderGenomes)

# 3. Create virgin queens from founder genomes
basePop <- createVirginQueens(founderGenomes, simParamBee = SP)

# 4. Create a drone congregation area (DCA)
DCA <- createDrones(basePop[2:6], nInd = 100, simParamBee = SP)
droneGroup <- pullDroneGroupsFromDCA(DCA, n = 1, nDrones = 15, simParamBee = SP)

# 5. Create and mate a colony queen
colony <- createColony(x = basePop[1], simParamBee = SP)
colony <- cross(colony, drones = droneGroup[[1]], checkCross = 'warning', simParamBee = SP)

# 6. Build up the colony
colony <- buildUp(colony, nWorkers = 1000, nDrones = 500, exact = TRUE, simParamBee = SP)

# 7. Inspect
nWorkers(colony)
nDrones(colony)
pHomBrood(colony, simParamBee = SP)")
          )
        )
      )
    ),

    # ===========================================================
    # TAB 2 – Colony Setup
    # ===========================================================
    tabPanel("Colony Setup",
      br(),
      fluidRow(
        column(4,
          div(class = "bee-card",
            h4("⚙️ Simulation Parameters"),
            sliderInput("nFounders",  "Number of founder genomes:", 3, 20, 6),
            sliderInput("nChr",       "Number of chromosomes:",     1, 16, 1),
            sliderInput("nSegSites",  "Segregating sites per chr:", 50, 500, 100),
            sliderInput("nCsdAlleles", "Possible csd alleles:", 2, 128, 128, step = 1),
            sliderInput("nFathers",   "Fathers (drones) queen mates with:", 1, 40, 12),
            sliderInput("nWorkers_c", "Workers at build-up:",       200, 5000, 1000, step = 100),
            sliderInput("nDrones_c",  "Resident drones at build-up:", 100, 2000, 500, step = 100),
            radioButtons("matingDesign", "Mating design:",
              choices = c(
                "Open DCA mating (biologically realistic)" = "open",
                "Inbred brother mating (classroom CSD demo)" = "inbred"
              ),
              selected = "open"
            ),
            helpText("Tip: with one father, open mating often still gives 0% CSD risk because the father may share neither queen csd haplotype. Use the inbred demo to make matching much more likely."),
            numericInput("seedBase", "Shared random seed (used in all simulation tabs):",
              value = DEFAULT_APP_SEED, min = 1, step = 1, width = "100%"),
            helpText("All stochastic tabs derive reproducible, module-specific seeds from this base value."),
            actionButton("runSetup", "🚀 Create Colony", class = "btn-run", width = "100%")
          )
        ),
        column(8,
          div(class = "bee-card",
            h4("📋 Colony Setup Log"),
            verbatimTextOutput("setupLog")
          ),
          br(),
          div(class = "bee-card",
            h4("🧬 Founder Genome Summary"),
            plotOutput("genomePlot", height = "260px")
          ),
          br(),
          div(class = "bee-card",
            h4("🔑 R Code Used"),
            uiOutput("setupCode")
          )
        )
      )
    ),

    # ===========================================================
    # TAB 3 – Colony Structure
    # ===========================================================
    tabPanel("Colony Structure",
      br(),
      fluidRow(
        column(12,
          div(class = "info-box",
            span(class = "title", "ℹ️ Note: "),
            "Run the ", tags$b("Colony Setup"), " tab first to create a colony before exploring its structure."
          )
        )
      ),
      fluidRow(
        column(3,
          div(class = "metric-box queen-box",
            div(class = "metric-value", textOutput("nQueens", container = span)),
            div(class = "metric-label", "👑 Queens")
          )
        ),
        column(3,
          div(class = "metric-box worker-box",
            div(class = "metric-value", textOutput("nWorkersOut", container = span)),
            div(class = "metric-label", "🐝 Workers")
          )
        ),
        column(3,
          div(class = "metric-box drone-box",
            div(class = "metric-value", textOutput("nDronesOut", container = span)),
            div(class = "metric-label", "🐝 Drones")
          )
        ),
        column(3,
          div(class = "metric-box father-box",
            div(class = "metric-value", textOutput("nFathersOut", container = span)),
            div(class = "metric-label", "💑 Stored fathers")
          )
        )
      ),
      br(),
      fluidRow(
        column(6,
          div(class = "bee-card",
            h4("🍩 Live Colony Members"),
            p("This plot shows castes physically represented in the colony. Fathers are tracked separately as stored mating drones."),
            plotOutput("castePie", height = "320px")
          )
        ),
        column(6,
          div(class = "bee-card",
            h4("🧬 CSD Homozygous Brood"),
            p("CSD brood risk is computed directly for the current colony with SIMplyBee's CSD functions. The panel below also shows the exact queen and stored-father csd haplotypes used in that calculation."),
            uiOutput("csdBox"),
            uiOutput("csdAlleleBox"),
            tableOutput("csdFatherTable"),
            plotOutput("csdPlot", height = "260px")
          )
        )
      ),
      fluidRow(
        column(12,
          div(class = "bee-card",
            h4("📊 Worker Relatedness and Parentage"),
            p("Worker sisterhood is computed from realised worker parentage when available, and falls back to the stored number of fathers otherwise."),
            plotOutput("relPlot", height = "260px")
          )
        )
      )
    ),

    # ===========================================================
    # TAB 4 – Colony Events
    # ===========================================================
    tabPanel("Colony Events",
      br(),
      fluidRow(
        column(12,
          div(class = "info-box",
            span(class = "title", "ℹ️ Note: "),
            "Requires a colony created in the ", tags$b("Colony Setup"), " tab."
          )
        )
      ),
      fluidRow(
        column(4,
          div(class = "bee-card",
            h4("🌪️ Simulate Event"),
            radioButtons("eventType", "Choose colony event:",
              choices = c(
                "Swarm (colony reproduces)"   = "swarm",
                "Supersede (queen replaced)"  = "supersede",
                "Collapse (colony dies)"      = "collapse"
              )
            ),
            conditionalPanel("input.eventType == 'swarm'",
              sliderInput("swarmProp", "Proportion leaving with swarm:", 0.1, 0.7, 0.4, 0.05)
            ),
            actionButton("runEvent", "⚡ Simulate Event", class = "btn-bee", width = "100%"),
            br(), br(),
            div(class = "r-code", uiOutput("eventCode"))
          )
        ),
        column(8,
          div(class = "bee-card",
            h4("📋 Event Log"),
            verbatimTextOutput("eventLog")
          ),
          br(),
          div(class = "bee-card",
            h4("📊 Before vs After Event"),
            plotOutput("eventPlot", height = "300px")
          )
        )
      )
    ),

    # ===========================================================
    # TAB 5 – Quantitative Genetics
    # ===========================================================
    tabPanel("Quant. Genetics",
      br(),
      fluidRow(
        column(4,
          div(class = "bee-card",
            h4("🍯 Trait Definition"),
            p("Colony honey yield is modelled as queen contribution + summed worker contribution."),
            tags$ul(
              tags$li(tags$b("Queen trait"), ": colony-level queen effect"),
              tags$li(tags$b("Worker trait"), ": colony-level total worker effect, internally scaled by worker number")
            ),
            sliderInput("meanQ",   "Queen trait mean:", 1, 20, 10),
            sliderInput("meanW",   "Worker trait mean (colony total):", 1, 20, 10),
            sliderInput("h2",      "Heritability (h²):", 0.05, 0.9, 0.25, step = 0.05),
            sliderInput("corA",    "Genetic correlation Q–W:", -0.9, 0.9, -0.5, step = 0.05),
            sliderInput("envSD",   "Additional colony environmental SD:", 0, 30, 0, step = 1),
            sliderInput("nColQG",  "Number of colonies:", 5, 40, 20),
            sliderInput("nWrkQG",  "Workers per colony:", 100, 2000, 500, step = 100),
            actionButton("runQG", "🧬 Run Simulation", class = "btn-run", width = "100%")
          )
        ),
        column(8,
          div(class = "bee-card",
            h4("📊 Colony Honey Yield Distribution"),
            plotOutput("qgPlot", height = "300px")
          ),
          br(),
          div(class = "bee-card",
            h4("🔬 Genetic Signal and Selection Response"),
            p("These plots are meant to respond to heritability: higher h² makes colony phenotype track genetic value more closely, so selection on phenotype captures more genetic merit."),
            plotOutput("bvPlot", height = "460px")
          ),
          br(),
          div(class = "bee-card",
            h4("📋 Summary Statistics"),
            tableOutput("qgTable")
          )
        )
      )
    ),

    # ===========================================================
    # TAB 6 – Multi-Colony
    # ===========================================================
    tabPanel("Multi-Colony",
      br(),
      fluidRow(
        column(4,
          div(class = "bee-card",
            h4("🏠 Apiary Setup"),
            sliderInput("nColonies", "Number of colonies in apiary:", 2, 20, 8),
            sliderInput("nFoundersMC", "Founder genomes:", 3, 20, 10),
            sliderInput("nWorkersMC", "Workers per colony:", 200, 3000, 800, step = 100),
            sliderInput("nCsdAllelesMC", "Possible csd alleles:", 2, 128, 128, step = 1),
            checkboxInput("doSwarmMC", "Simulate swarming in some colonies?", FALSE),
            conditionalPanel("input.doSwarmMC",
              sliderInput("swarmPropMC", "Proportion of colonies that swarm:", 0.1, 0.8, 0.3, step = 0.05)
            ),
            actionButton("runMC", "🚀 Create Apiary", class = "btn-run", width = "100%")
          )
        ),
        column(8,
          div(class = "bee-card",
            h4("📊 Colony Sizes in Apiary"),
            plotOutput("mcPlot", height = "300px")
          ),
          br(),
          div(class = "bee-card",
            h4("🌡️ Colony Status"),
            tableOutput("mcTable")
          ),
          br(),
          div(class = "bee-card",
            h4("📋 MultiColony Log"),
            verbatimTextOutput("mcLog")
          )
        )
      )
    )
  )
)

# ============================================================
# SERVER
# ============================================================
server <- function(input, output, session) {

  # ---- Reactive simulation state ----
  sim_state <- reactiveValues(
    colony       = NULL,
    SP           = NULL,
    founderGenomes = NULL,
    founderSummary = NULL,
    basePop      = NULL,
    DCA          = NULL,
    setup_info   = NULL,
    before_event = NULL,
    event_done   = FALSE,
    event_type   = NULL,
    event_seed   = NULL,
    swarm_out    = NULL
  )

  placeholder_plot <- function(title, label, subtitle = NULL) {
    ggplot() +
      xlim(0, 1) + ylim(0, 1) +
      annotate("text", x = 0.5, y = 0.5, label = label,
               color = bee_colors[["brown"]], fontface = "bold", size = 5) +
      labs(title = title, subtitle = subtitle) +
      theme_void() +
      theme(
        plot.background = element_rect(fill = "#FFF8E7", color = NA),
        plot.title = element_text(face = "bold", color = "#8B5E3C", size = 15),
        plot.subtitle = element_text(color = "#8B5E3C", size = 11)
      )
  }

  colony_counts <- function(col) {
    if (is.null(col)) {
      return(c(
        Queens = 0L,
        VirginQueens = 0L,
        Workers = 0L,
        Drones = 0L,
        Fathers = 0L
      ))
    }

    c(
      Queens       = tryCatch(as.integer(nQueens(col)), error = function(e) 0L),
      VirginQueens = tryCatch(as.integer(nVirginQueens(col)), error = function(e) 0L),
      Workers      = tryCatch(as.integer(nWorkers(col)), error = function(e) 0L),
      Drones       = tryCatch(as.integer(nDrones(col)), error = function(e) 0L),
      Fathers      = tryCatch(as.integer(nFathers(col)), error = function(e) 0L)
    )
  }

  queen_state <- function(col) {
    if (is.null(col)) {
      return("No colony")
    }

    nq  <- tryCatch(as.integer(nQueens(col)), error = function(e) 0L)
    nvq <- tryCatch(as.integer(nVirginQueens(col)), error = function(e) 0L)
    nf  <- tryCatch(as.integer(nFathers(col)), error = function(e) 0L)

    if (nq > 0L && nf > 0L) {
      "Mated queen"
    } else if (nq > 0L && nf == 0L) {
      "Queen present, no stored fathers"
    } else if (nvq > 0L) {
      "Virgin queen"
    } else {
      "No queen"
    }
  }

  founder_summary_df <- function(founderGenomes) {
    geno <- tryCatch(AlphaSimR::pullSegSiteGeno(founderGenomes), error = function(e) NULL)

    if (is.null(geno)) {
      return(data.frame(
        Individual = "Founder 1",
        AltAlleles = 0,
        MeanAlleleCount = 0,
        Heterozygosity = 0,
        stringsAsFactors = FALSE
      ))
    }

    if (is.list(geno) && !is.data.frame(geno)) {
      geno <- tryCatch(do.call(rbind, geno), error = function(e) NULL)
    }

    geno <- as.matrix(geno)
    if (is.null(dim(geno))) {
      geno <- matrix(geno, nrow = 1L)
    }

    data.frame(
      Individual      = paste0("Founder ", seq_len(nrow(geno))),
      AltAlleles      = rowSums(geno, na.rm = TRUE),
      MeanAlleleCount = rowMeans(geno, na.rm = TRUE),
      Heterozygosity  = rowMeans(geno == 1, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }

  safe_seg_sites <- function(seg_sites, n_csd_alleles, minimum = 10L) {
    max(as.integer(seg_sites), as.integer(n_csd_alleles), as.integer(minimum))
  }


  # Some SIMplyBee / AlphaSimR internals still consult a global `SP`
  # object even when `simParamBee` is passed explicitly. We therefore
  # bind the session's SimParamBee object to `.GlobalEnv$SP` only for
  # the duration of the relevant calculation and then restore the prior
  # value immediately afterwards.
  with_global_SP <- function(sp_obj, expr) {
    expr_sub <- substitute(expr)

    if (is.null(sp_obj)) {
      return(eval(expr_sub, envir = parent.frame()))
    }

    had_sp <- exists("SP", envir = .GlobalEnv, inherits = FALSE)
    if (had_sp) {
      old_sp <- get("SP", envir = .GlobalEnv, inherits = FALSE)
    }

    assign("SP", sp_obj, envir = .GlobalEnv)

    on.exit({
      if (had_sp) {
        assign("SP", old_sp, envir = .GlobalEnv)
      } else if (exists("SP", envir = .GlobalEnv, inherits = FALSE)) {
        rm("SP", envir = .GlobalEnv)
      }
    }, add = TRUE)

    eval(expr_sub, envir = parent.frame())
  }

  prepare_SP <- function(SP) {
    tryCatch({
      SP$nThreads <- 1L
    }, error = function(e) NULL)

    tryCatch({
      SP$setTrackPed(isTrackPed = TRUE)
    }, error = function(e) NULL)

    SP
  }

  make_dca <- function(queen_pop, donor_idx, n_groups, n_fathers, SP) {
    donor_idx <- donor_idx[donor_idx >= 1 & donor_idx <= nInd(queen_pop)]
    if (length(donor_idx) < 1L) {
      stop("No valid donor queens available for the DCA.")
    }

    required_total <- max(1L, as.integer(n_groups)) * max(1L, as.integer(n_fathers))
    drones_per_donor <- max(100L, ceiling(2 * required_total / length(donor_idx)))

    dca_parts <- lapply(donor_idx, function(i) {
      createDrones(x = queen_pop[i], nInd = drones_per_donor, simParamBee = SP)
    })

    DCA <- if (length(dca_parts) == 1L) dca_parts[[1]] else do.call(c, dca_parts)

    list(
      DCA = DCA,
      donors = length(donor_idx),
      drones_per_donor = drones_per_donor,
      total_dca_drones = length(donor_idx) * drones_per_donor
    )
  }

  csd_strings_from_object <- function(x) {
    if (is.null(x)) {
      return(character(0))
    }

    if (is.list(x) && !is.data.frame(x) && !is.matrix(x)) {
      out <- unlist(lapply(x, csd_strings_from_object), use.names = FALSE)
      out <- out[!is.na(out) & nzchar(out)]
      return(unname(out))
    }

    if (is.data.frame(x)) {
      x <- as.matrix(x)
    }

    if (is.null(dim(x))) {
      x <- matrix(x, nrow = 1L)
    }

    x <- as.matrix(x)

    if (length(x) < 1L || nrow(x) < 1L) {
      return(character(0))
    }

    out <- apply(
      X = x,
      MARGIN = 1,
      FUN = function(row) paste0(as.character(row), collapse = "")
    )

    out <- out[!is.na(out) & nzchar(out)]
    unname(out)
  }

  get_csd_metrics <- function(col, SP) {
    with_global_SP(SP, {
      if (is.null(col)) {
        return(list(available = FALSE, reason = "No colony available yet."))
      }

      nq <- tryCatch(as.integer(nQueens(col)), error = function(e) 0L)
      nf <- tryCatch(as.integer(nFathers(col)), error = function(e) 0L)

      if (nq < 1L || nf < 1L) {
        return(list(
          available = FALSE,
          reason = "CSD brood risk is defined only for a mated queen with stored fathers."
        ))
      }

      queen_haplotypes <- tryCatch(
        csd_strings_from_object(getQueenCsdAlleles(col, simParamBee = SP)),
        error = function(e) character(0)
      )
      father_haplotypes <- tryCatch(
        csd_strings_from_object(getFathersCsdAlleles(col, simParamBee = SP)),
        error = function(e) character(0)
      )

      if (length(queen_haplotypes) < 1L || length(father_haplotypes) < 1L) {
        return(list(
          available = FALSE,
          reason = "SIMplyBee could not recover the queen/father csd haplotypes for the current colony."
        ))
      }

      father_matches <- father_haplotypes %in% unique(queen_haplotypes)
      matched_father_idx <- which(father_matches)
      matching_haplotypes <- unique(father_haplotypes[father_matches])

      p_hom_from_alleles <- sum(father_matches) / (length(queen_haplotypes) * length(father_haplotypes))
      p_hom <- tryCatch(as.numeric(pHomBrood(col, simParamBee = SP)), error = function(e) NA_real_)
      if (!is.finite(p_hom)) {
        p_hom <- p_hom_from_alleles
      }

      n_hom <- tryCatch(as.numeric(nHomBrood(col, simParamBee = SP)), error = function(e) NA_real_)
      n_hom_label <- if (is.finite(n_hom)) {
        format(round(n_hom), big.mark = ",")
      } else {
        "Not yet recorded"
      }

      formula_label <- sprintf(
        "%d / (%d x %d)",
        length(matched_father_idx),
        length(queen_haplotypes),
        length(father_haplotypes)
      )

      interpretation <- if (length(matched_father_idx) < 1L) {
        "No stored father shares either queen csd haplotype, so expected CSD-homozygous brood is 0%."
      } else if (length(father_haplotypes) == 1L) {
        sprintf(
          "This single stored father shares one queen csd haplotype, so expected CSD-homozygous brood is %s = %.2f%%.",
          formula_label,
          100 * p_hom_from_alleles
        )
      } else {
        sprintf(
          "%d of %d stored fathers share a queen csd haplotype. SIMplyBee's exact rule is %s = %.2f%%.",
          length(matched_father_idx),
          length(father_haplotypes),
          formula_label,
          100 * p_hom_from_alleles
        )
      }

      father_table <- data.frame(
        "Stored father" = paste0("Father ", seq_along(father_haplotypes)),
        "CSD haplotype" = father_haplotypes,
        "Matches queen haplotype?" = ifelse(father_matches, "Yes", "No"),
        "Contribution to pHom" = ifelse(
          father_matches,
          sprintf("%.2f%%", 100 / (length(queen_haplotypes) * length(father_haplotypes))),
          "0.00%"
        ),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )

      list(
        available = TRUE,
        p_hom = p_hom,
        p_hom_from_alleles = p_hom_from_alleles,
        n_hom = n_hom,
        n_hom_label = n_hom_label,
        n_fathers = length(father_haplotypes),
        queen_haplotypes = queen_haplotypes,
        father_haplotypes = father_haplotypes,
        queen_labels = paste0("Queen ", seq_along(queen_haplotypes), ": ", queen_haplotypes),
        father_labels = paste0("Father ", seq_along(father_haplotypes), ": ", father_haplotypes),
        matching_haplotypes = matching_haplotypes,
        matched_father_idx = matched_father_idx,
        formula_label = formula_label,
        interpretation = interpretation,
        father_table = father_table
      )
    })
  }

  get_relatedness_metrics <- function(col, SP = NULL) {
    with_global_SP(SP, {
      if (is.null(col)) {
        return(NULL)
      }

      nw <- tryCatch(as.integer(nWorkers(col)), error = function(e) 0L)
      if (nw < 2L) {
        return(NULL)
      }

      ped <- tryCatch(AlphaSimR::getPed(getWorkers(col)), error = function(e) NULL)

      if (!is.null(ped)) {
        ped <- as.data.frame(ped, stringsAsFactors = FALSE)

        if ("father" %in% names(ped)) {
          father_ids <- ped$father
          father_ids <- father_ids[!is.na(father_ids) & father_ids != 0 & father_ids != "0"]

          if (length(father_ids) >= 2L) {
            counts <- as.numeric(table(father_ids))
            total_pairs <- choose(sum(counts), 2)
            full_pairs  <- if (total_pairs > 0) sum(choose(counts, 2)) else 0

            if (total_pairs > 0) {
              p_full <- full_pairs / total_pairs
              p_half <- 1 - p_full
              avg_r  <- 0.75 * p_full + 0.25 * p_half

              return(list(
                p_full = p_full,
                p_half = p_half,
                avg_r = avg_r,
                n_patrilines = length(counts),
                method_label = "realised worker pedigree"
              ))
            }
          }
        }
      }

      nf <- tryCatch(as.integer(nFathers(col)), error = function(e) 0L)
      if (nf < 1L) {
        return(NULL)
      }

      p_full <- 1 / nf
      p_half <- 1 - p_full
      avg_r  <- 0.75 * p_full + 0.25 * p_half

      list(
        p_full = p_full,
        p_half = p_half,
        avg_r = avg_r,
        n_patrilines = nf,
        method_label = "equal-father expectation"
      )
    })
  }

  safe_cor <- function(x, y) {
    ok <- is.finite(x) & is.finite(y)
    x <- x[ok]
    y <- y[ok]

    if (length(x) < 2L || length(unique(x)) < 2L || length(unique(y)) < 2L) {
      return(NA_real_)
    }

    suppressWarnings(cor(x, y))
  }

  sanitize_seed <- function(seed, offset = 0L, default = DEFAULT_APP_SEED) {
    seed_num <- suppressWarnings(as.numeric(seed[1]))
    offset_num <- suppressWarnings(as.numeric(offset[1]))

    if (length(seed_num) < 1L || is.na(seed_num) || !is.finite(seed_num)) {
      seed_num <- default
    }
    if (length(offset_num) < 1L || is.na(offset_num) || !is.finite(offset_num)) {
      offset_num <- 0
    }

    seed_out <- floor(abs(seed_num + offset_num))
    if (length(seed_out) < 1L || is.na(seed_out) || !is.finite(seed_out) || seed_out < 1) {
      seed_out <- 1
    }
    seed_out <- ((seed_out - 1) %% 2147483646) + 1

    as.integer(seed_out)
  }

  module_seed <- function(base_seed, module = c("setup", "event", "qg", "mc"), sub = 0L) {
    module <- match.arg(module)
    offsets <- c(setup = 1000L, event = 2000L, qg = 3000L, mc = 4000L)

    sanitize_seed(base_seed, offsets[[module]] + as.integer(sub))
  }

  event_seed_offset <- function(event_type) {
    switch(event_type, swarm = 1L, supersede = 2L, collapse = 3L, 0L)
  }

  # ============================================================
  # TAB 2 – Colony Setup
  # ============================================================
  observeEvent(input$runSetup, {
    base_seed <- sanitize_seed(input$seedBase)
    setup_seed <- module_seed(base_seed, "setup")
    set.seed(setup_seed)

    withProgress(message = "🐝 Simulating colony…", value = 0, {

      requested_seg_sites <- as.integer(input$nSegSites)
      seg_sites_used <- safe_seg_sites(requested_seg_sites, input$nCsdAlleles)

      incProgress(0.15, detail = "Creating founder genomes")
      founderGenomes <- quickHaplo(
        nInd     = input$nFounders,
        nChr     = input$nChr,
        segSites = seg_sites_used
      )

      incProgress(0.20, detail = "Setting up SimParamBee")
      csd_chr <- min(3L, as.integer(input$nChr))
      source_workers <- NA_integer_
      source_drones <- NA_integer_

      SP <- SimParamBee$new(
        founderGenomes,
        nWorkers    = input$nWorkers_c,
        nDrones     = input$nDrones_c,
        nFathers    = input$nFathers,
        nCsdAlleles = as.integer(input$nCsdAlleles),
        csdChr      = csd_chr
      )
      SP <- prepare_SP(SP)

      with_global_SP(SP, {
        incProgress(0.15, detail = "Creating virgin queens")
        basePop <- createVirginQueens(founderGenomes, simParamBee = SP)

        incProgress(0.20, detail = "Creating drone congregation area")
        n_dca_donors <- input$nFounders - 1L
        dca_info <- make_dca(
          queen_pop = basePop,
          donor_idx = seq_len(n_dca_donors),
          n_groups  = 1L,
          n_fathers = input$nFathers,
          SP = SP
        )

        droneGroup <- pullDroneGroupsFromDCA(
          dca_info$DCA,
          n = 1,
          nDrones = input$nFathers,
          simParamBee = SP
        )

        if (identical(input$matingDesign, "open")) {
          incProgress(0.15, detail = "Creating and mating colony")
          colony <- createColony(x = basePop[input$nFounders], simParamBee = SP)
          colony <- cross(
            colony,
            drones = droneGroup[[1]],
            checkCross = "warning",
            simParamBee = SP
          )

          mating_label <- "Open DCA mating"
          mating_note <- paste(
            "Open DCA mating: this is the biologically realistic default.",
            "Even with one stored father, expected CSD-homozygous brood can be 0% if that father shares neither queen csd haplotype."
          )
        } else {
          incProgress(0.15, detail = "Creating inbred brother-mating demo")
          source_colony <- createColony(x = basePop[input$nFounders], simParamBee = SP)
          source_colony <- cross(
            source_colony,
            drones = droneGroup[[1]],
            checkCross = "warning",
            simParamBee = SP
          )

          source_workers <- max(200L, as.integer(input$nWorkers_c))
          source_drones  <- max(100L, as.integer(input$nDrones_c), as.integer(input$nFathers) * 4L)
          source_colony <- buildUp(
            source_colony,
            nWorkers = source_workers,
            nDrones  = source_drones,
            exact    = TRUE,
            simParamBee = SP
          )
          source_colony <- addVirginQueens(source_colony, nInd = 1, simParamBee = SP)

          brother_drones <- getDrones(
            source_colony,
            nInd = input$nFathers,
            removeFathers = FALSE,
            collapse = FALSE
          )
          focal_virgin <- getVirginQueens(source_colony, nInd = 1, collapse = FALSE)

          colony <- createColony(x = focal_virgin, simParamBee = SP)
          colony <- cross(
            colony,
            drones = brother_drones,
            checkCross = "warning",
            simParamBee = SP
          )

          mating_label <- "Inbred brother mating"
          mating_note <- paste(
            "Inbred brother mating: a virgin queen from a source colony is mated to her brothers.",
            "This strongly increases the chance of csd haplotype matching and is meant as a classroom CSD demonstration, not as a realistic open-mating scenario."
          )
        }

        incProgress(0.10, detail = "Building up colony")
        colony <- buildUp(
          colony,
          nWorkers = input$nWorkers_c,
          nDrones  = input$nDrones_c,
          exact    = TRUE,
          simParamBee = SP
        )
      })

      incProgress(0.05, detail = "Done!")

      sim_state$SP            <- SP
      sim_state$founderGenomes <- founderGenomes
      sim_state$founderSummary <- founder_summary_df(founderGenomes)
      sim_state$basePop       <- basePop
      sim_state$DCA           <- dca_info$DCA
      sim_state$colony        <- colony
      sim_state$setup_info    <- list(
        base_seed = base_seed,
        setup_seed = setup_seed,
        n_founders = input$nFounders,
        n_chr = input$nChr,
        requested_seg_sites = requested_seg_sites,
        seg_sites_used = seg_sites_used,
        seg_sites_adjusted = seg_sites_used > requested_seg_sites,
        n_csd_alleles = as.integer(input$nCsdAlleles),
        csd_chr = csd_chr,
        n_dca_donors = n_dca_donors,
        dca_donors = dca_info$donors,
        drones_per_donor = dca_info$drones_per_donor,
        total_dca_drones = dca_info$total_dca_drones,
        n_fathers = input$nFathers,
        n_workers = input$nWorkers_c,
        n_drones  = input$nDrones_c,
        mating_design = input$matingDesign,
        mating_label = mating_label,
        mating_note = mating_note,
        source_workers = source_workers,
        source_drones = source_drones
      )

      sim_state$before_event  <- NULL
      sim_state$event_done    <- FALSE
      sim_state$event_type    <- NULL
      sim_state$event_seed    <- NULL
      sim_state$swarm_out     <- NULL
    })
  })

  output$setupLog <- renderPrint({
    req(sim_state$colony, sim_state$setup_info)

    with_global_SP(sim_state$SP, {
      cts <- colony_counts(sim_state$colony)
      csd <- get_csd_metrics(sim_state$colony, sim_state$SP)
      info <- sim_state$setup_info

      cat("Colony created successfully!\n\n")
      cat("--- Simulation design ---\n")
      cat(sprintf("  Base random seed         : %d\n", info$base_seed))
      cat(sprintf("  Colony Setup seed        : %d\n", info$setup_seed))
      cat(sprintf("  Founder genomes          : %d\n", info$n_founders))
      cat(sprintf("  Chromosomes              : %d\n", info$n_chr))
      cat(sprintf("  Segregating sites/chr    : requested %d | used %d\n",
                  info$requested_seg_sites, info$seg_sites_used))
      cat(sprintf("  Possible csd alleles     : %d\n", info$n_csd_alleles))
      cat(sprintf("  csd chromosome           : %d\n", info$csd_chr))
      cat(sprintf("  DCA donor queens         : %d\n", info$n_dca_donors))
      cat(sprintf("  DCA drones per donor     : %d\n", info$drones_per_donor))
      cat(sprintf("  Total DCA drones created : %s\n", format(info$total_dca_drones, big.mark = ",")))
      cat(sprintf("  Fathers sampled at mating: %d\n", info$n_fathers))
      cat(sprintf("  Mating design            : %s\n", info$mating_label))
      cat(sprintf("  Build-up target          : %s workers, %s resident drones\n",
                  format(info$n_workers, big.mark = ","),
                  format(info$n_drones, big.mark = ",")))
      if (isTRUE(info$seg_sites_adjusted)) {
        cat("  Note                     : segSites per chromosome was increased automatically so the requested number of csd alleles could be represented at the csd locus.\n")
      }
      if (!is.na(info$source_workers) && !is.na(info$source_drones)) {
        cat(sprintf("  Source colony build-up   : %s workers, %s resident drones\n",
                    format(info$source_workers, big.mark = ","),
                    format(info$source_drones, big.mark = ",")))
      }
      if (!is.null(info$mating_note) && nzchar(info$mating_note)) {
        cat(sprintf("  Teaching note            : %s\n", info$mating_note))
      }

      cat("\n--- Colony counts ---\n")
      cat(sprintf("  Queens        : %d\n", cts["Queens"]))
      cat(sprintf("  Virgin queens : %d\n", cts["VirginQueens"]))
      cat(sprintf("  Workers       : %s\n", format(cts["Workers"], big.mark = ",")))
      cat(sprintf("  Drones        : %s\n", format(cts["Drones"], big.mark = ",")))
      cat(sprintf("  Fathers       : %d\n", cts["Fathers"]))
      cat(sprintf("  Queen state   : %s\n", queen_state(sim_state$colony)))
      cat(sprintf("  Productive    : %s\n", isProductive(sim_state$colony)))

      if (isTRUE(csd$available)) {
        cat("\n--- CSD ---\n")
        cat(sprintf("  Queen csd haplotypes      : %s\n", paste(csd$queen_labels, collapse = " | ")))
        cat(sprintf("  Father csd haplotypes     : %s\n", paste(csd$father_labels, collapse = " | ")))
        cat(sprintf("  Matching father(s)        : %s\n",
                    if (length(csd$matched_father_idx) >= 1L) paste(csd$matched_father_idx, collapse = ", ") else "none"))
        cat(sprintf("  Matching haplotype(s)     : %s\n",
                    if (length(csd$matching_haplotypes) >= 1L) paste(csd$matching_haplotypes, collapse = ", ") else "none"))
        cat(sprintf("  Exact haplotype rule      : %s = %.2f%%\n", csd$formula_label, 100 * csd$p_hom_from_alleles))
        cat(sprintf("  Expected homozygous brood : %.2f%%\n", 100 * csd$p_hom))
        cat(sprintf("  Realised homozygous brood : %s\n", csd$n_hom_label))
        cat(sprintf("  Interpretation            : %s\n", csd$interpretation))
      }

      cat("\n--- Colony object ---\n")
      print(sim_state$colony)
    })
  })

  output$genomePlot <- renderPlot({
    req(sim_state$founderSummary)

    df <- sim_state$founderSummary

    ggplot(df, aes(x = reorder(Individual, AltAlleles), y = AltAlleles, fill = Heterozygosity)) +
      geom_col(color = "white", linewidth = 0.4) +
      coord_flip() +
      scale_fill_gradient(low = bee_colors[["honey"]], high = bee_colors[["brown"]]) +
      labs(
        title = "Founder Genome Summary",
        subtitle = "Observed segregating-site genotypes from quickHaplo founders",
        x = NULL,
        y = "Total alternative-allele count",
        fill = "Heterozygosity"
      ) +
      theme_bee()
  })

  output$setupCode <- renderUI({
    req(sim_state$setup_info)

    info <- sim_state$setup_info

    code_txt <- if (identical(info$mating_design, "open")) {
      sprintf(
"# Reproducible run
set.seed(%d)

# Founder genomes
founderGenomes <- quickHaplo(nInd = %d, nChr = %d, segSites = %d)
SP <- SimParamBee$new(
  founderGenomes,
  nWorkers = %d,
  nDrones = %d,
  nFathers = %d,
  nCsdAlleles = %d,
  csdChr = %d
)
SP$nThreads <- 1L
SP$setTrackPed(isTrackPed = TRUE)

# Virgin queens and DCA
basePop <- createVirginQueens(founderGenomes, simParamBee = SP)
dca_parts <- lapply(1:%d, function(i) {
  createDrones(basePop[i], nInd = %d, simParamBee = SP)
})
DCA <- do.call(c, dca_parts)
droneGroup <- pullDroneGroupsFromDCA(DCA, n = 1, nDrones = %d, simParamBee = SP)

# Open mating of the focal founder queen
colony <- createColony(x = basePop[%d], simParamBee = SP)
colony <- cross(colony, drones = droneGroup[[1]], checkCross = 'warning', simParamBee = SP)
colony <- buildUp(colony, nWorkers = %d, nDrones = %d, exact = TRUE, simParamBee = SP)",
        info$setup_seed,
        info$n_founders, info$n_chr, info$seg_sites_used,
        info$n_workers, info$n_drones, info$n_fathers,
        info$n_csd_alleles, info$csd_chr,
        info$n_dca_donors, info$drones_per_donor, info$n_fathers,
        info$n_founders,
        info$n_workers, info$n_drones
      )
    } else {
      sprintf(
"# Reproducible run
set.seed(%d)

# Founder genomes
founderGenomes <- quickHaplo(nInd = %d, nChr = %d, segSites = %d)
SP <- SimParamBee$new(
  founderGenomes,
  nWorkers = %d,
  nDrones = %d,
  nFathers = %d,
  nCsdAlleles = %d,
  csdChr = %d
)
SP$nThreads <- 1L
SP$setTrackPed(isTrackPed = TRUE)

# Create a source colony, then mate a daughter virgin queen to her brothers
basePop <- createVirginQueens(founderGenomes, simParamBee = SP)
dca_parts <- lapply(1:%d, function(i) {
  createDrones(basePop[i], nInd = %d, simParamBee = SP)
})
DCA <- do.call(c, dca_parts)
droneGroup <- pullDroneGroupsFromDCA(DCA, n = 1, nDrones = %d, simParamBee = SP)

source_colony <- createColony(x = basePop[%d], simParamBee = SP)
source_colony <- cross(source_colony, drones = droneGroup[[1]], checkCross = 'warning', simParamBee = SP)
source_colony <- buildUp(source_colony, nWorkers = %d, nDrones = %d, exact = TRUE, simParamBee = SP)
source_colony <- addVirginQueens(source_colony, nInd = 1, simParamBee = SP)

brother_drones <- getDrones(source_colony, nInd = %d, removeFathers = FALSE, collapse = FALSE)
focal_virgin <- getVirginQueens(source_colony, nInd = 1, collapse = FALSE)
colony <- createColony(x = focal_virgin, simParamBee = SP)
colony <- cross(colony, drones = brother_drones, checkCross = 'warning', simParamBee = SP)
colony <- buildUp(colony, nWorkers = %d, nDrones = %d, exact = TRUE, simParamBee = SP)",
        info$setup_seed,
        info$n_founders, info$n_chr, info$seg_sites_used,
        info$n_workers, info$n_drones, info$n_fathers,
        info$n_csd_alleles, info$csd_chr,
        info$n_dca_donors, info$drones_per_donor, info$n_fathers,
        info$n_founders,
        info$source_workers, info$source_drones,
        info$n_fathers,
        info$n_workers, info$n_drones
      )
    }

    div(class = "r-code", code_txt)
  })

  # ============================================================
  # TAB 3 – Colony Structure
  # ============================================================
  output$nQueens <- renderText({
    req(sim_state$colony)
    colony_counts(sim_state$colony)["Queens"]
  })

  output$nWorkersOut <- renderText({
    req(sim_state$colony)
    format(colony_counts(sim_state$colony)["Workers"], big.mark = ",")
  })

  output$nDronesOut <- renderText({
    req(sim_state$colony)
    format(colony_counts(sim_state$colony)["Drones"], big.mark = ",")
  })

  output$nFathersOut <- renderText({
    req(sim_state$colony)
    colony_counts(sim_state$colony)["Fathers"]
  })

  output$castePie <- renderPlot({
    req(sim_state$colony)

    cts <- colony_counts(sim_state$colony)
    df <- data.frame(
      Caste = c("Workers", "Drones", "Queen", "Virgin Queens"),
      Count = c(cts["Workers"], cts["Drones"], cts["Queens"], cts["VirginQueens"]),
      stringsAsFactors = FALSE
    ) %>%
      filter(Count > 0) %>%
      mutate(
        Pct = Count / sum(Count) * 100,
        Label = sprintf("%s\n%s (%.1f%%)", Caste, format(Count, big.mark = ","), Pct)
      )

    if (nrow(df) < 1L) {
      print(placeholder_plot(
        title = "Live Colony Members",
        label = "No live colony members to display"
      ))
      return()
    }

    colors <- c(
      "Workers" = bee_colors[["gold"]],
      "Drones" = bee_colors[["blue"]],
      "Queen" = bee_colors[["purple"]],
      "Virgin Queens" = bee_colors[["green"]]
    )

    ggplot(df, aes(x = "", y = Count, fill = Caste)) +
      geom_col(width = 1, color = "white", linewidth = 0.6) +
      coord_polar("y", start = 0) +
      scale_fill_manual(values = colors) +
      geom_text(
        aes(label = Label),
        position = position_stack(vjust = 0.5),
        size = 3.5,
        color = "white",
        fontface = "bold"
      ) +
      labs(
        title = "Live colony members",
        subtitle = sprintf("Stored fathers tracked separately: %d", cts["Fathers"]),
        x = NULL, y = NULL
      ) +
      theme_bee() +
      theme(
        axis.text = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none"
      )
  })

  output$csdBox <- renderUI({
    req(sim_state$colony, sim_state$SP)

    csd <- get_csd_metrics(sim_state$colony, sim_state$SP)

    if (!isTRUE(csd$available)) {
      return(
        div(
          style = "background:#B0BEC5; color:white; border-radius:8px; padding:12px; text-align:center; margin:10px 0;",
          tags$b("CSD calculation not available"),
          br(), br(),
          csd$reason
        )
      )
    }

    status_col <- if (length(csd$matched_father_idx) < 1L) {
      bee_colors[["green"]]
    } else if (csd$p_hom <= 0.10) {
      bee_colors[["dark_gold"]]
    } else {
      bee_colors[["red"]]
    }

    div(
      style = sprintf("background:%s; color:white; border-radius:8px; padding:12px; text-align:center; margin:10px 0;", status_col),
      tags$b(sprintf("Expected CSD homozygous brood: %.2f%%", 100 * csd$p_hom)),
      br(), br(),
      sprintf("Realised homozygous brood produced: %s", csd$n_hom_label),
      br(),
      sprintf("Stored fathers: %d | Matching fathers: %d", csd$n_fathers, length(csd$matched_father_idx)),
      br(),
      sprintf("Exact haplotype rule: %s", csd$formula_label),
      br(),
      if (!is.null(sim_state$setup_info$mating_note)) tags$small(sim_state$setup_info$mating_note),
      br(),
      tags$small("This is the same row-collapsed haplotype logic used by SIMplyBee's calcQueensPHomBrood().")
    )
  })

  output$csdAlleleBox <- renderUI({
    req(sim_state$colony, sim_state$SP, sim_state$setup_info)

    csd <- get_csd_metrics(sim_state$colony, sim_state$SP)
    if (!isTRUE(csd$available)) {
      return(NULL)
    }

    info <- sim_state$setup_info
    matched_haplotype_label <- if (length(csd$matching_haplotypes) >= 1L) {
      paste(csd$matching_haplotypes, collapse = " | ")
    } else {
      "None"
    }

    div(
      class = "info-box",
      div(class = "title", "🔎 Exact csd haplotypes used by SIMplyBee"),
      p(tags$b("Mating design: "), info$mating_label),
      p(tags$b("Possible csd alleles in SP: "), info$n_csd_alleles),
      p(tags$b("Queen haplotypes: "), tags$code(paste(csd$queen_labels, collapse = " | "))),
      p(tags$b("Shared queen/father haplotype(s): "), tags$code(matched_haplotype_label)),
      p(tags$b("Interpretation: "), csd$interpretation)
    )
  })

  output$csdFatherTable <- renderTable({
    req(sim_state$colony, sim_state$SP)

    csd <- get_csd_metrics(sim_state$colony, sim_state$SP)
    if (!isTRUE(csd$available)) {
      return(NULL)
    }

    csd$father_table
  }, striped = TRUE, bordered = TRUE, hover = TRUE, rownames = FALSE)

  output$csdPlot <- renderPlot({
    req(sim_state$colony, sim_state$SP)

    csd <- get_csd_metrics(sim_state$colony, sim_state$SP)

    if (!isTRUE(csd$available)) {
      print(placeholder_plot(
        title = "Expected brood composition at the CSD locus",
        label = "Not available until a queen is mated",
        subtitle = "Swarm and supersedure remnants carry a virgin queen"
      ))
      return()
    }

    match_subtitle <- if (length(csd$matched_father_idx) >= 1L) {
      sprintf(
        "Current colony: %d stored fathers | matching fathers: %s | matching haplotypes: %s",
        csd$n_fathers,
        paste(csd$matched_father_idx, collapse = ", "),
        paste(csd$matching_haplotypes, collapse = ", ")
      )
    } else {
      sprintf(
        "Current colony: %d stored fathers | no stored father haplotype matches the queen",
        csd$n_fathers
      )
    }

    df <- data.frame(
      Outcome = c("CSD-homozygous brood", "Viable diploid brood"),
      Percent = c(csd$p_hom * 100, (1 - csd$p_hom) * 100)
    )

    ggplot(df, aes(x = Outcome, y = Percent, fill = Outcome)) +
      geom_col(width = 0.6, show.legend = FALSE) +
      geom_text(
        aes(label = sprintf("%.2f%%", Percent)),
        vjust = -0.35, fontface = "bold", size = 4
      ) +
      scale_fill_manual(values = c(
        "Viable diploid brood" = bee_colors[["green"]],
        "CSD-homozygous brood" = bee_colors[["red"]]
      )) +
      scale_y_continuous(limits = c(0, max(100, max(df$Percent) * 1.18))) +
      labs(
        title = "Expected brood composition at the CSD locus",
        subtitle = match_subtitle,
        caption = csd$interpretation,
        x = NULL,
        y = "Percentage of diploid brood"
      ) +
      theme_bee() +
      theme(plot.caption = element_text(color = bee_colors[["brown"]], face = "italic"))
  })

  output$relPlot <- renderPlot({
    req(sim_state$colony)

    rel <- get_relatedness_metrics(sim_state$colony, sim_state$SP)

    if (is.null(rel)) {
      print(placeholder_plot(
        title = "Worker relatedness and parentage",
        label = "Need at least two workers with known fathers",
        subtitle = "After swarm or supersedure, relatedness is undefined until workers are produced"
      ))
      return()
    }

    df <- data.frame(
      Type = c("Full sisters\n(same father)", "Half sisters\n(different father)"),
      Prop = c(rel$p_full, rel$p_half),
      RelCoeff = c(0.75, 0.25)
    )

    ggplot(df, aes(x = Type, y = Prop * 100, fill = Type)) +
      geom_col(width = 0.5, show.legend = FALSE) +
      geom_text(
        aes(label = sprintf("%.1f%%\n(r = %.2f)", Prop * 100, RelCoeff)),
        vjust = -0.3, fontface = "bold", size = 4
      ) +
      scale_fill_manual(values = c(bee_colors[["gold"]], bee_colors[["blue"]])) +
      scale_y_continuous(limits = c(0, max(df$Prop * 100) * 1.25)) +
      annotate(
        "text",
        x = 1.5,
        y = max(df$Prop * 100) * 1.18,
        label = sprintf(
          "Mean worker-pair relatedness = %.3f\nPatrilines = %d (%s)",
          rel$avg_r, rel$n_patrilines, rel$method_label
        ),
        fontface = "bold", color = bee_colors[["brown"]], size = 4
      ) +
      labs(title = "Worker pair relatedness", x = NULL, y = "% of worker pairs") +
      theme_bee()
  })

  # ============================================================
  # TAB 4 – Colony Events
  # ============================================================
  observeEvent(input$runEvent, {
    req(sim_state$colony, sim_state$SP)

    event_seed <- module_seed(input$seedBase, "event", event_seed_offset(input$eventType))
    set.seed(event_seed)

    sim_state$before_event <- sim_state$colony
    sim_state$event_type   <- input$eventType
    sim_state$event_done   <- FALSE
    sim_state$swarm_out    <- NULL
    sim_state$event_seed   <- event_seed

    colony <- sim_state$colony
    SP     <- sim_state$SP

    with_global_SP(SP, {
      tryCatch({
        if (input$eventType == "swarm") {
          result <- SIMplyBee::swarm(colony, p = input$swarmProp, simParamBee = SP)
          sim_state$colony    <- result$remnant
          sim_state$swarm_out <- result$swarm

        } else if (input$eventType == "supersede") {
          sim_state$colony <- SIMplyBee::supersede(colony, simParamBee = SP)

        } else if (input$eventType == "collapse") {
          sim_state$colony <- SIMplyBee::collapse(colony)
        }

        sim_state$event_done <- TRUE
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      })
    })
  })

  output$eventLog <- renderPrint({
    if (!isTRUE(sim_state$event_done)) {
      cat("No event simulated yet. Choose an event and click 'Simulate Event'.\n")
      return()
    }

    before <- sim_state$before_event
    after  <- sim_state$colony
    ev     <- sim_state$event_type

    c_before <- colony_counts(before)
    c_after  <- colony_counts(after)

    cat(sprintf("Event: %s\n", toupper(ev)))
    if (!is.null(sim_state$event_seed)) {
      cat(sprintf("Seed used: %d\n", sim_state$event_seed))
    }
    cat("\n")
    cat("--- BEFORE ---\n")
    cat(sprintf(
      "  Queen state: %s | Workers: %s | Drones: %s | Queens: %d | Virgin queens: %d | Fathers: %d\n",
      queen_state(before),
      format(c_before["Workers"], big.mark = ","),
      format(c_before["Drones"], big.mark = ","),
      c_before["Queens"],
      c_before["VirginQueens"],
      c_before["Fathers"]
    ))
    cat(sprintf("  Productive: %s | Collapsed: %s\n", isProductive(before), SIMplyBee::hasCollapsed(before)))

    cat("\n--- AFTER / REMNANT ---\n")
    cat(sprintf(
      "  Queen state: %s | Workers: %s | Drones: %s | Queens: %d | Virgin queens: %d | Fathers: %d\n",
      queen_state(after),
      format(c_after["Workers"], big.mark = ","),
      format(c_after["Drones"], big.mark = ","),
      c_after["Queens"],
      c_after["VirginQueens"],
      c_after["Fathers"]
    ))
    cat(sprintf("  Productive: %s | Collapsed: %s\n", isProductive(after), SIMplyBee::hasCollapsed(after)))

    if (ev %in% c("swarm", "supersede") && c_after["VirginQueens"] > 0L && c_after["Fathers"] == 0L) {
      cat("  Note: the remnant now carries a virgin queen and has zero stored fathers until re-mated.\n")
    }

    if (ev == "collapse") {
      cat("  Note: collapse() marks the colony as collapsed and non-productive, but retains colony members for retrospective analysis.\n")
    }

    if (ev == "swarm" && !is.null(sim_state$swarm_out)) {
      sw <- sim_state$swarm_out
      c_sw <- colony_counts(sw)
      cat("\n--- SWARM COLONY ---\n")
      cat(sprintf(
        "  Queen state: %s | Workers: %s | Drones: %s | Queens: %d | Virgin queens: %d | Fathers: %d\n",
        queen_state(sw),
        format(c_sw["Workers"], big.mark = ","),
        format(c_sw["Drones"], big.mark = ","),
        c_sw["Queens"],
        c_sw["VirginQueens"],
        c_sw["Fathers"]
      ))
      cat(sprintf("  Productive: %s | Collapsed: %s\n", isProductive(sw), SIMplyBee::hasCollapsed(sw)))
    }
  })

  output$eventCode <- renderUI({
    ev <- input$eventType
    event_seed <- module_seed(input$seedBase, "event", event_seed_offset(ev))

    code_txt <- if (ev == "swarm") {
      sprintf(
"set.seed(%d)
result <- SIMplyBee::swarm(colony, p = %.2f, simParamBee = SP)
remnant <- result$remnant
swarm_colony <- result$swarm",
        event_seed, input$swarmProp
      )
    } else if (ev == "supersede") {
      sprintf("set.seed(%d)\ncolony <- SIMplyBee::supersede(colony, simParamBee = SP)", event_seed)
    } else {
      sprintf("set.seed(%d)\ncolony <- SIMplyBee::collapse(colony)", event_seed)
    }

    code_txt
  })

  output$eventPlot <- renderPlot({
    req(sim_state$before_event, sim_state$colony, sim_state$event_type)

    get_counts_df <- function(col, label) {
      cts <- colony_counts(col)
      data.frame(
        Caste = c("Queens", "Virgin Queens", "Workers", "Drones"),
        Count = c(cts["Queens"], cts["VirginQueens"], cts["Workers"], cts["Drones"]),
        When  = label,
        stringsAsFactors = FALSE
      )
    }

    after_label <- if (identical(sim_state$event_type, "swarm")) "Remnant" else "After"

    df <- bind_rows(
      get_counts_df(sim_state$before_event, "Before"),
      get_counts_df(sim_state$colony, after_label)
    )

    if (identical(sim_state$event_type, "swarm") && !is.null(sim_state$swarm_out)) {
      df <- bind_rows(df, get_counts_df(sim_state$swarm_out, "Swarm"))
    }

    df$When <- factor(df$When, levels = unique(df$When))
    df$Caste <- factor(df$Caste, levels = c("Queens", "Virgin Queens", "Workers", "Drones"))

    cols <- c(
      "Queens" = bee_colors[["purple"]],
      "Virgin Queens" = bee_colors[["green"]],
      "Workers" = bee_colors[["gold"]],
      "Drones" = bee_colors[["blue"]]
    )

    subtitle_txt <- if (identical(sim_state$event_type, "collapse")) {
      "collapse() retains colony members but marks the colony as collapsed"
    } else {
      NULL
    }

    ggplot(df, aes(x = Caste, y = Count, fill = Caste)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~ When) +
      scale_fill_manual(values = cols) +
      geom_text(aes(label = format(Count, big.mark = ",")),
                vjust = -0.35, size = 3.8, fontface = "bold") +
      labs(
        title = sprintf("Colony composition: %s event", toupper(sim_state$event_type %||% "")),
        subtitle = subtitle_txt,
        x = NULL, y = "Number of bees"
      ) +
      theme_bee()
  })

  # ============================================================
  # TAB 5 – Quantitative Genetics
  # ============================================================
  qg_result <- eventReactive(input$runQG, {
    base_seed <- sanitize_seed(input$seedBase)
    qg_seed <- module_seed(base_seed, "qg")
    set.seed(qg_seed)

    withProgress(message = "🧬 Running QG simulation…", value = 0, {

      nCol <- input$nColQG
      nW   <- input$nWrkQG
      envSD <- input$envSD

      incProgress(0.10, detail = "Simulating founders")
      n_dca_donors   <- max(3L, ceiling(nCol / 2))
      total_founders <- n_dca_donors + nCol
      qg_n_csd_alleles <- 64L
      qg_seg_sites <- safe_seg_sites(128L, qg_n_csd_alleles)
      founderGenomes <- quickHaplo(nInd = total_founders, nChr = 1, segSites = qg_seg_sites)

      incProgress(0.15, detail = "Setting quantitative-genetic parameters")
      SP <- SimParamBee$new(
        founderGenomes,
        nWorkers = nW,
        nDrones  = 100,
        nFathers = 12,
        nCsdAlleles = qg_n_csd_alleles,
        csdChr = 1L
      )
      SP <- prepare_SP(SP)

      meanQ  <- input$meanQ
      meanW_total <- input$meanW
      corAv  <- input$corA
      h2v    <- input$h2

      varA  <- c(h2v, h2v / nW)
      varE  <- c(1 - h2v, (1 - h2v) / nW)
      corAm <- matrix(c(1, corAv, corAv, 1), nrow = 2)

      SP$addTraitA(
        nQtlPerChr = 100,
        mean = c(meanQ, meanW_total / nW),
        var  = varA,
        corA = corAm
      )
      SP$setVarE(varE = varE)

      with_global_SP(SP, {
        incProgress(0.15, detail = "Creating queens and the DCA")
        basePop <- createVirginQueens(founderGenomes, simParamBee = SP)

        dca_info <- make_dca(
          queen_pop = basePop,
          donor_idx = seq_len(n_dca_donors),
          n_groups  = nCol,
          n_fathers = 12,
          SP = SP
        )

        droneGroups <- pullDroneGroupsFromDCA(
          dca_info$DCA,
          n = nCol,
          nDrones = 12,
          simParamBee = SP
        )

        incProgress(0.45, detail = "Creating colonies")
        colonies <- vector("list", nCol)
        for (i in seq_len(nCol)) {
          col_i <- createColony(x = basePop[n_dca_donors + i], simParamBee = SP)
          col_i <- cross(
            col_i,
            drones = droneGroups[[i]],
            checkCross = "warning",
            simParamBee = SP
          )
          col_i <- buildUp(
            col_i,
            nWorkers = nW,
            nDrones = 100,
            exact = TRUE,
            simParamBee = SP
          )
          colonies[[i]] <- col_i
        }

        incProgress(0.15, detail = "Calculating colony values")
        honey <- sapply(colonies, function(col) {
          as.numeric(calcColonyPheno(col, queenTrait = 1, workersTrait = 2, simParamBee = SP))
        })
        if (isTRUE(envSD > 0)) {
          honey <- honey + rnorm(length(honey), mean = 0, sd = envSD)
        }

        colonyGV <- sapply(colonies, function(col) {
          as.numeric(calcColonyGv(col, queenTrait = 1, workersTrait = 2, simParamBee = SP))
        })

        queenGV1 <- sapply(colonies, function(col) as.numeric(gv(getQueen(col))[, 1]))
        queenGV2 <- sapply(colonies, function(col) as.numeric(gv(getQueen(col))[, 2]))
        workerGV2 <- sapply(colonies, function(col) mean(gv(getWorkers(col))[, 2]))
      })

      selected_n <- max(1L, ceiling(0.20 * nCol))
      selected_idx <- order(honey, decreasing = TRUE)[seq_len(selected_n)]

      list(
        honey    = honey,
        colonyGV = colonyGV,
        queenGV1 = queenGV1,
        queenGV2 = queenGV2,
        workerGV2 = workerGV2,
        base_seed = base_seed,
        qg_seed = qg_seed,
        envSD = envSD,
        h2 = h2v,
        corA = corAv,
        nCol = nCol,
        nW = nW,
        selected_n = selected_n,
        selected_idx = selected_idx
      )
    })
  })

  output$qgPlot <- renderPlot({
    res <- qg_result()
    req(res)

    df <- data.frame(Colony = seq_len(res$nCol), Honey = res$honey)

    ggplot(df, aes(x = Honey)) +
      geom_histogram(bins = 15, fill = bee_colors[["gold"]], color = "white") +
      geom_vline(xintercept = mean(res$honey), color = bee_colors[["red"]],
                 linetype = "dashed", linewidth = 1.2) +
      annotate("text", x = mean(res$honey), y = Inf, vjust = 2, hjust = -0.05,
               label = sprintf("Mean = %.2f", mean(res$honey)),
               color = bee_colors[["red"]], fontface = "bold") +
      labs(
        title = "Colony honey-yield distribution",
        subtitle = if (isTRUE(res$envSD > 0)) sprintf("Honey yield = colony phenotype value + environmental noise (SD = %s)", res$envSD) else "Honey yield is calculated as a colony phenotype value",
        x = "Honey yield (arbitrary units)",
        y = "Number of colonies"
      ) +
      theme_bee()
  })

  output$bvPlot <- renderPlot({
    res <- qg_result()
    req(res)

    df <- data.frame(
      Colony = seq_len(res$nCol),
      Colony_GV = res$colonyGV,
      Queen_GV1 = res$queenGV1,
      Queen_GV2 = res$queenGV2,
      Worker_GV2 = res$workerGV2,
      Honey = res$honey
    )

    cor_gv_honey <- round(safe_cor(df$Colony_GV, df$Honey), 2)
    cor_q_honey  <- round(safe_cor(df$Queen_GV1, df$Honey), 2)
    cor_q_colony <- round(safe_cor(df$Queen_GV1, df$Colony_GV), 2)

    selected_df <- df[res$selected_idx, , drop = FALSE]
    # selection_summary <- data.frame(
    #   Group = factor(c("All colonies", "Top 20% by phenotype"), levels = c("All colonies", "Top 20% by phenotype")),
    #   MeanColonyGV = c(mean(df$Colony_GV), mean(selected_df$Colony_GV)),
    #   MeanHoney = c(mean(df$Honey), mean(selected_df$Honey))
    # )
    selection_summary <- data.frame(
      Group = factor(
        c("All colonies", "Top 20% by phenotype"),
        levels = c("All colonies", "Top 20% by phenotype")
      ),
      MeanColonyGV = c(
        mean(df$Colony_GV, na.rm = TRUE),
        mean(selected_df$Colony_GV, na.rm = TRUE)
      ),
      MeanHoney = c(
        mean(df$Honey, na.rm = TRUE),
        mean(selected_df$Honey, na.rm = TRUE)
      )
    )
    delta_gv <- selection_summary$MeanColonyGV[2] - selection_summary$MeanColonyGV[1]

    p1 <- ggplot(df, aes(x = Colony_GV, y = Honey)) +
      geom_point(aes(color = Queen_GV1), size = 3, alpha = 0.85) +
      scale_color_gradient(low = bee_colors[["honey"]], high = bee_colors[["brown"]],
                           name = "Queen GV1") +
      geom_smooth(method = "lm", se = TRUE, color = bee_colors[["red"]], linetype = "dashed") +
      annotate("text", x = -Inf, y = Inf, hjust = -0.05, vjust = 1.3,
               label = paste0("r = ", cor_gv_honey),
               color = bee_colors[["red"]], fontface = "bold", size = 3.8) +
      labs(
        title = "Colony GV vs colony phenotype",
        subtitle = "Higher h² should tighten this relationship: phenotype tracks genetic merit more closely",
        x = "Colony GV",
        y = "Honey phenotype"
      ) +
      theme_bee()

    p2 <- ggplot(df, aes(x = Queen_GV1, y = Colony_GV)) +
      geom_point(aes(color = Worker_GV2), size = 3, alpha = 0.85) +
      scale_color_gradient(low = bee_colors[["honey"]], high = bee_colors[["blue"]],
                           name = "Mean worker GV2") +
      geom_smooth(method = "lm", se = TRUE, color = bee_colors[["purple"]], linetype = "dashed") +
      annotate("text", x = -Inf, y = Inf, hjust = -0.05, vjust = 1.3,
               label = paste0("r = ", cor_q_colony,
                              "
Input corA(Q-W) = ", round(res$corA, 2)),
               color = bee_colors[["purple"]], fontface = "bold", size = 3.6) +
      labs(
        title = "Queen GV vs colony GV",
        subtitle = "This shows how the worker component pulls colony merit toward or away from the queen as Q-W correlation changes",
        x = "Queen GV — trait 1",
        y = "Colony GV"
      ) +
      theme_bee()

    p3 <- ggplot(selection_summary, aes(x = Group, y = MeanColonyGV, fill = Group)) +
      geom_col(width = 0.6, show.legend = FALSE) +
      geom_text(aes(label = sprintf("%.2f", MeanColonyGV)), vjust = -0.35, fontface = "bold", size = 4) +
      scale_fill_manual(values = c(
        "All colonies" = bee_colors[["blue"]],
        "Top 20% by phenotype" = bee_colors[["gold"]]
      )) +
      annotate("text", x = 1.5, y = max(selection_summary$MeanColonyGV) * 1.12,
               label = paste0("Selected group mean GV gain = ", sprintf("%.2f", delta_gv),
                              "
Observed cor(Queen GV1, phenotype) = ", cor_q_honey),
               color = bee_colors[["brown"]], fontface = "bold", size = 3.8) +
      labs(
        title = "Selection on phenotype captures genetic merit",
        subtitle = "Top 20% of colonies are chosen by phenotype; at higher h² they should have higher mean GV",
        x = NULL,
        y = "Mean colony GV"
      ) +
      coord_cartesian(
        ylim = c(
          min(0, min(selection_summary$MeanColonyGV, na.rm = TRUE) * 1.05),
          max(selection_summary$MeanColonyGV, na.rm = TRUE) * 1.22
        )
      ) +
      # scale_y_continuous(limits = c(min(0, min(selection_summary$MeanColonyGV) * 1.05),
      #                               max(selection_summary$MeanColonyGV) * 1.22)) +
      theme_bee()

    gridExtra::grid.arrange(
      p1, p2, p3,
      layout_matrix = rbind(c(1, 2), c(3, 3)),
      heights = c(2.0, 1.35)
    )
  })

  output$qgTable <- renderTable({
    res <- qg_result()
    req(res)

    selected_honey <- res$honey[res$selected_idx]
    selected_gv <- res$colonyGV[res$selected_idx]

    data.frame(
      Metric = c(
        "Base random seed",
        "Quant. Genetics seed",
        "Heritability (h²)",
        "Additional colony env. SD",
        "Mean honey yield",
        "SD honey yield",
        "Minimum",
        "Maximum",
        "Mean colony GV",
        "Mean queen GV (trait 1)",
        "Mean queen GV (trait 2)",
        "Mean worker GV (trait 2)",
        "Obs. corr. colony GV vs honey",
        "Obs. corr. queen GV1 vs honey",
        "Obs. corr. queen GV1 vs colony GV",
        "Input genetic correlation Q-W",
        "Obs. corr. queen GV traits",
        "Obs. corr. queen GV1 vs worker GV2",
        "Selected colonies (top phenotype)",
        "Mean honey of selected colonies",
        "Mean colony GV of selected colonies",
        "Selected - all mean colony GV"
      ),
      Value = round(c(
        res$base_seed,
        res$qg_seed,
        res$h2,
        res$envSD,
        mean(res$honey),
        sd(res$honey),
        min(res$honey),
        max(res$honey),
        mean(res$colonyGV),
        mean(res$queenGV1),
        mean(res$queenGV2),
        mean(res$workerGV2),
        safe_cor(res$colonyGV, res$honey),
        safe_cor(res$queenGV1, res$honey),
        safe_cor(res$queenGV1, res$colonyGV),
        res$corA,
        safe_cor(res$queenGV1, res$queenGV2),
        safe_cor(res$queenGV1, res$workerGV2),
        res$selected_n,
        mean(selected_honey),
        mean(selected_gv),
        mean(selected_gv) - mean(res$colonyGV)
      ), 3)
    )
  }, striped = TRUE, bordered = TRUE, hover = TRUE)

  # ============================================================
  # TAB 6 – Multi-Colony
  # ============================================================
  mc_result <- eventReactive(input$runMC, {
    base_seed <- sanitize_seed(input$seedBase)
    mc_seed <- module_seed(base_seed, "mc")
    set.seed(mc_seed)

    withProgress(message = "🏠 Creating apiary…", value = 0, {

      nCol <- input$nColonies
      requested_founders <- input$nFoundersMC
      n_csd_alleles <- as.integer(input$nCsdAllelesMC)
      seg_sites_used <- safe_seg_sites(100L, n_csd_alleles)

      incProgress(0.10, detail = "Planning founders and DCA donors")
      min_total_founders <- nCol + 3L
      n_founders <- max(requested_founders, min_total_founders)
      n_dca_donors <- n_founders - nCol

      incProgress(0.15, detail = "Simulating founders")
      founderGenomes <- quickHaplo(
        nInd     = n_founders,
        nChr     = 1,
        segSites = seg_sites_used
      )

      incProgress(0.15, detail = "Creating queens and the DCA")
      SP <- SimParamBee$new(
        founderGenomes,
        nWorkers    = input$nWorkersMC,
        nDrones     = 100,
        nFathers    = 12,
        nCsdAlleles = n_csd_alleles,
        csdChr      = 1L
      )
      SP <- prepare_SP(SP)

      with_global_SP(SP, {
        basePop <- createVirginQueens(founderGenomes, simParamBee = SP)

        dca_info <- make_dca(
          queen_pop = basePop,
          donor_idx = seq_len(n_dca_donors),
          n_groups  = nCol,
          n_fathers = 12,
          SP = SP
        )

        droneGroups <- pullDroneGroupsFromDCA(
          dca_info$DCA,
          n = nCol,
          nDrones = 12,
          simParamBee = SP
        )

        incProgress(0.40, detail = "Building colonies")
        colonies <- vector("list", nCol)
        for (i in seq_len(nCol)) {
          col_i <- createColony(x = basePop[n_dca_donors + i], simParamBee = SP)
          col_i <- cross(
            col_i,
            drones = droneGroups[[i]],
            checkCross = "warning",
            simParamBee = SP
          )
          col_i <- buildUp(
            col_i,
            nWorkers = input$nWorkersMC,
            nDrones = 100,
            exact = TRUE,
            simParamBee = SP
          )
          colonies[[i]] <- col_i
        }

        incProgress(0.15, detail = "Applying optional swarming")
        swarmed <- rep(FALSE, nCol)
        if (isTRUE(input$doSwarmMC)) {
          n_swarm <- max(1L, round(nCol * input$swarmPropMC))
          swarm_idx <- sort(sample(seq_len(nCol), n_swarm))
          for (i in swarm_idx) {
            res_i <- SIMplyBee::swarm(colonies[[i]], p = 0.4, simParamBee = SP)
            colonies[[i]] <- res_i$remnant
            swarmed[i] <- TRUE
          }
        }
      })

      list(
        colonies = colonies,
        SP = SP,
        swarmed = swarmed,
        base_seed = base_seed,
        mc_seed = mc_seed,
        nCol = nCol,
        requested_founders = requested_founders,
        founders_used = n_founders,
        dca_donors = n_dca_donors,
        total_dca_drones = dca_info$total_dca_drones,
        n_csd_alleles = n_csd_alleles,
        requested_seg_sites = 100L,
        seg_sites_used = seg_sites_used,
        seg_sites_adjusted = seg_sites_used > 100L
      )
    })
  })

  output$mcPlot <- renderPlot({
    res <- mc_result()
    req(res)

    base_df <- data.frame(
      Colony  = paste0("C", seq_len(res$nCol)),
      Workers = sapply(res$colonies, function(col) colony_counts(col)["Workers"]),
      Drones  = sapply(res$colonies, function(col) colony_counts(col)["Drones"]),
      Swarmed = res$swarmed,
      stringsAsFactors = FALSE
    )

    df <- base_df %>%
      pivot_longer(c(Workers, Drones), names_to = "Caste", values_to = "Count")

    df$Colony <- factor(df$Colony, levels = paste0("C", seq_len(res$nCol)))

    max_total <- max(base_df$Workers + base_df$Drones)
    star_df <- base_df %>%
      filter(Swarmed) %>%
      mutate(
        Colony = factor(Colony, levels = paste0("C", seq_len(res$nCol))),
        StarY = Workers + Drones + max(50, 0.05 * max_total)
      )

    ggplot(df, aes(x = Colony, y = Count, fill = Caste)) +
      geom_col(position = "stack") +
      geom_point(
        data = star_df,
        aes(x = Colony, y = StarY),
        shape = 8, size = 4, color = bee_colors[["red"]],
        inherit.aes = FALSE
      ) +
      scale_fill_manual(values = c(
        "Workers" = bee_colors[["gold"]],
        "Drones"  = bee_colors[["blue"]]
      )) +
      labs(
        title = "Apiary colony sizes",
        subtitle = "⭐ = swarmed colony",
        x = "Colony",
        y = "Number of bees"
      ) +
      theme_bee() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })

  output$mcTable <- renderTable({
    res <- mc_result()

    expected_csd <- sapply(res$colonies, function(col) {
      csd <- get_csd_metrics(col, res$SP)
      if (isTRUE(csd$available)) sprintf("%.2f%%", 100 * csd$p_hom) else "Not mated"
    })

    data.frame(
      Colony                  = paste0("Colony ", seq_len(res$nCol)),
      QueenState              = sapply(res$colonies, queen_state),
      Workers                 = sapply(res$colonies, function(col) colony_counts(col)["Workers"]),
      Drones                  = sapply(res$colonies, function(col) colony_counts(col)["Drones"]),
      Fathers                 = sapply(res$colonies, function(col) colony_counts(col)["Fathers"]),
      Expected_CSD_hom_brood  = expected_csd,
      Productive              = sapply(res$colonies, function(col) if (isProductive(col)) "Yes" else "No"),
      Swarmed                 = ifelse(res$swarmed, "Yes", "No"),
      stringsAsFactors        = FALSE
    )
  }, striped = TRUE, bordered = TRUE, hover = TRUE)

  output$mcLog <- renderPrint({
    res <- mc_result()
    req(res)

    total_workers <- sum(sapply(res$colonies, function(col) colony_counts(col)["Workers"]))
    total_drones  <- sum(sapply(res$colonies, function(col) colony_counts(col)["Drones"]))
    productive_n  <- sum(sapply(res$colonies, isProductive))

    cat(sprintf("Apiary with %d colonies created.\n\n", res$nCol))
    cat(sprintf("  Base random seed   : %d\n", res$base_seed))
    cat(sprintf("  Multi-Colony seed  : %d\n", res$mc_seed))
    cat(sprintf("  Founders requested : %d\n", res$requested_founders))
    cat(sprintf("  Founders used      : %d\n", res$founders_used))
    cat(sprintf("  Segregating sites/chr: requested %d | used %d\n",
                res$requested_seg_sites, res$seg_sites_used))
    cat(sprintf("  Possible csd alleles: %d\n", res$n_csd_alleles))
    cat(sprintf("  Total workers       : %s\n", format(total_workers, big.mark = ",")))
    cat(sprintf("  Total drones        : %s\n", format(total_drones, big.mark = ",")))
    cat(sprintf("  Swarmed             : %d colonies\n", sum(res$swarmed)))
    cat(sprintf("  Productive          : %d colonies\n", productive_n))

    if (isTRUE(res$seg_sites_adjusted)) {
      cat("  Note: segSites per chromosome was increased automatically so the requested number of csd alleles could be represented at the csd locus.\n")
    }

    if (any(res$swarmed)) {
      cat("  Note: swarmed remnants carry a virgin queen and zero stored fathers until re-mated.\n")
    }

    if (res$founders_used > res$requested_founders) {
      cat("  Note: founder count was increased automatically to leave separate donor queens for the DCA and one founder queen per colony.\n")
    }
  })
}

# ---- Null-coalescing helper ----
`%||%` <- function(a, b) if (!is.null(a)) a else b

shinyApp(ui, server)
