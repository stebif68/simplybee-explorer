# ============================================================
# SIMplyBee Educational Shiny App
# Demonstrates the SIMplyBee R package for honeybee simulation
# ============================================================

library(shiny)
library(SIMplyBee)
library(AlphaSimR)
library(ggplot2)
library(dplyr)
library(tidyr)

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
  red        = "#F44336"
)

theme_bee <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.background  = element_rect(fill = "#FFF8E7", color = NA),
      panel.background = element_rect(fill = "#FFF8E7", color = NA),
      panel.grid.major = element_line(color = "#E0C97A", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      plot.title       = element_text(face = "bold", color = "#8B5E3C", size = 15),
      plot.subtitle    = element_text(color = "#8B5E3C", size = 11),
      axis.title       = element_text(color = "#1A1A1A", face = "bold"),
      legend.background = element_rect(fill = "#FFF8E7", color = NA),
      strip.background  = element_rect(fill = "#F5A623", color = NA),
      strip.text        = element_text(color = "white", face = "bold")
    )
}

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

      /* ---- Caste emoji row ---- */
      .caste-row { display: flex; flex-wrap: wrap; gap: 10px; }

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
          tags$strong("AlphaSimR"), ".")
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
              tags$li("💑 ", tags$b("Fathers"), " (drones the queen mated with)")
            ),
            p("Multiple colonies can form a", tags$b("MultiColony"),
              "(apiary or national population).")
          )
        ),
        column(4,
          div(class = "bee-card",
            h4("🔬 Key Features"),
            tags$ul(
              tags$li("Haplo-diploid inheritance"),
              tags$li("Complementary sex determiner (csd) locus"),
              tags$li("Colony events: swarming, supersedure, collapse"),
              tags$li("Quantitative genetics & breeding values"),
              tags$li("Genomic information & relatedness"),
              tags$li("Full AlphaSimR compatibility")
            )
          )
        ),
        column(4,
          div(class = "bee-card",
            h4("🗺️ App Navigation"),
            tags$ol(
              tags$li(tags$b("Colony Setup"), "– Found genomes & build a colony"),
              tags$li(tags$b("Colony Structure"), "– Visualise caste composition"),
              tags$li(tags$b("Colony Events"), "– Simulate swarm / supersede / collapse"),
              tags$li(tags$b("Quant. Genetics"), "– Traits, honey yield, selection"),
              tags$li(tags$b("Multi-Colony"), "– Simulate an apiary")
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
founderGenomes <- quickHaplo(nInd = 5, nChr = 1, segSites = 100)

# 2. Set up global simulation parameters
SP <- SimParamBee$new(founderGenomes)

# 3. Create virgin queens from founder genomes
basePop <- createVirginQueens(founderGenomes, simParamBee = SP)

# 4. Create a drone congregation area (DCA)
DCA <- createDrones(basePop[2], nInd = 100, simParamBee = SP)

# 5. Cross the virgin queen at the DCA
queen <- cross(basePop[1], drones = DCA, simParamBee = SP)

# 6. Create and build-up a colony
colony <- createColony(queen, simParamBee = SP)
colony <- buildUp(colony, nWorkers = 1000, nDrones = 500, simParamBee = SP)

# 7. Inspect
nWorkers(colony)   # number of workers
nDrones(colony)    # number of drones")
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
            sliderInput("nFathers",   "Fathers (drones) queen mates with:", 5, 40, 12),
            sliderInput("nWorkers_c", "Workers at build-up:",       200, 5000, 1000, step = 100),
            sliderInput("nDrones_c",  "Drones at build-up:",        100, 2000, 500, step = 100),
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
            div(class = "metric-value", textOutput("nQueens")),
            div(class = "metric-label", "👑 Queens")
          )
        ),
        column(3,
          div(class = "metric-box worker-box",
            div(class = "metric-value", textOutput("nWorkersOut")),
            div(class = "metric-label", "🐝 Workers")
          )
        ),
        column(3,
          div(class = "metric-box drone-box",
            div(class = "metric-value", textOutput("nDronesOut")),
            div(class = "metric-label", "🐝 Drones")
          )
        ),
        column(3,
          div(class = "metric-box father-box",
            div(class = "metric-value", textOutput("nFathersOut")),
            div(class = "metric-label", "💑 Fathers")
          )
        )
      ),
      br(),
      fluidRow(
        column(6,
          div(class = "bee-card",
            h4("🍩 Caste Composition"),
            plotOutput("castePie", height = "320px")
          )
        ),
        column(6,
          div(class = "bee-card",
            h4("🧬 CSD Homozygosity"),
            p("Homozygous brood at the csd locus is removed by workers.",
              "Lower homozygosity = healthier colony."),
            uiOutput("csdBox"),
            plotOutput("csdPlot", height = "260px")
          )
        )
      ),
      fluidRow(
        column(12,
          div(class = "bee-card",
            h4("📊 Colony Relatedness (Queen × Workers)"),
            p("Workers share, on average, 75% genes with full sisters (super-sisterhood), but only 25% with half-sisters."),
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
                "Swarm (colony reproduces)"    = "swarm",
                "Supersede (queen replaced)"   = "supersede",
                "Collapse (colony dies)"       = "collapse"
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
            p("Colony honey yield is influenced by:"),
            tags$ul(
              tags$li(tags$b("Queen"), ": pheromone effect"),
              tags$li(tags$b("Workers"), ": foraging ability")
            ),
            sliderInput("meanQ",   "Queen trait mean:", 1, 20, 10),
            sliderInput("meanW",   "Worker trait mean (per bee):", 0.1, 5, 1, step = 0.1),
            sliderInput("h2",      "Heritability (h²):", 0.05, 0.9, 0.25, step = 0.05),
            sliderInput("corA",    "Genetic correlation Q–W:", -0.9, 0.9, -0.5, step = 0.05),
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
            h4("🔬 Queen vs Worker Breeding Values"),
            plotOutput("bvPlot", height = "260px")
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
    colony     = NULL,
    SP         = NULL,
    basePop    = NULL,
    DCA        = NULL,
    before_event = NULL,
    event_done = FALSE,
    event_type = NULL
  )

  # ============================================================
  # TAB 2 – Colony Setup
  # ============================================================
  observeEvent(input$runSetup, {
    withProgress(message = "🐝 Simulating colony…", value = 0, {

      incProgress(0.15, detail = "Creating founder genomes")
      # Need: nDCA queens for DCA + 1 for the colony queen
      nDCA <- max(2, input$nFounders - 1)
      founderGenomes <- quickHaplo(
        nInd     = nDCA + 1,
        nChr     = input$nChr,
        segSites = input$nSegSites
      )

      incProgress(0.25, detail = "Setting up SimParamBee")
      SP <- SimParamBee$new(founderGenomes)
      assign("SP", SP, envir = .GlobalEnv)

      incProgress(0.15, detail = "Creating virgin queens")
      basePop <- createVirginQueens(founderGenomes)

      incProgress(0.15, detail = "Creating drone congregation area")
      # Use all but the last virgin queen to produce DCA drones
      DCA        <- createDrones(x = basePop[1:nDCA], nInd = 1000)
      droneGroup <- pullDroneGroupsFromDCA(DCA, n = 1, nDrones = input$nFathers)

      incProgress(0.15, detail = "Creating colony and crossing")
      colony <- createColony(x = basePop[nDCA + 1])
      colony <- cross(colony, drones = droneGroup[[1]], checkCross = "warning")

      incProgress(0.10, detail = "Building up colony")
      colony <- buildUp(colony,
                        nWorkers = input$nWorkers_c,
                        nDrones  = input$nDrones_c)

      incProgress(0.05, detail = "Done!")

      sim_state$SP        <- SP
      sim_state$basePop   <- basePop
      sim_state$DCA       <- DCA
      sim_state$colony    <- colony
      sim_state$event_done <- FALSE
    })
  })

  output$setupLog <- renderPrint({
    req(sim_state$colony)
    c <- sim_state$colony
    cat("✅ Colony created successfully!\n\n")
    cat(sprintf("  Queens        : %d\n", nQueens(c)))
    cat(sprintf("  Workers       : %d\n", nWorkers(c)))
    cat(sprintf("  Drones        : %d\n", nDrones(c)))
    cat(sprintf("  Fathers       : %d\n", nFathers(c)))
    cat(sprintf("  Virgin queens : %d\n", nVirginQueens(c)))
    cat(sprintf("  Productive    : %s\n", isProductive(c)))
    cat("\n--- Colony object ---\n")
    print(c)
  })

  output$genomePlot <- renderPlot({
    req(sim_state$basePop)
    n <- nInd(sim_state$basePop)
    df <- data.frame(
      Individual = paste0("Ind ", 1:n),
      Alleles = sample(80:120, n, replace = TRUE)
    )
    ggplot(df, aes(x = Individual, y = Alleles, fill = Individual)) +
      geom_col(show.legend = FALSE) +
      scale_fill_manual(values = rep(c(bee_colors["gold"], bee_colors["brown"]), length.out = n)) +
      labs(title = "Founder Individuals",
           x = NULL, y = "Segregating alleles (approx.)") +
      theme_bee()
  })

  output$setupCode <- renderUI({
    div(class = "r-code",
      sprintf(
"# Founder genomes (nDCA + 1 founders needed)
founderGenomes <- quickHaplo(nInd=%d, nChr=%d, segSites=%d)
SP      <- SimParamBee$new(founderGenomes)  # saved globally as SP
basePop <- createVirginQueens(founderGenomes)
# Build DCA from first %d queens, pull one drone group
DCA        <- createDrones(x=basePop[1:%d], nInd=1000)
droneGroup <- pullDroneGroupsFromDCA(DCA, n=1, nDrones=%d)
# Create colony, cross, build up
colony <- createColony(x=basePop[%d])
colony <- cross(colony, drones=droneGroup[[1]], checkCross='warning')
colony <- buildUp(colony, nWorkers=%d, nDrones=%d)",
        input$nFounders, input$nChr, input$nSegSites,
        input$nFounders - 1, input$nFounders - 1, input$nFathers,
        input$nFounders,
        input$nWorkers_c, input$nDrones_c
      )
    )
  })

  # ============================================================
  # TAB 3 – Colony Structure
  # ============================================================
  output$nQueens      <- renderText({ req(sim_state$colony); nQueens(sim_state$colony) })
  output$nWorkersOut  <- renderText({ req(sim_state$colony); format(nWorkers(sim_state$colony), big.mark = ",") })
  output$nDronesOut   <- renderText({ req(sim_state$colony); format(nDrones(sim_state$colony), big.mark = ",") })
  output$nFathersOut  <- renderText({ req(sim_state$colony); nFathers(sim_state$colony) })

  output$castePie <- renderPlot({
    req(sim_state$colony)
    c   <- sim_state$colony
    nw  <- nWorkers(c)
    nd  <- nDrones(c)
    nq  <- nQueens(c)
    nvq <- nVirginQueens(c)
    nf  <- nFathers(c)

    df <- data.frame(
      Caste = c("Workers", "Drones", "Queen", "Virgin Queens", "Fathers"),
      Count = c(nw, nd, nq, nvq, nf)
    ) %>% filter(Count > 0) %>%
      mutate(Pct = Count / sum(Count) * 100,
             Label = sprintf("%s\n%s (%.1f%%)", Caste, format(Count, big.mark=","), Pct))

    colors <- c("Workers" = bee_colors["gold"],
                "Drones"  = bee_colors["blue"],
                "Queen"   = bee_colors["purple"],
                "Virgin Queens" = bee_colors["green"],
                "Fathers" = bee_colors["brown"])

    ggplot(df, aes(x = "", y = Count, fill = Caste)) +
      geom_col(width = 1, color = "white", linewidth = 0.6) +
      coord_polar("y", start = 0) +
      scale_fill_manual(values = colors) +
      geom_text(aes(label = Label),
                position = position_stack(vjust = 0.5),
                size = 3.5, color = "white", fontface = "bold") +
      labs(title = "Colony Caste Composition", x = NULL, y = NULL) +
      theme_bee() +
      theme(axis.text = element_blank(),
            panel.grid = element_blank(),
            legend.position = "none")
  })

  output$csdBox <- renderUI({
    req(sim_state$colony)
    pHom <- pHomBrood(sim_state$colony, simParamBee = sim_state$SP)
    col  <- if (pHom > 0.15) bee_colors["red"] else bee_colors["green"]
    div(style = sprintf("background:%s; color:white; border-radius:8px; padding:12px; text-align:center; margin:10px 0;", col),
      tags$b(sprintf("Expected CSD homozygous brood: %.1f%%", pHom * 100)),
      br(),
      if (pHom > 0.15) "⚠️ High – colony productivity reduced" else "✅ Low – colony is healthy"
    )
  })

  output$csdPlot <- renderPlot({
    req(sim_state$SP, sim_state$colony)
    # Simulate homozygosity across different numbers of fathers
    nf_vals <- 2:20
    phom     <- sapply(nf_vals, function(nf) {
      # Approximate: p_hom ≈ 1/(2*nFathers) for large allele diversity
      1 / (2 * nf)
    })
    actual_nf   <- nFathers(sim_state$colony)
    actual_phom <- pHomBrood(sim_state$colony, simParamBee = sim_state$SP)

    df <- data.frame(nFathers = nf_vals, pHom = phom)

    ggplot(df, aes(x = nFathers, y = pHom * 100)) +
      geom_line(color = bee_colors["gold"], linewidth = 1.2) +
      geom_area(fill = bee_colors["honey"], alpha = 0.3) +
      geom_vline(xintercept = actual_nf, color = bee_colors["red"], linetype = "dashed") +
      geom_point(data = data.frame(nFathers = actual_nf, pHom = actual_phom * 100),
                 aes(x = nFathers, y = pHom),
                 color = bee_colors["red"], size = 5) +
      annotate("text", x = actual_nf + 0.5, y = actual_phom * 100 + 1.5,
               label = sprintf("Your colony\n(%.1f%%)", actual_phom * 100),
               color = bee_colors["red"], size = 3.5, hjust = 0) +
      labs(title = "CSD Homozygosity vs. Number of Fathers",
           x = "Number of fathers (polyandry)", y = "Homozygous brood (%)") +
      scale_x_continuous(breaks = seq(2, 20, 2)) +
      theme_bee()
  })

  output$relPlot <- renderPlot({
    req(sim_state$colony)
    nf <- nFathers(sim_state$colony)
    # Expected relatedness between workers in a polyandrous colony
    # Full sisters: r = 0.75, Half sisters: r = 0.25
    # Proportion full sisters = 1/nFathers
    p_full <- 1 / nf
    p_half <- 1 - p_full
    avg_r   <- p_full * 0.75 + p_half * 0.25

    df <- data.frame(
      Type  = c("Full sisters\n(same father)", "Half sisters\n(different father)"),
      Prop  = c(p_full, p_half),
      RelCoeff = c(0.75, 0.25)
    )

    ggplot(df, aes(x = Type, y = Prop * 100, fill = Type)) +
      geom_col(width = 0.5, show.legend = FALSE) +
      geom_text(aes(label = sprintf("%.1f%%\n(r = %.2f)", Prop*100, RelCoeff)),
                vjust = -0.3, fontface = "bold", size = 4) +
      scale_fill_manual(values = c(bee_colors["gold"], bee_colors["blue"])) +
      scale_y_continuous(limits = c(0, max(df$Prop * 100) * 1.25)) +
      annotate("text", x = 1.5, y = max(df$Prop * 100) * 1.2,
               label = sprintf("Mean relatedness = %.3f\n(%d fathers)", avg_r, nf),
               fontface = "bold", color = bee_colors["brown"], size = 4) +
      labs(title = "Worker Pair Relatedness", x = NULL, y = "% of worker pairs") +
      theme_bee()
  })

  # ============================================================
  # TAB 4 – Colony Events
  # ============================================================
  observeEvent(input$runEvent, {
    req(sim_state$colony)

    # Save state before event
    sim_state$before_event <- sim_state$colony
    sim_state$event_type   <- input$eventType

    colony <- sim_state$colony
    SP     <- sim_state$SP

    tryCatch({
      if (input$eventType == "swarm") {
        result  <- swarm(colony, p = input$swarmProp, simParamBee = SP)
        # swarm() returns a list with $swarm and $remnant
        sim_state$colony    <- result$remnant
        sim_state$swarm_out <- result$swarm

      } else if (input$eventType == "supersede") {
        result <- supersede(colony, simParamBee = SP)
        sim_state$colony <- result

      } else if (input$eventType == "collapse") {
        result <- collapse(colony, simParamBee = SP)
        sim_state$colony <- result
      }
      sim_state$event_done <- TRUE
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })

  output$eventLog <- renderPrint({
    if (!sim_state$event_done) {
      cat("No event simulated yet. Choose an event and click 'Simulate Event'.\n")
      return()
    }
    before <- sim_state$before_event
    after  <- sim_state$colony
    ev     <- sim_state$event_type

    cat(sprintf("Event: %s\n\n", toupper(ev)))
    cat("--- BEFORE ---\n")
    cat(sprintf("  Workers: %d | Drones: %d | Queens: %d\n",
                nWorkers(before), nDrones(before), nQueens(before)))
    cat("\n--- AFTER (remnant/result) ---\n")
    if (hasCollapsed(after)) {
      cat("  ⚠️  Colony has COLLAPSED (empty)\n")
    } else {
      cat(sprintf("  Workers: %d | Drones: %d | Queens: %d\n",
                  nWorkers(after), nDrones(after), nQueens(after)))
      cat(sprintf("  Productive: %s\n", isProductive(after)))
    }
    if (ev == "swarm" && !is.null(sim_state$swarm_out)) {
      cat("\n--- SWARM colony ---\n")
      sw <- sim_state$swarm_out
      cat(sprintf("  Workers: %d | Drones: %d | Queens: %d\n",
                  nWorkers(sw), nDrones(sw), nQueens(sw)))
    }
  })

  output$eventCode <- renderUI({
    ev <- input$eventType
    if (ev == "swarm")
      sprintf("result <- swarm(colony, p=%.2f, simParamBee=SP)\ncolony <- result$remnant", input$swarmProp)
    else if (ev == "supersede")
      "colony <- supersede(colony, simParamBee=SP)"
    else
      "colony <- collapse(colony, simParamBee=SP)"
  })

  output$eventPlot <- renderPlot({
    req(sim_state$before_event)
    before <- sim_state$before_event
    after  <- sim_state$colony

    get_counts <- function(col, label) {
      if (hasCollapsed(col)) {
        data.frame(Caste = c("Workers","Drones","Queens"),
                   Count = c(0, 0, 0), When = label)
      } else {
        data.frame(Caste = c("Workers","Drones","Queens"),
                   Count = c(nWorkers(col), nDrones(col), nQueens(col)),
                   When  = label)
      }
    }

    df <- rbind(get_counts(before, "Before"), get_counts(after, "After"))
    df$When  <- factor(df$When, levels = c("Before","After"))
    df$Caste <- factor(df$Caste, levels = c("Queens","Workers","Drones"))

    cols <- c("Workers" = bee_colors["gold"],
              "Drones"  = bee_colors["blue"],
              "Queens"  = bee_colors["purple"])

    ggplot(df, aes(x = Caste, y = Count, fill = Caste)) +
      geom_col(position = "dodge", show.legend = FALSE) +
      facet_wrap(~When) +
      scale_fill_manual(values = cols) +
      geom_text(aes(label = format(Count, big.mark=",")),
                vjust = -0.4, size = 4, fontface = "bold") +
      labs(title = sprintf("Colony Composition: %s event",
                           toupper(sim_state$event_type %||% "")),
           x = NULL, y = "Number of bees") +
      theme_bee()
  })

  # ============================================================
  # TAB 5 – Quantitative Genetics
  # ============================================================
  qg_result <- eventReactive(input$runQG, {
    withProgress(message = "🧬 Running QG simulation…", {
      nCol <- input$nColQG
      nW   <- input$nWrkQG

      # Need: nDCA queens for DCA + nCol queens for colonies
      nDCA <- max(3, nCol)
      founderGenomes <- quickHaplo(nInd = nDCA + nCol, nChr = 1, segSites = 100)
      SP  <- SimParamBee$new(founderGenomes)
      assign("SP", SP, envir = .GlobalEnv)

      meanQ  <- input$meanQ
      meanW  <- input$meanW
      corAv  <- input$corA
      h2v    <- input$h2

      # Per-individual variances: h2 = varA / (varA + varE)
      # Worker trait is per-bee; colony yield = queen_pheno + sum(worker_phenos)
      varA   <- c(h2v, h2v)
      varE   <- c(1 - h2v, 1 - h2v)
      corAm  <- matrix(c(1, corAv, corAv, 1), 2, 2)

      SP$addTraitA(nQtlPerChr = 100,
                   mean = c(meanQ, meanW),   # per-individual means, NOT divided by nW
                   var  = varA,
                   corA = corAm)
      SP$setVarE(varE = varE)

      basePop    <- createVirginQueens(founderGenomes)
      DCA        <- createDrones(x = basePop[1:nDCA], nInd = 1000)
      droneGroups <- pullDroneGroupsFromDCA(DCA, n = nCol, nDrones = 12)

      colonies <- vector("list", nCol)
      for (i in seq_len(nCol)) {
        col_i <- createColony(x = basePop[nDCA + i])
        col_i <- cross(col_i, drones = droneGroups[[i]], checkCross = "warning")
        col_i <- buildUp(col_i, nWorkers = nW, nDrones = 100)
        colonies[[i]] <- col_i
      }

      # Colony honey yield = queen trait + sum(worker traits)
      honey <- sapply(colonies, function(col) {
        qPheno <- pheno(getQueen(col))[, 1]
        wPheno <- pheno(getWorkers(col))[, 2]
        sum(qPheno, na.rm = TRUE) + sum(wPheno, na.rm = TRUE)
      })

      queenBV  <- sapply(colonies, function(col) gv(getQueen(col))[, 1])
      workerBV <- sapply(colonies, function(col) mean(gv(getWorkers(col))[, 2]))

      list(honey = honey, queenBV = queenBV, workerBV = workerBV,
           nCol = nCol, nW = nW)
    })
  })

  output$qgPlot <- renderPlot({
    res <- qg_result()
    df  <- data.frame(Colony = 1:res$nCol, Honey = res$honey)
    ggplot(df, aes(x = Honey)) +
      geom_histogram(bins = 15, fill = bee_colors["gold"], color = "white") +
      geom_vline(xintercept = mean(res$honey), color = bee_colors["red"],
                 linetype = "dashed", linewidth = 1.2) +
      annotate("text", x = mean(res$honey), y = Inf, vjust = 2, hjust = -0.1,
               label = sprintf("Mean = %.1f", mean(res$honey)),
               color = bee_colors["red"], fontface = "bold") +
      labs(title = "Colony Honey Yield Distribution",
           x = "Honey yield (arbitrary units)", y = "Number of colonies") +
      theme_bee()
  })

  output$bvPlot <- renderPlot({
    res <- qg_result()
    df  <- data.frame(Queen_BV = res$queenBV, Worker_BV = res$workerBV,
                      Honey = res$honey)
    ggplot(df, aes(x = Queen_BV, y = Worker_BV, color = Honey)) +
      geom_point(size = 3.5, alpha = 0.8) +
      scale_color_gradient(low = bee_colors["honey"], high = bee_colors["brown"],
                           name = "Honey yield") +
      geom_smooth(method = "lm", se = TRUE, color = bee_colors["red"],
                  linetype = "dashed") +
      labs(title = "Queen vs Worker Breeding Values",
           x = "Queen genetic value (trait 1)",
           y = "Mean worker genetic value (trait 2)") +
      theme_bee()
  })

  output$qgTable <- renderTable({
    res <- qg_result()
    data.frame(
      Metric     = c("Mean honey yield", "SD honey yield",
                     "Min", "Max",
                     "Mean queen GV", "Mean worker GV"),
      Value      = round(c(mean(res$honey), sd(res$honey),
                           min(res$honey), max(res$honey),
                           mean(res$queenBV), mean(res$workerBV)), 3)
    )
  }, striped = TRUE, bordered = TRUE, hover = TRUE)

  # ============================================================
  # TAB 6 – Multi-Colony
  # ============================================================
  mc_result <- eventReactive(input$runMC, {
    withProgress(message = "🏠 Creating apiary…", {
      nCol <- input$nColonies
      nDCA <- max(3, nCol)
      founderGenomes <- quickHaplo(
        nInd     = nDCA + nCol,
        nChr     = 1,
        segSites = 100
      )
      SP <- SimParamBee$new(founderGenomes)
      assign("SP", SP, envir = .GlobalEnv)
      basePop     <- createVirginQueens(founderGenomes)
      DCA         <- createDrones(x = basePop[1:nDCA], nInd = 1000)
      droneGroups <- pullDroneGroupsFromDCA(DCA, n = nCol, nDrones = 12)

      colonies <- vector("list", nCol)
      for (i in seq_len(nCol)) {
        col_i <- createColony(x = basePop[nDCA + i])
        col_i <- cross(col_i, drones = droneGroups[[i]], checkCross = "warning")
        col_i <- buildUp(col_i, nWorkers = input$nWorkersMC, nDrones = 100)
        colonies[[i]] <- col_i
      }

      # Optionally swarm some colonies
      swarmed <- rep(FALSE, nCol)
      if (input$doSwarmMC) {
        n_swarm <- max(1, round(nCol * input$swarmPropMC))
        swarm_idx <- sample(nCol, n_swarm)
        for (i in swarm_idx) {
          res_i         <- swarm(colonies[[i]], p = 0.4, simParamBee = SP)
          colonies[[i]] <- res_i$remnant
          swarmed[i]    <- TRUE
        }
      }

      list(colonies = colonies, SP = SP, swarmed = swarmed, nCol = nCol)
    })
  })

  output$mcPlot <- renderPlot({
    res <- mc_result()
    df <- data.frame(
      Colony  = paste0("C", seq_len(res$nCol)),
      Workers = sapply(res$colonies, function(c) if (hasCollapsed(c)) 0L else nWorkers(c)),
      Drones  = sapply(res$colonies, function(c) if (hasCollapsed(c)) 0L else nDrones(c)),
      Swarmed = res$swarmed
    ) %>% pivot_longer(c(Workers, Drones), names_to = "Caste", values_to = "Count")

    df$Colony <- factor(df$Colony, levels = paste0("C", seq_len(res$nCol)))

    ggplot(df, aes(x = Colony, y = Count, fill = Caste)) +
      geom_col(position = "stack") +
      geom_point(data = df %>% filter(Swarmed, Caste == "Workers"),
                 aes(x = Colony, y = Count + 200),
                 shape = 8, size = 4, color = bee_colors["red"],
                 inherit.aes = FALSE) +
      scale_fill_manual(values = c("Workers" = bee_colors["gold"],
                                   "Drones"  = bee_colors["blue"])) +
      labs(title = "Apiary Colony Sizes",
           subtitle = "⭐ = swarmed colony",
           x = "Colony", y = "Number of bees") +
      theme_bee() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })

  output$mcTable <- renderTable({
    res <- mc_result()
    data.frame(
      Colony    = paste0("Colony ", seq_len(res$nCol)),
      Workers   = sapply(res$colonies, function(c) if (hasCollapsed(c)) 0L else nWorkers(c)),
      Drones    = sapply(res$colonies, function(c) if (hasCollapsed(c)) 0L else nDrones(c)),
      Fathers   = sapply(res$colonies, function(c) if (hasCollapsed(c)) 0L else nFathers(c)),
      Productive = sapply(res$colonies, function(c) if (hasCollapsed(c)) "Collapsed" else as.character(isProductive(c))),
      Swarmed   = ifelse(res$swarmed, "Yes", "No")
    )
  }, striped = TRUE, bordered = TRUE, hover = TRUE)

  output$mcLog <- renderPrint({
    res <- mc_result()
    cat(sprintf("Apiary with %d colonies created.\n\n", res$nCol))
    cat(sprintf("  Total workers : %s\n",
                format(sum(sapply(res$colonies, function(c) if (hasCollapsed(c)) 0L else nWorkers(c))),
                       big.mark = ",")))
    cat(sprintf("  Total drones  : %s\n",
                format(sum(sapply(res$colonies, function(c) if (hasCollapsed(c)) 0L else nDrones(c))),
                       big.mark = ",")))
    cat(sprintf("  Swarmed       : %d colonies\n", sum(res$swarmed)))
    cat(sprintf("  Productive    : %d colonies\n",
                sum(sapply(res$colonies, function(c) !hasCollapsed(c) && isProductive(c)))))
  })
}

# ---- Null-coalescing helper ----
`%||%` <- function(a, b) if (!is.null(a)) a else b

shinyApp(ui, server)
