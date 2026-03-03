# ============================================================
#  Dart Game T-Test Explorer - A test for BIOL1404 TTU
#  - Tab 1: raw dart score distributions
#  - Tabs 2-4: t-distribution simulations 
#  - Pooled SD entered as numeric box (empirical from class)
#  All colors are color blind safe
# ============================================================
#  install.packages(c("shiny", "ggplot2", "scico"))
#  shiny::runApp("dart_ttest_shiny_v3.R")
# ============================================================

library(shiny)
library(ggplot2)
library(scico)   # palettes

# ── Colorblind-safe palette (scico "roma") ──────────────────
#   roma[1] ~ deep blue  |  roma[end] ~ deep red/orange
#   Both distinguishable under deuteranopia & protanopia
ROMA   <- scico(10, palette = "roma")
COL_A      <- ROMA[2]    # blue-ish
COL_B      <- ROMA[9]    # red-ish
COL_OVER   <- scico(10, palette = "vanimo")[2]  # neutral mid purple
COL_REJECT <- "#90A198" # manually picked from the palette
COL_STAT   <- scico(5, palette = "bamako")[4] # green
COL_NULL   <- ROMA[2]
COL_ALT    <- ROMA[9]

# ── Pooled SD helper ─────────────────────────────────────────
pooled_sd <- function(sd_a, n_a, sd_b, n_b) {
  sqrt(((n_a - 1)*sd_a^2 + (n_b - 1)*sd_b^2) / (n_a + n_b - 2))
}

# ============================================================
#  UI
# ============================================================
ui <- fluidPage(
  tags$head(tags$style(HTML("
    body  { background-color:#1a1a2e; color:#e0e0e0;
            font-family:'Georgia',serif; }
    h2    { color:#E07B54; letter-spacing:1px; }
    h4    { color:#5B9BD5; }
    .well { background-color:#16213e; border:1px solid #0f3460; }
    label { color:#ccc !important; }
    hr    { border-color:#0f3460; }
    .shiny-output-error-validation { color:#ff9999; }
  "))),

  titlePanel(h2("🎯 Dart Game T-Test Explorer")),
  p("Interactive tool for exploring group differences — two-sample equal-variance t-test.",
    style = "color:#aaa; margin-bottom:16px;"),

  tabsetPanel(

    # ══════════════════════════════════════════════════════
    # TAB 1 : Raw Score Distributions (student-friendly)
    # ══════════════════════════════════════════════════════
    tabPanel("🎯 Dart Score Distributions",
      br(),
      fluidRow(
        column(3,
          wellPanel(
            h4("Before the game"),
            p("Use these sliders to explore what different scenarios look like.",
              style = "color:#aaa; font-size:12px;"),
            sliderInput("n_per_group", "Players per group (n):",
                        min = 3, max = 60, value = 15, step = 1),
            sliderInput("mean_a", "Group A average score:",
                        min = 10, max = 100, value = 55, step = 1),
            sliderInput("sigma_slide", "Spread of scores (SD) — theoretical:",
                        min = 1, max = 25, value = 12, step = 0.5),
            sliderInput("delta", "What if Group B scores higher by:",
                        min = 0, max = 40, value = 0, step = 1),
            helpText("Delta = 0 → both groups equal (null hypothesis)"),
            hr(),

            h4("After the game"),
            p("Enter your class results to overlay them on the plot.",
              style = "color:#aaa; font-size:12px;"),
            numericInput("mean_obs_a", "Group A observed mean:",
                         value = NA, step = 0.1),
            numericInput("mean_obs_b", "Group B observed mean:",
                         value = NA, step = 0.1),
            numericInput("sd_obs_a",   "Group A observed SD:",
                         value = NA, min = 0.1, step = 0.1),
            numericInput("sd_obs_b",   "Group B observed SD:",
                         value = NA, min = 0.1, step = 0.1),
            helpText("Pooled SD will be computed automatically from your data.
                      Leave blank to use the theoretical SD slider above."),
            hr(),
            sliderInput("alpha_raw", "Significance level (α):",
                        min = 0.01, max = 0.20, value = 0.05, step = 0.01),
            radioButtons("tail_raw", "Test direction:",
                         choices = c("Two-tailed (any difference?)" = "two",
                                     "One-tailed (did B score higher?)" = "right"),
                         selected = "two")
          )
        ),

        column(9,
          plotOutput("raw_dist_plot", height = "460px"),
          br(),
          fluidRow(
            column(3, wellPanel(h4("SD used"),       verbatimTextOutput("sd_used_out"))),
            column(3, wellPanel(h4("Mean difference"),verbatimTextOutput("raw_diff_out"))),
            column(3, wellPanel(h4("t-statistic"),    verbatimTextOutput("raw_t_out"))),
            column(3, wellPanel(h4("p-value / verdict"), verbatimTextOutput("raw_verdict_out")))
          ),
          wellPanel(
            p(strong("How to read this plot:"),
              " Dashed wide curves = spread of individual scores.
                Solid filled curves = where the group AVERAGE is likely to land.
                Wide overlap (purple) = hard to tell groups apart — could be luck!",
              style = "color:#ccc; font-size:13px;")
          )
        )
      )
    ), # end Tab 1

    # ══════════════════════════════════════════════════════
    # TAB 2 : Simulation
    # ══════════════════════════════════════════════════════
    tabPanel("🎲 Simulation (Randomness)",
      br(),
      fluidRow(
        column(3,
          wellPanel(
            h4("Simulation Settings"),
            sliderInput("sim_n",     "Players per group (n):",
                        min=3, max=60, value=15, step=1),
            sliderInput("sim_sigma", "Within-group SD:",
                        min=1, max=20, value=8,  step=0.5),
            sliderInput("sim_delta", "True mean difference (δ):",
                        min=0, max=20, value=0,  step=0.5),
            helpText("Set δ = 0 to simulate under H₀"),
            sliderInput("sim_alpha", "Alpha level:",
                        min=0.01, max=0.20, value=0.05, step=0.01),
            sliderInput("n_sims", "Number of dart games to simulate:",
                        min=50, max=5000, value=1000, step=50),
            actionButton("run_sim", "▶  Run Simulation",
                         style="background:#E07B54;color:white;border:none;
                                width:100%;margin-top:8px;font-size:15px;padding:8px;")
          )
        ),
        column(9,
          plotOutput("sim_plot", height="440px"),
          br(),
          wellPanel(h4("Summary"), verbatimTextOutput("sim_summary"))
        )
      )
    ), # end Tab 2

    # ══════════════════════════════════════════════════════
    # TAB 3 : p-value & Effect Size
    # ══════════════════════════════════════════════════════
    tabPanel("📐 p-value & Effect Size",
      br(),
      fluidRow(
        column(3,
          wellPanel(
            h4("Observed Result"),
            sliderInput("es_n",       "n per group:", min=3, max=60, value=15),
            sliderInput("es_sigma",   "SD:", min=1, max=20, value=8, step=0.5),
            sliderInput("es_obs_diff","Observed mean difference:",
                        min=-20, max=20, value=4, step=0.5),
            radioButtons("es_tail", "Test type:",
                         choices=c("Two-tailed"="two","One-tailed (right)"="right"),
                         selected="two"),
            hr(),
            h4("Cohen's d"),
            verbatimTextOutput("cohens_d")
          )
        ),
        column(9,
          plotOutput("pval_plot", height="440px"),
          br(),
          wellPanel(fluidRow(
            column(6, h4("t-statistic"), verbatimTextOutput("es_t_stat")),
            column(6, h4("p-value"),     verbatimTextOutput("es_pval"))
          ))
        )
      )
    ), # end Tab 3

    # ══════════════════════════════════════════════════════
    # TAB 4 : Teaching Notes
    # ══════════════════════════════════════════════════════
    tabPanel("📝 Teaching Notes",
      br(),
      fluidRow(column(10, offset=1,
        wellPanel(
          h4("Key Teaching Points"),
          tags$ol(
            tags$li(strong("Before/After design:"),
              " The 'Before the game' sliders let you set up theoretical expectations.
                The 'After the game' inputs let you enter real class data and overlay it.
                Both live on the same scale (actual dart scores 10–100)."),
            tags$li(strong("Two curve layers:"),
              " Dashed wide curves show individual score spread.
                Solid filled curves show where the GROUP MEAN will likely land — much narrower.
                The t-test is about those narrower curves."),
            tags$li(strong("Pooled SD:"),
              " When class SDs are entered, the app computes the pooled SD automatically
                and uses it for both the curves and the t-test calculation."),
            tags$li(strong("Delta slider:"),
              " Use before the game: 'What if one group truly scored 10 points higher?'
                Watch the curves separate. Ask students: at what delta do you think
                we could reliably detect the difference?"),
            tags$li(strong("Simulation tab:"),
              " Simulate 1000 tournaments under δ = 0. About α% reject H₀ by luck alone
                (Type I error). Raise δ to show power increasing."),
            tags$li(strong("Color palette:"),
              " Uses the scico 'roma' palette — distinguishable under deuteranopia,
                protanopia, and tritanopia (colorblind-safe).")
          ),
          hr(),
          h4("Suggested Classroom Sequence"),
          tags$ol(
            tags$li("Open Tab 1. Set delta = 0. Ask: 'Could we tell the groups apart if there's no real difference?'"),
            tags$li("Drag delta up to 5, 10, 20. Ask: 'When does the difference become obvious?'"),
            tags$li("Change n. Show how more players = narrower sampling curves = easier detection."),
            tags$li("Play the dart game. Enter means and SDs. Ask: 'Where does your result land?'"),
            tags$li("Tab 2: Simulate 1000 games at delta = 0. Count false positives."),
            tags$li("Tab 3: Connect p-value to the tail area on the t-distribution.")
          )
        )
      ))
    ) # end Tab 4
  ) # end tabsetPanel
) # end ui

# ============================================================
#  SERVER
# ============================================================
server <- function(input, output, session) {

  # ── dark theme ──────────────────────────────────────────
  dark_theme <- function() {
    theme_minimal(base_size = 14) +
      theme(
        plot.background   = element_rect(fill="#1a1a2e", colour=NA),
        panel.background  = element_rect(fill="#16213e", colour=NA),
        panel.grid        = element_line(colour="#0f3460"),
        text              = element_text(colour="#e0e0e0"),
        axis.text         = element_text(colour="#aaaaaa"),
        plot.title        = element_text(colour="#E07B54", face="bold", size=15),
        plot.subtitle     = element_text(colour="#aaaaaa", size=11),
        legend.background = element_rect(fill="#16213e"),
        legend.text       = element_text(colour="#e0e0e0"),
        legend.title      = element_text(colour="#cccccc")
      )
  }

  # ── Reactive: which SD to use? ───────────────────────────
  # Returns pooled SD from class data if all four values entered,
  # otherwise falls back to the slider.
  sigma_use <- reactive({
    a  <- input$mean_obs_a;  b  <- input$mean_obs_b
    sa <- input$sd_obs_a;    sb <- input$sd_obs_b
    n  <- input$n_per_group
    if (!is.na(a) && !is.na(b) && !is.na(sa) && !is.na(sb) &&
        sa > 0 && sb > 0) {
      pooled_sd(sa, n, sb, n)
    } else {
      input$sigma_slide
    }
  })

  class_data_complete <- reactive({
    !is.na(input$mean_obs_a) && !is.na(input$mean_obs_b) &&
    !is.na(input$sd_obs_a)   && !is.na(input$sd_obs_b)
  })

  mean_b_theory <- reactive({ input$mean_a + input$delta })

  df_raw   <- reactive({ 2 * input$n_per_group - 2 })
  se_diff  <- reactive({ sigma_use() * sqrt(2 / input$n_per_group) })

  # ══════════════════════════════════════════════════════════
  # TAB 1 PLOT
  # ══════════════════════════════════════════════════════════
  output$raw_dist_plot <- renderPlot({

    mu_a  <- input$mean_a
    mu_b  <- mean_b_theory()
    sigma <- sigma_use()
    n     <- input$n_per_group
    se    <- sigma / sqrt(n)

    # x range over dartboard, covering both groups
    x_lo  <- max(10,  min(mu_a, mu_b) - 4.5 * sigma)
    x_hi  <- min(100, max(mu_a, mu_b) + 4.5 * sigma)
    # expand if observed means go outside
    if (class_data_complete()) {
      x_lo <- max(10,  min(x_lo, input$mean_obs_a, input$mean_obs_b) - 2*sigma)
      x_hi <- min(100, max(x_hi, input$mean_obs_a, input$mean_obs_b) + 2*sigma)
    }
    xs <- seq(x_lo, x_hi, length.out = 1000)

    # individual score densities
    dA_pop  <- dnorm(xs, mu_a, sigma)
    dB_pop  <- dnorm(xs, mu_b, sigma)
    # sampling distribution of the mean
    dA_samp <- dnorm(xs, mu_a, se)
    dB_samp <- dnorm(xs, mu_b, se)

    df_pop  <- data.frame(x=xs, dA=dA_pop,  dB=dB_pop,  ov=pmin(dA_pop,  dB_pop))
    df_samp <- data.frame(x=xs, dA=dA_samp, dB=dB_samp, ov=pmin(dA_samp, dB_samp))

    p <- ggplot() +
      # individual distributions (wide, transparent)
      geom_area(data=df_pop,  aes(x, dA), fill=COL_A,    alpha=0.15) +
      geom_area(data=df_pop,  aes(x, dB), fill=COL_B,    alpha=0.15) +
      geom_area(data=df_pop,  aes(x, ov), fill=COL_OVER,  alpha=0.20) +
      geom_line(data=df_pop,  aes(x, dA), colour=COL_A,  linewidth=1.0, linetype="dashed") +
      geom_line(data=df_pop,  aes(x, dB), colour=COL_B,  linewidth=1.0, linetype="dashed") +
      # sampling distributions (narrow, solid)
      geom_area(data=df_samp, aes(x, dA), fill=COL_A,    alpha=0.40) +
      geom_area(data=df_samp, aes(x, dB), fill=COL_B,    alpha=0.40) +
      geom_area(data=df_samp, aes(x, ov), fill=COL_OVER,  alpha=0.50) +
      geom_line(data=df_samp, aes(x, dA), colour=COL_A,  linewidth=2.0) +
      geom_line(data=df_samp, aes(x, dB), colour=COL_B,  linewidth=2.0) +
      # theoretical mean lines
      geom_vline(xintercept=mu_a, colour=COL_A, linewidth=1.0, linetype="dotted") +
      geom_vline(xintercept=mu_b, colour=COL_B, linewidth=1.0, linetype="dotted") +
      # labels for theoretical means
      annotate("text", x=mu_a, y=max(dA_samp)*1.07,
               label=paste0("Group A\n(theory mean=", round(mu_a,1), ")"),
               colour=COL_A, size=4.2, fontface="bold", hjust=0.5) +
      annotate("text", x=mu_b, y=max(dB_samp)*1.07,
               label=paste0("Group B\n(theory mean=", round(mu_b,1), ")"),
               colour=COL_B, size=4.2, fontface="bold", hjust=0.5)

    # overlay observed class means if entered
    if (class_data_complete()) {
      obs_diff <- input$mean_obs_b - input$mean_obs_a
      t_val    <- obs_diff / se_diff()
      df_      <- df_raw()
      pv       <- if (input$tail_raw=="two") 2*pt(-abs(t_val), df_)
                  else pt(t_val, df_, lower.tail=FALSE)
      sig_col  <- if (pv <= input$alpha_raw) COL_REJECT else "#aaaaaa"

      p <- p +
        geom_vline(xintercept=input$mean_obs_a, colour=COL_A,
                   linewidth=2.0, linetype="solid") +
        geom_vline(xintercept=input$mean_obs_b, colour=COL_B,
                   linewidth=2.0, linetype="solid") +
        annotate("text", x=input$mean_obs_a, y=max(dA_samp)*0.55,
                 label=paste0("Your A\n", round(input$mean_obs_a,1)),
                 colour=COL_A, size=3.8, fontface="italic", hjust=-0.1) +
        annotate("text", x=input$mean_obs_b, y=max(dB_samp)*0.55,
                 label=paste0("Your B\n", round(input$mean_obs_b,1)),
                 colour=COL_B, size=3.8, fontface="italic", hjust=-0.1) +
        # annotate the gap with an arrow-style label
        annotate("text",
                 x=(input$mean_obs_a + input$mean_obs_b)/2,
                 y=max(dA_samp, dB_samp)*0.25,
                 label=paste0("observed\ndiff = ", round(obs_diff,1),
                              "\np = ", round(pv,3)),
                 colour=sig_col, size=4.0, fontface="bold", hjust=0.5)
    }

    p +
      scale_x_continuous(
        limits=c(x_lo, x_hi),
        breaks=seq(0, 100, by=5)
      ) +
      labs(
        title = paste0("Dart Score Distributions  |  n = ", n,
                       " per group  |  SD = ", round(sigma_use(),1)),
        subtitle = paste0(
          "Dashed = individual score spread   |   ",
          "Solid = where the group AVERAGE will land   |   ",
          "Purple = overlap\n",
          if (input$delta==0) "Currently: NO true difference (H\u2080 scenario)"
          else paste0("Scenario: Group B truly scores ", input$delta, " points higher"),
          if (class_data_complete()) "   |   Solid vertical lines = your class data" else ""
        ),
        x = "Dart Score  (range: 10 \u2013 100)",
        y = "Probability Density"
      ) +
      dark_theme()
  })

  # ── info boxes ──────────────────────────────────────────
  output$sd_used_out <- renderText({
    if (class_data_complete()) {
      sp <- pooled_sd(input$sd_obs_a, input$n_per_group,
                      input$sd_obs_b, input$n_per_group)
      paste0("Pooled SD = ", round(sp,2),
             "\n(from class data)")
    } else {
      paste0("Theoretical SD = ", input$sigma_slide,
             "\n(slider; enter class SDs to update)")
    }
  })

  output$raw_diff_out <- renderText({
    if (class_data_complete()) {
      paste0(round(input$mean_obs_b - input$mean_obs_a, 2), " pts\n(B minus A, observed)")
    } else {
      paste0(input$delta, " pts\n(theoretical delta)")
    }
  })

  output$raw_t_out <- renderText({
    if (!class_data_complete()) return("Enter class data\nto compute t")
    obs_diff <- input$mean_obs_b - input$mean_obs_a
    t_val    <- obs_diff / se_diff()
    paste0("t = ", round(t_val, 3), "\ndf = ", df_raw())
  })

  output$raw_verdict_out <- renderText({
    if (!class_data_complete()) return("Enter class data\nfor verdict")
    obs_diff <- input$mean_obs_b - input$mean_obs_a
    t_val    <- obs_diff / se_diff()
    df_      <- df_raw()
    pv <- if (input$tail_raw=="two") 2*pt(-abs(t_val), df_)
          else pt(t_val, df_, lower.tail=FALSE)
    sig <- pv <= input$alpha_raw
    paste0("p = ", round(pv,4), "\n",
           if (sig) paste0("SIGNIFICANT (α=",input$alpha_raw,")\nUnlikely to be luck!")
           else     paste0("Not significant (α=",input$alpha_raw,")\nCould be luck."))
  })

  # ══════════════════════════════════════════════════════════
  # TAB 2 : Simulation
  # ══════════════════════════════════════════════════════════
  sim_results <- eventReactive(input$run_sim, {
    n     <- input$sim_n
    sigma <- input$sim_sigma
    delta <- input$sim_delta
    nsim  <- input$n_sims
    alpha <- input$sim_alpha
    df    <- 2*n - 2
    crit  <- qt(1 - alpha/2, df)
    t_stats <- replicate(nsim, {
      g1 <- rnorm(n, 0,     sigma)
      g2 <- rnorm(n, delta, sigma)
      t.test(g1, g2, var.equal=TRUE)$statistic
    })
    list(t_stats=as.numeric(t_stats), crit=crit, df=df,
         alpha=alpha, delta=delta)
  })

  output$sim_plot <- renderPlot({
    res <- sim_results()
    ts  <- res$t_stats
    cr  <- res$crit
    rejected <- abs(ts) >= cr
    df_plot  <- data.frame(t=ts, rejected=rejected)

    # Use plain hex strings — avoids any RGB spec issues
    fill_vals <- c("FALSE"=COL_NULL, "TRUE"=COL_REJECT)

    ggplot(df_plot, aes(x=t, fill=rejected)) +
      geom_histogram(bins=60, colour="#222222", alpha=0.88) +
      scale_fill_manual(values=fill_vals,
                        labels=c("FALSE"="Not rejected","TRUE"="Rejected"),
                        name="") +
      geom_vline(xintercept= cr, colour=COL_REJECT, linewidth=1.2, linetype="dashed") +
      geom_vline(xintercept=-cr, colour=COL_REJECT, linewidth=1.2, linetype="dashed") +
      labs(
        title    = paste0("t-statistics across ", length(ts), " simulated dart games"),
        subtitle = paste0("\u03b4 = ", res$delta,
                          "   |   Red bars = fell in rejection region (\u03b1 = ",
                          res$alpha, ")"),
        x="t-statistic", y="Count"
      ) +
      dark_theme()
  })

  output$sim_summary <- renderText({
    res   <- sim_results()
    ts    <- res$t_stats
    n_rej <- sum(abs(ts) >= res$crit)
    pct   <- round(n_rej/length(ts)*100, 1)
    if (res$delta == 0) {
      paste0("Under H\u2080 (\u03b4 = 0): ", n_rej, " of ", length(ts),
             " games (", pct, "%) fell in the rejection region.\n",
             "Expected by chance alone: ~", round(res$alpha*100,1),
             "%.  All of these are Type I errors (false positives).")
    } else {
      ncp    <- res$delta / (input$sim_sigma * sqrt(2/input$sim_n))
      cr     <- res$crit; df <- res$df
      th_pwr <- round((pt(-cr,df,ncp=ncp)+pt(cr,df,ncp=ncp,lower.tail=FALSE))*100, 1)
      paste0("With \u03b4 = ", res$delta, ": ", n_rej, " of ", length(ts),
             " games (", pct, "%) correctly rejected H\u2080.\n",
             "Theoretical power at these settings: ~", th_pwr, "%.")
    }
  })

  # ══════════════════════════════════════════════════════════
  # TAB 3 : p-value & Effect Size
  # ══════════════════════════════════════════════════════════
  es_t  <- reactive({ input$es_obs_diff / (input$es_sigma * sqrt(2/input$es_n)) })
  es_df <- reactive({ 2*input$es_n - 2 })

  output$es_t_stat <- renderText({ round(es_t(), 4) })
  output$es_pval <- renderText({
    t  <- es_t(); df <- es_df()
    pv <- if(input$es_tail=="two") 2*pt(-abs(t),df) else pt(t,df,lower.tail=FALSE)
    paste0(round(pv,4), if(pv<0.001) "  (< 0.001)" else "")
  })
  output$cohens_d <- renderText({
    d   <- abs(input$es_obs_diff)/input$es_sigma
    lbl <- if(d<0.2)"negligible" else if(d<0.5)"small" else if(d<0.8)"medium" else "large"
    paste0("d = ", round(d,3), "  (", lbl, ")")
  })

  output$pval_plot <- renderPlot({
    t_obs <- es_t(); df <- es_df()
    xlim  <- c(-max(5,abs(t_obs)+1.5), max(5,abs(t_obs)+1.5))
    t_seq <- seq(xlim[1], xlim[2], length.out=800)
    dens  <- dt(t_seq, df)
    pv    <- if(input$es_tail=="two") 2*pt(-abs(t_obs),df)
             else pt(t_obs,df,lower.tail=FALSE)
    sc    <- if(pv<0.05) COL_REJECT else COL_NULL

    p <- ggplot() +
      geom_line(data=data.frame(x=t_seq,y=dens), aes(x,y),
                colour=COL_NULL, linewidth=1.6)

    shade <- function(cond) {
      xs <- t_seq[cond]; ys <- dt(xs, df)
      geom_area(data=data.frame(x=xs,y=ys), aes(x,y), fill=sc, alpha=0.55)
    }

    if(input$es_tail=="two") {
      p <- p + shade(t_seq >= abs(t_obs)) + shade(t_seq <= -abs(t_obs))
    } else {
      p <- p + shade(t_seq >= t_obs)
    }

    p +
      geom_vline(xintercept=t_obs, colour=COL_STAT, linewidth=1.8) +
      { if(input$es_tail=="two")
          geom_vline(xintercept=-t_obs, colour=COL_STAT,
                     linewidth=1.8, linetype="dashed") } +
      annotate("text", x=t_obs, y=max(dens)*0.6,
               label=paste0("t = ",round(t_obs,2)),
               colour=COL_STAT, hjust=-0.15, size=5, fontface="bold") +
      annotate("text", x=xlim[1]+0.2, y=max(dens)*0.95,
               label=paste0("p = ",round(pv,4),
                             if(pv<0.05)"\nsignificant!" else "\nnot significant"),
               colour=sc, hjust=0, size=5, fontface="bold") +
      labs(title="p-value as tail area beyond your observed t",
           subtitle=paste0("Shaded = P(|t| \u2265 ",round(abs(t_obs),2),
                           " | H\u2080)   Cohen's d = ",
                           round(abs(input$es_obs_diff)/input$es_sigma,2)),
           x="t-statistic", y="Density") +
      dark_theme() + xlim(xlim)
  })

} # end server

shinyApp(ui, server)
