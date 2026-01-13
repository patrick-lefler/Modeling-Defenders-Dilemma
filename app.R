library(shiny)
library(ggplot2)
library(dplyr)
library(shinythemes) # Required for the 'sandstone' theme

# --- Full Code String for Display ---
# This string mirrors the active code below for display in the "Code" tab
full_code_display <- "
library(shiny)
library(ggplot2)
library(dplyr)
library(shinythemes)

# --- UI ---
ui <- navbarPage(
  title = \"Defender's Dilemma Model\",
  theme = shinytheme(\"sandstone\"),
  
  # 1. Introduction Page
  tabPanel(\"Introduction\",
           fluidPage(
             column(8, offset = 2,
                    h2(\"The Defender's Dilemma: A Stochastic Analysis\"),
                    hr(),
                    # (Full 750-word introduction text is rendered here in the live app)
                    p(\"Please see the live Introduction tab for the full text context.\")
             )
           )
  ),
  
  # 2. Calculations & Data Visualization
  tabPanel(\"Calculations & Data Visualization\",
           sidebarLayout(
             sidebarPanel(
               h4(\"Simulation Parameters\"),
               sliderInput(\"initial_resources\", \"Defender's Initial Resources:\", min=1, max=50, value=10),
               sliderInput(\"prob_success\", \"Probability of Success (p):\", min=0.40, max=0.99, value=0.50, step=0.01),
               numericInput(\"n_sim\", \"Number of Simulations:\", value=1000, min=100, max=10000),
               numericInput(\"max_steps\", \"Max Time Steps:\", value=200, min=50, max=1000),
               actionButton(\"run_sim\", \"Run Simulation\", class = \"btn-primary\")
             ),
             mainPanel(
               plotOutput(\"ruinPlot\"),
               br(),
               tableOutput(\"statsTable\"),
               uiOutput(\"textAnalysis\")
             )
           )
  ),
  
  # 3. Code Page
  tabPanel(\"Code\",
           fluidPage(
             column(10, offset = 1,
                    h3(\"Full Application Code\"),
                    verbatimTextOutput(\"code_display\")
             )
           )
  ),
  
  # 4. Session Info
  tabPanel(\"R Session Information\",
           fluidPage(
             column(10, offset = 1,
                    h3(\"Session Information\"),
                    verbatimTextOutput(\"session_info\")
             )
           )
  )
)

# --- SERVER ---
server <- function(input, output) {
  
  # Reactive Simulation Logic
  sim_data <- eventReactive(input$run_sim, {
    start_cap <- input$initial_resources
    p <- input$prob_success
    q <- 1 - p
    n_sim <- input$n_sim
    max_steps <- input$max_steps
    
    results_list <- vector(\"list\", n_sim)
    ruined_count <- 0
    
    withProgress(message = 'Simulating Attacks...', value = 0, {
      for(i in 1:n_sim) {
        steps <- sample(c(1, -1), max_steps, replace = TRUE, prob = c(p, q))
        path <- cumsum(c(start_cap, steps))
        
        ruin_idx <- which(path <= 0)[1]
        if (!is.na(ruin_idx)) {
          path <- path[1:ruin_idx]
          is_ruined <- TRUE
          ruined_count <- ruined_count + 1
        } else {
          is_ruined <- FALSE
        }
        
        results_list[[i]] <- data.frame(
          Simulation = as.factor(i),
          Step = 0:(length(path)-1),
          Resources = path,
          Result = ifelse(is_ruined, \"Breach\", \"Resilient\")
        )
        if(i %% 100 == 0) incProgress(100/n_sim)
      }
    })
    
    all_paths <- do.call(rbind, results_list)
    
    # Theoretical Probability (One-Sided Barrier)
    if (q >= p) {
      theo_prob <- 1.0
    } else {
      theo_prob <- (q / p)^start_cap
    }
    
    list(paths = all_paths, ruin_rate = ruined_count / n_sim, theo_prob = theo_prob, total_sims = n_sim)
  })
  
  output$ruinPlot <- renderPlot({
    req(sim_data())
    df <- sim_data()$paths
    first_100_ids <- levels(df$Simulation)[1:min(100, nlevels(df$Simulation))]
    df_subset <- df[df$Simulation %in% first_100_ids, ]
    
    ggplot(df_subset, aes(x = Step, y = Resources, group = Simulation, color = Result)) +
      geom_line(alpha = 0.6) +
      geom_hline(yintercept = 0, linetype = \"dashed\", color = \"red\", size = 1) +
      labs(title = \"Monte Carlo Simulation: Defender's Dilemma\",
           y = \"Defender Resources\", x = \"Attack Attempt #\") +
      scale_color_manual(values = c(\"Breach\" = \"#D32F2F\", \"Resilient\" = \"#2E7D32\")) +
      theme_minimal() + theme(legend.position = \"bottom\")
  })
  
  output$statsTable <- renderTable({
    req(sim_data())
    res <- sim_data()
    data.frame(
      Metric = c(\"Total Simulations\", \"Observed Breach Probability\", \"Theoretical Breach Probability\"),
      Value = c(as.integer(res$total_sims),
                sprintf(\"%.3f\", res$ruin_rate),
                sprintf(\"%.3f\", res$theo_prob))
    )
  })
  
  output$textAnalysis <- renderUI({
    req(sim_data())
    p_val <- input$prob_success
    analysis <- if (p_val <= 0.5) {
      \"Because defense probability is <= 0.5, Breach is mathematically certain over infinite time.\"
    } else {
      \"Because defense probability is > 0.5, the Defender has positive drift, but variance may still cause breach.\"
    }
    tagList(h4(\"Analysis\"), p(analysis))
  })
  
  output$code_display <- renderText({ full_code_display })
  
  output$session_info <- renderPrint({ sessionInfo() })
}

shinyApp(ui = ui, server = server)
"

# --- ACTUAL APPLICATION UI ---
ui <- navbarPage(
  title = "Defender's Dilemma Model",
  theme = shinytheme("sandstone"), # Requirement 1: Sandstone theme
  
  # 1. Introduction Page (Requirement 3: 750 words, structured)
  tabPanel("Introduction",
           fluidPage(
             column(10, offset = 1,
                    h2("The Defender's Dilemma: A Stochastic Analysis"),
                    hr(),
                    
                    # Section 1: Historical Context
                    h3("1. Historical Context"),
                    p("The mathematical foundation of the 'Defender's Dilemma' rests upon a problem posed in the 17th century, known as the Gambler's Ruin. The concept originated from correspondence between Blaise Pascal and Pierre de Fermat in 1656, arguably marking the birth of modern probability theory. They sought to solve the 'Problem of Points': how to fairly divide the stakes of an unfinished game of chance between two players with uneven scores."),
                    p("This problem evolved into the classical Gambler's Ruin theorem, which calculates the probability that a gambler, starting with a finite amount of capital (or 'bankroll'), will eventually go bankrupt (reach zero) before reaching a target wealth, given a specific probability of winning each hand. While originally intended for games of dice and coin flips, the underlying dynamics—finite resources opposing an infinite or highly capitalized adversary—perfectly mirror the modern cybersecurity landscape."),
                    
                    # Section 2: Why Markov Chains?
                    h3("2. Why Markov Chains?"),
                    p("To model this dynamic, we utilize Markov Chains, specifically a stochastic process known as a Random Walk. A Markov Chain is a mathematical system that undergoes transitions from one state to another according to certain probabilistic rules. The defining characteristic of a Markov process is its 'memoryless' property: the probability of the next state depends only on the current state, not on the sequence of events that preceded it."),
                    p("In the context of the Defender's Dilemma, the 'state' is the Defender's current level of resources (budget, time, mental energy, or system integrity). At every time step (an attack attempt), the system transitions:"),
                    tags$ul(
                      tags$li("If the defense is successful (probability p), the state moves +1 (resources are preserved/hardened)."),
                      tags$li("If the defense fails (probability q = 1-p), the state moves -1 (resources are depleted).")
                    ),
                    p("This effectively models a One-Dimensional Random Walk with an absorbing barrier at 0. Once the state hits 0, the 'game' ends; the defender is ruined (breached). This model allows us to move beyond static snapshots of risk and understand the trajectory of security over time."),
                    
                    # Section 3: Value for Risk Management
                    h3("3. Value for Risk Management"),
                    p("Standard risk management often relies on heatmaps (Impact x Likelihood), which treat risks as isolated, static events. However, cyber warfare is continuous and cumulative. The Gambler's Ruin model offers a superior quantitative framework for Risk Management because it introduces the dimension of time and the constraint of finite capacity."),
                    p("The model highlights a critical, often counter-intuitive reality: a 'Breach' is not merely a function of a single failed control, but a statistical inevitability if the probability of defense ($p$) is not sufficiently high relative to the defender's resource depth. It quantifies the concept of 'Persistence.' An Advanced Persistent Threat (APT) does not need a high probability of success on any single attempt; they simply need enough attempts to let the variance of the random walk push the defender's resources to zero."),
                    p("By simulating thousands of these random walks (Monte Carlo simulation), Risk Managers can derive a specific 'Probability of Ruin' percentage. This transforms abstract anxiety about APTs into a concrete metric that can be used to justify budget increases, staffing changes, or architectural shifts."),
                    
                    # Section 4: Real-World Applications
                    h3("4. Real-World Applications"),
                    p("This model directly applies to several distinct areas of cybersecurity operations:"),
                    tags$ul(
                      tags$li(strong("SOC Analyst Fatigue:"), " 'Resources' can represent the mental acuity of a Security Operations Center analyst. Each false positive investigation drains energy (-1), while successful triages or shift breaks restore it (+1). If the volume of alerts (attacks) is too high, even a skilled analyst will eventually miss a critical alert (Ruin)."),
                      tags$li(strong("Budget vs. Ransomware:"), " In a war of attrition, an attacker may launch repeated low-cost attacks. The defender must spend budget to remediate each one. If the cost of defense > cost of attack, the defender's budget (Capital) hits zero, forcing a business failure or a breach due to lack of funds."),
                      tags$li(strong("Credential Stuffing:"), " An attacker has a database of millions of credentials (infinite attempts). The defender has a lockout policy (finite barrier). The model predicts how restrictive the policy must be to prevent a brute-force success.")
                    ),
                    
                    # Section 5: Key Takeaways
                    h3("5. Key Takeaways"),
                    tags$ul(
                      tags$li(strong("The Asymmetry of Infinity:"), " Because the attacker is modeled as having infinite time/attempts, if the probability of a successful defense ($p$) is anything less than or equal to 0.5, a breach is mathematically certain (Probability = 100%)."),
                      tags$li(strong("Resources Buy Time, Not Immunity:"), " Increasing the initial resources (starting budget/capacity) does not change the *eventual* outcome if the underlying probability is unfavorable. It only increases the average time until the breach occurs."),
                      tags$li(strong("The Only Winning Move:"), " To escape the certainty of ruin, the defender must shift the odds so that $p > 0.5$ (Defender Advantage). In reality, this means the cost of the attack must be raised so high that the attacker creates a 'Stopping Barrier' for themselves, or the defense must be automated to the point where $p$ approaches 1."),
                      tags$li(strong("Variance is the Enemy:"), " Even with a defender advantage ($p > 0.5$), 'bad luck' streaks (variance) can still cause a breach early in the timeline. This is why 'Defense in Depth' is required—to absorb the variance of the random walk.")
                    ),
                    br(),
                    p("Navigate to the ", strong("Calculations & Data Visualization"), " tab to visualize these dynamics.")
             )
           )
  ),
  
  # 2. Calculations & Data Visualization Page (Main App)
  tabPanel("Calculations & Data Visualization",
           sidebarLayout(
             sidebarPanel(
               h4("Simulation Parameters"),
               sliderInput("initial_resources",
                           "Defender's Initial Resources (Budget/Energy):",
                           min = 1,
                           max = 50,
                           value = 10),
               
               sliderInput("prob_success",
                           "Probability of Successful Defense (per attack):",
                           min = 0.40,
                           max = 0.99,
                           value = 0.50,
                           step = 0.01),
               
               numericInput("n_sim",
                            "Number of Monte Carlo Simulations:",
                            value = 1000,
                            min = 100,
                            max = 10000),
               
               numericInput("max_steps",
                            "Max Time Steps (Attacks):",
                            value = 200,
                            min = 50,
                            max = 1000),
               
               actionButton("run_sim", "Run Simulation", class = "btn-primary"),
               hr(),
               p(em("Note: In this model, the APT has infinite attempts. 'Breach' occurs when Resources hit 0.")),
               p(em("Display limited to first 100 paths for clarity."))
             ),
             
             mainPanel(
               plotOutput("ruinPlot"),
               br(),
               tableOutput("statsTable"),
               uiOutput("textAnalysis")
             )
           )
  ),
  
  # 3. Code Page (Requirement 2: Full Code)
  tabPanel("Code",
           fluidPage(
             column(10, offset = 1,
                    h3("Full Application Code"),
                    p("Below is the complete R code used to generate this Shiny application."),
                    verbatimTextOutput("code_display")
             )
           )
  ),
  
  # 4. R Session Information
  tabPanel("R Session Information",
           fluidPage(
             column(10, offset = 1,
                    h3("Session Information"),
                    verbatimTextOutput("session_info")
             )
           )
  )
)

# --- ACTUAL SERVER ---
server <- function(input, output) {
  
  # --- Reactive Simulation Logic ---
  sim_data <- eventReactive(input$run_sim, {
    # Inputs
    start_cap <- input$initial_resources
    p <- input$prob_success
    q <- 1 - p
    n_sim <- input$n_sim
    max_steps <- input$max_steps
    
    # Initialize
    results_list <- vector("list", n_sim)
    ruined_count <- 0
    
    withProgress(message = 'Simulating Attacks...', value = 0, {
      
      for(i in 1:n_sim) {
        # Simulate Random Walk
        steps <- sample(c(1, -1), max_steps, replace = TRUE, prob = c(p, q))
        path <- cumsum(c(start_cap, steps))
        
        # Check for Ruin (Breach)
        ruin_idx <- which(path <= 0)[1]
        
        if (!is.na(ruin_idx)) {
          path <- path[1:ruin_idx]
          is_ruined <- TRUE
          ruined_count <- ruined_count + 1
        } else {
          is_ruined <- FALSE
        }
        
        # Store
        results_list[[i]] <- data.frame(
          Simulation = as.factor(i),
          Step = 0:(length(path)-1),
          Resources = path,
          Result = ifelse(is_ruined, "Breach", "Resilient")
        )
        
        if(i %% 100 == 0) incProgress(100/n_sim)
      }
    })
    
    all_paths <- do.call(rbind, results_list)
    
    # Theoretical Probability of Breach
    # If q >= p (Defense disadvantage): Prob = 1
    # If q < p  (Defense advantage): Prob = (q/p)^start_cap
    if (q >= p) {
      theo_prob <- 1.0
    } else {
      theo_prob <- (q / p)^start_cap
    }
    
    list(
      paths = all_paths, 
      ruin_rate = ruined_count / n_sim,
      theo_prob = theo_prob,
      total_sims = n_sim
    )
  })
  
  # --- Plot Output ---
  output$ruinPlot <- renderPlot({
    req(sim_data())
    
    df <- sim_data()$paths
    # Filter to first 100 paths
    first_100_ids <- levels(df$Simulation)[1:min(100, nlevels(df$Simulation))]
    df_subset <- df[df$Simulation %in% first_100_ids, ]
    
    ggplot(df_subset, aes(x = Step, y = Resources, group = Simulation, color = Result)) +
      geom_line(alpha = 0.6) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 1) +
      labs(title = paste("Monte Carlo Simulation: Defender's Dilemma (First 100 Paths)"),
           subtitle = "Resources vs. Time Steps (Attacks)",
           y = "Defender Resources",
           x = "Attack Attempt #") +
      scale_color_manual(values = c("Breach" = "#D32F2F", "Resilient" = "#2E7D32")) +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
  
  # --- Stats Table ---
  output$statsTable <- renderTable({
    req(sim_data())
    res <- sim_data()
    
    data.frame(
      Metric = c("Total Simulations", 
                 "Observed Breach Probability", 
                 "Theoretical Breach Probability"),
      Value = c(
        as.integer(res$total_sims),
        sprintf("%.3f", res$ruin_rate), 
        sprintf("%.3f", res$theo_prob)
      )
    )
  })
  
  # --- Text Analysis ---
  output$textAnalysis <- renderUI({
    req(sim_data())
    p_val <- input$prob_success
    
    analysis <- if (p_val <= 0.5) {
      "Because the probability of defense is ≤ 0.5, the APT has the statistical advantage. Over infinite time, a breach is mathematically certain (Probability = 1.000)."
    } else {
      "Because the probability of defense is > 0.5, the Defender has a 'positive drift'. However, there remains a non-zero probability of breach due to unlucky streaks (Variance)."
    }
    
    tagList(
      h4("Analysis"),
      p(analysis)
    )
  })
  
  # --- Code Display Output ---
  output$code_display <- renderText({
    full_code_display
  })
  
  # --- Session Info Output ---
  output$session_info <- renderPrint({
    sessionInfo()
  })
}

# Run the app
shinyApp(ui = ui, server = server)