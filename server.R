# Define the SUVR range from calibration
suvr_young <- 1.0  # Example SUVR for young controls (CL = 0)
suvr_ad <- 2.0     # Example SUVR for AD patients (CL = 100)


server <- function(input, output) {
  #bs_themer()
  
  observeEvent(input$go_to_interpretation, {
   updateNavs(inputId = "navs", selected = "interpretation")
  })
  
  source("global.R")
  
  #---------------------------
  # TAB 1: Why quantify Ab?
  #---------------------------
  
  output$image_cl_vr <- renderImage({
    req(image_files)
    list(
      src = file.path("./images/CL_VR.png"),
      contentType = "image/png",
      width = "80%", height = "80%"
    )
  }, deleteFile = FALSE)
  
  #---------------------------
  # TAB 2: Why quantify Ab?
  #---------------------------
  
  suvr_data <- PiB_gaain |> 
    dplyr::select(sub_id, suvr_wc) |>
    dplyr::mutate(
      dx = dplyr::case_when(
        grepl("^AD", sub_id) ~ "AD",
        grepl("^YC", sub_id) ~ "YC",
        TRUE ~ "Other"
      )
    )
  
  # Extract means for AD and YC
  mean_AD <- reactive({
    suvr_data |> 
      dplyr::filter(grepl("^AD", sub_id)) |> 
      dplyr::pull(suvr_wc) |> 
      mean(na.rm = TRUE)
  })
  
  mean_YC <- reactive({
    suvr_data |> 
      dplyr::filter(grepl("^YC", sub_id)) |> 
      dplyr::pull(suvr_wc) |> 
      mean(na.rm = TRUE)
  })
  
  
  output$stretchPlot1 <- renderPlot({
    suvr_min <- input$stretch[1]
    suvr_max <- input$stretch[2]
    
    # Linear transformation: SUVR to CL
    suvr_data <- suvr_data |>
      dplyr::mutate(CL = 100 * (suvr_wc - suvr_min) / (suvr_max - suvr_min))
    
    ggplot(suvr_data, aes(x = suvr_wc, y = 0)) +
      geom_hline(yintercept = 0) +
      geom_point(aes(x = 93.755 * suvr_wc - 94.642, y=0.01, color = dx), shape = 4, size = 5,stroke = 1.5) + 
      scale_color_manual(values = c("AD" = "red", "YC" = "blue")) +
      # Add mean for AD (red) and YC (blue)
      geom_point(aes(x = 93.755 * suvr_min - 94.642, y = 0), 
                 color = "blue", size = 5, shape = 19, stroke = 1.5) +
      geom_point(aes(x = 93.755 * suvr_max - 94.642, y = 0), 
                 color = "red", size = 5, shape = 19, stroke = 1.5) +
      scale_x_continuous(limits = c(-20, 115)) +
      theme_minimal() +
      theme(axis.title = element_blank(),
            legend.position = "none",
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank()) +
      labs(title = "Dynamic SUVR-to-Centiloid Stretch",
           subtitle = paste("AD mean:", round(mean_AD(), 2), " | YC mean:", 
                            round(mean_YC(), 2)))
  }, height = 300, res = 72)
  
  output$h2hPlot <- renderPlot({

        h2h_data <- h2h_table 
    
    ggplot(h2h_data, aes(x = pib_suvr_wcb, y = f18_suvr_wcb, color = tracer)) +
      geom_abline(color="darkgrey") +
      geom_point(size = 2) +
      
      # Linear model per tracer
      geom_smooth(method = "lm", se = FALSE, formula = y ~ x) +
      
      # Add equations and R²
      stat_poly_eq(
        aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
        formula = y ~ x,
        parse = TRUE,
        label.x = "left",
        label.y = "top",
        size = 6
      ) +
      scale_color_manual(values = c(
        "Flutemetamol" = "#640acc", 
        "Florbetapir" = "#ed1c23",
        "Florbetaben" = "#011f5f",
        "NAV4694" = "#027f47"
      )) +
      labs(
        title = "H2H PiB vs F-18 Tracers",
        x = "PiB SUVr (WCB)",
        y = "F-18 SUVr (WCB)",
        color = "Tracer"
      ) +
      theme_minimal(base_size = 16) +
      theme(legend.position = "bottom",
        plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 13)
      )
  })
  
  
  # Compute centiloid from suvr
  cl_value <- reactive({
    100 * (input$suvr_input - suvr_young) / (suvr_ad - suvr_young)
  })
  
  output$cl_output <- renderText({
    paste0("Centiloid value: ", round(cl_value(), 1), " CL")
  })
  
  output$centiloidPlot <- renderPlot({
    suvr_range <- seq(1.0, 2.0, length.out = 100)
    cl_range <- 100 * (suvr_range - suvr_young) / (suvr_ad - suvr_young)
    
    ggplot(data.frame(SUVR = suvr_range, CL = cl_range), aes(x = SUVR, y = CL)) +
      geom_line(color = "blue", linewidth = 1.2) +
      geom_point(aes(x = input$suvr_input, y = cl_value()), color = "red", size = 4) +
      geom_text(aes(x = input$suvr_input, y = cl_value(), 
                    label = paste0(round(cl_value(), 1), " CL")), 
                vjust = -1, hjust = -1, color = "red") +
      labs(title = "Mapping SUVR to Centiloid",
           x = "SUVR (custom pipeline)",
           y = "Centiloid (CL)") +
      theme_minimal() +
      theme()
  })
  
  # VALIDATE STEP 1
  random_df <- eventReactive(input$regen_data, {
    set.seed(sample(1:1e6, 1))  # random seed for new data each click
    df_new <- PiB_gaain[, c("sub_id", "cl_wc")]
    df_new$cl_rand <- round(sapply(df_new$cl_wc, function(x) {
      sign <- sample(c(-1, 1), 1)
      diff_pct <- runif(1, -0.01, 0.3)
      x * (1 + sign * diff_pct)
    }), 1)
    df_new
  }, ignoreNULL = FALSE)  # triggers once at startup
  
  # Reactive linear model fit
  fit <- reactive({
    lm(cl_rand ~ cl_wc, data = random_df())
    })

  output$scatterPlot <- renderPlot({
    model <- fit()
    eq <- paste0("y = ", round(coef(model)[2], 3), "x + ", round(coef(model)[1], 3))
    r2 <- summary(model)$r.squared
    
    ggplot(random_df(), aes(x = cl_wc, y = cl_rand)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "blue") +
      annotate("text", 
               x = 5, y = 140, 
               label = paste(eq, "\nR² = ", round(r2, 3)),
               hjust = 0, vjust = 1, 
               size = 5) +
      labs(
        x = "Centiloid (GAAIN)",   
        y = "Your Centiloid values" 
      ) +
      theme_minimal() + xlim(c(0, 150)) + ylim(c(0, 150)) + 
      theme(
        axis.title = element_text(size = 16),  
        axis.text = element_text(size = 14)   
      )
  }, width = 350, height = 350) 
  
  # Test conditions on button click
  observeEvent(input$testBtn, {
    df_current <- random_df()
    model <- fit()
    coef_est <- coef(model)
    slope <- coef_est[2]
    intercept <- coef_est[1]
    r2 <- summary(model)$r.squared
    
    tests <- data.frame(
      Condition = c("Slope between 0.98 and 1.02",
                    "Intercept between -2 and 2",
                    "R² > 0.98"),
      Passed = c(
        slope >= 0.98 & slope <= 1.02,
        intercept >= -2 & intercept <= 2,
        r2 > 0.98
      )
    )
    
    tests$Result <- ifelse(tests$Passed, "\u2713", "\u2717") # ✓ or ✗
    
    output$testResults <- renderTable({
      tests[, c("Condition", "Result")]
    }, colnames = FALSE)
    })
  


  # INTERPRETATION
  image_files <- mixedsort(list.files(file.path("./images/interpretation"), 
                                      pattern = ".(png|jpg|jpeg|gif)$", 
                                      full.names = FALSE))
  # Reactive index
  current_index <- reactiveVal(1)
  
  # Button logic
  observeEvent(input$next_img, {
    if (current_index() < length(image_files)) {
      current_index(current_index() + 1)
    }
  })
  
  observeEvent(input$prev_img, {
    if (current_index() > 1) {
      current_index(current_index() - 1)
    }
  })
  
  # Display current image
  output$image_display <- renderImage({
    req(image_files)
    list(
      src = file.path("./images/interpretation/", image_files[current_index()]),
      contentType = "image/png",
      alt = image_files[current_index()],
      width = "80%", height = "80%"
    )
  }, deleteFile = FALSE)
  
  # Counter
  output$image_counter <- renderText({
    paste("Slide", current_index(), "of", length(image_files))
  })
  
  # EXAMPLE EQUATIONS
  
  # SUVR example points
  suvr_values <- seq(1, 2.5, length.out = 5)
  
  # Define 5 linear equations as list of (slope, intercept)
  linear_eqs <- list(
    list(slope = 183.07, intercept = -177.26), 
    list(slope = 203.68, intercept = -207.15),
    list(slope = 230.47, intercept = -233.33),
    list(slope = 184.12, intercept = -233.72),
    list(slope = 205.72, intercept = -209.63),
    list(slope = 174.52, intercept = -187.26),
    list(slope = 188.22, intercept = -189.16)
  )
  
  output$animPlot <- renderPlot({
    # For each linear equation, compute transformed values
    df_list <- lapply(seq_along(linear_eqs), function(i) {
      eq <- linear_eqs[[i]]
      data.frame(
        SUVR = input$suvr_input_eq,#suvr_values,
        Transformed = eq$slope * suvr_values + eq$intercept,
        Equation = paste0("y = ", eq$slope, " * x ", eq$intercept)
      )
    })
    
    df_all <- do.call(rbind, df_list)
    df_all$SUVr <- as.factor(df_all$SUVR)
    
    # ggplot(df_all, aes(x = SUVR, y = Transformed, color = Equation, shape = SUVr)) +
    #   geom_point(size = 3) +
    #   geom_line(aes(group = Equation)) +
    #   labs(
    #     title = "SUVR Values Transformed with Different Linear Equations",
    #     x = "Original SUVR",
    #     y = "\"Centiloid\"",
    #     color = "Equations"
    #   ) +
    #   theme_minimal(base_size = 16)
  })
  
  equations <- list(
    "Equation A" = function(suvr) 183.07 * suvr -177.26,
    "Equation B" = function(suvr) 203.68 * suvr -207.15,
    "Equation C" = function(suvr) 230.47 * suvr -233.33
  )
  linear_eqs <- list(
    list(slope = 183.07, intercept = -177.26), 
    list(slope = 203.68, intercept = -207.15),
    list(slope = 230.47, intercept = -233.33)
    #list(slope = 184.12, intercept = -233.72),
    #list(slope = 205.72, intercept = -209.63),
    #list(slope = 174.52, intercept = -187.26),
    #list(slope = 188.22, intercept = -189.16)
  )
  
  
  output$cl_eq_plot <- renderPlot({
    suvr <- input$suvr_input_eq
    
    eq_params <- data.frame(
      Equation = paste0("y = ", c(183.07, 203.68, 230.47, 184.12, 205.72, 174.52, 188.22), 
                        " * x ", 
                        c(-177.26, -207.15, -233.33, -233.72, -209.63, -187.26, -189.16)),
      Slope = c(183.07, 203.68, 230.47, 184.12, 205.72, 174.52, 188.22),
      Intercept = c(-177.26, -207.15, -233.33, -233.72, -209.63, -187.26, -189.16)
    )
    
    df <- data.frame(
      Equation = eq_params$Equation,
      SUVR = suvr,
      Centiloid = eq_params$Slope * suvr + eq_params$Intercept
    )

    
    ggplot() +
      geom_abline(data = eq_params, aes(slope = Slope, intercept = Intercept, color = Equation), linewidth = 1.2) +
      geom_point(data = df, aes(x = SUVR, y = Centiloid, color = Equation), size = 3) +
      geom_text(data = df, aes(x = SUVR, y = Centiloid, label = paste0(round(Centiloid, 1), " CL"), color = Equation),
                vjust = -1, hjust = -0.1) +
      labs(
        title = "SUVR Values Transformed with Different Linear Equations",
        x = "SUVR (custom pipeline)",
        y = "Centiloid (CL)"
      ) +
      coord_cartesian(xlim = c(0.5, 2), ylim = c(0, 110)) +
      theme_minimal(base_size = 16)
  }, width = 600, height = 350)
  
  output$table_software_clinical <- renderTable({software_clinical_table}, align = "lcccc", striped = TRUE) 
  output$table_software_research <- renderTable({software_research_table}, align = "ll", striped = TRUE) 
  
 #show intro modal
  #  observeEvent("", {
  #   showModal(modalDialog(
  #    includeHTML("intro_text.Rhtml"),
  #   easyClose = TRUE,
  #  footer = tagList(
  #   actionButton(inputId = "intro", label = "INTRODUCTION TOUR", icon = icon("info-circle")),
  #  modalButton("Close")
  #   )
  #  ))
  # })
  

  
}

