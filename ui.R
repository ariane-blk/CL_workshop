ui <- page_fillable(
  # Add fixed size CSS styling
  # tags$head(
  #   tags$style(HTML("
  #     body, html {
  #       width: 1600px;
  #       height: 1000px;
  #       overflow: hidden;
  #       margin: 0 auto;
  #     }
  #     .container-fluid {
  #       width: 100%;
  #       height: 100%;
  #     }
  #   "))
  # ),
  titlePanel("AAIC25 | GAAIN Centiloid workshop"),
  theme = bs_theme(bootswatch = "lumen"),
  # sidebar = sidebar(
  #   title = "a sidebar",
  #   id = "sidebar",
  # ),
  navset_card_tab(id = "navs",  
    nav_panel("THE CENTILOID SCALE ⚖️",
              includeMarkdown("intro.Rmd"),
              p("Once mapped to this scale, SUVR values from any validated tracer and pipeline can be ",
                "converted to Centiloids, enabling consistent interpretation and comparison in various contexts. ",
                actionLink("go_to_interpretation", "Go to the Interpretation section.")
              )
              ), 
    nav_panel("🧠 Centiloid 101",  "layout_columns",     
              layout_columns( 
                card(
                  card_header("Mapping SUVR to Centiloid"),
                  plotOutput("centiloidPlot"),
                  sliderInput("suvr_input", "Select SUVR value", min = 1.0, max = 2.0, value = 1.4, step = 0.01),
                  textOutput("cl_output")
                ),
                card( 
                  card_header("Card 1 header"),
                  p("The Centiloid (CL) scale is a standardization method that makes it easier to compare amyloid PET results across studies and processing pipelines. In this notebook, we’ll explain what the Centiloid scale is, why it's useful, and how to convert values from a custom pipeline to the CL scale using dummy data.

> **Goal**: By the end of this notebook, you'll know how to:
                      > - Understand the rationale behind the Centiloid scale
                    > - Apply a simple linear transformation to convert your PET values to Centiloids
                    > - Interpret the results meaningfully"),
                  sliderInput("stretch", "Adjust SUVR range (blue = 0 CL, red = 100 CL)",
                              min = 0.5, max = 3.0, value = c(1.0, 1.5), step = 0.01),
                  plotOutput("stretchPlot1")
                ),
                card(
                  card_header("Stretch SUVR to Centiloid Scale"),
                  chooseSliderSkin("Flat", color="black"),
                  sliderInput("stretch_range", "Adjust SUVR range (blue = 0 CL, red = 100 CL)",
                              min = 0.5, max = 3.0, value = c(1.0, 2.0), step = 0.01),
                  plotOutput("stretchPlot")
                  
                )
              )
              ),
    nav_panel("How to interpret it",  value = "interpretation",
              layout_columns(
                card(
                  card_header("General guidelines for Centiloid scale interpretation"),
                  div(
                    style = "text-align: center;",
                    imageOutput("image_display", height = "auto", width = "75%")
                  ),
                  fluidRow(
                    column(1, actionButton("prev_img", "⬅️")),
                    column(10, textOutput("image_counter")),
                    column(1, actionButton("next_img", "➡️"))
                  )
                ),
                col_widths = c(12)
              ),
              "Collij, Bollack, et al., Centiloid recommendations for clinical 
              context‐of‐use from the AMYPAD consortium, Alzheimer's & Dementia (2024)"
              ),
    #nav_panel("Notable stuff", "Dynamic range Gill"),
    nav_panel("What not to do", "Why can't you apply any Centiloid equation?",
    layout_columns( 
            card(
              numericInput("suvr_input_eq", "Input SUVR value", value = 1.4, min = 0, max = 2,  step = 0.1),
              #sliderInput("suvr_input_eq", "Select SUVR value", min = 0, max = 2, value = 1.4, step = 0.01),
              plotOutput("cl_eq_plot")),
            card(
              h3("Level-3"),
              "Coming soon..."
            ),
            col_widths = c(6, 6) 
          ) ), 
          
    nav_panel("Validate your own pipeline", #"Page B content",
              layout_columns( 
                card(
                  h3("Level-1"),
                  plotOutput("scatterPlot"),
                  actionButton("testBtn", "Test"),
                  actionButton("regen_data", "Regenerate Data"),
                  uiOutput("testResultsUI"),
                  tableOutput("testResults")
                ),
                card(
                  h3("Level-2"),
                  "Coming soon..."
                ),
                card(
                  h3("Level-3"),
                  "Coming soon..."
                ),
                col_widths = c(4, 4, 4) 
              ) ), 
    nav_panel("Software with Centiloid", "Page C content"), 
    nav_menu( 
      "Resources", 
      nav_panel("Literature", "Panel D content"), 
      "----", 
      "Websites:", 
      nav_item( 
        a("GAAIN", href = "http://www.gaain.org/centiloid-project") 
      ), 
    ), 
  ), 
  id = "tab",
  fluid = TRUE
)


