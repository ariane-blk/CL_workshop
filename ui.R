ui <- page_fillable(
  titlePanel("AAIC25 | GAAIN Centiloid workshop"),
  theme = bs_theme(bootswatch = "lumen"),
  navset_card_tab(id = "navs",  
                  
    nav_panel(icon = bsicons::bs_icon("thermometer-half"),"Why quantify Aβ?",
              accordion(  
                accordion_panel(
                  open = TRUE,
                  title = "From visual read to quantification", 
                  icon = bsicons::bs_icon("eye-fill"),
                  div(
                    style = "display: flex; justify-content: center;",
                    imageOutput("image_cl_vr", height = "75%", width = "75%")
                  ),
                  div(
                    style = "font-size: 0.85em; line-height: 0.5em; margin-top: 1px;font-style: italic;",
                    p(""),
                    p("Adapted from:"),
                    p("Sabri, Osama, et al. \"Beta-amyloid imaging with florbetaben.\" Clinical and translational imaging 3.1 (2015): 13-26."),
                    p("Cortes‐Blanco, Anabel, et al. \"Florbetapir (18F) for brain amyloid positron emission tomography: Highlights on the European marketing approval.\" Alzheimer's & Dementia 10 (2014): S395-S399.")
                  ),                
                  ),  
                accordion_panel(
                  open = FALSE,
                  title = "Need for a unfifed measure of amyloid load",
                  icon = bsicons::bs_icon("thermometer-half"),
                  includeMarkdown("texts/intro.Rmd")
                )
              )
              ), 
    nav_panel("🧠 Centiloid 101",      
              layout_columns( 
                card(
                  card_header("Centiloid Scaling of F-18 tracers"),
                  div(
                    style = "text-align: center;",
                    div(
                      style = "display: block; margin-left: auto; margin-right: auto; width: 100%;",
                      imageOutput("cl_image", height = "auto", width = "100%")
                    )
                  ),
                  div(
                    style = "font-size: 0.85em; line-height: 0.5em; margin-top: 1px;font-style: italic;",
                    p(""),
                    p("Courtesy of Renaud La Joie"),)
                ),
                card(
                  card_header("Mapping SUVR to Centiloid"),
                  plotOutput("centiloidPlot"),
                  sliderInput("suvr_input", "Select SUVR value", min = 1.0, max = 2.0, value = 1.4, step = 0.01),
                  textOutput("cl_output")
                ),
                card( 
                  card_header("Stretch SUVR to Centiloid Scale"),
          
                  sliderInput("stretch", "Adjust SUVR range (blue = 0 CL, red = 100 CL)",
                              min = 0.5, max = 3.0, value = c(1.0, 1.5), step = 0.01),
                  plotOutput("stretchPlot1")
                ),

              )
              ),
    nav_panel("❓ How to interpret it",  value = "interpretation",
              layout_columns(
                card(
                  card_header("General guidelines for Centiloid scale interpretation"),
                  div(
                    style = "text-align: center;",
                    imageOutput(
                      "image_display",
                      height = "auto",
                      width = "75%",
                      inline = FALSE
                    ) %>% 
                      tagAppendAttributes(style = "display: block; margin: 0 auto;")
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
    nav_panel("⚠️ Notes of caution",
              accordion(
                accordion_panel("Quality control",
                                imageOutput("qc", height = "70%", width = "70%")
                )
              ),
              
              layout_columns(
                col_widths = c(6, 6),  
                accordion(
                  accordion_panel("Acquisition time window",
                                  imageOutput("acq", height = "70%", width = "70%"),
                                  p(""),
                                  p("Centiloid equations for each tracers were calculated with data acquired during the following time windows:"),
                                  tags$ul(
                                    tags$li("PiB: 50-70 min post injection (p.i.)"),
                                    tags$li("NAV: 50-70 min p.i."),
                                    tags$li("FMM: 90-110 min p.i."),
                                    tags$li("FBB: 90-110 min p.i."),
                                    tags$li("FBP: 50-60 min p.i.")
                                  )
                  )
                ),
                accordion(
                  accordion_panel("Atypical pattern",
                                  p("Images atypical patterns.")
                  )
                )
              )
              ),
    nav_panel("🚫 What not to do", "Why can't you apply any Centiloid equation?",
              layout_columns( 
                card(
                  div(style = "display: flex; justify-content: center;",
                      sliderInput("suvr_input_eq", "Select SUVR value", min = 1.4, max = 2, value = 1.4, step = 0.1)
                  ),
                  plotOutput("cl_eq_plot"),
                  tableOutput("cl_eq_table_1")
                ),
                card(
                  #h3("Level-3"),
                  "In reality, each pipeline would give slightly different SUVr. For a specific amyloid PET scan, this could look like",
                  tableOutput("cl_eq_table_2")
                ),
                col_widths = c(6, 6) 
              ), "Iaccarino, et al. A practical overview of the use of amyloid-PET Centiloid values in clinical trials and research. NeuroImage: Clinical (2025)"), 
          
    nav_panel("🧑‍💻 Validate your own pipeline", #"Page B content",
              layout_columns( 
                card(
                  h3("Level-1"),
                  accordion(
                    open = FALSE,
                    accordion_panel(
                      "Test & Validate the new pipeline with GAAIN data",
                      div(style = "display: flex; justify-content: center;",
                          div(style = "width: 600px;",  # adjust width as needed
                              plotOutput("scatterPlot")
                          )),
                      div(style = "display: flex; justify-content: center;",
                          actionButton("validateBtn", "Generate Data & Run Test"),
                      ),
                      div(style = "display: flex; margin-top: 2px; justify-content: center;",
                          tableOutput("testResults")
                      )
                      
                    )
                    )
                ),
                card(
                  h3("Level-2 (Step 1)"),
                  div(
                    style = "text-align: center;",
                    div(
                      style = "display: block; margin-left: auto; margin-right: auto; width: 85%;",
                      imageOutput("suvr_pib", height = "auto", width = "100%")
                    )
                  ),
                  div(
                    style = "font-size: 0.85em; line-height: 0.5em; margin-top: 1px;font-style: italic;",
                    p(""),
                    p("Courtesy of Renaud La Joie"),),
                  accordion(
                    open = FALSE,
                    accordion_panel(
                      "Compute PiB-equivalent SUVr for each tracer",
                      plotOutput("suvrfpib"),
                      div(
                        style = "text-align: center; font-size: 1em; line-height: 1.5em; margin-top: 1px;",
                        p(""),
                        tags$ul(
                          tags$li("NAV SUVr = 1.1 x PiB SUVr - 0.07"),
                          tags$li("FMM SUVr = 0.61 x PiB SUVr + 0.39"),
                          tags$li("FBB SUVr = 0.77 x PiB SUVr + 0.22"),
                          tags$li("FBP SUVr = 0.54 x PiB SUVr + 0.50")
                        ),
                        p(HTML("Test condition: R<sup>2</sup> > 0.70 (satisfied for all tracers here)."))
                      ) 

                    )
                  )
                ),
                card(
                  h3("Level-2 (Step 2)"),
                  accordion(
                    open = FALSE,
                    accordion_panel(
                      "Compute SUVr-to-Centiloid convertion equation",
                      plotOutput("suvrtocl"),
                      div(
                        style = "text-align: center; font-size: 1em; line-height: 1.5em; margin-top: 1px;",
                        p(""),
                        tags$ul(
                          tags$li("NAV CL = 85.36 x NAV SUVr - 88.66"),
                          tags$li("FMM CL = 121.47 x FMM SUVr - 121.3"),
                          tags$li("FBB CL = 153.55 x FBB SUVr - 155.09"),
                          tags$li("FBP CL = 175.18 x FBP SUVr - 182.24")
                        ),
                        p(HTML("<span style='color: red; font-weight: bold;'>THESE ARE NOT TO REUSE</span>"))
                      ) 

                    )
                  )
                ),
                col_widths = c(4, 4, 4) 
              ) ), 
    nav_panel("💿 Software", 
    accordion(  
      open = FALSE,
      accordion_panel( 
        title = "Clinical software", 
        icon = bsicons::bs_icon("file-earmark-medical"),
        "(List is not exhaustive!)",
        tableOutput("table_software_clinical"),
        "* Regulatory approval as of July 2025"
       
      ),  
      accordion_panel(
        title = "Research software",
        icon = bsicons::bs_icon("easel2"),
        "(List is not exhaustive!)",
        tableOutput("table_software_research")
        
      ))),
    nav_menu( 
      "Resources", 
      nav_panel("Literature", 
                includeMarkdown("texts/refs.Rmd")), 
      "----", 
      "Websites:", 
      nav_item( 
        a("GAAIN", href = "http://www.gaain.org/centiloid-project"),
        a("EMA Qualification Opinion",  href = "https://www.ema.europa.eu/en/documents/other/qualification-opinion-centiloid-measure-amyloid-pet-quantify-brain-amyloid-deposition_en.pdf"),
        a("PubMed Centiloid", href = "https://pubmed.ncbi.nlm.nih.gov/?term=centiloid")
        
      ), 
    ), 
  ), 
  id = "tab",
  fluid = TRUE
)


