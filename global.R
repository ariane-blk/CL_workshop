# Welcome to the Centiloid workshop

# INSTALL DEPENDENCIES ----------------------------------------------------
setwd("~/Desktop/CL_workshop")
source('dependencies.R')
# load all packages
lapply(required_packages, require, character.only = TRUE)


# DATA -----------------------------------
PiB_gaain <- read_csv("data/PiB_gaain.csv")
software_clinical_table <- read_csv("data/software_clinical.csv") %>%
  select(-Website)
software_research_table <- read_csv("data/software_research.csv") 
h2h_table <- read_csv("data/h2h_f18_tracers.csv") 

#rmarkdown::render("intro.Rmd", output_file = "intro.html", quiet = TRUE)
