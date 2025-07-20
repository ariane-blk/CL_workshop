# Welcome to the Centiloid workshop

# INSTALL DEPENDENCIES ----------------------------------------------------
setwd("~/Desktop/CL_workshop")
source('dependencies.R')
# load all packages
lapply(required_packages, require, character.only = TRUE)


# DATA -----------------------------------
PiB_gaain <- read_csv("data/PiB_gaain.csv")


