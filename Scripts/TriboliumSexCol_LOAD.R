library(tidyverse)
library(cowplot)
library(survival)
library(lme4)
library(glmmADMB)
library(MASS)

# Run if data updated on google sheets ------------------------------------

# library(googledrive)
# library(googlesheets4)
# temp <- drive_get("Tribolium_MoPo_Colonisation") %>%
#   read_sheet()
# write_csv(temp,"Data/Tribolium_MoPo_Colonisation.csv")
# rm(temp)



# Load data ---------------------------------------------------------------
dd <- read_csv("Data/Tribolium_MoPo_Colonisation.csv")


# Load model outputs (see models script) ----------------------------------

poplin <- read_csv("Data/poplin.csv")
poplinint <- read_csv("Data/poplinint.csv")
poppoly <- read_csv("Data/poppoly.csv")
popfac <- read_csv("Data/popfac.csv")

