library(tidyverse)


# Run if data updated on google sheets ------------------------------------

# library(googledrive)
# library(googlesheets4)
# temp <- drive_get("Tribolium_MoPo_Colonisation") %>%
#   read_sheet()
# write_csv(temp,"Data/Tribolium_MoPo_Colonisation.csv")
# rm(temp)



# Load data ---------------------------------------------------------------
dd <- read_csv("Data/Tribolium_MoPo_Colonisation.csv")
