library(tidyverse)

set.seed(15678)

ID <- c(1:117)
Treatment <- rep(c("Mono","Poly"),c(57,60))
Treatment <- sample(Treatment,length(Treatment),replace = F)

write_csv(tibble(ID,Treatment),"~/Documents/Research/Tribolium_MoPo_Colonisation/Randomised_treatments.csv")

dd <- tibble(ID,Treatment)
