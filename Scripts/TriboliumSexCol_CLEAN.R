
# Get rid of missing populations ------------------------------------------

dd <- filter(dd,!is.na(Offspring_Gen1))

# Define number of generations --------------------------------------------

ngens <- length(grep("Offspring",colnames(dd)))



# Create and set a theme --------------------------------------------------

theme_lgs <- theme_bw()
theme_set(theme_lgs)
mycols <- c("dodgerblue3","darkorange")



# Generate long data frame ------------------------------------------------

ddlong <- dd%>%
  gather(key = Generation, value = Offspring, paste0("Offspring_Gen",c(1:ngens))) %>%
  mutate(Generation = str_split(Generation, "Gen",simplify = T)[,2])
ddlong$Generation <- factor(ddlong$Generation, levels = as.character(c(1:10)))



# Survival ----------------------------------------------------------------



temp <- dd

temp$Offspring_Gen11 <- 0
temp$SurvivalTime <- NA

for(i in 1:nrow(temp))
{
  temp$SurvivalTime[i] <- min(which(temp[i,] == 0))-5
}

dd$Cens <- ifelse(temp$SurvivalTime == 11,0,1)
dd$SurvTime <- temp$SurvivalTime

temp <- NULL



# Subset removing first gen extinctions -----------------------------------

dd1 <- filter(dd,Offspring_Gen1 > 0)
ddlong$RID <- c(1:nrow(ddlong))

