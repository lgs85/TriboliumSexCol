
# Get rid of missing populations ------------------------------------------

dd <- dd[complete.cases(dd),]
dd$ID <- factor(dd$ID)

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

ddm <- filter(ddlong,Offspring > 0)
dd1 <- filter(dd,Offspring_Gen1 > 0)
dd10 <- filter(dd,Offspring_Gen10 > 0)
ddlong$RID <- c(1:nrow(ddlong))
ddm$RID <- c(1:nrow(ddm))



# Rates of change ---------------------------------------------------------

x1 <- dd[,grepl(paste0("Gen",c(1:9),"$",collapse = "|"),colnames(dd))]
x2 <- dd[,grepl(paste0("Gen",c(2:10),"$",collapse = "|"),colnames(dd))]

ch <- cbind(dd[,c("ID","Treatment")],x2-x1) %>%
  gather(key = Generation, value = Offspring, paste0("Offspring_Gen",c(2:10))) %>%
  mutate(Generation = str_split(Generation, "Gen",simplify = T)[,2])
ch$Generation <- factor(ch$Generation, levels = as.character(c(2:10)))
