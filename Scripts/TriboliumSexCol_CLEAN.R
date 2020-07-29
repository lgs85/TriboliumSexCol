
# Get rid of missing populations ------------------------------------------

dd <- dd[complete.cases(dd),]
dd$ID <- factor(dd$ID)

# Define number of generations --------------------------------------------

ngens <- length(grep("Offspring",colnames(dd)))



# Create and set a theme --------------------------------------------------

theme_lgs <- theme_bw()
theme_set(theme_lgs)
mycols <- c("dodgerblue4","darkorange")



# Generate long data frame ------------------------------------------------

ddlong <- dd%>%
  gather(key = Generation, value = Offspring, paste0("Offspring_Gen",c(1:ngens))) %>%
  mutate(Generation = str_split(Generation, "Gen",simplify = T)[,2]) %>%
  mutate(Generation = factor(Generation, levels = as.character(c(1:ngens))))


# Survival ----------------------------------------------------------------

dd %<>%
  rowwise() %>%
  mutate(Offspring_Gen11 = 0) %>%
  rowwise() %>%
  mutate(SurvTime = min(which(c_across(Offspring_Gen2:Offspring_Gen11) == 0))) %>%
  mutate(Cens = ifelse(SurvTime == 10,0,1)) %>%
  dplyr::select(-Offspring_Gen11)


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
