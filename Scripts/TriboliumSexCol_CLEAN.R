
# Define number of generations --------------------------------------------

ngens <- length(grep("Offspring",colnames(dd)))



# Create and set a theme --------------------------------------------------

theme_lgs <- theme_bw()
theme_set(theme_lgs)
mycols <- c("navy","darkorange")



# Generate long data frame ------------------------------------------------

ddlong <- dd%>%
  gather(key = Generation, value = Offspring, paste0("Offspring_Gen",c(1:ngens))) %>%
  mutate(Generation = str_split(Generation, "Gen",simplify = T)[,2])
ddlong$Generation <- factor(ddlong$Generation, levels = as.character(c(1:10)))
