library(tidyverse)

dd <- read_csv("Tribolium_MoPo_Colonisation - Sheet1.csv")
theme_set(theme_bw())

dd %>% 
  ggplot(aes(x = Treatment, y = Offspring_Gen1_Live))+
  geom_boxplot(notch = T,fill = "grey")

dd %>% 
  subset(Offspring_Gen1_Live > 0) %>%
  ggplot(aes(x = Offspring_Gen1_Live,fill = Treatment))+
  geom_density(alpha = 0.4)+
  xlim(-30,150)


m1 <- lm(Offspring_Gen1_Live~Treatment,data = dd)
summary(m1)
