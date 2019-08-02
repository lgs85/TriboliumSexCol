library(tidyverse)
library(googledrive)
library(googlesheets4)

dd <- drive_get("Tribolium_MoPo_Colonisation") %>%
  read_sheet()
theme_set(theme_bw())

dd %>%
  ggplot(aes(x = Treatment, y = Offspring_Gen5))+
  geom_boxplot(notch = T,fill = "grey")

dd %>%
  gather(key = Generation, value = Offspring,Offspring_Gen1,Offspring_Gen2,Offspring_Gen3,Offspring_Gen4,Offspring_Gen5) %>%
  ggplot(aes(x = Generation,y = Offspring)) +
  geom_point() +
  geom_line(aes(group = ID),alpha = 0.4)+
  facet_wrap(~Treatment)


dd %>%
  gather(key = Generation, value = Offspring,Offspring_Gen1,Offspring_Gen2,Offspring_Gen3,Offspring_Gen4,Offspring_Gen5) %>%
  ggplot(aes(x = Generation,y = Offspring,col = Treatment)) +
  geom_boxplot(notch=T)

dd %>%
  gather(key = Generation, value = Offspring,Offspring_Gen1,Offspring_Gen2,Offspring_Gen3,Offspring_Gen4,Offspring_Gen5) %>%
  drop_na() %>%
  group_by(Generation,Treatment) %>%
  summarise(MeanOffspring = mean(Offspring),
            SEOffspring = sd(Offspring)/(sqrt(length(Offspring)))) %>%
  ggplot(aes(x = Generation,y = MeanOffspring,col = Treatment)) +
  geom_point()+
  geom_line(aes(group = Treatment))+
  geom_errorbar(aes(ymin = MeanOffspring - SEOffspring,ymax = MeanOffspring + SEOffspring),width = 0.2)


dd %>%
  gather(key = Generation, value = Offspring,Offspring_Gen1,Offspring_Gen2,Offspring_Gen3,Offspring_Gen4,Offspring_Gen5) %>%
  ggplot(aes(x = as.numeric(factor(Generation)),y = Offspring,col = Treatment)) +
  geom_jitter(position = position_jitterdodge())+
  geom_smooth()
