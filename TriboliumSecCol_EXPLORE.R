library(tidyverse)
library(googledrive)
library(googlesheets4)

dd <- drive_get("Tribolium_MoPo_Colonisation") %>%
  read_sheet()
theme_set(theme_bw())

ngens <- 7

#Boxplot of most recent generation
dd %>%
  ggplot(aes(x = Treatment, y = get(paste0("Offspring_Gen",ngens))))+
  geom_boxplot(notch = T,fill = "grey")


# Density dependence
dd %>%
  filter(Offspring_Gen7 > 0) %>%
  ggplot(aes(x = Offspring_Gen6, y = Offspring_Gen7,
         col = Treatment))+
  geom_point()+
  geom_smooth()

#Raw data
dd %>%
  gather(key = Generation, value = Offspring,paste0("Offspring_Gen",c(1:ngens))) %>%
  ggplot(aes(x = Generation,y = Offspring)) +
  geom_point() +
  geom_line(aes(group = ID),alpha = 0.4)+
  facet_wrap(~Treatment)

#Treatment boxplot
dd %>%
  gather(key = Generation, value = Offspring,paste0("Offspring_Gen",c(1:ngens))) %>%
  ggplot(aes(x = Generation,y = Offspring,col = Treatment)) +
  geom_boxplot(notch=T)

#Treamtent means
dd %>%
  filter(Offspring_Gen7 > 0) %>%
  gather(key = Generation, value = Offspring,paste0("Offspring_Gen",c(1:ngens))) %>%
  drop_na() %>%
  group_by(Generation,Treatment) %>%
  summarise(MeanOffspring = mean(Offspring),
            SEOffspring = sd(Offspring)/(sqrt(length(Offspring)))) %>%
  ggplot(aes(x = Generation,y = MeanOffspring,col = Treatment)) +
  geom_point()+
  geom_line(aes(group = Treatment))+
  geom_errorbar(aes(ymin = MeanOffspring - SEOffspring,ymax = MeanOffspring + SEOffspring),width = 0.2)

#Jitter plot with smoothed lines
dd %>%
  gather(key = Generation, value = Offspring, paste0("Offspring_Gen",c(1:ngens))) %>%
  ggplot(aes(x = as.numeric(factor(Generation)),y = Offspring,col = Treatment)) +
  geom_jitter(position = position_jitterdodge())+
  geom_smooth()

#Extinction
dd %>%
  gather(key = Generation, value = Offspring,paste0("Offspring_Gen",c(1:ngens))) %>%
  drop_na() %>%
  group_by(Generation,Treatment) %>%
  summarise(Extinct = mean(Offspring == 0)) %>%
  ggplot(aes(x = Generation,y = Extinct,col = Treatment)) +
  geom_point()
