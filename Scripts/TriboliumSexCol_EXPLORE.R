library(tidyverse)
library(googledrive)
library(googlesheets4)

dd <- drive_get("Tribolium_MoPo_Colonisation") %>%
  read_sheet()


ngens <- length(grep("Offspring",colnames(dd)))

#Boxplot of most recent generation
dd %>%
  filter(Offspring_Gen9 > 0) %>%
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
   mutate(Offspring2 = ifelse(Offspring > 100, 100, Offspring)) %>%
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
  # filter(Offspring_Gen9 > 0) %>%
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
  geom_jitter(position = position_jitterdodge(),alpha = 0.4)+
  geom_smooth()

#Extinction
dd %>%
  gather(key = Generation, value = Offspring,paste0("Offspring_Gen",c(1:ngens))) %>%
  drop_na() %>%
  group_by(Generation,Treatment) %>%
  summarise(Extinct = mean(Offspring == 0)) %>%
  ggplot(aes(x = Generation,y = Extinct,col = Treatment)) +
  geom_line(aes(group = Treatment))

x <- dd %>%
  gather(key = Generation, value = Offspring,paste0("Offspring_Gen",c(1:ngens))) %>%
  mutate(Gen = as.numeric(factor(Generation)))


library(lme4)
y <- glm(Offspring~Treatment+Gen,data=subset(x,Offspring > 0),family = "poisson")
summary(y)






#Extinction
dd %>%
  gather(key = Generation, value = Offspring,paste0("Offspring_Gen",c(1:ngens))) %>%
  drop_na() %>%
  group_by(Generation,Treatment) %>%
  summarise(Extinct = mean(Offspring == 0),
            Established = mean(Offspring > 100)) %>%
  filter(Treatment == "Mono") %>%
  mutate(Gen2 = str_split(Generation,"Gen",simplify = T)[,2]) %>%
  ggplot(aes(x = Gen2,y = Extinct)) +
  geom_line(aes(group = Treatment),lty = 2)+
  geom_line(aes(y = Established, group = Treatment))+
  ylab("Proportion populations established/extinct")+
  xlab("Generation")+
  annotate("text",6,0.4,label = "Established",size = 6)+
  annotate("text",6.6,0.22,label = "Extinct",size = 6)+
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 13))

  str_split(x,"_",simplify = T)
