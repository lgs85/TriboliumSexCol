source("Scripts/TriboliumSexCol_LOAD.R")
source("Scripts/TriboliumSexCol_FUNCTIONS.r")
source("Scripts/TriboliumSexCol_CLEAN.r")
source("Scripts/TriboliumSexCol_MODELS.r")


# Figure 2 ----------------------------------------------------------------

Fig2 <- ddlong %>%
  ggplot(aes(x = Generation,y = Offspring,fill = Treatment)) +
  geom_boxplot()+
  scale_fill_manual(values = mycols)+
  ylab("Number of adults")+
  theme(legend.position = "top")

pdf("Figures/Figure_2.pdf",
    width = 8,height = 4)
Fig2
dev.off()


# Figure 3 ----------------------------------------------------------------

Fig3A <- ddlong %>%
  drop_na() %>%
  group_by(Generation, Treatment) %>%
  summarise(Extant = mean(Offspring > 0)) %>%
  ggplot(aes(x = Generation, y = Extant, col = Treatment))+
  geom_line(aes(group = Treatment))+
  ylab("Proportion of populations surviving")+
  scale_colour_manual(values = mycols)+
  theme(legend.position = "none")

Fig3B <- ddm %>%
  ggplot(aes(x = Generation,y = Offspring,col = Treatment)) +
  geom_line(aes(group = ID),alpha = 0.1)+
  geom_smooth(aes(x = as.numeric(Generation)),method = "glm.nb",se = T) +
  ylab("Number of adults")+
  theme(legend.position = "none")+
  scale_colour_manual(values = mycols)

plots <- plot_grid(Fig3A, Fig3B, labels = "AUTO")
legend_b <- get_legend(
  Fig3A + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom")
)

pdf("Figures/Figure_3.pdf",
    width = 8,height = 4)
plot_grid(legend_b, plots, ncol = 1, rel_heights = c(0.1, 1))
dev.off()

