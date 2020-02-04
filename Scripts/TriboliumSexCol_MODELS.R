
# Fast models -------------------------------------------------------------

survall <- coxph(Surv(time = dd$SurvTime,event = dd$Cens)~Treatment,data=dd)
surv1 <- coxph(Surv(time = dd1$SurvTime,event = dd1$Cens)~Treatment,data=dd1)
pop10 <- glm.nb(Offspring_Gen10~Treatment,data=dd10)
pop1 <- glm.nb(Offspring_Gen1~Treatment,data=dd1)



# Slow models - only run if updating --------------------------------------

# poplin <- glmmadmb(Offspring~Treatment  + as.numeric(Generation), random = ~1|ID, data=ddm,family = "nbinom")
# poplinint <- glmmadmb(Offspring~Treatment  * as.numeric(Generation), random = ~1|ID, data=ddm,family = "nbinom")
# poppoly <- glmmadmb(Offspring~Treatment  + poly(as.numeric(Generation),3), random = ~1|ID, data=ddm,family = "nbinom")
# popfac <- glmmadmb(Offspring~Treatment  + Generation, random = ~1|ID, data=ddm,family = "nbinom")
# 
# summary(poplin)$coefficients %>%
#   as_tibble(rownames = "x") %>%
#   write_csv("Data/poplin.csv")
# 
# summary(poplinint)$coefficients %>%
#   as_tibble(rownames = "x") %>%
#   write_csv("Data/poplinint.csv")
# 
# summary(poppoly)$coefficients %>%
#   as_tibble(rownames = "x") %>%
#   write_csv("Data/poppoly.csv")
# 
# summary(popfac)$coefficients %>%
#   as_tibble(rownames = "x") %>%
#   write_csv("Data/popfac.csv")
