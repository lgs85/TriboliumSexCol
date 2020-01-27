survall <- coxph(Surv(time = dd$SurvTime,event = dd$Cens)~Treatment,data=dd)
surv1 <- coxph(Surv(time = dd1$SurvTime,event = dd1$Cens)~Treatment,data=dd1)
poplin <- glmer(Offspring~Treatment+as.numeric(Generation) + (1|ID) + (1|RID),data=filter(ddlong,Offspring>0),family = "poisson")
poplinint <- glmer(Offspring~Treatment*as.numeric(Generation)+ (1|ID) + (1|RID),data=filter(ddlong,Offspring>0),family = "poisson")
poppoly <- glmer(Offspring~Treatment+poly(as.numeric(Generation),3)+ (1|ID) + (1|RID),data=filter(ddlong,Offspring>0),family = "poisson")
popfac <- glmer(Offspring~Treatment+Generation+ (1|ID),data=filter(ddlong,Offspring>0),family = "poisson")

