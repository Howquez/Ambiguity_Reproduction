
###############To Dos?
#Stata-Sem Package, SUR, Clustered SEs--> Stata Sem Package

setwd("C:/Users/u290322/Desktop/Hauke Project/Data_Full/surveyUncertainty/raw")


################Analysis##################
load(file="ful.Rdata")
#subset 0


#subset 1: Only good comprehension (Pregistration)
ful<-subset(ful, ful$Outro.1.player.Comprehension=="yes"|ful$Outro.1.player.Comprehension=="rather yes")

#subset 2: Time to answer/ Reading the introductions (Preregistration Analysis Plan), 10 and 90%
load(file="completiontime.Rdata")
ful<-merge(ful,completion_time,by.x="participant.code",by.y = "participant_code")
ful$lowquantile<-quantile(ful$completion_time,probs=c(0.1))
ful$highquantile<-quantile(ful$completion_time,probs=c(0.9))
ful<-subset(ful,ful$completion_time>ful$lowquantile&ful$completion_time<ful$highquantile)

#subset 3.1: Weak  Monotonic Answers? (one Event, multiple Events: MS<=MC)
ful<-subset(ful,ful$MC>=ful$MS)
ful<-subset(ful,duplicated(ful$participant.label)|duplicated(ful$participant.label,fromLast = TRUE))
table(duplicated(ful$participant.label)|duplicated(ful$participant.label,fromLast = TRUE))
length(unique(ful$participant.label))

#subset 3.2: Strong Monotonic Answers? (E1<=E12 & E1<=E13 etc.)
ful<-subset(ful,ful$EE12>=ful$EE1 & ful$EE13>=ful$EE1 &   ful$EE12>=ful$EE2 & ful$EE23>=ful$EE2 & ful$EE23>=ful$EE3 & ful$EE13>=ful$EE3 )
ful<-subset(ful,duplicated(ful$participant.label)|duplicated(ful$participant.label,fromLast = TRUE))
table(duplicated(ful$participant.label)|duplicated(ful$participant.label,fromLast = TRUE))
length(unique(ful$participant.label))

#subset 4: Randomisation (session.config.treatment or session.config.location)
table(ful$session.config.location)
table(ful$session.config.treatment)
table(ful$treat)
table(ful$session.config.location=="Ilomantsi"&ful$session.config.treatment=="best_guess")
ful<-subset(ful,ful$session.config.location!="Ilomantsi")

#subset 5: Number of wrong answers (control questions) (Preregistration)
table(ful$Intro.1.player.wrong_answer_1)
table(ful$Intro.1.player.wrong_answer_2)
ful<-subset(ful,ful$Intro.1.player.wrong_answer_1<1 & ful$Intro.1.player.wrong_answer_2<1)

#subset 6: Same Answers for every Events (EE1, EE2, etc.)
ful$same<-ifelse(ful$EE1==ful$EE2&ful$EE2==ful$EE3&ful$EE3==ful$EE12&ful$EE13==ful$EE12&ful$EE23==ful$EE13,1,0)
table(ful$same)
ful<-subset(ful,ful$same!=1)
ful<-subset(ful,duplicated(ful$participant.label)|duplicated(ful$participant.label,fromLast = TRUE))
table(duplicated(ful$participant.label)|duplicated(ful$participant.label,fromLast = TRUE))
length(unique(ful$participant.label))



####################### Controlvariables#################
list_dem<-c("age_35_52","age_53_69","gender_female", "education_high","income_high","family_ssu_married","Outro.1.player.Kids")
list_all<-c("age_35_52","age_53_69","gender_female", "education_high","income_high","family_ssu_married","Outro.1.player.Kids","median_temp","median_usage","median_risk_gen","median_risk_weat","median_accuracy","median_credibility")





################################################################################
############################ Mean tests Part 2#########################################
#################################################################################

#Subsets for tests: Tests between individuals
fgr<-subset(ful,ful$round==2)
fgr1<-subset(fgr,fgr$treat=="conf.-best guess")
fgr2<-subset(fgr,fgr$treat=="conf.-interval")
fgr3<-subset(fgr,fgr$treat=="conf.-both")
fgr4<-subset(fgr,fgr$treat=="contrad.-best guess")
fgr5<-subset(fgr,fgr$treat=="contrad.-interval")
fgr6<-subset(fgr,fgr$treat=="contrad.-both")

table(ful$Intro.1.player.location)
fgr7<-subset(fgr,fgr$Intro.1.player.location=="Ilomantsi")
fgr8<-subset(fgr,fgr$Intro.1.player.location=="Weiskirchen")

#Subsets for tests: Tests within individuals
up<-subset(ful)
up1<-subset(up,up$treat=="conf.-best guess")
up2<-subset(up,up$treat=="conf.-interval")
up3<-subset(up,up$treat=="conf.-both")
up4<-subset(up,up$treat=="contrad.-best guess")
up5<-subset(up,up$treat=="contrad.-interval")
up6<-subset(up,up$treat=="contrad.-both")

table(ful$Intro.1.player.location)
up7<-subset(up,up$Intro.1.player.location=="Ilomantsi")
up8<-subset(up,up$Intro.1.player.location=="Weiskirchen")

#Testing normality assumption for t-tests
?shapiro.test

shapiro.test(fgr1$AA1)
shapiro.test(fgr1$AA2)
shapiro.test(fgr1$EE1)
shapiro.test(fgr1$EE2)
shapiro.test(fgr1$EE3)
shapiro.test(fgr1$EE12)
shapiro.test(fgr1$EE23)
shapiro.test(fgr1$EE13)

shapiro.test(fgr2$AA1)
shapiro.test(fgr2$AA2)
shapiro.test(fgr2$EE1)
shapiro.test(fgr2$EE2)
shapiro.test(fgr2$EE3)
shapiro.test(fgr2$EE12)
shapiro.test(fgr2$EE23)
shapiro.test(fgr2$EE13)

shapiro.test(fgr3$AA1)
shapiro.test(fgr3$AA2)
shapiro.test(fgr3$EE1)
shapiro.test(fgr3$EE2)
shapiro.test(fgr3$EE3)
shapiro.test(fgr3$EE12)
shapiro.test(fgr3$EE23)
shapiro.test(fgr3$EE13)


shapiro.test(fgr4$AA1)
shapiro.test(fgr4$AA2)
shapiro.test(fgr4$EE1)
shapiro.test(fgr4$EE2)
shapiro.test(fgr4$EE3)
shapiro.test(fgr4$EE12)
shapiro.test(fgr4$EE23)
shapiro.test(fgr4$EE13)


shapiro.test(fgr5$AA1)
shapiro.test(fgr5$AA2)
shapiro.test(fgr5$EE1)
shapiro.test(fgr5$EE2)
shapiro.test(fgr5$EE3)
shapiro.test(fgr5$EE12)
shapiro.test(fgr5$EE23)
shapiro.test(fgr5$EE13)

shapiro.test(fgr6$AA1)
shapiro.test(fgr6$AA2)
shapiro.test(fgr6$EE1)
shapiro.test(fgr6$EE2)
shapiro.test(fgr6$EE3)
shapiro.test(fgr6$EE12)
shapiro.test(fgr6$EE23)
shapiro.test(fgr6$EE13)

shapiro.test(fgr7$AA1)
shapiro.test(fgr7$AA2)
shapiro.test(fgr7$EE1)
shapiro.test(fgr7$EE2)
shapiro.test(fgr7$EE3)
shapiro.test(fgr7$EE12)
shapiro.test(fgr7$EE23)
shapiro.test(fgr7$EE13)

shapiro.test(fgr8$AA1)
shapiro.test(fgr8$AA2)
shapiro.test(fgr8$EE1)
shapiro.test(fgr8$EE2)
shapiro.test(fgr8$EE3)
shapiro.test(fgr8$EE12)
shapiro.test(fgr8$EE23)
shapiro.test(fgr8$EE13)



######## No normality --> Non-parametric tests:



########Graph series 1: Confirmation#####################################
?wilcox.test
#AA1: Within individals
wilcox.test(up1$AA1~up1$round,alternative = c("two.sided"),paired=TRUE)
wilcox.test(up2$AA1~up2$round,alternative = c("two.sided"),paired=TRUE)
wilcox.test(up3$AA1~up3$round,alternative = c("two.sided"),paired=TRUE)
#AA1: Between individuals
wilcox.test(fgr1$AA1,fgr2$AA1,alternative = c("two.sided"))
wilcox.test(fgr1$AA1,fgr3$AA1,alternative = c("two.sided"))
wilcox.test(fgr2$AA1,fgr3$AA1,alternative = c("two.sided"))

#AA2: Within individals
wilcox.test(up1$AA2~up1$round,alternative = c("two.sided"),paired=TRUE)
wilcox.test(up2$AA2~up2$round,alternative = c("two.sided"),paired=TRUE)
wilcox.test(up3$AA2~up3$round,alternative = c("two.sided"),paired=TRUE)
#AA2: Between individuals
wilcox.test(fgr1$AA2,fgr2$AA2,alternative = c("two.sided"))
wilcox.test(fgr1$AA2,fgr3$AA2,alternative = c("two.sided"))
wilcox.test(fgr2$AA2,fgr3$AA2,alternative = c("two.sided"))

#EE1: Within individals
wilcox.test(up1$EE1~up1$round,alternative = c("two.sided"),paired=TRUE)
wilcox.test(up2$EE1~up2$round,alternative = c("two.sided"),paired=TRUE)
wilcox.test(up3$EE1~up3$round,alternative = c("two.sided"),paired=TRUE)
#EE1: Between individals
wilcox.test(fgr1$EE1,fgr3$EE1,alternative = c("two.sided"))
wilcox.test(fgr1$EE1,fgr3$EE1,alternative = c("two.sided"))
wilcox.test(fgr2$EE1,fgr3$EE1,alternative = c("two.sided"))

#EE2: Within individals
wilcox.test(up1$EE2~up1$round,alternative = c("two.sided"),paired=TRUE)
wilcox.test(up2$EE2~up2$round,alternative = c("two.sided"),paired=TRUE)
wilcox.test(up3$EE2~up3$round,alternative = c("two.sided"),paired=TRUE)

#EE2: Between individals
wilcox.test(fgr1$EE2,fgr2$EE2,alternative = c("two.sided"))
wilcox.test(fgr1$EE2,fgr3$EE2,alternative = c("two.sided"))
wilcox.test(fgr2$EE2,fgr3$EE2,alternative = c("two.sided"))

#EE3: Within individals
wilcox.test(up1$EE3~up1$round,alternative = c("two.sided"),paired=TRUE)
wilcox.test(up2$EE3~up2$round,alternative = c("two.sided"),paired=TRUE)
wilcox.test(up3$EE3~up3$round,alternative = c("two.sided"),paired=TRUE)

#EE3: Between individals
wilcox.test(fgr1$EE3,fgr2$EE3,alternative = c("two.sided"))
wilcox.test(fgr1$EE3,fgr3$EE3,alternative = c("two.sided"))
wilcox.test(fgr2$EE3,fgr3$EE3,alternative = c("two.sided"))

#EE12: Within individals
wilcox.test(up1$EE12~up1$round,alternative = c("two.sided"),paired=TRUE)
wilcox.test(up2$EE12~up2$round,alternative = c("two.sided"),paired=TRUE)
wilcox.test(up3$EE12~up3$round,alternative = c("two.sided"),paired=TRUE)

#EE12: Between individals
wilcox.test(fgr1$EE12,fgr2$EE12,alternative = c("two.sided"))
wilcox.test(fgr1$EE12,fgr3$EE12,alternative = c("two.sided"))
wilcox.test(fgr2$EE12,fgr3$EE12,alternative = c("two.sided"))

#EE23: Within individals
wilcox.test(up1$EE23~up1$round,alternative = c("two.sided"),paired=TRUE)
wilcox.test(up2$EE23~up2$round,alternative = c("two.sided"),paired=TRUE)
wilcox.test(up3$EE23~up3$round,alternative = c("two.sided"),paired=TRUE)

#EE23: Between individals
wilcox.test(fgr1$EE23,fgr2$EE23,alternative = c("two.sided"))
wilcox.test(fgr1$EE23,fgr3$EE23,alternative = c("two.sided"))
wilcox.test(fgr2$EE23,fgr3$EE23,alternative = c("two.sided"))

#EE13: Within individals
wilcox.test(up1$EE13~up1$round,alternative = c("two.sided"),paired=TRUE)
wilcox.test(up2$EE13~up2$round,alternative = c("two.sided"),paired=TRUE)
wilcox.test(up3$EE13~up3$round,alternative = c("two.sided"),paired=TRUE)

#EE13: Between individals
wilcox.test(fgr1$EE13,fgr2$EE13,alternative = c("two.sided"))
wilcox.test(fgr1$EE13,fgr3$EE13,alternative = c("two.sided"))
wilcox.test(fgr2$EE13,fgr3$EE13,alternative = c("two.sided"))


########Graph series 2: Contradiction#####################################

#AA1: Within individals
wilcox.test(up4$AA1~up4$round,alternative = c("two.sided"),paired=TRUE)
wilcox.test(up5$AA1~up5$round,alternative = c("two.sided"),paired=TRUE)
wilcox.test(up6$AA1~up6$round,alternative = c("two.sided"),paired=TRUE)
#AA1: Between Indivdials
wilcox.test(fgr4$AA1,fgr5$AA1,alternative = c("two.sided"))
wilcox.test(fgr4$AA1,fgr6$AA1,alternative = c("two.sided"))
wilcox.test(fgr5$AA1,fgr6$AA1,alternative = c("two.sided"))

#AA2: Within Individuals
wilcox.test(up4$AA2~up4$round,alternative = c("two.sided"),paired=TRUE)
wilcox.test(up5$AA2~up5$round,alternative = c("two.sided"),paired=TRUE)
wilcox.test(up6$AA2~up6$round,alternative = c("two.sided"),paired=TRUE)
#AA2: Between Individuals
wilcox.test(fgr4$AA2,fgr5$AA2,alternative = c("two.sided"))
wilcox.test(fgr4$AA2,fgr6$AA2,alternative = c("two.sided"))
wilcox.test(fgr5$AA2,fgr6$AA2,alternative = c("two.sided"))

#EE1: Within Individuals
wilcox.test(up4$EE1~up4$round,alternative = c("two.sided"),paired=TRUE)
wilcox.test(up5$EE1~up5$round,alternative = c("two.sided"),paired=TRUE)
wilcox.test(up6$EE1~up6$round,alternative = c("two.sided"),paired=TRUE)
#EE1: Between Individuals
wilcox.test(fgr4$EE1,fgr5$EE1,alternative = c("two.sided"))
wilcox.test(fgr4$EE1,fgr6$EE1,alternative = c("two.sided"))
wilcox.test(fgr5$EE1,fgr6$EE1,alternative = c("two.sided"))

#EE2: Within Individuals
wilcox.test(up4$EE2~up4$round,alternative = c("two.sided"),paired=TRUE)
wilcox.test(up5$EE2~up5$round,alternative = c("two.sided"),paired=TRUE)
wilcox.test(up6$EE2~up6$round,alternative = c("two.sided"),paired=TRUE)
#EE2: Between Individuals
wilcox.test(fgr4$EE2,fgr5$EE2,alternative = c("two.sided"))
wilcox.test(fgr4$EE2,fgr6$EE2,alternative = c("two.sided"))
wilcox.test(fgr5$EE2,fgr6$EE2,alternative = c("two.sided"))

#EE3: Within Individuals
wilcox.test(up4$EE3~up4$round,alternative = c("two.sided"),paired=TRUE)
wilcox.test(up5$EE3~up5$round,alternative = c("two.sided"),paired=TRUE)
wilcox.test(up6$EE3~up6$round,alternative = c("two.sided"),paired=TRUE)
#EE3: Between Individuals
wilcox.test(fgr4$EE3,fgr5$EE3,alternative = c("two.sided"))
wilcox.test(fgr4$EE3,fgr6$EE3,alternative = c("two.sided"))
wilcox.test(fgr5$EE3,fgr6$EE3,alternative = c("two.sided"))


#EE12: Within Individuals
wilcox.test(up4$EE12~up4$round,alternative = c("two.sided"),paired=TRUE)
wilcox.test(up5$EE12~up5$round,alternative = c("two.sided"),paired=TRUE)
wilcox.test(up6$EE12~up6$round,alternative = c("two.sided"),paired=TRUE)
#EE12: Between Individuals
wilcox.test(fgr4$EE12,fgr5$EE12,alternative = c("two.sided"))
wilcox.test(fgr4$EE12,fgr6$EE12,alternative = c("two.sided"))
wilcox.test(fgr5$EE12,fgr6$EE12,alternative = c("two.sided"))

#EE23: Within Individuals
wilcox.test(up4$EE23~up4$round,alternative = c("two.sided"),paired=TRUE)
wilcox.test(up5$EE23~up5$round,alternative = c("two.sided"),paired=TRUE)
wilcox.test(up6$EE23~up6$round,alternative = c("two.sided"),paired=TRUE)

#EE23: Between Individuals
wilcox.test(fgr4$EE23,fgr5$EE23,alternative = c("two.sided"))
wilcox.test(fgr4$EE23,fgr6$EE23,alternative = c("two.sided"))
wilcox.test(fgr5$EE23,fgr6$EE23,alternative = c("two.sided"))

#EE13: Within Individuals
wilcox.test(up4$EE13~up4$round,alternative = c("two.sided"),paired=TRUE)
wilcox.test(up5$EE13~up5$round,alternative = c("two.sided"),paired=TRUE)
wilcox.test(up6$EE13~up6$round,alternative = c("two.sided"),paired=TRUE)
#EE13: Between Individuals
wilcox.test(fgr4$EE13,fgr5$EE13,alternative = c("two.sided"))
wilcox.test(fgr4$EE13,fgr6$EE13,alternative = c("two.sided"))
wilcox.test(fgr5$EE13,fgr6$EE13,alternative = c("two.sided"))


################Graph series 3: Contradiction, COnfirmation difference#########

#AA1: Within Indivdiuals
wilcox.test(up7$AA1~up7$round,alternative = c("two.sided"),paired=TRUE)
wilcox.test(up8$AA1~up8$round,alternative = c("two.sided"),paired=TRUE)
#AA1: Between Individuals
wilcox.test(fgr7$AA1,fgr8$AA1,alternative = c("two.sided"))

#AA2: Within Indidviduals
wilcox.test(up7$AA2~up7$round,alternative = c("two.sided"),paired=TRUE)
wilcox.test(up8$AA2~up8$round,alternative = c("two.sided"),paired=TRUE)
#AA2: Between Individuals
wilcox.test(fgr7$AA2,fgr8$AA2,alternative = c("two.sided"))

#EE1: Within Individials.
wilcox.test(up7$EE1~up7$round,alternative = c("two.sided"),paired=TRUE)
wilcox.test(up8$EE1~up8$round,alternative = c("two.sided"),paired=TRUE)
#EE1: Between Individuals
wilcox.test(fgr7$EE1,fgr8$EE1,alternative = c("two.sided"))

#EE2: Within Individials.
wilcox.test(up7$EE2~up7$round,alternative = c("two.sided"),paired=TRUE)
wilcox.test(up8$EE2~up8$round,alternative = c("two.sided"),paired=TRUE)
#EE2: Between Individuals
wilcox.test(fgr7$EE2,fgr8$EE2,alternative = c("two.sided"))

#EE3: Within Individials.
wilcox.test(up7$EE3~up7$round,alternative = c("two.sided"),paired=TRUE)
wilcox.test(up8$EE3~up8$round,alternative = c("two.sided"),paired=TRUE)
#EE3: Between Individuals
wilcox.test(fgr7$EE3,fgr8$EE3,alternative = c("two.sided"))

#EE12: Within Individials.
wilcox.test(up7$EE12~up7$round,alternative = c("two.sided"),paired=TRUE)
wilcox.test(up8$EE12~up8$round,alternative = c("two.sided"),paired=TRUE)
#EE12: Between Individuals
wilcox.test(fgr7$EE12,fgr8$EE12,alternative = c("two.sided"))

#EE23: Within Individials.
wilcox.test(up7$EE23~up7$round,alternative = c("two.sided"),paired=TRUE)
wilcox.test(up8$EE23~up8$round,alternative = c("two.sided"),paired=TRUE)
#EE23: Between Individuals
wilcox.test(fgr7$EE23,fgr8$EE23,alternative = c("two.sided"))

#EE13: Within Individials.
wilcox.test(up7$EE13~up7$round,alternative = c("two.sided"),paired=TRUE)
wilcox.test(up8$EE13~up8$round,alternative = c("two.sided"),paired=TRUE)
#EE13: Between Individuals
wilcox.test(fgr7$EE13,fgr8$EE13,alternative = c("two.sided"))




########################################################################
#################################Regressions############################
########################################################################
#Control variables:
table(ful$median_accuracy)
table(ful$median_credibility)

#age_18_34 control
table(ful$age_53_69)
table(ful$age_35_52)

#education low control
table(ful$education_high)


#not ssu_married as control
table(ful$family_ssu_married)

#gender male und diverse as control
table(ful$gender_female)


#income low as control
table(ful$income_high)


table(ful$Outro.1.player.Kids)

table(ful$median_risk_gen)
table(ful$median_risk_weat)
table(ful$median_temp)
table(ful$median_usage)



#######################################
#####OLS Regs##########################
#######################################






ful$round2<-ifelse(ful$round==2,1,0)
table(ful$round2)

#Subsets
ful1<-subset(ful)
ful2<-subset(ful,ful$Intro.1.player.location=="Weiskirchen")
ful3<-subset(ful,ful$Intro.1.player.location=="Ilomantsi")

###########Regression dummys
ful1$contradiction<-ifelse(ful1$Intro.1.player.location=="Ilomantsi",1,0)
table(ful1$contradiction)

##AA1##
#Confirmation,Contradiction
ols1<-lm(reformulate(c("contradiction*round2"), response="AA1"), data=ful1)
summary(ols1)

ols2<-lm(reformulate(c("contradiction*round2",list_dem), response="AA1"), data=ful1)
summary(ols2)

ols3<-lm(reformulate(c("contradiction*round2",list_all), response="AA1"), data=ful1)
summary(ols3)

#Confirmation
ols4<-lm(reformulate(c("treat*round2"), response="AA1"), data=ful2)
summary(ols4)

ols5<-lm(reformulate(c("treat*round2",list_dem), response="AA1"), data=ful2)
summary(ols5)

ols6<-lm(reformulate(c("treat*round2",list_all), response="AA1"), data=ful2)
summary(ols6)

#Contradiction
ols7<-lm(reformulate(c("treat*round2"), response="AA1"), data=ful3)
summary(ols7)

ols8<-lm(reformulate(c("treat*round2",list_dem), response="AA1"), data=ful3)
summary(ols8)

ols9<-lm(reformulate(c("treat*round2",list_all), response="AA1"), data=ful3)
summary(ols9)

##AA2##
#Confirmation,Contradiction
ols1a<-lm(reformulate(c("contradiction*round2"), response="AA2"), data=ful1)
summary(ols1a)

ols2a<-lm(reformulate(c("contradiction*round2",list_dem), response="AA2"), data=ful1)
summary(ols2a)

ols3a<-lm(reformulate(c("contradiction*round2",list_all), response="AA2"), data=ful1)
summary(ols3a)

#Confirmation
ols4a<-lm(reformulate(c("treat*round2"), response="AA2"), data=ful2)
summary(ols4a)

ols5a<-lm(reformulate(c("treat*round2",list_dem), response="AA2"), data=ful2)
summary(ols5a)

ols6a<-lm(reformulate(c("treat*round2",list_all), response="AA2"), data=ful2)
summary(ols6a)

#Contradiction
ols7a<-lm(reformulate(c("treat*round2"), response="AA2"), data=ful3)
summary(ols7a)

ols8a<-lm(reformulate(c("treat*round2",list_dem), response="AA2"), data=ful3)
summary(ols8a)

ols9a<-lm(reformulate(c("treat*round2",list_all), response="AA2"), data=ful3)
summary(ols9a)





##EE1##
#Confirmation,Contradiction

ols11<-lm(reformulate(c("contradiction*round2"), response="EE1"), data=ful1)
summary(ols11)

ols12<-lm(reformulate(c("contradiction*round2",list_dem), response="EE1"), data=ful1)
summary(ols12)

ols13<-lm(reformulate(c("contradiction*round2",list_all), response="EE1"), data=ful1)
summary(ols13)

#Confirmation
ols14<-lm(reformulate(c("treat*round2"), response="EE1"), data=ful2)
summary(ols14)

ols15<-lm(reformulate(c("treat*round2",list_dem), response="EE1"), data=ful2)
summary(ols15)

ols16<-lm(reformulate(c("treat*round2",list_all), response="EE1"), data=ful2)
summary(ols16)

#Contradiction
ols17<-lm(reformulate(c("treat*round2"), response="EE1"), data=ful3)
summary(ols17)

ols18<-lm(reformulate(c("treat*round2",list_dem), response="EE1"), data=ful3)
summary(ols18)

ols19<-lm(reformulate(c("treat*round2",list_all), response="EE1"), data=ful3)
summary(ols19)

##EE2##
#Confirmation,Contradiction

ols21<-lm(reformulate(c("contradiction*round2"), response="EE2"), data=ful1)
summary(ols21)

ols22<-lm(reformulate(c("contradiction*round2",list_dem), response="EE2"), data=ful1)
summary(ols22)

ols23<-lm(reformulate(c("contradiction*round2",list_all), response="EE2"), data=ful1)
summary(ols23)

#Confirmation
ols24<-lm(reformulate(c("treat*round2"), response="EE2"), data=ful2)
summary(ols24)

ols25<-lm(reformulate(c("treat*round2",list_dem), response="EE2"), data=ful2)
summary(ols25)

ols26<-lm(reformulate(c("treat*round2",list_all), response="EE2"), data=ful2)
summary(ols26)

#Contradiction
ols27<-lm(reformulate(c("treat*round2"), response="EE2"), data=ful3)
summary(ols27)

ols28<-lm(reformulate(c("treat*round2",list_dem), response="EE2"), data=ful3)
summary(ols28)

ols29<-lm(reformulate(c("treat*round2",list_all), response="EE2"), data=ful3)
summary(ols29)

##EE3##
#Confirmation,Contradiction

ols31<-lm(reformulate(c("contradiction*round2"), response="EE3"), data=ful1)
summary(ols31)

ols32<-lm(reformulate(c("contradiction*round2",list_dem), response="EE3"), data=ful1)
summary(ols32)

ols33<-lm(reformulate(c("contradiction*round2",list_all), response="EE3"), data=ful1)
summary(ols33)

#Confirmation
ols34<-lm(reformulate(c("treat*round2"), response="EE3"), data=ful2)
summary(ols34)

ols35<-lm(reformulate(c("treat*round2",list_dem), response="EE3"), data=ful2)
summary(ols35)

ols36<-lm(reformulate(c("treat*round2",list_all), response="EE3"), data=ful2)
summary(ols36)

#Contradiction
ols37<-lm(reformulate(c("treat*round2"), response="EE3"), data=ful3)
summary(ols37)

ols38<-lm(reformulate(c("treat*round2",list_dem), response="EE3"), data=ful3)
summary(ols38)

ols39<-lm(reformulate(c("treat*round2",list_all), response="EE3"), data=ful3)
summary(ols39)




##EE12##
#Confirmation,Contradiction
ols41<-lm(reformulate(c("contradiction*round2"), response="EE12"), data=ful1)
summary(ols41)

ols42<-lm(reformulate(c("contradiction*round2",list_dem), response="EE12"), data=ful1)
summary(ols42)

ols43<-lm(reformulate(c("contradiction*round2",list_all), response="EE12"), data=ful1)
summary(ols43)

#Confirmation
ols44<-lm(reformulate(c("treat*round2"), response="EE12"), data=ful2)
summary(ols44)

ols45<-lm(reformulate(c("treat*round2",list_dem), response="EE12"), data=ful2)
summary(ols45)

ols46<-lm(reformulate(c("treat*round2",list_all), response="EE12"), data=ful2)
summary(ols46)

#Contradiction
ols47<-lm(reformulate(c("treat*round2"), response="EE12"), data=ful3)
summary(ols47)

ols48<-lm(reformulate(c("treat*round2",list_dem), response="EE12"), data=ful3)
summary(ols48)

ols49<-lm(reformulate(c("treat*round2",list_all), response="EE12"), data=ful3)
summary(ols49)

##EE23##
#Confirmation,Contradiction
ols51<-lm(reformulate(c("contradiction*round2"), response="EE23"), data=ful1)
summary(ols51)

ols52<-lm(reformulate(c("contradiction*round2",list_dem), response="EE23"), data=ful1)
summary(ols52)

ols53<-lm(reformulate(c("contradiction*round2",list_all), response="EE23"), data=ful1)
summary(ols53)

#Confirmation
ols54<-lm(reformulate(c("treat*round2"), response="EE23"), data=ful2)
summary(ols54)

ols55<-lm(reformulate(c("treat*round2",list_dem), response="EE23"), data=ful2)
summary(ols55)

ols56<-lm(reformulate(c("treat*round2",list_all), response="EE23"), data=ful2)
summary(ols56)

#Contradiction
ols57<-lm(reformulate(c("treat*round2"), response="EE23"), data=ful3)
summary(ols57)

ols58<-lm(reformulate(c("treat*round2",list_dem), response="EE23"), data=ful3)
summary(ols58)

ols59<-lm(reformulate(c("treat*round2",list_all), response="EE23"), data=ful3)
summary(ols59)
##EE13##
#Confirmation,Contradiction
ols61<-lm(reformulate(c("contradiction*round2"), response="EE13"), data=ful1)
summary(ols61)

ols62<-lm(reformulate(c("contradiction*round2",list_dem), response="EE13"), data=ful1)
summary(ols62)

ols63<-lm(reformulate(c("contradiction*round2",list_all), response="EE13"), data=ful1)
summary(ols63)

#Confirmation
ols64<-lm(reformulate(c("treat*round2"), response="EE13"), data=ful2)
summary(ols64)

ols65<-lm(reformulate(c("treat*round2",list_dem), response="EE13"), data=ful2)
summary(ols65)

ols66<-lm(reformulate(c("treat*round2",list_all), response="EE13"), data=ful2)
summary(ols66)

#Contradiction
ols67<-lm(reformulate(c("treat*round2"), response="EE13"), data=ful3)
summary(ols67)

ols68<-lm(reformulate(c("treat*round2",list_dem), response="EE13"), data=ful3)
summary(ols68)

ols69<-lm(reformulate(c("treat*round2",list_all), response="EE13"), data=ful3)
summary(ols69)

######Graph E1-E13

library("lmtest")
library("sandwich")
?coefci
# no control variables
cie11<-coefci(ols11, parm=c("contradiction:round2"), vcov = vcovCL(ols11, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice11<-ols11$coefficients[c("contradiction:round2")]
ci11<-cbind(cie11,cice11)
event<-c("E1")
ci11<-cbind(ci11,event)
ci11<-data.frame(ci11)


cie21<-coefci(ols21, parm=c("contradiction:round2"), vcov = vcovCL(ols21, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice21<-ols21$coefficients[c("contradiction:round2")]
ci21<-cbind(cie21,cice21)
event<-c("E2")
ci21<-cbind(ci21,event)
ci21<-data.frame(ci21)
names(ci21)[3]<-paste("cice11")

cie31<-coefci(ols31, parm=c("contradiction:round2"), vcov = vcovCL(ols31, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice31<-ols31$coefficients[c("contradiction:round2")]
ci31<-cbind(cie31,cice31)
event<-c("E3")
ci31<-cbind(ci31,event)
ci31<-data.frame(ci31)
names(ci31)[3]<-paste("cice11")

cie41<-coefci(ols41, parm=c("contradiction:round2"), vcov = vcovCL(ols41, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice41<-ols41$coefficients[c("contradiction:round2")]
ci41<-cbind(cie41,cice41)
event<-c("E12")
ci41<-cbind(ci41,event)
ci41<-data.frame(ci41)
names(ci41)[3]<-paste("cice11")

cie51<-coefci(ols51, parm=c("contradiction:round2"), vcov = vcovCL(ols51, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice51<-ols51$coefficients[c("contradiction:round2")]
ci51<-cbind(cie51,cice51)
event<-c("E23")
ci51<-cbind(ci51,event)
ci51<-data.frame(ci51)
names(ci51)[3]<-paste("cice11")

cie61<-coefci(ols61, parm=c("contradiction:round2"), vcov = vcovCL(ols61, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice61<-ols61$coefficients[c("contradiction:round2")]
ci61<-cbind(cie61,cice61)
event<-c("E13")
ci61<-cbind(ci61,event)
ci61<-data.frame(ci61)
names(ci61)[3]<-paste("cice11")

ciful<-rbind(ci11,ci21,ci31,ci41,ci51,ci61)
ciful$X2.5..<-as.numeric(ciful$X2.5..)
ciful$X97.5..<-as.numeric(ciful$X97.5..)
ciful$cice11<-as.numeric(ciful$cice11)
ciful$controls<-"1.) none"

#with demographic variables
cie12<-coefci(ols12, parm=c("contradiction:round2"), vcov = vcovCL(ols12, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice12<-ols12$coefficients[c("contradiction:round2")]
ci12<-cbind(cie12,cice12)
event<-c("E1")
ci12<-cbind(ci12,event)
ci12<-data.frame(ci12)
names(ci12)[3]<-paste("cice11")

cie22<-coefci(ols22, parm=c("contradiction:round2"), vcov = vcovCL(ols22, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice22<-ols22$coefficients[c("contradiction:round2")]
ci22<-cbind(cie22,cice22)
event<-c("E2")
ci22<-cbind(ci22,event)
ci22<-data.frame(ci22)
names(ci22)[3]<-paste("cice11")

cie32<-coefci(ols32, parm=c("contradiction:round2"), vcov = vcovCL(ols32, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice32<-ols32$coefficients[c("contradiction:round2")]
ci32<-cbind(cie32,cice32)
event<-c("E3")
ci32<-cbind(ci32,event)
ci32<-data.frame(ci32)
names(ci32)[3]<-paste("cice11")

cie42<-coefci(ols42, parm=c("contradiction:round2"), vcov = vcovCL(ols42, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice42<-ols42$coefficients[c("contradiction:round2")]
ci42<-cbind(cie42,cice42)
event<-c("E12")
ci42<-cbind(ci42,event)
ci42<-data.frame(ci42)
names(ci42)[3]<-paste("cice11")

cie52<-coefci(ols52, parm=c("contradiction:round2"), vcov = vcovCL(ols52, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice52<-ols52$coefficients[c("contradiction:round2")]
ci52<-cbind(cie52,cice52)
event<-c("E23")
ci52<-cbind(ci52,event)
ci52<-data.frame(ci52)
names(ci52)[3]<-paste("cice11")

cie62<-coefci(ols62, parm=c("contradiction:round2"), vcov = vcovCL(ols62, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice62<-ols62$coefficients[c("contradiction:round2")]
ci62<-cbind(cie62,cice62)
event<-c("E13")
ci62<-cbind(ci62,event)
ci62<-data.frame(ci62)
names(ci62)[3]<-paste("cice11")

ciful2<-rbind(ci12,ci22,ci32,ci42,ci52,ci62)
ciful2$X2.5..<-as.numeric(ciful2$X2.5..)
ciful2$X97.5..<-as.numeric(ciful2$X97.5..)
ciful2$cice11<-as.numeric(ciful2$cice11)
ciful2$controls<-"2.) demographic"

ciful<-rbind(ciful,ciful2)


#with demographic variables + further controls
cie13<-coefci(ols13, parm=c("contradiction:round2"), vcov = vcovCL(ols13, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice13<-ols13$coefficients[c("contradiction:round2")]
ci13<-cbind(cie13,cice13)
event<-c("E1")
ci13<-cbind(ci13,event)
ci13<-data.frame(ci13)
names(ci13)[3]<-paste("cice11")

cie23<-coefci(ols23, parm=c("contradiction:round2"), vcov = vcovCL(ols23, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice23<-ols23$coefficients[c("contradiction:round2")]
ci23<-cbind(cie23,cice23)
event<-c("E2")
ci23<-cbind(ci23,event)
ci23<-data.frame(ci23)
names(ci23)[3]<-paste("cice11")

cie33<-coefci(ols33, parm=c("contradiction:round2"), vcov = vcovCL(ols33, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice33<-ols33$coefficients[c("contradiction:round2")]
ci33<-cbind(cie33,cice33)
event<-c("E3")
ci33<-cbind(ci33,event)
ci33<-data.frame(ci33)
names(ci33)[3]<-paste("cice11")

cie43<-coefci(ols43, parm=c("contradiction:round2"), vcov = vcovCL(ols43, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice43<-ols43$coefficients[c("contradiction:round2")]
ci43<-cbind(cie43,cice43)
event<-c("E12")
ci43<-cbind(ci43,event)
ci43<-data.frame(ci43)
names(ci43)[3]<-paste("cice11")

cie53<-coefci(ols53, parm=c("contradiction:round2"), vcov = vcovCL(ols53, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice53<-ols53$coefficients[c("contradiction:round2")]
ci53<-cbind(cie53,cice53)
event<-c("E23")
ci53<-cbind(ci53,event)
ci53<-data.frame(ci53)
names(ci53)[3]<-paste("cice11")

cie63<-coefci(ols63, parm=c("contradiction:round2"), vcov = vcovCL(ols63, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice63<-ols63$coefficients[c("contradiction:round2")]
ci63<-cbind(cie63,cice63)
event<-c("E13")
ci63<-cbind(ci63,event)
ci63<-data.frame(ci63)
names(ci63)[3]<-paste("cice11")

ciful3<-rbind(ci13,ci23,ci33,ci43,ci53,ci63)
ciful3$X2.5..<-as.numeric(ciful3$X2.5..)
ciful3$X97.5..<-as.numeric(ciful3$X97.5..)
ciful3$cice11<-as.numeric(ciful3$cice11)
ciful3$controls<-"3.) demographic + further"

ciful<-rbind(ciful,ciful3)


ciful$controls<- with(ciful, factor(controls, levels=c("1.) none","2.) demographic", "3.) demographic + further")))
library(ggplot2)
ggplot(
  mapping = aes(
    y = ciful$event
  )
) +
  scale_y_discrete( limits=c("E13", "E23","E12", "E3","E2", "E1"))+
  geom_pointrange(aes(
    x = ciful$cice11,
    y = ciful$event,
    xmin = ciful$X2.5..,
    xmax = ciful$X97.5..,
    shape=factor(ciful$controls, levels=c("3.) demographic + further","2.) demographic", "1.) none"))
  ), 
    position = position_dodge(width = 0.4),
  
    # Optional decoration:
    fatten=5,
    alpha=.8
  ) +
  
  # Add a line marker for y = 0 (to see if the CI overlaps 0)
  geom_vline(xintercept = 0, color = "red", alpha = 0.2, show.legend=F) +
  # Additional decoration:
  theme_bw() +
 # guides(linetype=guide_legend(override.aes = list(shape = NA))) + 
  theme(
    plot.title = element_text(face = "bold", size = 12),
    legend.background = element_rect(fill = "white", size = 4, colour = "white"),
    #legend.justification = c(0, 1),
    axis.ticks = element_line(colour = "white", size = 0.1),
    panel.grid.major = element_line(colour = "white", size = 0.07),
    panel.grid.minor = element_blank(),
    legend.key.size = unit(1, "lines"),
    legend.box = 'horizontal',
    legend.position = "top"
  ) +
  labs(y = "Event", x = "Estimate",shape="Control variables")+
  scale_shape_manual(values = c("1.) none" = 15, "2.) demographic" = 16,"3.) demographic + further"=17))



ggsave("Events_regs_1_all.pdf",height=3.5, width=5.7)



###Graph 2 (Confirmation DiDs)
# no control variables
cie14<-coefci(ols14, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols14, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice14<-ols14$coefficients[c("treatconf.-both:round2")]
ci14<-cbind(cie14,cice14)
event<-c("E1 (both)")
ci14<-cbind(ci14,event)
ci14<-data.frame(ci14)


cie24<-coefci(ols24, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols24, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice24<-ols24$coefficients[c("treatconf.-both:round2")]
ci24<-cbind(cie24,cice24)
event<-c("E2 (both)")
ci24<-cbind(ci24,event)
ci24<-data.frame(ci24)
names(ci24)[3]<-paste("cice14")

cie34<-coefci(ols34, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols34, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice34<-ols34$coefficients[c("treatconf.-both:round2")]
ci34<-cbind(cie34,cice34)
event<-c("E3 (both)")
ci34<-cbind(ci34,event)
ci34<-data.frame(ci34)
names(ci34)[3]<-paste("cice14")

cie44<-coefci(ols44, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols44, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice44<-ols44$coefficients[c("treatconf.-both:round2")]
ci44<-cbind(cie44,cice44)
event<-c("E12 (both)")
ci44<-cbind(ci44,event)
ci44<-data.frame(ci44)
names(ci44)[3]<-paste("cice14")

cie54<-coefci(ols54, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols54, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice54<-ols54$coefficients[c("treatconf.-both:round2")]
ci54<-cbind(cie54,cice54)
event<-c("E23 (both)")
ci54<-cbind(ci54,event)
ci54<-data.frame(ci54)
names(ci54)[3]<-paste("cice14")

cie64<-coefci(ols64, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols64, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice64<-ols64$coefficients[c("treatconf.-both:round2")]
ci64<-cbind(cie64,cice64)
event<-c("E13 (both)")
ci64<-cbind(ci64,event)
ci64<-data.frame(ci64)
names(ci64)[3]<-paste("cice14")

ciful<-rbind(ci14,ci24,ci34,ci44,ci54,ci64)
ciful$X2.5..<-as.numeric(ciful$X2.5..)
ciful$X97.5..<-as.numeric(ciful$X97.5..)
ciful$cice14<-as.numeric(ciful$cice14)
ciful$controls<-"1.) none"

#with demographic variables
cie15<-coefci(ols15, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols15, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice15<-ols15$coefficients[c("treatconf.-both:round2")]
ci15<-cbind(cie15,cice15)
event<-c("E1 (both)")
ci15<-cbind(ci15,event)
ci15<-data.frame(ci15)
names(ci15)[3]<-paste("cice14")

cie25<-coefci(ols25, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols25, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice25<-ols25$coefficients[c("treatconf.-both:round2")]
ci25<-cbind(cie25,cice25)
event<-c("E2 (both)")
ci25<-cbind(ci25,event)
ci25<-data.frame(ci25)
names(ci25)[3]<-paste("cice14")

cie35<-coefci(ols35, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols35, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice35<-ols35$coefficients[c("treatconf.-both:round2")]
ci35<-cbind(cie35,cice35)
event<-c("E3 (both)")
ci35<-cbind(ci35,event)
ci35<-data.frame(ci35)
names(ci35)[3]<-paste("cice14")

cie45<-coefci(ols45, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols45, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice45<-ols45$coefficients[c("treatconf.-both:round2")]
ci45<-cbind(cie45,cice45)
event<-c("E12 (both)")
ci45<-cbind(ci45,event)
ci45<-data.frame(ci45)
names(ci45)[3]<-paste("cice14")

cie55<-coefci(ols55, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols55, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice55<-ols55$coefficients[c("treatconf.-both:round2")]
ci55<-cbind(cie55,cice55)
event<-c("E23 (both)")
ci55<-cbind(ci55,event)
ci55<-data.frame(ci55)
names(ci55)[3]<-paste("cice14")

cie65<-coefci(ols65, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols65, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice65<-ols65$coefficients[c("treatconf.-both:round2")]
ci65<-cbind(cie65,cice65)
event<-c("E13 (both)")
ci65<-cbind(ci65,event)
ci65<-data.frame(ci65)
names(ci65)[3]<-paste("cice14")

ciful2<-rbind(ci15,ci25,ci35,ci45,ci55,ci65)
ciful2$X2.5..<-as.numeric(ciful2$X2.5..)
ciful2$X97.5..<-as.numeric(ciful2$X97.5..)
ciful2$cice14<-as.numeric(ciful2$cice14)
ciful2$controls<-"2.) demographic"

ciful<-rbind(ciful,ciful2)


#with demographic variables + further controls
cie16<-coefci(ols16, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols16, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice16<-ols16$coefficients[c("treatconf.-both:round2")]
ci16<-cbind(cie16,cice16)
event<-c("E1 (both)")
ci16<-cbind(ci16,event)
ci16<-data.frame(ci16)
names(ci16)[3]<-paste("cice14")

cie26<-coefci(ols26, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols26, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice26<-ols26$coefficients[c("treatconf.-both:round2")]
ci26<-cbind(cie26,cice26)
event<-c("E2 (both)")
ci26<-cbind(ci26,event)
ci26<-data.frame(ci26)
names(ci26)[3]<-paste("cice14")

cie36<-coefci(ols36, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols36, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice36<-ols36$coefficients[c("treatconf.-both:round2")]
ci36<-cbind(cie36,cice36)
event<-c("E3 (both)")
ci36<-cbind(ci36,event)
ci36<-data.frame(ci36)
names(ci36)[3]<-paste("cice14")

cie46<-coefci(ols46, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols46, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice46<-ols46$coefficients[c("treatconf.-both:round2")]
ci46<-cbind(cie46,cice46)
event<-c("E12 (both)")
ci46<-cbind(ci46,event)
ci46<-data.frame(ci46)
names(ci46)[3]<-paste("cice14")

cie56<-coefci(ols56, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols56, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice56<-ols56$coefficients[c("treatconf.-both:round2")]
ci56<-cbind(cie56,cice56)
event<-c("E23 (both)")
ci56<-cbind(ci56,event)
ci56<-data.frame(ci56)
names(ci56)[3]<-paste("cice14")

cie66<-coefci(ols66, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols66, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice66<-ols66$coefficients[c("treatconf.-both:round2")]
ci66<-cbind(cie66,cice66)
event<-c("E13 (both)")
ci66<-cbind(ci66,event)
ci66<-data.frame(ci66)
names(ci66)[3]<-paste("cice14")

ciful3<-rbind(ci16,ci26,ci36,ci46,ci56,ci66)
ciful3$X2.5..<-as.numeric(ciful3$X2.5..)
ciful3$X97.5..<-as.numeric(ciful3$X97.5..)
ciful3$cice14<-as.numeric(ciful3$cice14)
ciful3$controls<-"3.) demographic + further"

cifula<-rbind(ciful,ciful3)


# no control variables
cie14<-coefci(ols14, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols14, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice14<-ols14$coefficients[c("treatconf.-interval:round2")]
ci14<-cbind(cie14,cice14)
event<-c("E1 (interval)")
ci14<-cbind(ci14,event)
ci14<-data.frame(ci14)


cie24<-coefci(ols24, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols24, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice24<-ols24$coefficients[c("treatconf.-interval:round2")]
ci24<-cbind(cie24,cice24)
event<-c("E2 (interval)")
ci24<-cbind(ci24,event)
ci24<-data.frame(ci24)
names(ci24)[3]<-paste("cice14")

cie34<-coefci(ols34, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols34, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice34<-ols34$coefficients[c("treatconf.-interval:round2")]
ci34<-cbind(cie34,cice34)
event<-c("E3 (interval)")
ci34<-cbind(ci34,event)
ci34<-data.frame(ci34)
names(ci34)[3]<-paste("cice14")

cie44<-coefci(ols44, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols44, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice44<-ols44$coefficients[c("treatconf.-interval:round2")]
ci44<-cbind(cie44,cice44)
event<-c("E12 (interval)")
ci44<-cbind(ci44,event)
ci44<-data.frame(ci44)
names(ci44)[3]<-paste("cice14")

cie54<-coefci(ols54, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols54, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice54<-ols54$coefficients[c("treatconf.-interval:round2")]
ci54<-cbind(cie54,cice54)
event<-c("E23 (interval)")
ci54<-cbind(ci54,event)
ci54<-data.frame(ci54)
names(ci54)[3]<-paste("cice14")

cie64<-coefci(ols64, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols64, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice64<-ols64$coefficients[c("treatconf.-interval:round2")]
ci64<-cbind(cie64,cice64)
event<-c("E13 (interval)")
ci64<-cbind(ci64,event)
ci64<-data.frame(ci64)
names(ci64)[3]<-paste("cice14")

ciful<-rbind(ci14,ci24,ci34,ci44,ci54,ci64)
ciful$X2.5..<-as.numeric(ciful$X2.5..)
ciful$X97.5..<-as.numeric(ciful$X97.5..)
ciful$cice14<-as.numeric(ciful$cice14)
ciful$controls<-"1.) none"

#with demographic variables
cie15<-coefci(ols15, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols15, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice15<-ols15$coefficients[c("treatconf.-interval:round2")]
ci15<-cbind(cie15,cice15)
event<-c("E1 (interval)")
ci15<-cbind(ci15,event)
ci15<-data.frame(ci15)
names(ci15)[3]<-paste("cice14")

cie25<-coefci(ols25, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols25, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice25<-ols25$coefficients[c("treatconf.-interval:round2")]
ci25<-cbind(cie25,cice25)
event<-c("E2 (interval)")
ci25<-cbind(ci25,event)
ci25<-data.frame(ci25)
names(ci25)[3]<-paste("cice14")

cie35<-coefci(ols35, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols35, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice35<-ols35$coefficients[c("treatconf.-interval:round2")]
ci35<-cbind(cie35,cice35)
event<-c("E3 (interval)")
ci35<-cbind(ci35,event)
ci35<-data.frame(ci35)
names(ci35)[3]<-paste("cice14")

cie45<-coefci(ols45, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols45, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice45<-ols45$coefficients[c("treatconf.-interval:round2")]
ci45<-cbind(cie45,cice45)
event<-c("E12 (interval)")
ci45<-cbind(ci45,event)
ci45<-data.frame(ci45)
names(ci45)[3]<-paste("cice14")

cie55<-coefci(ols55, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols55, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice55<-ols55$coefficients[c("treatconf.-interval:round2")]
ci55<-cbind(cie55,cice55)
event<-c("E23 (interval)")
ci55<-cbind(ci55,event)
ci55<-data.frame(ci55)
names(ci55)[3]<-paste("cice14")

cie65<-coefci(ols65, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols65, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice65<-ols65$coefficients[c("treatconf.-interval:round2")]
ci65<-cbind(cie65,cice65)
event<-c("E13 (interval)")
ci65<-cbind(ci65,event)
ci65<-data.frame(ci65)
names(ci65)[3]<-paste("cice14")

ciful2<-rbind(ci15,ci25,ci35,ci45,ci55,ci65)
ciful2$X2.5..<-as.numeric(ciful2$X2.5..)
ciful2$X97.5..<-as.numeric(ciful2$X97.5..)
ciful2$cice14<-as.numeric(ciful2$cice14)
ciful2$controls<-"2.) demographic"

ciful<-rbind(ciful,ciful2)


#with demographic variables + further controls
cie16<-coefci(ols16, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols16, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice16<-ols16$coefficients[c("treatconf.-interval:round2")]
ci16<-cbind(cie16,cice16)
event<-c("E1 (interval)")
ci16<-cbind(ci16,event)
ci16<-data.frame(ci16)
names(ci16)[3]<-paste("cice14")

cie26<-coefci(ols26, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols26, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice26<-ols26$coefficients[c("treatconf.-interval:round2")]
ci26<-cbind(cie26,cice26)
event<-c("E2 (interval)")
ci26<-cbind(ci26,event)
ci26<-data.frame(ci26)
names(ci26)[3]<-paste("cice14")

cie36<-coefci(ols36, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols36, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice36<-ols36$coefficients[c("treatconf.-interval:round2")]
ci36<-cbind(cie36,cice36)
event<-c("E3 (interval)")
ci36<-cbind(ci36,event)
ci36<-data.frame(ci36)
names(ci36)[3]<-paste("cice14")

cie46<-coefci(ols46, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols46, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice46<-ols46$coefficients[c("treatconf.-interval:round2")]
ci46<-cbind(cie46,cice46)
event<-c("E12 (interval)")
ci46<-cbind(ci46,event)
ci46<-data.frame(ci46)
names(ci46)[3]<-paste("cice14")

cie56<-coefci(ols56, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols56, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice56<-ols56$coefficients[c("treatconf.-interval:round2")]
ci56<-cbind(cie56,cice56)
event<-c("E23 (interval)")
ci56<-cbind(ci56,event)
ci56<-data.frame(ci56)
names(ci56)[3]<-paste("cice14")

cie66<-coefci(ols66, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols66, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice66<-ols66$coefficients[c("treatconf.-interval:round2")]
ci66<-cbind(cie66,cice66)
event<-c("E13 (interval)")
ci66<-cbind(ci66,event)
ci66<-data.frame(ci66)
names(ci66)[3]<-paste("cice14")

ciful3<-rbind(ci16,ci26,ci36,ci46,ci56,ci66)
ciful3$X2.5..<-as.numeric(ciful3$X2.5..)
ciful3$X97.5..<-as.numeric(ciful3$X97.5..)
ciful3$cice14<-as.numeric(ciful3$cice14)
ciful3$controls<-"3.) demographic + further"

ciful<-rbind(ciful,ciful3)

ciful<-rbind(cifula,ciful)

ciful$controls<- with(ciful, factor(controls, levels=c("1.) none","2.) demographic", "3.) demographic + further")))
library(ggplot2)
ggplot(
  mapping = aes(
    y = ciful$event
  )
) +
  scale_y_discrete( limits=c("E13 (interval)","E13 (both)", "E23 (interval)","E23 (both)","E12 (interval)","E12 (both)", "E3 (interval)","E3 (both)","E2 (interval)","E2 (both)", "E1 (interval)","E1 (both)"))+
  geom_pointrange(aes(
    x = ciful$cice14,
    y = ciful$event,
    xmin = ciful$X2.5..,
    xmax = ciful$X97.5..,
    shape=factor(ciful$controls, levels=c("3.) demographic + further","2.) demographic", "1.) none"))
  ), 
  position = position_dodge(width = 0.4),
  
  # Optional decoration:
  fatten=5,
  alpha=.8
  ) +
  
  # Add a line marker for y = 0 (to see if the CI overlaps 0)
  geom_vline(xintercept = 0, color = "red", alpha = 0.2, show.legend=F) +
  # Additional decoration:
  theme_bw() +
  # guides(linetype=guide_legend(override.aes = list(shape = NA))) + 
  theme(
    plot.title = element_text(face = "bold", size = 12),
    legend.background = element_rect(fill = "white", size = 4, colour = "white"),
    #legend.justification = c(0, 1),
    axis.ticks = element_line(colour = "white", size = 0.1),
    panel.grid.major = element_line(colour = "white", size = 0.07),
    panel.grid.minor = element_blank(),
    legend.key.size = unit(1, "lines"),
    legend.box = 'horizontal',
    legend.position = "top"
  ) +
  labs(y = "Event", x = "Estimate",shape="Control variables")+
  scale_shape_manual(values = c("1.) none" = 15, "2.) demographic" = 16,"3.) demographic + further"=17))

ggsave("Events_regs_2_confirmation.pdf",height=6.5, width=6.3)


####Graph 3 (Contradiction DiDs)

# no control variables
cie17<-coefci(ols17, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols17, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice17<-ols17$coefficients[c("treatcontrad.-both:round2")]
ci17<-cbind(cie17,cice17)
event<-c("E1 (both)")
ci17<-cbind(ci17,event)
ci17<-data.frame(ci17)


cie27<-coefci(ols27, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols27, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice27<-ols27$coefficients[c("treatcontrad.-both:round2")]
ci27<-cbind(cie27,cice27)
event<-c("E2 (both)")
ci27<-cbind(ci27,event)
ci27<-data.frame(ci27)
names(ci27)[3]<-paste("cice17")

cie37<-coefci(ols37, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols37, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice37<-ols37$coefficients[c("treatcontrad.-both:round2")]
ci37<-cbind(cie37,cice37)
event<-c("E3 (both)")
ci37<-cbind(ci37,event)
ci37<-data.frame(ci37)
names(ci37)[3]<-paste("cice17")

cie47<-coefci(ols47, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols47, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice47<-ols47$coefficients[c("treatcontrad.-both:round2")]
ci47<-cbind(cie47,cice47)
event<-c("E12 (both)")
ci47<-cbind(ci47,event)
ci47<-data.frame(ci47)
names(ci47)[3]<-paste("cice17")

cie57<-coefci(ols57, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols57, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice57<-ols57$coefficients[c("treatcontrad.-both:round2")]
ci57<-cbind(cie57,cice57)
event<-c("E23 (both)")
ci57<-cbind(ci57,event)
ci57<-data.frame(ci57)
names(ci57)[3]<-paste("cice17")

cie67<-coefci(ols67, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols67, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice67<-ols67$coefficients[c("treatcontrad.-both:round2")]
ci67<-cbind(cie67,cice67)
event<-c("E13 (both)")
ci67<-cbind(ci67,event)
ci67<-data.frame(ci67)
names(ci67)[3]<-paste("cice17")

ciful<-rbind(ci17,ci27,ci37,ci47,ci57,ci67)
ciful$X2.5..<-as.numeric(ciful$X2.5..)
ciful$X97.5..<-as.numeric(ciful$X97.5..)
ciful$cice17<-as.numeric(ciful$cice17)
ciful$controls<-"1.) none"

#with demographic variables
cie18<-coefci(ols18, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols18, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice18<-ols18$coefficients[c("treatcontrad.-both:round2")]
ci18<-cbind(cie18,cice18)
event<-c("E1 (both)")
ci18<-cbind(ci18,event)
ci18<-data.frame(ci18)
names(ci18)[3]<-paste("cice17")

cie28<-coefci(ols28, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols28, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice28<-ols28$coefficients[c("treatcontrad.-both:round2")]
ci28<-cbind(cie28,cice28)
event<-c("E2 (both)")
ci28<-cbind(ci28,event)
ci28<-data.frame(ci28)
names(ci28)[3]<-paste("cice17")

cie38<-coefci(ols38, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols38, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice38<-ols38$coefficients[c("treatcontrad.-both:round2")]
ci38<-cbind(cie38,cice38)
event<-c("E3 (both)")
ci38<-cbind(ci38,event)
ci38<-data.frame(ci38)
names(ci38)[3]<-paste("cice17")

cie48<-coefci(ols48, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols48, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice48<-ols48$coefficients[c("treatcontrad.-both:round2")]
ci48<-cbind(cie48,cice48)
event<-c("E12 (both)")
ci48<-cbind(ci48,event)
ci48<-data.frame(ci48)
names(ci48)[3]<-paste("cice17")

cie58<-coefci(ols58, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols58, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice58<-ols58$coefficients[c("treatcontrad.-both:round2")]
ci58<-cbind(cie58,cice58)
event<-c("E23 (both)")
ci58<-cbind(ci58,event)
ci58<-data.frame(ci58)
names(ci58)[3]<-paste("cice17")

cie68<-coefci(ols68, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols68, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice68<-ols68$coefficients[c("treatcontrad.-both:round2")]
ci68<-cbind(cie68,cice68)
event<-c("E13 (both)")
ci68<-cbind(ci68,event)
ci68<-data.frame(ci68)
names(ci68)[3]<-paste("cice17")

ciful3<-rbind(ci18,ci28,ci38,ci48,ci58,ci68)
ciful3$X2.5..<-as.numeric(ciful3$X2.5..)
ciful3$X97.5..<-as.numeric(ciful3$X97.5..)
ciful3$cice17<-as.numeric(ciful3$cice17)
ciful3$controls<-"2.) demographic"

ciful<-rbind(ciful,ciful3)


#with demographic variables + further controls
cie19<-coefci(ols19, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols19, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice19<-ols19$coefficients[c("treatcontrad.-both:round2")]
ci19<-cbind(cie19,cice19)
event<-c("E1 (both)")
ci19<-cbind(ci19,event)
ci19<-data.frame(ci19)
names(ci19)[3]<-paste("cice17")

cie29<-coefci(ols29, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols29, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice29<-ols29$coefficients[c("treatcontrad.-both:round2")]
ci29<-cbind(cie29,cice29)
event<-c("E2 (both)")
ci29<-cbind(ci29,event)
ci29<-data.frame(ci29)
names(ci29)[3]<-paste("cice17")

cie39<-coefci(ols39, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols39, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice39<-ols39$coefficients[c("treatcontrad.-both:round2")]
ci39<-cbind(cie39,cice39)
event<-c("E3 (both)")
ci39<-cbind(ci39,event)
ci39<-data.frame(ci39)
names(ci39)[3]<-paste("cice17")

cie49<-coefci(ols49, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols49, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice49<-ols49$coefficients[c("treatcontrad.-both:round2")]
ci49<-cbind(cie49,cice49)
event<-c("E12 (both)")
ci49<-cbind(ci49,event)
ci49<-data.frame(ci49)
names(ci49)[3]<-paste("cice17")

cie59<-coefci(ols59, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols59, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice59<-ols59$coefficients[c("treatcontrad.-both:round2")]
ci59<-cbind(cie59,cice59)
event<-c("E23 (both)")
ci59<-cbind(ci59,event)
ci59<-data.frame(ci59)
names(ci59)[3]<-paste("cice17")

cie69<-coefci(ols69, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols69, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice69<-ols69$coefficients[c("treatcontrad.-both:round2")]
ci69<-cbind(cie69,cice69)
event<-c("E13 (both)")
ci69<-cbind(ci69,event)
ci69<-data.frame(ci69)
names(ci69)[3]<-paste("cice17")

ciful3<-rbind(ci19,ci29,ci39,ci49,ci59,ci69)
ciful3$X2.5..<-as.numeric(ciful3$X2.5..)
ciful3$X97.5..<-as.numeric(ciful3$X97.5..)
ciful3$cice17<-as.numeric(ciful3$cice17)
ciful3$controls<-"3.) demographic + further"

cifula<-rbind(ciful,ciful3)


# no control variables
cie17<-coefci(ols17, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols17, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice17<-ols17$coefficients[c("treatcontrad.-interval:round2")]
ci17<-cbind(cie17,cice17)
event<-c("E1 (interval)")
ci17<-cbind(ci17,event)
ci17<-data.frame(ci17)


cie27<-coefci(ols27, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols27, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice27<-ols27$coefficients[c("treatcontrad.-interval:round2")]
ci27<-cbind(cie27,cice27)
event<-c("E2 (interval)")
ci27<-cbind(ci27,event)
ci27<-data.frame(ci27)
names(ci27)[3]<-paste("cice17")

cie37<-coefci(ols37, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols37, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice37<-ols37$coefficients[c("treatcontrad.-interval:round2")]
ci37<-cbind(cie37,cice37)
event<-c("E3 (interval)")
ci37<-cbind(ci37,event)
ci37<-data.frame(ci37)
names(ci37)[3]<-paste("cice17")

cie47<-coefci(ols47, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols47, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice47<-ols47$coefficients[c("treatcontrad.-interval:round2")]
ci47<-cbind(cie47,cice47)
event<-c("E12 (interval)")
ci47<-cbind(ci47,event)
ci47<-data.frame(ci47)
names(ci47)[3]<-paste("cice17")

cie57<-coefci(ols57, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols57, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice57<-ols57$coefficients[c("treatcontrad.-interval:round2")]
ci57<-cbind(cie57,cice57)
event<-c("E23 (interval)")
ci57<-cbind(ci57,event)
ci57<-data.frame(ci57)
names(ci57)[3]<-paste("cice17")

cie67<-coefci(ols67, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols67, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice67<-ols67$coefficients[c("treatcontrad.-interval:round2")]
ci67<-cbind(cie67,cice67)
event<-c("E13 (interval)")
ci67<-cbind(ci67,event)
ci67<-data.frame(ci67)
names(ci67)[3]<-paste("cice17")

ciful<-rbind(ci17,ci27,ci37,ci47,ci57,ci67)
ciful$X2.5..<-as.numeric(ciful$X2.5..)
ciful$X97.5..<-as.numeric(ciful$X97.5..)
ciful$cice17<-as.numeric(ciful$cice17)
ciful$controls<-"1.) none"

#with demographic variables
cie18<-coefci(ols18, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols18, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice18<-ols18$coefficients[c("treatcontrad.-interval:round2")]
ci18<-cbind(cie18,cice18)
event<-c("E1 (interval)")
ci18<-cbind(ci18,event)
ci18<-data.frame(ci18)
names(ci18)[3]<-paste("cice17")

cie28<-coefci(ols28, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols28, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice28<-ols28$coefficients[c("treatcontrad.-interval:round2")]
ci28<-cbind(cie28,cice28)
event<-c("E2 (interval)")
ci28<-cbind(ci28,event)
ci28<-data.frame(ci28)
names(ci28)[3]<-paste("cice17")

cie38<-coefci(ols38, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols38, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice38<-ols38$coefficients[c("treatcontrad.-interval:round2")]
ci38<-cbind(cie38,cice38)
event<-c("E3 (interval)")
ci38<-cbind(ci38,event)
ci38<-data.frame(ci38)
names(ci38)[3]<-paste("cice17")

cie48<-coefci(ols48, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols48, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice48<-ols48$coefficients[c("treatcontrad.-interval:round2")]
ci48<-cbind(cie48,cice48)
event<-c("E12 (interval)")
ci48<-cbind(ci48,event)
ci48<-data.frame(ci48)
names(ci48)[3]<-paste("cice17")

cie58<-coefci(ols58, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols58, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice58<-ols58$coefficients[c("treatcontrad.-interval:round2")]
ci58<-cbind(cie58,cice58)
event<-c("E23 (interval)")
ci58<-cbind(ci58,event)
ci58<-data.frame(ci58)
names(ci58)[3]<-paste("cice17")

cie68<-coefci(ols68, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols68, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice68<-ols68$coefficients[c("treatcontrad.-interval:round2")]
ci68<-cbind(cie68,cice68)
event<-c("E13 (interval)")
ci68<-cbind(ci68,event)
ci68<-data.frame(ci68)
names(ci68)[3]<-paste("cice17")

ciful3<-rbind(ci18,ci28,ci38,ci48,ci58,ci68)
ciful3$X2.5..<-as.numeric(ciful3$X2.5..)
ciful3$X97.5..<-as.numeric(ciful3$X97.5..)
ciful3$cice17<-as.numeric(ciful3$cice17)
ciful3$controls<-"2.) demographic"

ciful<-rbind(ciful,ciful3)


#with demographic variables + further controls
cie19<-coefci(ols19, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols19, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice19<-ols19$coefficients[c("treatcontrad.-interval:round2")]
ci19<-cbind(cie19,cice19)
event<-c("E1 (interval)")
ci19<-cbind(ci19,event)
ci19<-data.frame(ci19)
names(ci19)[3]<-paste("cice17")

cie29<-coefci(ols29, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols29, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice29<-ols29$coefficients[c("treatcontrad.-interval:round2")]
ci29<-cbind(cie29,cice29)
event<-c("E2 (interval)")
ci29<-cbind(ci29,event)
ci29<-data.frame(ci29)
names(ci29)[3]<-paste("cice17")

cie39<-coefci(ols39, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols39, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice39<-ols39$coefficients[c("treatcontrad.-interval:round2")]
ci39<-cbind(cie39,cice39)
event<-c("E3 (interval)")
ci39<-cbind(ci39,event)
ci39<-data.frame(ci39)
names(ci39)[3]<-paste("cice17")

cie49<-coefci(ols49, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols49, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice49<-ols49$coefficients[c("treatcontrad.-interval:round2")]
ci49<-cbind(cie49,cice49)
event<-c("E12 (interval)")
ci49<-cbind(ci49,event)
ci49<-data.frame(ci49)
names(ci49)[3]<-paste("cice17")

cie59<-coefci(ols59, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols59, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice59<-ols59$coefficients[c("treatcontrad.-interval:round2")]
ci59<-cbind(cie59,cice59)
event<-c("E23 (interval)")
ci59<-cbind(ci59,event)
ci59<-data.frame(ci59)
names(ci59)[3]<-paste("cice17")

cie69<-coefci(ols69, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols69, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice69<-ols69$coefficients[c("treatcontrad.-interval:round2")]
ci69<-cbind(cie69,cice69)
event<-c("E13 (interval)")
ci69<-cbind(ci69,event)
ci69<-data.frame(ci69)
names(ci69)[3]<-paste("cice17")

ciful3<-rbind(ci19,ci29,ci39,ci49,ci59,ci69)
ciful3$X2.5..<-as.numeric(ciful3$X2.5..)
ciful3$X97.5..<-as.numeric(ciful3$X97.5..)
ciful3$cice17<-as.numeric(ciful3$cice17)
ciful3$controls<-"3.) demographic + further"

ciful<-rbind(ciful,ciful3)

ciful<-rbind(cifula,ciful)

ciful$controls<- with(ciful, factor(controls, levels=c("1.) none","2.) demographic", "3.) demographic + further")))
library(ggplot2)
ggplot(
  mapping = aes(
    y = ciful$event
  )
) +
  scale_y_discrete( limits=c("E13 (interval)","E13 (both)", "E23 (interval)","E23 (both)","E12 (interval)","E12 (both)", "E3 (interval)","E3 (both)","E2 (interval)","E2 (both)", "E1 (interval)","E1 (both)"))+
  geom_pointrange(aes(
    x = ciful$cice17,
    y = ciful$event,
    xmin = ciful$X2.5..,
    xmax = ciful$X97.5..,
    shape=factor(ciful$controls, levels=c("3.) demographic + further","2.) demographic", "1.) none"))
  ), 
  position = position_dodge(width = 0.4),
  
  # Optional decoration:
  fatten=5,
  alpha=.8
  ) +
  
  # Add a line marker for y = 0 (to see if the CI overlaps 0)
  geom_vline(xintercept = 0, color = "red", alpha = 0.2, show.legend=F) +
  # Additional decoration:
  theme_bw() +
  # guides(linetype=guide_legend(override.aes = list(shape = NA))) + 
  theme(
    plot.title = element_text(face = "bold", size = 12),
    legend.background = element_rect(fill = "white", size = 4, colour = "white"),
    #legend.justification = c(0, 1),
    axis.ticks = element_line(colour = "white", size = 0.1),
    panel.grid.major = element_line(colour = "white", size = 0.07),
    panel.grid.minor = element_blank(),
    legend.key.size = unit(1, "lines"),
    legend.box = 'horizontal',
    legend.position = "top"
  ) +
  labs(y = "Event", x = "Estimate",shape="Control variables")+
  scale_shape_manual(values = c("1.) none" = 15, "2.) demographic" = 16,"3.) demographic + further"=17))



ggsave("Events_regs_3_contradiction.pdf",height=6.5, width=6.3)


###########################################
######Graphs AA1 and AA2###################
###########################################

library("lmtest")
library("sandwich")
?coefci

#Graph 1: DiDs Contradiction in comparison to Confirmation
# no control variables
cie11<-coefci(ols1, parm=c("contradiction:round2"), vcov = vcovCL(ols1, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice11<-ols1$coefficients[c("contradiction:round2")]
ci11<-cbind(cie11,cice11)
event<-c("b")
ci11<-cbind(ci11,event)
ci11<-data.frame(ci11)


cie21<-coefci(ols1a, parm=c("contradiction:round2"), vcov = vcovCL(ols1a, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice21<-ols1a$coefficients[c("contradiction:round2")]
ci21<-cbind(cie21,cice21)
event<-c("a")
ci21<-cbind(ci21,event)
ci21<-data.frame(ci21)
names(ci21)[3]<-paste("cice11")



ciful<-rbind(ci11,ci21)
ciful$X2.5..<-as.numeric(ciful$X2.5..)
ciful$X97.5..<-as.numeric(ciful$X97.5..)
ciful$cice11<-as.numeric(ciful$cice11)
ciful$controls<-"1.) none"

#with demographic variables
cie12<-coefci(ols2, parm=c("contradiction:round2"), vcov = vcovCL(ols2, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice12<-ols2$coefficients[c("contradiction:round2")]
ci12<-cbind(cie12,cice12)
event<-c("b")
ci12<-cbind(ci12,event)
ci12<-data.frame(ci12)
names(ci12)[3]<-paste("cice11")

cie22<-coefci(ols2a, parm=c("contradiction:round2"), vcov = vcovCL(ols2a, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice22<-ols2a$coefficients[c("contradiction:round2")]
ci22<-cbind(cie22,cice22)
event<-c("a")
ci22<-cbind(ci22,event)
ci22<-data.frame(ci22)
names(ci22)[3]<-paste("cice11")


ciful2<-rbind(ci12,ci22)
ciful2$X2.5..<-as.numeric(ciful2$X2.5..)
ciful2$X97.5..<-as.numeric(ciful2$X97.5..)
ciful2$cice11<-as.numeric(ciful2$cice11)
ciful2$controls<-"2.) demographic"

ciful<-rbind(ciful,ciful2)


#with demographic variables + further controls
cie13<-coefci(ols3, parm=c("contradiction:round2"), vcov = vcovCL(ols3, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice13<-ols3$coefficients[c("contradiction:round2")]
ci13<-cbind(cie13,cice13)
event<-c("b")
ci13<-cbind(ci13,event)
ci13<-data.frame(ci13)
names(ci13)[3]<-paste("cice11")

cie23<-coefci(ols3a, parm=c("contradiction:round2"), vcov = vcovCL(ols3a, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice23<-ols3a$coefficients[c("contradiction:round2")]
ci23<-cbind(cie23,cice23)
event<-c("a")
ci23<-cbind(ci23,event)
ci23<-data.frame(ci23)
names(ci23)[3]<-paste("cice11")



ciful3<-rbind(ci13,ci23)
ciful3$X2.5..<-as.numeric(ciful3$X2.5..)
ciful3$X97.5..<-as.numeric(ciful3$X97.5..)
ciful3$cice11<-as.numeric(ciful3$cice11)
ciful3$controls<-"3.) demographic + further"

ciful<-rbind(ciful,ciful3)


ciful$controls<- with(ciful, factor(controls, levels=c("1.) none","2.) demographic", "3.) demographic + further")))
library(ggplot2)
ggplot(
  mapping = aes(
    y = ciful$event
  )
) +
  scale_y_discrete( limits=c("a", "b"))+
  geom_pointrange(aes(
    x = ciful$cice11,
    y = ciful$event,
    xmin = ciful$X2.5..,
    xmax = ciful$X97.5..,
    shape=factor(ciful$controls, levels=c("3.) demographic + further","2.) demographic", "1.) none"))
  ), 
  position = position_dodge(width = 0.4),
  
  # Optional decoration:
  fatten=5,
  alpha=.8
  ) +
  
  # Add a line marker for y = 0 (to see if the CI overlaps 0)
  geom_vline(xintercept = 0, color = "red", alpha = 0.2, show.legend=F) +
  # Additional decoration:
  theme_bw() +
  # guides(linetype=guide_legend(override.aes = list(shape = NA))) + 
  theme(
    plot.title = element_text(face = "bold", size = 12),
    legend.background = element_rect(fill = "white", size = 4, colour = "white"),
    #legend.justification = c(0, 1),
    axis.ticks = element_line(colour = "white", size = 0.1),
    panel.grid.major = element_line(colour = "white", size = 0.07),
    panel.grid.minor = element_blank(),
    legend.key.size = unit(1, "lines"),
    legend.box = 'horizontal',
    legend.position = "top"
  ) +
  labs(y = "Ambiguity Index", x = "Estimate",shape="Control variables")+
  scale_shape_manual(values = c("1.) none" = 15, "2.) demographic" = 16,"3.) demographic + further"=17))

ggsave("Ambiguity_regs_1_all.pdf",height=2, width=5.4)

###Graph 2 (Confirmation DiDs)
# no control variables
cie14<-coefci(ols4, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols4, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice14<-ols4$coefficients[c("treatconf.-both:round2")]
ci14<-cbind(cie14,cice14)
event<-c("b (both)")
ci14<-cbind(ci14,event)
ci14<-data.frame(ci14)


cie24<-coefci(ols4a, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols4a, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice24<-ols4a$coefficients[c("treatconf.-both:round2")]
ci24<-cbind(cie24,cice24)
event<-c("a (both)")
ci24<-cbind(ci24,event)
ci24<-data.frame(ci24)
names(ci24)[3]<-paste("cice14")


ciful<-rbind(ci14,ci24)
ciful$X2.5..<-as.numeric(ciful$X2.5..)
ciful$X97.5..<-as.numeric(ciful$X97.5..)
ciful$cice14<-as.numeric(ciful$cice14)
ciful$controls<-"1.) none"

#with demographic variables
cie15<-coefci(ols5, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols5, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice15<-ols5$coefficients[c("treatconf.-both:round2")]
ci15<-cbind(cie15,cice15)
event<-c("b (both)")
ci15<-cbind(ci15,event)
ci15<-data.frame(ci15)
names(ci15)[3]<-paste("cice14")

cie25<-coefci(ols5a, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols5a, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice25<-ols5a$coefficients[c("treatconf.-both:round2")]
ci25<-cbind(cie25,cice25)
event<-c("a (both)")
ci25<-cbind(ci25,event)
ci25<-data.frame(ci25)
names(ci25)[3]<-paste("cice14")



ciful2<-rbind(ci15,ci25)
ciful2$X2.5..<-as.numeric(ciful2$X2.5..)
ciful2$X97.5..<-as.numeric(ciful2$X97.5..)
ciful2$cice14<-as.numeric(ciful2$cice14)
ciful2$controls<-"2.) demographic"

ciful<-rbind(ciful,ciful2)


#with demographic variables + further controls
cie16<-coefci(ols6, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols6, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice16<-ols6$coefficients[c("treatconf.-both:round2")]
ci16<-cbind(cie16,cice16)
event<-c("b (both)")
ci16<-cbind(ci16,event)
ci16<-data.frame(ci16)
names(ci16)[3]<-paste("cice14")

cie26<-coefci(ols6a, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols6a, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice26<-ols6a$coefficients[c("treatconf.-both:round2")]
ci26<-cbind(cie26,cice26)
event<-c("a (both)")
ci26<-cbind(ci26,event)
ci26<-data.frame(ci26)
names(ci26)[3]<-paste("cice14")



ciful3<-rbind(ci16,ci26)
ciful3$X2.5..<-as.numeric(ciful3$X2.5..)
ciful3$X97.5..<-as.numeric(ciful3$X97.5..)
ciful3$cice14<-as.numeric(ciful3$cice14)
ciful3$controls<-"3.) demographic + further"

cifula<-rbind(ciful,ciful3)


# no control variables
cie14<-coefci(ols4, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols4, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice14<-ols4$coefficients[c("treatconf.-interval:round2")]
ci14<-cbind(cie14,cice14)
event<-c("b (interval)")
ci14<-cbind(ci14,event)
ci14<-data.frame(ci14)


cie24<-coefci(ols4a, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols4a, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice24<-ols4a$coefficients[c("treatconf.-interval:round2")]
ci24<-cbind(cie24,cice24)
event<-c("a (interval)")
ci24<-cbind(ci24,event)
ci24<-data.frame(ci24)
names(ci24)[3]<-paste("cice14")


ciful<-rbind(ci14,ci24)
ciful$X2.5..<-as.numeric(ciful$X2.5..)
ciful$X97.5..<-as.numeric(ciful$X97.5..)
ciful$cice14<-as.numeric(ciful$cice14)
ciful$controls<-"1.) none"

#with demographic variables
cie15<-coefci(ols5, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols5, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice15<-ols5$coefficients[c("treatconf.-interval:round2")]
ci15<-cbind(cie15,cice15)
event<-c("b (interval)")
ci15<-cbind(ci15,event)
ci15<-data.frame(ci15)
names(ci15)[3]<-paste("cice14")

cie25<-coefci(ols5a, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols5a, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice25<-ols5a$coefficients[c("treatconf.-interval:round2")]
ci25<-cbind(cie25,cice25)
event<-c("a (interval)")
ci25<-cbind(ci25,event)
ci25<-data.frame(ci25)
names(ci25)[3]<-paste("cice14")


ciful2<-rbind(ci15,ci25)
ciful2$X2.5..<-as.numeric(ciful2$X2.5..)
ciful2$X97.5..<-as.numeric(ciful2$X97.5..)
ciful2$cice14<-as.numeric(ciful2$cice14)
ciful2$controls<-"2.) demographic"

ciful<-rbind(ciful,ciful2)


#with demographic variables + further controls
cie16<-coefci(ols6, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols6, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice16<-ols6$coefficients[c("treatconf.-interval:round2")]
ci16<-cbind(cie16,cice16)
event<-c("b (interval)")
ci16<-cbind(ci16,event)
ci16<-data.frame(ci16)
names(ci16)[3]<-paste("cice14")

cie26<-coefci(ols6a, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols6a, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice26<-ols6a$coefficients[c("treatconf.-interval:round2")]
ci26<-cbind(cie26,cice26)
event<-c("a (interval)")
ci26<-cbind(ci26,event)
ci26<-data.frame(ci26)
names(ci26)[3]<-paste("cice14")



ciful3<-rbind(ci16,ci26)
ciful3$X2.5..<-as.numeric(ciful3$X2.5..)
ciful3$X97.5..<-as.numeric(ciful3$X97.5..)
ciful3$cice14<-as.numeric(ciful3$cice14)
ciful3$controls<-"3.) demographic + further"

ciful<-rbind(ciful,ciful3)

ciful<-rbind(cifula,ciful)

ciful$controls<- with(ciful, factor(controls, levels=c("1.) none","2.) demographic", "3.) demographic + further")))
library(ggplot2)
ggplot(
  mapping = aes(
    y = ciful$event
  )
) +
  scale_y_discrete( limits=c("a (interval)","a (both)", "b (interval)","b (both)"))+
  geom_pointrange(aes(
    x = ciful$cice14,
    y = ciful$event,
    xmin = ciful$X2.5..,
    xmax = ciful$X97.5..,
    shape=factor(ciful$controls, levels=c("3.) demographic + further","2.) demographic", "1.) none"))
  ), 
  position = position_dodge(width = 0.4),
  
  # Optional decoration:
  fatten=5,
  alpha=.8
  ) +
  
  # Add a line marker for y = 0 (to see if the CI overlaps 0)
  geom_vline(xintercept = 0, color = "red", alpha = 0.2, show.legend=F) +
  # Additional decoration:
  theme_bw() +
  # guides(linetype=guide_legend(override.aes = list(shape = NA))) + 
  theme(
    plot.title = element_text(face = "bold", size = 12),
    legend.background = element_rect(fill = "white", size = 4, colour = "white"),
    #legend.justification = c(0, 1),
    axis.ticks = element_line(colour = "white", size = 0.1),
    panel.grid.major = element_line(colour = "white", size = 0.07),
    panel.grid.minor = element_blank(),
    legend.key.size = unit(1, "lines"),
    legend.box = 'horizontal',
    legend.position = "top"
  ) +
  labs(y = "Ambiguity Index", x = "Estimate",shape="Control variables")+
  scale_shape_manual(values = c("1.) none" = 15, "2.) demographic" = 16,"3.) demographic + further"=17))

ggsave("Ambiguity_regs_2_confirmation.pdf",height=3, width=6.3)

####Graph 3 (Contradiction DiDs)

# no control variables
cie17<-coefci(ols7, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols7, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice17<-ols7$coefficients[c("treatcontrad.-both:round2")]
ci17<-cbind(cie17,cice17)
event<-c("b (both)")
ci17<-cbind(ci17,event)
ci17<-data.frame(ci17)


cie27<-coefci(ols7a, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols7a, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice27<-ols7a$coefficients[c("treatcontrad.-both:round2")]
ci27<-cbind(cie27,cice27)
event<-c("a (both)")
ci27<-cbind(ci27,event)
ci27<-data.frame(ci27)
names(ci27)[3]<-paste("cice17")

ciful<-rbind(ci17,ci27)
ciful$X2.5..<-as.numeric(ciful$X2.5..)
ciful$X97.5..<-as.numeric(ciful$X97.5..)
ciful$cice17<-as.numeric(ciful$cice17)
ciful$controls<-"1.) none"

#with demographic variables
cie18<-coefci(ols8, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols8, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice18<-ols8$coefficients[c("treatcontrad.-both:round2")]
ci18<-cbind(cie18,cice18)
event<-c("b (both)")
ci18<-cbind(ci18,event)
ci18<-data.frame(ci18)
names(ci18)[3]<-paste("cice17")

cie28<-coefci(ols8a, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols8a, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice28<-ols8a$coefficients[c("treatcontrad.-both:round2")]
ci28<-cbind(cie28,cice28)
event<-c("a (both)")
ci28<-cbind(ci28,event)
ci28<-data.frame(ci28)
names(ci28)[3]<-paste("cice17")



ciful3<-rbind(ci18,ci28)
ciful3$X2.5..<-as.numeric(ciful3$X2.5..)
ciful3$X97.5..<-as.numeric(ciful3$X97.5..)
ciful3$cice17<-as.numeric(ciful3$cice17)
ciful3$controls<-"2.) demographic"

ciful<-rbind(ciful,ciful3)


#with demographic variables + further controls
cie19<-coefci(ols9, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols9, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice19<-ols9$coefficients[c("treatcontrad.-both:round2")]
ci19<-cbind(cie19,cice19)
event<-c("b (both)")
ci19<-cbind(ci19,event)
ci19<-data.frame(ci19)
names(ci19)[3]<-paste("cice17")

cie29<-coefci(ols9a, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols9a, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice29<-ols9a$coefficients[c("treatcontrad.-both:round2")]
ci29<-cbind(cie29,cice29)
event<-c("a (both)")
ci29<-cbind(ci29,event)
ci29<-data.frame(ci29)
names(ci29)[3]<-paste("cice17")


ciful3<-rbind(ci19,ci29)
ciful3$X2.5..<-as.numeric(ciful3$X2.5..)
ciful3$X97.5..<-as.numeric(ciful3$X97.5..)
ciful3$cice17<-as.numeric(ciful3$cice17)
ciful3$controls<-"3.) demographic + further"

cifula<-rbind(ciful,ciful3)


# no control variables
cie17<-coefci(ols7, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols7, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice17<-ols7$coefficients[c("treatcontrad.-interval:round2")]
ci17<-cbind(cie17,cice17)
event<-c("b (interval)")
ci17<-cbind(ci17,event)
ci17<-data.frame(ci17)


cie27<-coefci(ols7a, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols7a, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice27<-ols7a$coefficients[c("treatcontrad.-interval:round2")]
ci27<-cbind(cie27,cice27)
event<-c("a (interval)")
ci27<-cbind(ci27,event)
ci27<-data.frame(ci27)
names(ci27)[3]<-paste("cice17")

ciful<-rbind(ci17,ci27)
ciful$X2.5..<-as.numeric(ciful$X2.5..)
ciful$X97.5..<-as.numeric(ciful$X97.5..)
ciful$cice17<-as.numeric(ciful$cice17)
ciful$controls<-"1.) none"

#with demographic variables
cie18<-coefci(ols8, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols8, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice18<-ols8$coefficients[c("treatcontrad.-interval:round2")]
ci18<-cbind(cie18,cice18)
event<-c("b (interval)")
ci18<-cbind(ci18,event)
ci18<-data.frame(ci18)
names(ci18)[3]<-paste("cice17")

cie28<-coefci(ols8a, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols8a, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice28<-ols8a$coefficients[c("treatcontrad.-interval:round2")]
ci28<-cbind(cie28,cice28)
event<-c("a (interval)")
ci28<-cbind(ci28,event)
ci28<-data.frame(ci28)
names(ci28)[3]<-paste("cice17")


ciful3<-rbind(ci18,ci28)
ciful3$X2.5..<-as.numeric(ciful3$X2.5..)
ciful3$X97.5..<-as.numeric(ciful3$X97.5..)
ciful3$cice17<-as.numeric(ciful3$cice17)
ciful3$controls<-"2.) demographic"

ciful<-rbind(ciful,ciful3)


#with demographic variables + further controls
cie19<-coefci(ols9, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols9, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice19<-ols9$coefficients[c("treatcontrad.-interval:round2")]
ci19<-cbind(cie19,cice19)
event<-c("b (interval)")
ci19<-cbind(ci19,event)
ci19<-data.frame(ci19)
names(ci19)[3]<-paste("cice17")

cie29<-coefci(ols9a, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols9a, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice29<-ols9a$coefficients[c("treatcontrad.-interval:round2")]
ci29<-cbind(cie29,cice29)
event<-c("a (interval)")
ci29<-cbind(ci29,event)
ci29<-data.frame(ci29)
names(ci29)[3]<-paste("cice17")



ciful3<-rbind(ci19,ci29)
ciful3$X2.5..<-as.numeric(ciful3$X2.5..)
ciful3$X97.5..<-as.numeric(ciful3$X97.5..)
ciful3$cice17<-as.numeric(ciful3$cice17)
ciful3$controls<-"3.) demographic + further"

ciful<-rbind(ciful,ciful3)

ciful<-rbind(cifula,ciful)

ciful$controls<- with(ciful, factor(controls, levels=c("1.) none","2.) demographic", "3.) demographic + further")))
library(ggplot2)
ggplot(
  mapping = aes(
    y = ciful$event
  )
) +
  scale_y_discrete( limits=c("a (interval)","a (both)", "b (interval)","b (both)"))+
  geom_pointrange(aes(
    x = ciful$cice17,
    y = ciful$event,
    xmin = ciful$X2.5..,
    xmax = ciful$X97.5..,
    shape=factor(ciful$controls, levels=c("3.) demographic + further","2.) demographic", "1.) none"))
  ), 
  position = position_dodge(width = 0.4),
  
  # Optional decoration:
  fatten=5,
  alpha=.8
  ) +
  
  # Add a line marker for y = 0 (to see if the CI overlaps 0)
  geom_vline(xintercept = 0, color = "red", alpha = 0.2, show.legend=F) +
  # Additional decoration:
  theme_bw() +
  # guides(linetype=guide_legend(override.aes = list(shape = NA))) + 
  theme(
    plot.title = element_text(face = "bold", size = 12),
    legend.background = element_rect(fill = "white", size = 4, colour = "white"),
    #legend.justification = c(0, 1),
    axis.ticks = element_line(colour = "white", size = 0.1),
    panel.grid.major = element_line(colour = "white", size = 0.07),
    panel.grid.minor = element_blank(),
    legend.key.size = unit(1, "lines"),
    legend.box = 'horizontal',
    legend.position = "top"
  ) +
  labs(y = "Ambiguity Index", x = "Estimate",shape="Control variables")+
  scale_shape_manual(values = c("1.) none" = 15, "2.) demographic" = 16,"3.) demographic + further"=17))


ggsave("Ambiguity_regs_3_contradiction.pdf",height=3, width=6.3)

#############################################################################################
##################Graphs-Interaction Effects (a and b as dependent variables)################
#############################################################################################

ful$round2<-ifelse(ful$round==2,1,0)
table(ful$round2)

#Interaction dummies for regressions

#female
table(ful$gender_female)
ful$female_con<-ifelse(ful$gender_female==1&ful$round2==1&ful$Intro.1.player.location=="Ilomantsi",1,0)
table(ful$female_con)
ful$female_both<-ifelse(ful$gender_female==1&ful$round2==1&ful$Intro.1.player.treatment=="both",1,0)
table(ful$female_both)
ful$female_int<-ifelse(ful$gender_female==1&ful$round2==1&ful$Intro.1.player.treatment=="interval",1,0)
table(ful$female_int)

#Accuracy
table(ful$median_accuracy)
ful$accu_con<-ifelse(ful$median_accuracy==1 &ful$round2==1&ful$Intro.1.player.location=="Ilomantsi",1,0)
table(ful$accu_con)
ful$accu_both<-ifelse(ful$median_accuracy==1&ful$round2==1&ful$Intro.1.player.treatment=="both",1,0)
table(ful$accu_both)
ful$accu_int<-ifelse(ful$median_accuracy==1&ful$round2==1&ful$Intro.1.player.treatment=="interval",1,0)
table(ful$accu_int)

#Credibility
table(ful$median_credibility)
ful$cred_con<-ifelse(ful$median_credibility==1 &ful$round2==1&ful$Intro.1.player.location=="Ilomantsi",1,0)
table(ful$cred_con)
ful$cred_both<-ifelse(ful$median_credibility==1&ful$round2==1&ful$Intro.1.player.treatment=="both",1,0)
table(ful$cred_both)
ful$cred_int<-ifelse(ful$median_credibility==1&ful$round2==1&ful$Intro.1.player.treatment=="interval",1,0)
table(ful$cred_int)

#Forecast Usage
table(ful$median_usage)

ful$fore_con<-ifelse(ful$median_usage==1 &ful$round2==1&ful$Intro.1.player.location=="Ilomantsi",1,0)
table(ful$fore_con)
ful$fore_both<-ifelse(ful$median_usage==1&ful$round2==1&ful$Intro.1.player.treatment=="both",1,0)
table(ful$fore_both)
ful$fore_int<-ifelse(ful$median_usage==1&ful$round2==1&ful$Intro.1.player.treatment=="interval",1,0)
table(ful$fore_int)


#Temperature
table(ful$median_temp)


ful$temp_con<-ifelse(ful$median_temp==1 &ful$round2==1&ful$Intro.1.player.location=="Ilomantsi",1,0)
table(ful$temp_con)
ful$temp_both<-ifelse(ful$median_temp==1&ful$round2==1&ful$Intro.1.player.treatment=="both",1,0)
table(ful$temp_both)
ful$temp_int<-ifelse(ful$median_temp==1&ful$round2==1&ful$Intro.1.player.treatment=="interval",1,0)
table(ful$temp_int)

#education
table(ful$education_high)
ful$study_con<-ifelse(ful$education_high==1&ful$round2==1&ful$Intro.1.player.location=="Ilomantsi",1,0)
table(ful$study_con)
ful$study_both<-ifelse(ful$education_high==1&ful$round2==1&ful$Intro.1.player.treatment=="both",1,0)
table(ful$study_both)
ful$study_int<-ifelse(ful$education_high==1&ful$round2==1&ful$Intro.1.player.treatment=="interval",1,0)
table(ful$study_int)


#Subsets
ful1<-subset(ful)
ful2<-subset(ful,ful$Intro.1.player.location=="Weiskirchen")
ful3<-subset(ful,ful$Intro.1.player.location=="Ilomantsi")

###########Regression dummys
ful1$contradiction<-ifelse(ful1$Intro.1.player.location=="Ilomantsi",1,0)
table(ful1$contradiction)




##AA1##female
#Confirmation,Contradiction
ols1<-lm(reformulate(c("contradiction*round2+female_con+gender_female*round2+gender_female*contradiction+gender_female"), response="AA1"), data=ful1)
summary(ols1)

ols2<-lm(reformulate(c("contradiction*round2+female_con+gender_female*round2+gender_female*contradiction+gender_female",list_dem), response="AA1"), data=ful1)
summary(ols2)

ols3<-lm(reformulate(c("contradiction*round2+female_con+gender_female*round2+gender_female*contradiction+gender_female",list_all), response="AA1"), data=ful1)
summary(ols3)


reformulate(c("treat*round2+female_both+female_int+gender_female*round2+gender_female*treat+gender_female"), response="AA1")
#Confirmation
ols4<-lm(reformulate(c("treat*round2+female_both+female_int+gender_female*round2+gender_female*treat+gender_female"), response="AA1"), data=ful2)
summary(ols4)

ols5<-lm(reformulate(c("treat*round2+female_both+female_int+gender_female*round2+gender_female*treat+gender_female",list_dem), response="AA1"), data=ful2)
summary(ols5)

ols6<-lm(reformulate(c("treat*round2+female_both+female_int+gender_female*round2+gender_female*treat+gender_female",list_all), response="AA1"), data=ful2)
summary(ols6)

#Contradiction
ols7<-lm(reformulate(c("treat*round2+female_both+female_int+gender_female*round2+gender_female*treat+gender_female"), response="AA1"), data=ful3)
summary(ols7)

ols8<-lm(reformulate(c("treat*round2+female_both+female_int+gender_female*round2+gender_female*treat+gender_female",list_dem), response="AA1"), data=ful3)
summary(ols8)

ols9<-lm(reformulate(c("treat*round2+female_both+female_int+gender_female*round2+gender_female*treat+gender_female",list_all), response="AA1"), data=ful3)
summary(ols9)

##AA2##female
#Confirmation,Contradiction
ols1a<-lm(reformulate(c("contradiction*round2+female_con+gender_female*round2+gender_female*contradiction+gender_female"),response="AA2"), data=ful1)
summary(ols1a)

ols2a<-lm(reformulate(c("contradiction*round2+female_con+gender_female*round2+gender_female*contradiction+gender_female",list_dem),response="AA2"), data=ful1)
summary(ols2a)

ols3a<-lm(reformulate(c("contradiction*round2+female_con+gender_female*round2+gender_female*contradiction+gender_female",list_all),response="AA2"), data=ful1)
summary(ols3a)

#Confirmation
ols4a<-lm(reformulate(c("treat*round2+female_both+female_int+gender_female*round2+gender_female*treat+gender_female"), response="AA2"), data=ful2)
summary(ols4a)

ols5a<-lm(reformulate(c("treat*round2+female_both+female_int+gender_female*round2+gender_female*treat+gender_female",list_dem), response="AA2"), data=ful2)
summary(ols5a)

ols6a<-lm(reformulate(c("treat*round2+female_both+female_int+gender_female*round2+gender_female*treat+gender_female",list_all), response="AA2"), data=ful2)
summary(ols6a)

#Contradiction
ols7a<-lm(reformulate(c("treat*round2+female_both+female_int+gender_female*round2+gender_female*treat+gender_female"), response="AA2"), data=ful3)
summary(ols7a)

ols8a<-lm(reformulate(c("treat*round2+female_both+female_int+gender_female*round2+gender_female*treat+gender_female",list_dem), response="AA2"), data=ful3)
summary(ols8a)

ols9a<-lm(reformulate(c("treat*round2+female_both+female_int+gender_female*round2+gender_female*treat+gender_female",list_all), response="AA2"), data=ful3)
summary(ols9a)




##AA1##Accuracy
#Confirmation,Contradiction
ols1c<-lm(reformulate(c("contradiction*round2+accu_con+median_accuracy*contradiction+ median_accuracy*round2+median_accuracy"), response="AA1"), data=ful1)
summary(ols1c)

ols2c<-lm(reformulate(c("contradiction*round2+accu_con+median_accuracy*contradiction+ median_accuracy*round2+median_accuracy",list_dem), response="AA1"), data=ful1)
summary(ols2c)

ols3c<-lm(reformulate(c("contradiction*round2+accu_con+median_accuracy*contradiction+ median_accuracy*round2+median_accuracy",list_all), response="AA1"), data=ful1)
summary(ols3c)

#Confirmation
ols4c<-lm(reformulate(c("treat*round2+accu_both+accu_int+median_accuracy*treat+ median_accuracy*round2+median_accuracy"), response="AA1"), data=ful2)
summary(ols4c)

ols5c<-lm(reformulate(c("treat*round2+accu_both+accu_int+median_accuracy*treat+ median_accuracy*round2+median_accuracy",list_dem), response="AA1"), data=ful2)
summary(ols5c)

ols6c<-lm(reformulate(c("treat*round2+accu_both+accu_int+median_accuracy*treat+ median_accuracy*round2+median_accuracy",list_all), response="AA1"), data=ful2)
summary(ols6c)

#Contradiction
ols7c<-lm(reformulate(c("treat*round2+accu_both+accu_int+median_accuracy*treat+ median_accuracy*round2+median_accuracy"), response="AA1"), data=ful3)
summary(ols7c)

ols8c<-lm(reformulate(c("treat*round2+accu_both+accu_int+median_accuracy*treat+ median_accuracy*round2+median_accuracy",list_dem), response="AA1"), data=ful3)
summary(ols8c)

ols9c<-lm(reformulate(c("treat*round2+accu_both+accu_int+median_accuracy*treat+ median_accuracy*round2+median_accuracy",list_all), response="AA1"), data=ful3)
summary(ols9c)

##AA2##Accuracy
#Confirmation,Contradiction
ols1d<-lm(reformulate(c("contradiction*round2+accu_con+median_accuracy*contradiction+ median_accuracy*round2+median_accuracy"), response="AA2"), data=ful1)
summary(ols1d)

ols2d<-lm(reformulate(c("contradiction*round2+accu_con+median_accuracy*contradiction+ median_accuracy*round2+median_accuracy",list_dem), response="AA2"), data=ful1)
summary(ols2d)

ols3d<-lm(reformulate(c("contradiction*round2+accu_con+median_accuracy*contradiction+ median_accuracy*round2+median_accuracy",list_all), response="AA2"), data=ful1)
summary(ols3d)

#Confirmation
ols4d<-lm(reformulate(c("treat*round2+accu_both+accu_int+median_accuracy*treat+ median_accuracy*round2+median_accuracy"), response="AA2"), data=ful2)
summary(ols4d)

ols5d<-lm(reformulate(c("treat*round2+accu_both+accu_int+median_accuracy*treat+ median_accuracy*round2+median_accuracy",list_dem), response="AA2"), data=ful2)
summary(ols5d)

ols6d<-lm(reformulate(c("treat*round2+accu_both+accu_int+median_accuracy*treat+ median_accuracy*round2+median_accuracy",list_all), response="AA2"), data=ful2)
summary(ols6d)

#Contradiction
ols7d<-lm(reformulate(c("treat*round2+accu_both+accu_int+median_accuracy*treat+ median_accuracy*round2+median_accuracy"), response="AA2"), data=ful3)
summary(ols7d)

ols8d<-lm(reformulate(c("treat*round2+accu_both+accu_int+median_accuracy*treat+ median_accuracy*round2+median_accuracy",list_dem), response="AA2"), data=ful3)
summary(ols8d)

ols9d<-lm(reformulate(c("treat*round2+accu_both+accu_int+median_accuracy*treat+ median_accuracy*round2+median_accuracy",list_all), response="AA2"), data=ful3)
summary(ols9d)





##AA1##Credibility
#Confirmation,Contradiction
ols1e<-lm(reformulate(c("contradiction*round2+cred_con+contradiction*median_credibility+round2*median_credibility+median_credibility"), response="AA1"), data=ful1)
summary(ols1e)

ols2e<-lm(reformulate(c("contradiction*round2+cred_con+contradiction*median_credibility+round2*median_credibility+median_credibility",list_dem), response="AA1"), data=ful1)
summary(ols2e)

ols3e<-lm(reformulate(c("contradiction*round2+cred_con+contradiction*median_credibility+round2*median_credibility+median_credibility",list_all), response="AA1"), data=ful1)
summary(ols3e)

#Confirmation
ols4e<-lm(reformulate(c("treat*round2+cred_both+cred_int+treat*median_credibility+round2*median_credibility+median_credibility"), response="AA1"), data=ful2)
summary(ols4e)

ols5e<-lm(reformulate(c("treat*round2+cred_both+cred_int+treat*median_credibility+round2*median_credibility+median_credibility",list_dem), response="AA1"), data=ful2)
summary(ols5e)

ols6e<-lm(reformulate(c("treat*round2+cred_both+cred_int+treat*median_credibility+round2*median_credibility+median_credibility",list_all), response="AA1"), data=ful2)
summary(ols6e)

#Contradiction
ols7e<-lm(reformulate(c("treat*round2+cred_both+cred_int+treat*median_credibility+round2*median_credibility+median_credibility"), response="AA1"), data=ful3)
summary(ols7e)

ols8e<-lm(reformulate(c("treat*round2+cred_both+cred_int+treat*median_credibility+round2*median_credibility+median_credibility",list_dem), response="AA1"), data=ful3)
summary(ols8e)

ols9e<-lm(reformulate(c("treat*round2+cred_both+cred_int+treat*median_credibility+round2*median_credibility+median_credibility",list_all), response="AA1"), data=ful3)
summary(ols9e)

##AA2##Credibility
#Confirmation,Contradiction
ols1f<-lm(reformulate(c("contradiction*round2+cred_con+contradiction*median_credibility+round2*median_credibility+median_credibility"), response="AA2"), data=ful1)
summary(ols1f)

ols2f<-lm(reformulate(c("contradiction*round2+cred_con+contradiction*median_credibility+round2*median_credibility+median_credibility",list_dem), response="AA2"), data=ful1)
summary(ols2f)

ols3f<-lm(reformulate(c("contradiction*round2+cred_con+contradiction*median_credibility+round2*median_credibility+median_credibility",list_all), response="AA2"), data=ful1)
summary(ols3f)

#Confirmation
ols4f<-lm(reformulate(c("treat*round2+cred_both+cred_int+treat*median_credibility+round2*median_credibility+median_credibility"), response="AA2"), data=ful2)
summary(ols4f)

ols5f<-lm(reformulate(c("treat*round2+cred_both+cred_int+treat*median_credibility+round2*median_credibility+median_credibility",list_dem), response="AA2"), data=ful2)
summary(ols5f)

ols6f<-lm(reformulate(c("treat*round2+cred_both+cred_int+treat*median_credibility+round2*median_credibility+median_credibility",list_all), response="AA2"), data=ful2)
summary(ols6f)

#Contradiction
ols7f<-lm(reformulate(c("treat*round2+cred_both+cred_int+treat*median_credibility+round2*median_credibility+median_credibility"), response="AA2"), data=ful3)
summary(ols7f)

ols8f<-lm(reformulate(c("treat*round2+cred_both+cred_int+treat*median_credibility+round2*median_credibility+median_credibility",list_dem), response="AA2"), data=ful3)
summary(ols8f)

ols9f<-lm(reformulate(c("treat*round2+cred_both+cred_int+treat*median_credibility+round2*median_credibility+median_credibility",list_all), response="AA2"), data=ful3)
summary(ols9f)



##AA1##Forecast usage
#Confirmation,Contradiction
ols1g<-lm(reformulate(c("contradiction*round2+fore_con+contradiction*median_usage+round2*median_usage+median_usage"), response="AA1"), data=ful1)
summary(ols1g)

ols2g<-lm(reformulate(c("contradiction*round2+fore_con+contradiction*median_usage+round2*median_usage+median_usage",list_dem), response="AA1"), data=ful1)
summary(ols2g)

ols3g<-lm(reformulate(c("contradiction*round2+fore_con+contradiction*median_usage+round2*median_usage+median_usage",list_all), response="AA1"), data=ful1)
summary(ols3g)

#Confirmation
ols4g<-lm(reformulate(c("treat*round2+fore_both+fore_int+treat*median_usage+round2*median_usage+median_usage"), response="AA1"), data=ful2)
summary(ols4g)

ols5g<-lm(reformulate(c("treat*round2+fore_both+fore_int+treat*median_usage+round2*median_usage+median_usage",list_dem), response="AA1"), data=ful2)
summary(ols5g)

ols6g<-lm(reformulate(c("treat*round2+fore_both+fore_int+treat*median_usage+round2*median_usage+median_usage",list_all), response="AA1"), data=ful2)
summary(ols6g)

#Contradiction
ols7g<-lm(reformulate(c("treat*round2+fore_both+fore_int+treat*median_usage+round2*median_usage+median_usage"), response="AA1"), data=ful3)
summary(ols7g)

ols8g<-lm(reformulate(c("treat*round2+fore_both+fore_int+treat*median_usage+round2*median_usage+median_usage",list_dem), response="AA1"), data=ful3)
summary(ols8g)

ols9g<-lm(reformulate(c("treat*round2+fore_both+fore_int+treat*median_usage+round2*median_usage+median_usage",list_all), response="AA1"), data=ful3)
summary(ols9g)

##AA2##Forecast Usage
#Confirmation,Contradiction
ols1h<-lm(reformulate(c("contradiction*round2+fore_con+contradiction*median_usage+round2*median_usage+median_usage"), response="AA2"), data=ful1)
summary(ols1h)

ols2h<-lm(reformulate(c("contradiction*round2+fore_con+contradiction*median_usage+round2*median_usage+median_usage",list_dem), response="AA2"), data=ful1)
summary(ols2h)

ols3h<-lm(reformulate(c("contradiction*round2+fore_con+contradiction*median_usage+round2*median_usage+median_usage",list_all), response="AA2"), data=ful1)
summary(ols3h)

#Confirmation
ols4h<-lm(reformulate(c("treat*round2+fore_both+fore_int+treat*median_usage+round2*median_usage+median_usage"), response="AA2"), data=ful2)
summary(ols4h)

ols5h<-lm(reformulate(c("treat*round2+fore_both+fore_int+treat*median_usage+round2*median_usage+median_usage",list_dem), response="AA2"), data=ful2)
summary(ols5h)

ols6h<-lm(reformulate(c("treat*round2+fore_both+fore_int+treat*median_usage+round2*median_usage+median_usage",list_all), response="AA2"), data=ful2)
summary(ols6h)

#Contradiction
ols7h<-lm(reformulate(c("treat*round2+fore_both+fore_int+treat*median_usage+round2*median_usage+median_usage"), response="AA2"), data=ful3)
summary(ols7h)

ols8h<-lm(reformulate(c("treat*round2+fore_both+fore_int+treat*median_usage+round2*median_usage+median_usage",list_dem), response="AA2"), data=ful3)
summary(ols8h)

ols9h<-lm(reformulate(c("treat*round2+fore_both+fore_int+treat*median_usage+round2*median_usage+median_usage",list_all), response="AA2"), data=ful3)
summary(ols9h)




##AA1##Temperature
#Confirmation,Contradiction
ols1i<-lm(reformulate(c("contradiction*round2+temp_con+contradiction*median_temp+round2*median_temp+median_temp"), response="AA1"), data=ful1)
summary(ols1i)

ols2i<-lm(reformulate(c("contradiction*round2+temp_con+contradiction*median_temp+round2*median_temp+median_temp",list_dem), response="AA1"), data=ful1)
summary(ols2i)

ols3i<-lm(reformulate(c("contradiction*round2+temp_con+contradiction*median_temp+round2*median_temp+median_temp",list_all), response="AA1"), data=ful1)
summary(ols3i)

#Confirmation
ols4i<-lm(reformulate(c("treat*round2+temp_both+temp_int+treat*median_temp+round2*median_temp+median_temp"), response="AA1"), data=ful2)
summary(ols4i)

ols5i<-lm(reformulate(c("treat*round2+temp_both+temp_int+treat*median_temp+round2*median_temp+median_temp",list_dem), response="AA1"), data=ful2)
summary(ols5i)

ols6i<-lm(reformulate(c("treat*round2+temp_both+temp_int+treat*median_temp+round2*median_temp+median_temp",list_all), response="AA1"), data=ful2)
summary(ols6i)

#Contradiction
ols7i<-lm(reformulate(c("treat*round2+temp_both+temp_int+treat*median_temp+round2*median_temp+median_temp"), response="AA1"), data=ful3)
summary(ols7i)

ols8i<-lm(reformulate(c("treat*round2+temp_both+temp_int+treat*median_temp+round2*median_temp+median_temp",list_dem), response="AA1"), data=ful3)
summary(ols8i)

ols9i<-lm(reformulate(c("treat*round2+temp_both+temp_int+treat*median_temp+round2*median_temp+median_temp",list_all), response="AA1"), data=ful3)
summary(ols9i)

##AA2##Temperature
#Confirmation,Contradiction
ols1j<-lm(reformulate(c("contradiction*round2+temp_con+contradiction*median_temp+round2*median_temp+median_temp"), response="AA2"), data=ful1)
summary(ols1j)

ols2j<-lm(reformulate(c("contradiction*round2+temp_con+contradiction*median_temp+round2*median_temp+median_temp",list_dem), response="AA2"), data=ful1)
summary(ols2j)

ols3j<-lm(reformulate(c("contradiction*round2+temp_con+contradiction*median_temp+round2*median_temp+median_temp",list_all), response="AA2"), data=ful1)
summary(ols3j)

#Confirmation
ols4j<-lm(reformulate(c("treat*round2+temp_both+temp_int+treat*median_temp+round2*median_temp+median_temp"), response="AA2"), data=ful2)
summary(ols4j)

ols5j<-lm(reformulate(c("treat*round2+temp_both+temp_int+treat*median_temp+round2*median_temp+median_temp",list_dem), response="AA2"), data=ful2)
summary(ols5j)

ols6j<-lm(reformulate(c("treat*round2+temp_both+temp_int+treat*median_temp+round2*median_temp+median_temp",list_all), response="AA2"), data=ful2)
summary(ols6j)

#Contradiction
ols7j<-lm(reformulate(c("treat*round2+temp_both+temp_int+treat*median_temp+round2*median_temp+median_temp"), response="AA2"), data=ful3)
summary(ols7j)

ols8j<-lm(reformulate(c("treat*round2+temp_both+temp_int+treat*median_temp+round2*median_temp+median_temp",list_dem), response="AA2"), data=ful3)
summary(ols8j)

ols9j<-lm(reformulate(c("treat*round2+temp_both+temp_int+treat*median_temp+round2*median_temp+median_temp",list_all), response="AA2"), data=ful3)
summary(ols9j)



##AA1##high education
#Confirmation,Contradiction
ols1k<-lm(reformulate(c("contradiction*round2+study_con+education_high*round2+education_high*contradiction+education_high"), response="AA1"), data=ful1)
summary(ols1k)

ols2k<-lm(reformulate(c("contradiction*round2+study_con+education_high*round2+education_high*contradiction+education_high",list_dem), response="AA1"), data=ful1)
summary(ols2k)

ols3k<-lm(reformulate(c("contradiction*round2+study_con+education_high*round2+education_high*contradiction+education_high",list_all), response="AA1"), data=ful1)
summary(ols3k)

#Confirmation
ols4k<-lm(reformulate(c("treat*round2+study_both+study_int+education_high*round2+education_high*treat+education_high"), response="AA1"), data=ful2)
summary(ols4k)

ols5k<-lm(reformulate(c("treat*round2+study_both+study_int+education_high*round2+education_high*treat+education_high",list_dem), response="AA1"), data=ful2)
summary(ols5k)

ols6k<-lm(reformulate(c("treat*round2+study_both+study_int+education_high*round2+education_high*treat+education_high",list_all), response="AA1"), data=ful2)
summary(ols6k)

#Contradiction
ols7k<-lm(reformulate(c("treat*round2+study_both+study_int+education_high*round2+education_high*treat+education_high"), response="AA1"), data=ful3)
summary(ols7k)

ols8k<-lm(reformulate(c("treat*round2+study_both+study_int+education_high*round2+education_high*treat+education_high",list_dem), response="AA1"), data=ful3)
summary(ols8k)

ols9k<-lm(reformulate(c("treat*round2+study_both+study_int+education_high*round2+education_high*treat+education_high",list_all), response="AA1"), data=ful3)
summary(ols9k)

##AA2##study
#Confirmation,Contradiction
ols1l<-lm(reformulate(c("contradiction*round2+study_con+education_high*round2+education_high*contradiction+education_high"), response="AA2"), data=ful1)
summary(ols1l)

ols2l<-lm(reformulate(c("contradiction*round2+study_con+education_high*round2+education_high*contradiction+education_high",list_dem), response="AA2"), data=ful1)
summary(ols2l)

ols3l<-lm(reformulate(c("contradiction*round2+study_con+education_high*round2+education_high*contradiction+education_high",list_all), response="AA2"), data=ful1)
summary(ols3l)

#Confirmation
ols4l<-lm(reformulate(c("treat*round2+study_both+study_int+education_high*round2+education_high*treat+education_high"), response="AA2"), data=ful2)
summary(ols4l)

ols5l<-lm(reformulate(c("treat*round2+study_both+study_int+education_high*round2+education_high*treat+education_high",list_dem), response="AA2"), data=ful2)
summary(ols5l)

ols6l<-lm(reformulate(c("treat*round2+study_both+study_int+education_high*round2+education_high*treat+education_high",list_all), response="AA2"), data=ful2)
summary(ols6l)

#Contradiction
ols7l<-lm(reformulate(c("treat*round2+study_both+study_int+education_high*round2+education_high*treat+education_high"), response="AA2"), data=ful3)
summary(ols7l)

ols8l<-lm(reformulate(c("treat*round2+study_both+study_int+education_high*round2+education_high*treat+education_high",list_dem), response="AA2"), data=ful3)
summary(ols8l)

ols9l<-lm(reformulate(c("treat*round2+study_both+study_int+education_high*round2+education_high*treat+education_high",list_all), response="AA2"), data=ful3)
summary(ols9l)


###########################################
######Graphs AA1 and AA2 (heterogeneous effects)###################
###########################################

library("lmtest")
library("sandwich")
?coefci

#Graph 1: DiDs Contradiction in comparison to Confirmation
# no control variables
cie11<-coefci(ols1, parm=c("contradiction:round2"), vcov = vcovCL(ols1, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice11<-ols1$coefficients[c("contradiction:round2")]
ci11<-cbind(cie11,cice11)
event<-c("b (not female)")
ci11<-cbind(ci11,event)
ci11<-data.frame(ci11)

cie31<-coefci(ols1, parm=c("female_con"), vcov = vcovCL(ols1, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice31<-ols1$coefficients[c("female_con")]
ci31<-cbind(cie31,cice31)
event<-c("b (female DDD)")
ci31<-cbind(ci31,event)
ci31<-data.frame(ci31)
names(ci31)[3]<-paste("cice11")

cie21<-coefci(ols1a, parm=c("contradiction:round2"), vcov = vcovCL(ols1a, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice21<-ols1a$coefficients[c("contradiction:round2")]
ci21<-cbind(cie21,cice21)
event<-c("a (not female)")
ci21<-cbind(ci21,event)
ci21<-data.frame(ci21)
names(ci21)[3]<-paste("cice11")

cie41<-coefci(ols1a, parm=c("female_con"), vcov = vcovCL(ols1a, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice41<-ols1a$coefficients[c("female_con")]
ci41<-cbind(cie41,cice41)
event<-c("a (female DDD)")
ci41<-cbind(ci41,event)
ci41<-data.frame(ci41)
names(ci41)[3]<-paste("cice11")


cie51<-coefci(ols1c, parm=c("contradiction:round2"), vcov = vcovCL(ols1c, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice51<-ols1c$coefficients[c("contradiction:round2")]
ci51<-cbind(cie51,cice51)
event<-c("b (less accurate)")
ci51<-cbind(ci51,event)
ci51<-data.frame(ci51)
names(ci51)[3]<-paste("cice11")

cie61<-coefci(ols1c, parm=c("accu_con"), vcov = vcovCL(ols1c, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice61<-ols1c$coefficients[c("accu_con")]
ci61<-cbind(cie61,cice61)
event<-c("b (accurate DDD)")
ci61<-cbind(ci61,event)
ci61<-data.frame(ci61)
names(ci61)[3]<-paste("cice11")


cie71<-coefci(ols1d, parm=c("contradiction:round2"), vcov = vcovCL(ols1d, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice71<-ols1d$coefficients[c("contradiction:round2")]
ci71<-cbind(cie71,cice71)
event<-c("a (less accurate)")
ci71<-cbind(ci71,event)
ci71<-data.frame(ci71)
names(ci71)[3]<-paste("cice11")

cie81<-coefci(ols1d, parm=c("accu_con"), vcov = vcovCL(ols1d, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice81<-ols1d$coefficients[c("accu_con")]
ci81<-cbind(cie81,cice81)
event<-c("a (accurate DDD)")
ci81<-cbind(ci81,event)
ci81<-data.frame(ci81)
names(ci81)[3]<-paste("cice11")

cie91<-coefci(ols1e, parm=c("contradiction:round2"), vcov = vcovCL(ols1e, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice91<-ols1e$coefficients[c("contradiction:round2")]
ci91<-cbind(cie91,cice91)
event<-c("b (less credible)")
ci91<-cbind(ci91,event)
ci91<-data.frame(ci91)
names(ci91)[3]<-paste("cice11")

cie101<-coefci(ols1e, parm=c("cred_con"), vcov = vcovCL(ols1e, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice101<-ols1e$coefficients[c("cred_con")]
ci101<-cbind(cie101,cice101)
event<-c("b (credible DDD)")
ci101<-cbind(ci101,event)
ci101<-data.frame(ci101)
names(ci101)[3]<-paste("cice11")

cie111<-coefci(ols1f, parm=c("contradiction:round2"), vcov = vcovCL(ols1f, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice111<-ols1f$coefficients[c("contradiction:round2")]
ci111<-cbind(cie111,cice111)
event<-c("a (less credible)")
ci111<-cbind(ci111,event)
ci111<-data.frame(ci111)
names(ci111)[3]<-paste("cice11")

cie121<-coefci(ols1f, parm=c("cred_con"), vcov = vcovCL(ols1f, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice121<-ols1f$coefficients[c("cred_con")]
ci121<-cbind(cie121,cice121)
event<-c("a (credible DDD)")
ci121<-cbind(ci121,event)
ci121<-data.frame(ci121)
names(ci121)[3]<-paste("cice11")

cie131<-coefci(ols1g, parm=c("contradiction:round2"), vcov = vcovCL(ols1g, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice131<-ols1g$coefficients[c("contradiction:round2")]
ci131<-cbind(cie131,cice131)
event<-c("b (less forecast usage)")
ci131<-cbind(ci131,event)
ci131<-data.frame(ci131)
names(ci131)[3]<-paste("cice11")

cie141<-coefci(ols1g, parm=c("fore_con"), vcov = vcovCL(ols1g, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice141<-ols1g$coefficients[c("fore_con")]
ci141<-cbind(cie141,cice141)
event<-c("b (forecast usage DDD)")
ci141<-cbind(ci141,event)
ci141<-data.frame(ci141)
names(ci141)[3]<-paste("cice11")

cie151<-coefci(ols1h, parm=c("contradiction:round2"), vcov = vcovCL(ols1h, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice151<-ols1h$coefficients[c("contradiction:round2")]
ci151<-cbind(cie151,cice151)
event<-c("a (less forecast usage)")
ci151<-cbind(ci151,event)
ci151<-data.frame(ci151)
names(ci151)[3]<-paste("cice11")

cie161<-coefci(ols1h, parm=c("fore_con"), vcov = vcovCL(ols1h, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice161<-ols1h$coefficients[c("fore_con")]
ci161<-cbind(cie161,cice161)
event<-c("a (forecast usage DDD)")
ci161<-cbind(ci161,event)
ci161<-data.frame(ci161)
names(ci161)[3]<-paste("cice11")

cie171<-coefci(ols1i, parm=c("contradiction:round2"), vcov = vcovCL(ols1i, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice171<-ols1i$coefficients[c("contradiction:round2")]
ci171<-cbind(cie171,cice171)
event<-c("b (lower temperature)")
ci171<-cbind(ci171,event)
ci171<-data.frame(ci171)
names(ci171)[3]<-paste("cice11")

cie181<-coefci(ols1i, parm=c("temp_con"), vcov = vcovCL(ols1i, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice181<-ols1i$coefficients[c("temp_con")]
ci181<-cbind(cie181,cice181)
event<-c("b (temperature DDD)")
ci181<-cbind(ci181,event)
ci181<-data.frame(ci181)
names(ci181)[3]<-paste("cice11")

cie191<-coefci(ols1j, parm=c("contradiction:round2"), vcov = vcovCL(ols1j, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice191<-ols1j$coefficients[c("contradiction:round2")]
ci191<-cbind(cie191,cice191)
event<-c("a (lower temperature)")
ci191<-cbind(ci191,event)
ci191<-data.frame(ci191)
names(ci191)[3]<-paste("cice11")

cie201<-coefci(ols1j, parm=c("temp_con"), vcov = vcovCL(ols1j, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice201<-ols1j$coefficients[c("temp_con")]
ci201<-cbind(cie201,cice201)
event<-c("a (temperature DDD)")
ci201<-cbind(ci201,event)
ci201<-data.frame(ci201)
names(ci201)[3]<-paste("cice11")


cie211<-coefci(ols1k, parm=c("contradiction:round2"), vcov = vcovCL(ols1k, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice211<-ols1k$coefficients[c("contradiction:round2")]
ci211<-cbind(cie211,cice211)
event<-c("b (lower education)")
ci211<-cbind(ci211,event)
ci211<-data.frame(ci211)
names(ci211)[3]<-paste("cice11")

cie221<-coefci(ols1k, parm=c("study_con"), vcov = vcovCL(ols1k, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice221<-ols1k$coefficients[c("study_con")]
ci221<-cbind(cie221,cice221)
event<-c("b (education DDD)")
ci221<-cbind(ci221,event)
ci221<-data.frame(ci221)
names(ci221)[3]<-paste("cice11")

cie231<-coefci(ols1l, parm=c("contradiction:round2"), vcov = vcovCL(ols1l, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice231<-ols1l$coefficients[c("contradiction:round2")]
ci231<-cbind(cie231,cice231)
event<-c("a (lower education)")
ci231<-cbind(ci231,event)
ci231<-data.frame(ci231)
names(ci231)[3]<-paste("cice11")

cie241<-coefci(ols1l, parm=c("study_con"), vcov = vcovCL(ols1l, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice241<-ols1l$coefficients[c("study_con")]
ci241<-cbind(cie241,cice241)
event<-c("a (education DDD)")
ci241<-cbind(ci241,event)
ci241<-data.frame(ci241)
names(ci241)[3]<-paste("cice11")

ciful<-rbind(ci11,ci31,ci21,ci41,ci51,ci61,ci71,ci81,ci91,ci101,ci111,ci121,ci131,ci141,ci151,ci161,ci171,ci181,ci191,ci201,ci211,ci221,ci231,ci241)
ciful$X2.5..<-as.numeric(ciful$X2.5..)
ciful$X97.5..<-as.numeric(ciful$X97.5..)
ciful$cice11<-as.numeric(ciful$cice11)
ciful$controls<-"1.) none"

#with demographic variables
cie11<-coefci(ols2, parm=c("contradiction:round2"), vcov = vcovCL(ols2, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice11<-ols2$coefficients[c("contradiction:round2")]
ci11<-cbind(cie11,cice11)
event<-c("b (not female)")
ci11<-cbind(ci11,event)
ci11<-data.frame(ci11)

cie31<-coefci(ols2, parm=c("female_con"), vcov = vcovCL(ols2, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice31<-ols2$coefficients[c("female_con")]
ci31<-cbind(cie31,cice31)
event<-c("b (female DDD)")
ci31<-cbind(ci31,event)
ci31<-data.frame(ci31)
names(ci31)[3]<-paste("cice11")

cie21<-coefci(ols2a, parm=c("contradiction:round2"), vcov = vcovCL(ols2a, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice21<-ols2a$coefficients[c("contradiction:round2")]
ci21<-cbind(cie21,cice21)
event<-c("a (not female)")
ci21<-cbind(ci21,event)
ci21<-data.frame(ci21)
names(ci21)[3]<-paste("cice11")

cie41<-coefci(ols2a, parm=c("female_con"), vcov = vcovCL(ols2a, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice41<-ols2a$coefficients[c("female_con")]
ci41<-cbind(cie41,cice41)
event<-c("a (female DDD)")
ci41<-cbind(ci41,event)
ci41<-data.frame(ci41)
names(ci41)[3]<-paste("cice11")


cie51<-coefci(ols2c, parm=c("contradiction:round2"), vcov = vcovCL(ols2c, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice51<-ols2c$coefficients[c("contradiction:round2")]
ci51<-cbind(cie51,cice51)
event<-c("b (less accurate)")
ci51<-cbind(ci51,event)
ci51<-data.frame(ci51)
names(ci51)[3]<-paste("cice11")

cie61<-coefci(ols2c, parm=c("accu_con"), vcov = vcovCL(ols2c, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice61<-ols2c$coefficients[c("accu_con")]
ci61<-cbind(cie61,cice61)
event<-c("b (accurate DDD)")
ci61<-cbind(ci61,event)
ci61<-data.frame(ci61)
names(ci61)[3]<-paste("cice11")


cie71<-coefci(ols2d, parm=c("contradiction:round2"), vcov = vcovCL(ols2d, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice71<-ols2d$coefficients[c("contradiction:round2")]
ci71<-cbind(cie71,cice71)
event<-c("a (less accurate)")
ci71<-cbind(ci71,event)
ci71<-data.frame(ci71)
names(ci71)[3]<-paste("cice11")

cie81<-coefci(ols2d, parm=c("accu_con"), vcov = vcovCL(ols2d, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice81<-ols2d$coefficients[c("accu_con")]
ci81<-cbind(cie81,cice81)
event<-c("a (accurate DDD)")
ci81<-cbind(ci81,event)
ci81<-data.frame(ci81)
names(ci81)[3]<-paste("cice11")

cie91<-coefci(ols2e, parm=c("contradiction:round2"), vcov = vcovCL(ols2e, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice91<-ols2e$coefficients[c("contradiction:round2")]
ci91<-cbind(cie91,cice91)
event<-c("b (less credible)")
ci91<-cbind(ci91,event)
ci91<-data.frame(ci91)
names(ci91)[3]<-paste("cice11")

cie101<-coefci(ols2e, parm=c("cred_con"), vcov = vcovCL(ols2e, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice101<-ols2e$coefficients[c("cred_con")]
ci101<-cbind(cie101,cice101)
event<-c("b (credible DDD)")
ci101<-cbind(ci101,event)
ci101<-data.frame(ci101)
names(ci101)[3]<-paste("cice11")

cie111<-coefci(ols2f, parm=c("contradiction:round2"), vcov = vcovCL(ols2f, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice111<-ols2f$coefficients[c("contradiction:round2")]
ci111<-cbind(cie111,cice111)
event<-c("a (less credible)")
ci111<-cbind(ci111,event)
ci111<-data.frame(ci111)
names(ci111)[3]<-paste("cice11")

cie121<-coefci(ols2f, parm=c("cred_con"), vcov = vcovCL(ols2f, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice121<-ols2f$coefficients[c("cred_con")]
ci121<-cbind(cie121,cice121)
event<-c("a (credible DDD)")
ci121<-cbind(ci121,event)
ci121<-data.frame(ci121)
names(ci121)[3]<-paste("cice11")

cie131<-coefci(ols2g, parm=c("contradiction:round2"), vcov = vcovCL(ols2g, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice131<-ols2g$coefficients[c("contradiction:round2")]
ci131<-cbind(cie131,cice131)
event<-c("b (less forecast usage)")
ci131<-cbind(ci131,event)
ci131<-data.frame(ci131)
names(ci131)[3]<-paste("cice11")

cie141<-coefci(ols2g, parm=c("fore_con"), vcov = vcovCL(ols2g, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice141<-ols2g$coefficients[c("fore_con")]
ci141<-cbind(cie141,cice141)
event<-c("b (forecast usage DDD)")
ci141<-cbind(ci141,event)
ci141<-data.frame(ci141)
names(ci141)[3]<-paste("cice11")

cie151<-coefci(ols2h, parm=c("contradiction:round2"), vcov = vcovCL(ols2h, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice151<-ols2h$coefficients[c("contradiction:round2")]
ci151<-cbind(cie151,cice151)
event<-c("a (less forecast usage)")
ci151<-cbind(ci151,event)
ci151<-data.frame(ci151)
names(ci151)[3]<-paste("cice11")

cie161<-coefci(ols2h, parm=c("fore_con"), vcov = vcovCL(ols2h, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice161<-ols2h$coefficients[c("fore_con")]
ci161<-cbind(cie161,cice161)
event<-c("a (forecast usage DDD)")
ci161<-cbind(ci161,event)
ci161<-data.frame(ci161)
names(ci161)[3]<-paste("cice11")

cie171<-coefci(ols2i, parm=c("contradiction:round2"), vcov = vcovCL(ols2i, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice171<-ols2i$coefficients[c("contradiction:round2")]
ci171<-cbind(cie171,cice171)
event<-c("b (lower temperature)")
ci171<-cbind(ci171,event)
ci171<-data.frame(ci171)
names(ci171)[3]<-paste("cice11")

cie181<-coefci(ols2i, parm=c("temp_con"), vcov = vcovCL(ols2i, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice181<-ols2i$coefficients[c("temp_con")]
ci181<-cbind(cie181,cice181)
event<-c("b (temperature DDD)")
ci181<-cbind(ci181,event)
ci181<-data.frame(ci181)
names(ci181)[3]<-paste("cice11")

cie191<-coefci(ols2j, parm=c("contradiction:round2"), vcov = vcovCL(ols2j, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice191<-ols2j$coefficients[c("contradiction:round2")]
ci191<-cbind(cie191,cice191)
event<-c("a (lower temperature)")
ci191<-cbind(ci191,event)
ci191<-data.frame(ci191)
names(ci191)[3]<-paste("cice11")

cie201<-coefci(ols2j, parm=c("temp_con"), vcov = vcovCL(ols2j, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice201<-ols2j$coefficients[c("temp_con")]
ci201<-cbind(cie201,cice201)
event<-c("a (temperature DDD)")
ci201<-cbind(ci201,event)
ci201<-data.frame(ci201)
names(ci201)[3]<-paste("cice11")

cie211<-coefci(ols2k, parm=c("contradiction:round2"), vcov = vcovCL(ols2k, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice211<-ols2k$coefficients[c("contradiction:round2")]
ci211<-cbind(cie211,cice211)
event<-c("b (lower education)")
ci211<-cbind(ci211,event)
ci211<-data.frame(ci211)
names(ci211)[3]<-paste("cice11")

cie221<-coefci(ols2k, parm=c("study_con"), vcov = vcovCL(ols2k, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice221<-ols2k$coefficients[c("study_con")]
ci221<-cbind(cie221,cice221)
event<-c("b (education DDD)")
ci221<-cbind(ci221,event)
ci221<-data.frame(ci221)
names(ci221)[3]<-paste("cice11")

cie231<-coefci(ols2l, parm=c("contradiction:round2"), vcov = vcovCL(ols2l, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice231<-ols2l$coefficients[c("contradiction:round2")]
ci231<-cbind(cie231,cice231)
event<-c("a (lower education)")
ci231<-cbind(ci231,event)
ci231<-data.frame(ci231)
names(ci231)[3]<-paste("cice11")

cie241<-coefci(ols2l, parm=c("study_con"), vcov = vcovCL(ols2l, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice241<-ols2l$coefficients[c("study_con")]
ci241<-cbind(cie241,cice241)
event<-c("a (education DDD)")
ci241<-cbind(ci241,event)
ci241<-data.frame(ci241)
names(ci241)[3]<-paste("cice11")


ciful2<-rbind(ci11,ci31,ci21,ci41,ci51,ci61,ci71,ci81,ci91,ci101,ci111,ci121,ci131,ci141,ci151,ci161,ci171,ci181,ci191,ci201,ci211,ci221,ci231,ci241)
ciful2$X2.5..<-as.numeric(ciful2$X2.5..)
ciful2$X97.5..<-as.numeric(ciful2$X97.5..)
ciful2$cice11<-as.numeric(ciful2$cice11)
ciful2$controls<-"2.) demographic"

ciful<-rbind(ciful,ciful2)


#with demographic variables + further controls
cie11<-coefci(ols3, parm=c("contradiction:round2"), vcov = vcovCL(ols3, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice11<-ols3$coefficients[c("contradiction:round2")]
ci11<-cbind(cie11,cice11)
event<-c("b (not female)")
ci11<-cbind(ci11,event)
ci11<-data.frame(ci11)

cie31<-coefci(ols3, parm=c("female_con"), vcov = vcovCL(ols3, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice31<-ols3$coefficients[c("female_con")]
ci31<-cbind(cie31,cice31)
event<-c("b (female DDD)")
ci31<-cbind(ci31,event)
ci31<-data.frame(ci31)
names(ci31)[3]<-paste("cice11")

cie21<-coefci(ols3a, parm=c("contradiction:round2"), vcov = vcovCL(ols3a, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice21<-ols3a$coefficients[c("contradiction:round2")]
ci21<-cbind(cie21,cice21)
event<-c("a (not female)")
ci21<-cbind(ci21,event)
ci21<-data.frame(ci21)
names(ci21)[3]<-paste("cice11")

cie41<-coefci(ols3a, parm=c("female_con"), vcov = vcovCL(ols3a, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice41<-ols3a$coefficients[c("female_con")]
ci41<-cbind(cie41,cice41)
event<-c("a (female DDD)")
ci41<-cbind(ci41,event)
ci41<-data.frame(ci41)
names(ci41)[3]<-paste("cice11")


cie51<-coefci(ols3c, parm=c("contradiction:round2"), vcov = vcovCL(ols3c, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice51<-ols3c$coefficients[c("contradiction:round2")]
ci51<-cbind(cie51,cice51)
event<-c("b (less accurate)")
ci51<-cbind(ci51,event)
ci51<-data.frame(ci51)
names(ci51)[3]<-paste("cice11")

cie61<-coefci(ols3c, parm=c("accu_con"), vcov = vcovCL(ols3c, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice61<-ols3c$coefficients[c("accu_con")]
ci61<-cbind(cie61,cice61)
event<-c("b (accurate DDD)")
ci61<-cbind(ci61,event)
ci61<-data.frame(ci61)
names(ci61)[3]<-paste("cice11")


cie71<-coefci(ols3d, parm=c("contradiction:round2"), vcov = vcovCL(ols3d, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice71<-ols3d$coefficients[c("contradiction:round2")]
ci71<-cbind(cie71,cice71)
event<-c("a (less accurate)")
ci71<-cbind(ci71,event)
ci71<-data.frame(ci71)
names(ci71)[3]<-paste("cice11")

cie81<-coefci(ols3d, parm=c("accu_con"), vcov = vcovCL(ols3d, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice81<-ols3d$coefficients[c("accu_con")]
ci81<-cbind(cie81,cice81)
event<-c("a (accurate DDD)")
ci81<-cbind(ci81,event)
ci81<-data.frame(ci81)
names(ci81)[3]<-paste("cice11")

cie91<-coefci(ols3e, parm=c("contradiction:round2"), vcov = vcovCL(ols3e, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice91<-ols3e$coefficients[c("contradiction:round2")]
ci91<-cbind(cie91,cice91)
event<-c("b (less credible)")
ci91<-cbind(ci91,event)
ci91<-data.frame(ci91)
names(ci91)[3]<-paste("cice11")

cie101<-coefci(ols3e, parm=c("cred_con"), vcov = vcovCL(ols3e, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice101<-ols3e$coefficients[c("cred_con")]
ci101<-cbind(cie101,cice101)
event<-c("b (credible DDD)")
ci101<-cbind(ci101,event)
ci101<-data.frame(ci101)
names(ci101)[3]<-paste("cice11")

cie111<-coefci(ols3f, parm=c("contradiction:round2"), vcov = vcovCL(ols3f, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice111<-ols3f$coefficients[c("contradiction:round2")]
ci111<-cbind(cie111,cice111)
event<-c("a (less credible)")
ci111<-cbind(ci111,event)
ci111<-data.frame(ci111)
names(ci111)[3]<-paste("cice11")

cie121<-coefci(ols3f, parm=c("cred_con"), vcov = vcovCL(ols3f, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice121<-ols3f$coefficients[c("cred_con")]
ci121<-cbind(cie121,cice121)
event<-c("a (credible DDD)")
ci121<-cbind(ci121,event)
ci121<-data.frame(ci121)
names(ci121)[3]<-paste("cice11")

cie131<-coefci(ols3g, parm=c("contradiction:round2"), vcov = vcovCL(ols3g, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice131<-ols3g$coefficients[c("contradiction:round2")]
ci131<-cbind(cie131,cice131)
event<-c("b (less forecast usage)")
ci131<-cbind(ci131,event)
ci131<-data.frame(ci131)
names(ci131)[3]<-paste("cice11")

cie141<-coefci(ols3g, parm=c("fore_con"), vcov = vcovCL(ols3g, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice141<-ols3g$coefficients[c("fore_con")]
ci141<-cbind(cie141,cice141)
event<-c("b (forecast usage DDD)")
ci141<-cbind(ci141,event)
ci141<-data.frame(ci141)
names(ci141)[3]<-paste("cice11")

cie151<-coefci(ols3h, parm=c("contradiction:round2"), vcov = vcovCL(ols3h, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice151<-ols3h$coefficients[c("contradiction:round2")]
ci151<-cbind(cie151,cice151)
event<-c("a (less forecast usage)")
ci151<-cbind(ci151,event)
ci151<-data.frame(ci151)
names(ci151)[3]<-paste("cice11")

cie161<-coefci(ols3h, parm=c("fore_con"), vcov = vcovCL(ols3h, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice161<-ols3h$coefficients[c("fore_con")]
ci161<-cbind(cie161,cice161)
event<-c("a (forecast usage DDD)")
ci161<-cbind(ci161,event)
ci161<-data.frame(ci161)
names(ci161)[3]<-paste("cice11")

cie171<-coefci(ols3i, parm=c("contradiction:round2"), vcov = vcovCL(ols3i, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice171<-ols3i$coefficients[c("contradiction:round2")]
ci171<-cbind(cie171,cice171)
event<-c("b (lower temperature)")
ci171<-cbind(ci171,event)
ci171<-data.frame(ci171)
names(ci171)[3]<-paste("cice11")

cie181<-coefci(ols3i, parm=c("temp_con"), vcov = vcovCL(ols3i, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice181<-ols3i$coefficients[c("temp_con")]
ci181<-cbind(cie181,cice181)
event<-c("b (temperature DDD)")
ci181<-cbind(ci181,event)
ci181<-data.frame(ci181)
names(ci181)[3]<-paste("cice11")

cie191<-coefci(ols3j, parm=c("contradiction:round2"), vcov = vcovCL(ols3j, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice191<-ols3j$coefficients[c("contradiction:round2")]
ci191<-cbind(cie191,cice191)
event<-c("a (lower temperature)")
ci191<-cbind(ci191,event)
ci191<-data.frame(ci191)
names(ci191)[3]<-paste("cice11")

cie201<-coefci(ols3j, parm=c("temp_con"), vcov = vcovCL(ols3j, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice201<-ols3j$coefficients[c("temp_con")]
ci201<-cbind(cie201,cice201)
event<-c("a (temperature DDD)")
ci201<-cbind(ci201,event)
ci201<-data.frame(ci201)
names(ci201)[3]<-paste("cice11")

cie211<-coefci(ols3k, parm=c("contradiction:round2"), vcov = vcovCL(ols3k, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice211<-ols3k$coefficients[c("contradiction:round2")]
ci211<-cbind(cie211,cice211)
event<-c("b (lower education)")
ci211<-cbind(ci211,event)
ci211<-data.frame(ci211)
names(ci211)[3]<-paste("cice11")

cie221<-coefci(ols3k, parm=c("study_con"), vcov = vcovCL(ols3k, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice221<-ols3k$coefficients[c("study_con")]
ci221<-cbind(cie221,cice221)
event<-c("b (education DDD)")
ci221<-cbind(ci221,event)
ci221<-data.frame(ci221)
names(ci221)[3]<-paste("cice11")

cie231<-coefci(ols3l, parm=c("contradiction:round2"), vcov = vcovCL(ols3l, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice231<-ols3l$coefficients[c("contradiction:round2")]
ci231<-cbind(cie231,cice231)
event<-c("a (lower education)")
ci231<-cbind(ci231,event)
ci231<-data.frame(ci231)
names(ci231)[3]<-paste("cice11")

cie241<-coefci(ols3l, parm=c("study_con"), vcov = vcovCL(ols3l, cluster=~ful1$participant.label, type="HC1"),level=0.95)
cice241<-ols3l$coefficients[c("study_con")]
ci241<-cbind(cie241,cice241)
event<-c("a (education DDD)")
ci241<-cbind(ci241,event)
ci241<-data.frame(ci241)
names(ci241)[3]<-paste("cice11")


ciful3<-rbind(ci11,ci31,ci21,ci41,ci51,ci61,ci71,ci81,ci91,ci101,ci111,ci121,ci131,ci141,ci151,ci161,ci171,ci181,ci191,ci201,ci211,ci221,ci231,ci241)
ciful3$X2.5..<-as.numeric(ciful3$X2.5..)
ciful3$X97.5..<-as.numeric(ciful3$X97.5..)
ciful3$cice11<-as.numeric(ciful3$cice11)
ciful3$controls<-"3.) demographic + further"

ciful<-rbind(ciful,ciful3)


ciful$controls<- with(ciful, factor(controls, levels=c("1.) none","2.) demographic", "3.) demographic + further")))
library(ggplot2)
ggplot(
  mapping = aes(
    y = ciful$event
  )
) +
  scale_y_discrete( limits=c("a (not female)","a (female DDD)", "b (not female)", "b (female DDD)","a (less accurate)","a (accurate DDD)", "b (less accurate)", "b (accurate DDD)","a (less credible)","a (credible DDD)", "b (less credible)", "b (credible DDD)","a (less forecast usage)","a (forecast usage DDD)", "b (less forecast usage)", "b (forecast usage DDD)","a (lower temperature)","a (temperature DDD)", "b (lower temperature)", "b (temperature DDD)","a (lower education)","a (education DDD)", "b (lower education)", "b (education DDD)") )+
  geom_pointrange(aes(
    x = ciful$cice11,
    y = ciful$event,
    xmin = ciful$X2.5..,
    xmax = ciful$X97.5..,
    shape=factor(ciful$controls, levels=c("3.) demographic + further","2.) demographic", "1.) none"))
  ), 
  position = position_dodge(width = 0.4),
  
  # Optional decoration:
  fatten=5,
  alpha=.8
  ) +
  
  # Add a line marker for y = 0 (to see if the CI overlaps 0)
  geom_vline(xintercept = 0, color = "red", alpha = 0.2, show.legend=F) +
  # Additional decoration:
  theme_bw() +
  # guides(linetype=guide_legend(override.aes = list(shape = NA))) + 
  theme(
    plot.title = element_text(face = "bold", size = 12),
    legend.background = element_rect(fill = "white", size = 4, colour = "white"),
    #legend.justification = c(0, 1),
    axis.ticks = element_line(colour = "white", size = 0.1),
    panel.grid.major = element_line(colour = "white", size = 0.07),
    panel.grid.minor = element_blank(),
    legend.key.size = unit(1, "lines"),
    legend.box = 'horizontal',
    legend.position = "top"
  ) +
  labs(y = "Ambiguity Index", x = "Estimate",shape="Control variables")+
  scale_shape_manual(values = c("1.) none" = 15, "2.) demographic" = 16,"3.) demographic + further"=17))



ggsave("Ambiguity_regs_het_all.pdf",height=6.5, width=6.7)


###Graph 2 (Confirmation DiDs)
# no control variables
cie11<-coefci(ols4, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols4, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice11<-ols4$coefficients[c("treatconf.-both:round2")]
ci11<-cbind(cie11,cice11)
event<-c("b (both-not female)")
ci11<-cbind(ci11,event)
ci11<-data.frame(ci11)

cie31<-coefci(ols4, parm=c("female_both"), vcov = vcovCL(ols4, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice31<-ols4$coefficients[c("female_both")]
ci31<-cbind(cie31,cice31)
event<-c("b (both-female DDD)")
ci31<-cbind(ci31,event)
ci31<-data.frame(ci31)
names(ci31)[3]<-paste("cice11")

cie21<-coefci(ols4a, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols4a, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice21<-ols4a$coefficients[c("treatconf.-both:round2")]
ci21<-cbind(cie21,cice21)
event<-c("a (both-not female)")
ci21<-cbind(ci21,event)
ci21<-data.frame(ci21)
names(ci21)[3]<-paste("cice11")

cie41<-coefci(ols4a, parm=c("female_both"), vcov = vcovCL(ols4a, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice41<-ols4a$coefficients[c("female_both")]
ci41<-cbind(cie41,cice41)
event<-c("a (both-female DDD)")
ci41<-cbind(ci41,event)
ci41<-data.frame(ci41)
names(ci41)[3]<-paste("cice11")


cie51<-coefci(ols4c, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols4c, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice51<-ols4c$coefficients[c("treatconf.-both:round2")]
ci51<-cbind(cie51,cice51)
event<-c("b (both-less accurate)")
ci51<-cbind(ci51,event)
ci51<-data.frame(ci51)
names(ci51)[3]<-paste("cice11")

cie61<-coefci(ols4c, parm=c("accu_both"), vcov = vcovCL(ols4c, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice61<-ols4c$coefficients[c("accu_both")]
ci61<-cbind(cie61,cice61)
event<-c("b (both-accurate DDD)")
ci61<-cbind(ci61,event)
ci61<-data.frame(ci61)
names(ci61)[3]<-paste("cice11")


cie71<-coefci(ols4d, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols4d, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice71<-ols4d$coefficients[c("treatconf.-both:round2")]
ci71<-cbind(cie71,cice71)
event<-c("a (both-less accurate)")
ci71<-cbind(ci71,event)
ci71<-data.frame(ci71)
names(ci71)[3]<-paste("cice11")

cie81<-coefci(ols4d, parm=c("accu_both"), vcov = vcovCL(ols4d, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice81<-ols4d$coefficients[c("accu_both")]
ci81<-cbind(cie81,cice81)
event<-c("a (both-accurate DDD)")
ci81<-cbind(ci81,event)
ci81<-data.frame(ci81)
names(ci81)[3]<-paste("cice11")

cie91<-coefci(ols4e, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols4e, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice91<-ols4e$coefficients[c("treatconf.-both:round2")]
ci91<-cbind(cie91,cice91)
event<-c("b (both-less credible)")
ci91<-cbind(ci91,event)
ci91<-data.frame(ci91)
names(ci91)[3]<-paste("cice11")

cie101<-coefci(ols4e, parm=c("cred_both"), vcov = vcovCL(ols4e, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice101<-ols4e$coefficients[c("cred_both")]
ci101<-cbind(cie101,cice101)
event<-c("b (both-credible DDD)")
ci101<-cbind(ci101,event)
ci101<-data.frame(ci101)
names(ci101)[3]<-paste("cice11")

cie111<-coefci(ols4f, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols4f, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice111<-ols4f$coefficients[c("treatconf.-both:round2")]
ci111<-cbind(cie111,cice111)
event<-c("a (both-less credible)")
ci111<-cbind(ci111,event)
ci111<-data.frame(ci111)
names(ci111)[3]<-paste("cice11")

cie121<-coefci(ols4f, parm=c("cred_both"), vcov = vcovCL(ols4f, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice121<-ols4f$coefficients[c("cred_both")]
ci121<-cbind(cie121,cice121)
event<-c("a (both-credible DDD)")
ci121<-cbind(ci121,event)
ci121<-data.frame(ci121)
names(ci121)[3]<-paste("cice11")

cie131<-coefci(ols4g, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols4g, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice131<-ols4g$coefficients[c("treatconf.-both:round2")]
ci131<-cbind(cie131,cice131)
event<-c("b (both-less forecast usage)")
ci131<-cbind(ci131,event)
ci131<-data.frame(ci131)
names(ci131)[3]<-paste("cice11")

cie141<-coefci(ols4g, parm=c("fore_both"), vcov = vcovCL(ols4g, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice141<-ols4g$coefficients[c("fore_both")]
ci141<-cbind(cie141,cice141)
event<-c("b (both-forecast usage DDD)")
ci141<-cbind(ci141,event)
ci141<-data.frame(ci141)
names(ci141)[3]<-paste("cice11")

cie151<-coefci(ols4h, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols4h, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice151<-ols4h$coefficients[c("treatconf.-both:round2")]
ci151<-cbind(cie151,cice151)
event<-c("a (both-less forecast usage)")
ci151<-cbind(ci151,event)
ci151<-data.frame(ci151)
names(ci151)[3]<-paste("cice11")

cie161<-coefci(ols4h, parm=c("fore_both"), vcov = vcovCL(ols4h, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice161<-ols4h$coefficients[c("fore_both")]
ci161<-cbind(cie161,cice161)
event<-c("a (both-forecast usage DDD)")
ci161<-cbind(ci161,event)
ci161<-data.frame(ci161)
names(ci161)[3]<-paste("cice11")

cie171<-coefci(ols4i, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols4i, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice171<-ols4i$coefficients[c("treatconf.-both:round2")]
ci171<-cbind(cie171,cice171)
event<-c("b (both-lower temperature)")
ci171<-cbind(ci171,event)
ci171<-data.frame(ci171)
names(ci171)[3]<-paste("cice11")

cie181<-coefci(ols4i, parm=c("temp_both"), vcov = vcovCL(ols4i, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice181<-ols4i$coefficients[c("temp_both")]
ci181<-cbind(cie181,cice181)
event<-c("b (both-temperature DDD)")
ci181<-cbind(ci181,event)
ci181<-data.frame(ci181)
names(ci181)[3]<-paste("cice11")

cie191<-coefci(ols4j, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols4j, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice191<-ols4j$coefficients[c("treatconf.-both:round2")]
ci191<-cbind(cie191,cice191)
event<-c("a (both-lower temperature)")
ci191<-cbind(ci191,event)
ci191<-data.frame(ci191)
names(ci191)[3]<-paste("cice11")

cie201<-coefci(ols4j, parm=c("temp_both"), vcov = vcovCL(ols4j, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice201<-ols4j$coefficients[c("temp_both")]
ci201<-cbind(cie201,cice201)
event<-c("a (both-temperature DDD)")
ci201<-cbind(ci201,event)
ci201<-data.frame(ci201)
names(ci201)[3]<-paste("cice11")

cie211<-coefci(ols4k, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols4k, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice211<-ols4k$coefficients[c("treatconf.-both:round2")]
ci211<-cbind(cie211,cice211)
event<-c("b (both-lower education)")
ci211<-cbind(ci211,event)
ci211<-data.frame(ci211)
names(ci211)[3]<-paste("cice11")

cie221<-coefci(ols4k, parm=c("study_both"), vcov = vcovCL(ols4k, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice221<-ols4k$coefficients[c("study_both")]
ci221<-cbind(cie221,cice221)
event<-c("b (both-education DDD)")
ci221<-cbind(ci221,event)
ci221<-data.frame(ci221)
names(ci221)[3]<-paste("cice11")

cie231<-coefci(ols4l, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols4l, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice231<-ols4l$coefficients[c("treatconf.-both:round2")]
ci231<-cbind(cie231,cice231)
event<-c("a (both-lower education)")
ci231<-cbind(ci231,event)
ci231<-data.frame(ci231)
names(ci231)[3]<-paste("cice11")

cie241<-coefci(ols4l, parm=c("study_both"), vcov = vcovCL(ols4l, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice241<-ols4l$coefficients[c("study_both")]
ci241<-cbind(cie241,cice241)
event<-c("a (both-education DDD)")
ci241<-cbind(ci241,event)
ci241<-data.frame(ci241)
names(ci241)[3]<-paste("cice11")

ciful<-rbind(ci11,ci31,ci21,ci41,ci51,ci61,ci71,ci81,ci91,ci101,ci111,ci121,ci131,ci141,ci151,ci161,ci171,ci181,ci191,ci201,ci211,ci221,ci231,ci241)
ciful$X2.5..<-as.numeric(ciful$X2.5..)
ciful$X97.5..<-as.numeric(ciful$X97.5..)
ciful$cice11<-as.numeric(ciful$cice11)
ciful$controls<-"1.) none"

#with demographic variables
cie11<-coefci(ols5, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols5, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice11<-ols5$coefficients[c("treatconf.-both:round2")]
ci11<-cbind(cie11,cice11)
event<-c("b (both-not female)")
ci11<-cbind(ci11,event)
ci11<-data.frame(ci11)

cie31<-coefci(ols5, parm=c("female_both"), vcov = vcovCL(ols5, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice31<-ols5$coefficients[c("female_both")]
ci31<-cbind(cie31,cice31)
event<-c("b (both-female DDD)")
ci31<-cbind(ci31,event)
ci31<-data.frame(ci31)
names(ci31)[3]<-paste("cice11")

cie21<-coefci(ols5a, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols5a, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice21<-ols5a$coefficients[c("treatconf.-both:round2")]
ci21<-cbind(cie21,cice21)
event<-c("a (both-not female)")
ci21<-cbind(ci21,event)
ci21<-data.frame(ci21)
names(ci21)[3]<-paste("cice11")

cie41<-coefci(ols5a, parm=c("female_both"), vcov = vcovCL(ols5a, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice41<-ols5a$coefficients[c("female_both")]
ci41<-cbind(cie41,cice41)
event<-c("a (both-female DDD)")
ci41<-cbind(ci41,event)
ci41<-data.frame(ci41)
names(ci41)[3]<-paste("cice11")


cie51<-coefci(ols5c, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols5c, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice51<-ols5c$coefficients[c("treatconf.-both:round2")]
ci51<-cbind(cie51,cice51)
event<-c("b (both-less accurate)")
ci51<-cbind(ci51,event)
ci51<-data.frame(ci51)
names(ci51)[3]<-paste("cice11")

cie61<-coefci(ols5c, parm=c("accu_both"), vcov = vcovCL(ols5c, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice61<-ols5c$coefficients[c("accu_both")]
ci61<-cbind(cie61,cice61)
event<-c("b (both-accurate DDD)")
ci61<-cbind(ci61,event)
ci61<-data.frame(ci61)
names(ci61)[3]<-paste("cice11")


cie71<-coefci(ols5d, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols5d, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice71<-ols5d$coefficients[c("treatconf.-both:round2")]
ci71<-cbind(cie71,cice71)
event<-c("a (both-less accurate)")
ci71<-cbind(ci71,event)
ci71<-data.frame(ci71)
names(ci71)[3]<-paste("cice11")

cie81<-coefci(ols5d, parm=c("accu_both"), vcov = vcovCL(ols5d, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice81<-ols5d$coefficients[c("accu_both")]
ci81<-cbind(cie81,cice81)
event<-c("a (both-accurate DDD)")
ci81<-cbind(ci81,event)
ci81<-data.frame(ci81)
names(ci81)[3]<-paste("cice11")

cie91<-coefci(ols5e, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols5e, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice91<-ols5e$coefficients[c("treatconf.-both:round2")]
ci91<-cbind(cie91,cice91)
event<-c("b (both-less credible)")
ci91<-cbind(ci91,event)
ci91<-data.frame(ci91)
names(ci91)[3]<-paste("cice11")

cie101<-coefci(ols5e, parm=c("cred_both"), vcov = vcovCL(ols5e, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice101<-ols5e$coefficients[c("cred_both")]
ci101<-cbind(cie101,cice101)
event<-c("b (both-credible DDD)")
ci101<-cbind(ci101,event)
ci101<-data.frame(ci101)
names(ci101)[3]<-paste("cice11")

cie111<-coefci(ols5f, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols5f, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice111<-ols5f$coefficients[c("treatconf.-both:round2")]
ci111<-cbind(cie111,cice111)
event<-c("a (both-less credible)")
ci111<-cbind(ci111,event)
ci111<-data.frame(ci111)
names(ci111)[3]<-paste("cice11")

cie121<-coefci(ols5f, parm=c("cred_both"), vcov = vcovCL(ols5f, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice121<-ols5f$coefficients[c("cred_both")]
ci121<-cbind(cie121,cice121)
event<-c("a (both-credible DDD)")
ci121<-cbind(ci121,event)
ci121<-data.frame(ci121)
names(ci121)[3]<-paste("cice11")

cie131<-coefci(ols5g, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols5g, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice131<-ols5g$coefficients[c("treatconf.-both:round2")]
ci131<-cbind(cie131,cice131)
event<-c("b (both-less forecast usage)")
ci131<-cbind(ci131,event)
ci131<-data.frame(ci131)
names(ci131)[3]<-paste("cice11")

cie141<-coefci(ols5g, parm=c("fore_both"), vcov = vcovCL(ols5g, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice141<-ols5g$coefficients[c("fore_both")]
ci141<-cbind(cie141,cice141)
event<-c("b (both-forecast usage DDD)")
ci141<-cbind(ci141,event)
ci141<-data.frame(ci141)
names(ci141)[3]<-paste("cice11")

cie151<-coefci(ols5h, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols5h, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice151<-ols5h$coefficients[c("treatconf.-both:round2")]
ci151<-cbind(cie151,cice151)
event<-c("a (both-less forecast usage)")
ci151<-cbind(ci151,event)
ci151<-data.frame(ci151)
names(ci151)[3]<-paste("cice11")

cie161<-coefci(ols5h, parm=c("fore_both"), vcov = vcovCL(ols5h, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice161<-ols5h$coefficients[c("fore_both")]
ci161<-cbind(cie161,cice161)
event<-c("a (both-forecast usage DDD)")
ci161<-cbind(ci161,event)
ci161<-data.frame(ci161)
names(ci161)[3]<-paste("cice11")

cie171<-coefci(ols5i, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols5i, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice171<-ols5i$coefficients[c("treatconf.-both:round2")]
ci171<-cbind(cie171,cice171)
event<-c("b (both-lower temperature)")
ci171<-cbind(ci171,event)
ci171<-data.frame(ci171)
names(ci171)[3]<-paste("cice11")

cie181<-coefci(ols5i, parm=c("temp_both"), vcov = vcovCL(ols5i, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice181<-ols5i$coefficients[c("temp_both")]
ci181<-cbind(cie181,cice181)
event<-c("b (both-temperature DDD)")
ci181<-cbind(ci181,event)
ci181<-data.frame(ci181)
names(ci181)[3]<-paste("cice11")

cie191<-coefci(ols5j, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols5j, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice191<-ols5j$coefficients[c("treatconf.-both:round2")]
ci191<-cbind(cie191,cice191)
event<-c("a (both-lower temperature)")
ci191<-cbind(ci191,event)
ci191<-data.frame(ci191)
names(ci191)[3]<-paste("cice11")

cie201<-coefci(ols5j, parm=c("temp_both"), vcov = vcovCL(ols5j, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice201<-ols5j$coefficients[c("temp_both")]
ci201<-cbind(cie201,cice201)
event<-c("a (both-temperature DDD)")
ci201<-cbind(ci201,event)
ci201<-data.frame(ci201)
names(ci201)[3]<-paste("cice11")

cie211<-coefci(ols5k, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols5k, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice211<-ols5k$coefficients[c("treatconf.-both:round2")]
ci211<-cbind(cie211,cice211)
event<-c("b (both-lower education)")
ci211<-cbind(ci211,event)
ci211<-data.frame(ci211)
names(ci211)[3]<-paste("cice11")

cie221<-coefci(ols5k, parm=c("study_both"), vcov = vcovCL(ols5k, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice221<-ols5k$coefficients[c("study_both")]
ci221<-cbind(cie221,cice221)
event<-c("b (both-education DDD)")
ci221<-cbind(ci221,event)
ci221<-data.frame(ci221)
names(ci221)[3]<-paste("cice11")

cie231<-coefci(ols5l, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols5l, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice231<-ols5l$coefficients[c("treatconf.-both:round2")]
ci231<-cbind(cie231,cice231)
event<-c("a (both-lower education)")
ci231<-cbind(ci231,event)
ci231<-data.frame(ci231)
names(ci231)[3]<-paste("cice11")

cie241<-coefci(ols5l, parm=c("study_both"), vcov = vcovCL(ols5l, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice241<-ols5l$coefficients[c("study_both")]
ci241<-cbind(cie241,cice241)
event<-c("a (both-education DDD)")
ci241<-cbind(ci241,event)
ci241<-data.frame(ci241)
names(ci241)[3]<-paste("cice11")

ciful2<-rbind(ci11,ci31,ci21,ci41,ci51,ci61,ci71,ci81,ci91,ci101,ci111,ci121,ci131,ci141,ci151,ci161,ci171,ci181,ci191,ci201,ci211,ci221,ci231,ci241)
ciful2$X2.5..<-as.numeric(ciful2$X2.5..)
ciful2$X97.5..<-as.numeric(ciful2$X97.5..)
ciful2$cice11<-as.numeric(ciful2$cice11)
ciful2$controls<-"2.) demographic"

ciful<-rbind(ciful,ciful2)


#with demographic variables + further controls
cie11<-coefci(ols6, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols6, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice11<-ols6$coefficients[c("treatconf.-both:round2")]
ci11<-cbind(cie11,cice11)
event<-c("b (both-not female)")
ci11<-cbind(ci11,event)
ci11<-data.frame(ci11)

cie31<-coefci(ols6, parm=c("female_both"), vcov = vcovCL(ols6, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice31<-ols6$coefficients[c("female_both")]
ci31<-cbind(cie31,cice31)
event<-c("b (both-female DDD)")
ci31<-cbind(ci31,event)
ci31<-data.frame(ci31)
names(ci31)[3]<-paste("cice11")

cie21<-coefci(ols6a, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols6a, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice21<-ols6a$coefficients[c("treatconf.-both:round2")]
ci21<-cbind(cie21,cice21)
event<-c("a (both-not female)")
ci21<-cbind(ci21,event)
ci21<-data.frame(ci21)
names(ci21)[3]<-paste("cice11")

cie41<-coefci(ols6a, parm=c("female_both"), vcov = vcovCL(ols6a, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice41<-ols6a$coefficients[c("female_both")]
ci41<-cbind(cie41,cice41)
event<-c("a (both-female DDD)")
ci41<-cbind(ci41,event)
ci41<-data.frame(ci41)
names(ci41)[3]<-paste("cice11")


cie51<-coefci(ols6c, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols6c, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice51<-ols6c$coefficients[c("treatconf.-both:round2")]
ci51<-cbind(cie51,cice51)
event<-c("b (both-less accurate)")
ci51<-cbind(ci51,event)
ci51<-data.frame(ci51)
names(ci51)[3]<-paste("cice11")

cie61<-coefci(ols6c, parm=c("accu_both"), vcov = vcovCL(ols6c, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice61<-ols6c$coefficients[c("accu_both")]
ci61<-cbind(cie61,cice61)
event<-c("b (both-accurate DDD)")
ci61<-cbind(ci61,event)
ci61<-data.frame(ci61)
names(ci61)[3]<-paste("cice11")


cie71<-coefci(ols6d, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols6d, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice71<-ols6d$coefficients[c("treatconf.-both:round2")]
ci71<-cbind(cie71,cice71)
event<-c("a (both-less accurate)")
ci71<-cbind(ci71,event)
ci71<-data.frame(ci71)
names(ci71)[3]<-paste("cice11")

cie81<-coefci(ols6d, parm=c("accu_both"), vcov = vcovCL(ols6d, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice81<-ols6d$coefficients[c("accu_both")]
ci81<-cbind(cie81,cice81)
event<-c("a (both-accurate DDD)")
ci81<-cbind(ci81,event)
ci81<-data.frame(ci81)
names(ci81)[3]<-paste("cice11")

cie91<-coefci(ols6e, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols6e, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice91<-ols6e$coefficients[c("treatconf.-both:round2")]
ci91<-cbind(cie91,cice91)
event<-c("b (both-less credible)")
ci91<-cbind(ci91,event)
ci91<-data.frame(ci91)
names(ci91)[3]<-paste("cice11")

cie101<-coefci(ols6e, parm=c("cred_both"), vcov = vcovCL(ols6e, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice101<-ols6e$coefficients[c("cred_both")]
ci101<-cbind(cie101,cice101)
event<-c("b (both-credible DDD)")
ci101<-cbind(ci101,event)
ci101<-data.frame(ci101)
names(ci101)[3]<-paste("cice11")

cie111<-coefci(ols6f, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols6f, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice111<-ols6f$coefficients[c("treatconf.-both:round2")]
ci111<-cbind(cie111,cice111)
event<-c("a (both-less credible)")
ci111<-cbind(ci111,event)
ci111<-data.frame(ci111)
names(ci111)[3]<-paste("cice11")

cie121<-coefci(ols6f, parm=c("cred_both"), vcov = vcovCL(ols6f, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice121<-ols6f$coefficients[c("cred_both")]
ci121<-cbind(cie121,cice121)
event<-c("a (both-credible DDD)")
ci121<-cbind(ci121,event)
ci121<-data.frame(ci121)
names(ci121)[3]<-paste("cice11")

cie131<-coefci(ols6g, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols6g, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice131<-ols6g$coefficients[c("treatconf.-both:round2")]
ci131<-cbind(cie131,cice131)
event<-c("b (both-less forecast usage)")
ci131<-cbind(ci131,event)
ci131<-data.frame(ci131)
names(ci131)[3]<-paste("cice11")

cie141<-coefci(ols6g, parm=c("fore_both"), vcov = vcovCL(ols6g, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice141<-ols6g$coefficients[c("fore_both")]
ci141<-cbind(cie141,cice141)
event<-c("b (both-forecast usage DDD)")
ci141<-cbind(ci141,event)
ci141<-data.frame(ci141)
names(ci141)[3]<-paste("cice11")

cie151<-coefci(ols6h, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols6h, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice151<-ols6h$coefficients[c("treatconf.-both:round2")]
ci151<-cbind(cie151,cice151)
event<-c("a (both-less forecast usage)")
ci151<-cbind(ci151,event)
ci151<-data.frame(ci151)
names(ci151)[3]<-paste("cice11")

cie161<-coefci(ols6h, parm=c("fore_both"), vcov = vcovCL(ols6h, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice161<-ols6h$coefficients[c("fore_both")]
ci161<-cbind(cie161,cice161)
event<-c("a (both-forecast usage DDD)")
ci161<-cbind(ci161,event)
ci161<-data.frame(ci161)
names(ci161)[3]<-paste("cice11")

cie171<-coefci(ols6i, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols6i, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice171<-ols6i$coefficients[c("treatconf.-both:round2")]
ci171<-cbind(cie171,cice171)
event<-c("b (both-lower temperature)")
ci171<-cbind(ci171,event)
ci171<-data.frame(ci171)
names(ci171)[3]<-paste("cice11")

cie181<-coefci(ols6i, parm=c("temp_both"), vcov = vcovCL(ols6i, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice181<-ols6i$coefficients[c("temp_both")]
ci181<-cbind(cie181,cice181)
event<-c("b (both-temperature DDD)")
ci181<-cbind(ci181,event)
ci181<-data.frame(ci181)
names(ci181)[3]<-paste("cice11")

cie191<-coefci(ols6j, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols6j, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice191<-ols6j$coefficients[c("treatconf.-both:round2")]
ci191<-cbind(cie191,cice191)
event<-c("a (both-lower temperature)")
ci191<-cbind(ci191,event)
ci191<-data.frame(ci191)
names(ci191)[3]<-paste("cice11")

cie201<-coefci(ols6j, parm=c("temp_both"), vcov = vcovCL(ols6j, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice201<-ols6j$coefficients[c("temp_both")]
ci201<-cbind(cie201,cice201)
event<-c("a (both-temperature DDD)")
ci201<-cbind(ci201,event)
ci201<-data.frame(ci201)
names(ci201)[3]<-paste("cice11")

cie211<-coefci(ols6k, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols6k, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice211<-ols6k$coefficients[c("treatconf.-both:round2")]
ci211<-cbind(cie211,cice211)
event<-c("b (both-lower education)")
ci211<-cbind(ci211,event)
ci211<-data.frame(ci211)
names(ci211)[3]<-paste("cice11")

cie221<-coefci(ols6k, parm=c("study_both"), vcov = vcovCL(ols6k, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice221<-ols6k$coefficients[c("study_both")]
ci221<-cbind(cie221,cice221)
event<-c("b (both-education DDD)")
ci221<-cbind(ci221,event)
ci221<-data.frame(ci221)
names(ci221)[3]<-paste("cice11")

cie231<-coefci(ols6l, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols6l, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice231<-ols6l$coefficients[c("treatconf.-both:round2")]
ci231<-cbind(cie231,cice231)
event<-c("a (both-lower education)")
ci231<-cbind(ci231,event)
ci231<-data.frame(ci231)
names(ci231)[3]<-paste("cice11")

cie241<-coefci(ols6l, parm=c("study_both"), vcov = vcovCL(ols6l, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice241<-ols6l$coefficients[c("study_both")]
ci241<-cbind(cie241,cice241)
event<-c("a (both-education DDD)")
ci241<-cbind(ci241,event)
ci241<-data.frame(ci241)
names(ci241)[3]<-paste("cice11")


ciful3<-rbind(ci11,ci31,ci21,ci41,ci51,ci61,ci71,ci81,ci91,ci101,ci111,ci121,ci131,ci141,ci151,ci161,ci171,ci181,ci191,ci201,ci211,ci221,ci231,ci241)
ciful3$X2.5..<-as.numeric(ciful3$X2.5..)
ciful3$X97.5..<-as.numeric(ciful3$X97.5..)
ciful3$cice11<-as.numeric(ciful3$cice11)
ciful3$controls<-"3.) demographic + further"

cifula<-rbind(ciful,ciful3)

#Graph 2: Interval estimates
# no control variables
cie11<-coefci(ols4, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols4, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice11<-ols4$coefficients[c("treatconf.-interval:round2")]
ci11<-cbind(cie11,cice11)
event<-c("b (interval-not female)")
ci11<-cbind(ci11,event)
ci11<-data.frame(ci11)

cie31<-coefci(ols4, parm=c("female_int"), vcov = vcovCL(ols4, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice31<-ols4$coefficients[c("female_int")]
ci31<-cbind(cie31,cice31)
event<-c("b (interval-female DDD)")
ci31<-cbind(ci31,event)
ci31<-data.frame(ci31)
names(ci31)[3]<-paste("cice11")

cie21<-coefci(ols4a, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols4a, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice21<-ols4a$coefficients[c("treatconf.-interval:round2")]
ci21<-cbind(cie21,cice21)
event<-c("a (interval-not female)")
ci21<-cbind(ci21,event)
ci21<-data.frame(ci21)
names(ci21)[3]<-paste("cice11")

cie41<-coefci(ols4a, parm=c("female_int"), vcov = vcovCL(ols4a, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice41<-ols4a$coefficients[c("female_int")]
ci41<-cbind(cie41,cice41)
event<-c("a (interval-female DDD)")
ci41<-cbind(ci41,event)
ci41<-data.frame(ci41)
names(ci41)[3]<-paste("cice11")


cie51<-coefci(ols4c, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols4c, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice51<-ols4c$coefficients[c("treatconf.-interval:round2")]
ci51<-cbind(cie51,cice51)
event<-c("b (interval-less accurate)")
ci51<-cbind(ci51,event)
ci51<-data.frame(ci51)
names(ci51)[3]<-paste("cice11")

cie61<-coefci(ols4c, parm=c("accu_int"), vcov = vcovCL(ols4c, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice61<-ols4c$coefficients[c("accu_int")]
ci61<-cbind(cie61,cice61)
event<-c("b (interval-accurate DDD)")
ci61<-cbind(ci61,event)
ci61<-data.frame(ci61)
names(ci61)[3]<-paste("cice11")


cie71<-coefci(ols4d, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols4d, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice71<-ols4d$coefficients[c("treatconf.-interval:round2")]
ci71<-cbind(cie71,cice71)
event<-c("a (interval-less accurate)")
ci71<-cbind(ci71,event)
ci71<-data.frame(ci71)
names(ci71)[3]<-paste("cice11")

cie81<-coefci(ols4d, parm=c("accu_int"), vcov = vcovCL(ols4d, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice81<-ols4d$coefficients[c("accu_int")]
ci81<-cbind(cie81,cice81)
event<-c("a (interval-accurate DDD)")
ci81<-cbind(ci81,event)
ci81<-data.frame(ci81)
names(ci81)[3]<-paste("cice11")

cie91<-coefci(ols4e, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols4e, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice91<-ols4e$coefficients[c("treatconf.-interval:round2")]
ci91<-cbind(cie91,cice91)
event<-c("b (interval-less credible)")
ci91<-cbind(ci91,event)
ci91<-data.frame(ci91)
names(ci91)[3]<-paste("cice11")

cie101<-coefci(ols4e, parm=c("cred_int"), vcov = vcovCL(ols4e, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice101<-ols4e$coefficients[c("cred_int")]
ci101<-cbind(cie101,cice101)
event<-c("b (interval-credible DDD)")
ci101<-cbind(ci101,event)
ci101<-data.frame(ci101)
names(ci101)[3]<-paste("cice11")

cie111<-coefci(ols4f, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols4f, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice111<-ols4f$coefficients[c("treatconf.-interval:round2")]
ci111<-cbind(cie111,cice111)
event<-c("a (interval-less credible)")
ci111<-cbind(ci111,event)
ci111<-data.frame(ci111)
names(ci111)[3]<-paste("cice11")

cie121<-coefci(ols4f, parm=c("cred_int"), vcov = vcovCL(ols4f, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice121<-ols4f$coefficients[c("cred_int")]
ci121<-cbind(cie121,cice121)
event<-c("a (interval-credible DDD)")
ci121<-cbind(ci121,event)
ci121<-data.frame(ci121)
names(ci121)[3]<-paste("cice11")

cie131<-coefci(ols4g, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols4g, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice131<-ols4g$coefficients[c("treatconf.-interval:round2")]
ci131<-cbind(cie131,cice131)
event<-c("b (interval-less forecast usage)")
ci131<-cbind(ci131,event)
ci131<-data.frame(ci131)
names(ci131)[3]<-paste("cice11")

cie141<-coefci(ols4g, parm=c("fore_int"), vcov = vcovCL(ols4g, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice141<-ols4g$coefficients[c("fore_int")]
ci141<-cbind(cie141,cice141)
event<-c("b (interval-forecast usage DDD)")
ci141<-cbind(ci141,event)
ci141<-data.frame(ci141)
names(ci141)[3]<-paste("cice11")

cie151<-coefci(ols4h, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols4h, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice151<-ols4h$coefficients[c("treatconf.-interval:round2")]
ci151<-cbind(cie151,cice151)
event<-c("a (interval-less forecast usage)")
ci151<-cbind(ci151,event)
ci151<-data.frame(ci151)
names(ci151)[3]<-paste("cice11")

cie161<-coefci(ols4h, parm=c("fore_int"), vcov = vcovCL(ols4h, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice161<-ols4h$coefficients[c("fore_int")]
ci161<-cbind(cie161,cice161)
event<-c("a (interval-forecast usage DDD)")
ci161<-cbind(ci161,event)
ci161<-data.frame(ci161)
names(ci161)[3]<-paste("cice11")

cie171<-coefci(ols4i, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols4i, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice171<-ols4i$coefficients[c("treatconf.-interval:round2")]
ci171<-cbind(cie171,cice171)
event<-c("b (interval-lower temperature)")
ci171<-cbind(ci171,event)
ci171<-data.frame(ci171)
names(ci171)[3]<-paste("cice11")

cie181<-coefci(ols4i, parm=c("temp_int"), vcov = vcovCL(ols4i, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice181<-ols4i$coefficients[c("temp_int")]
ci181<-cbind(cie181,cice181)
event<-c("b (interval-temperature DDD)")
ci181<-cbind(ci181,event)
ci181<-data.frame(ci181)
names(ci181)[3]<-paste("cice11")

cie191<-coefci(ols4j, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols4j, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice191<-ols4j$coefficients[c("treatconf.-interval:round2")]
ci191<-cbind(cie191,cice191)
event<-c("a (interval-lower temperature)")
ci191<-cbind(ci191,event)
ci191<-data.frame(ci191)
names(ci191)[3]<-paste("cice11")

cie201<-coefci(ols4j, parm=c("temp_int"), vcov = vcovCL(ols4j, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice201<-ols4j$coefficients[c("temp_int")]
ci201<-cbind(cie201,cice201)
event<-c("a (interval-temperature DDD)")
ci201<-cbind(ci201,event)
ci201<-data.frame(ci201)
names(ci201)[3]<-paste("cice11")

cie211<-coefci(ols4k, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols4k, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice211<-ols4k$coefficients[c("treatconf.-interval:round2")]
ci211<-cbind(cie211,cice211)
event<-c("b (interval-lower education)")
ci211<-cbind(ci211,event)
ci211<-data.frame(ci211)
names(ci211)[3]<-paste("cice11")

cie221<-coefci(ols4k, parm=c("study_int"), vcov = vcovCL(ols4k, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice221<-ols4k$coefficients[c("study_int")]
ci221<-cbind(cie221,cice221)
event<-c("b (interval-education DDD)")
ci221<-cbind(ci221,event)
ci221<-data.frame(ci221)
names(ci221)[3]<-paste("cice11")

cie231<-coefci(ols4l, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols4l, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice231<-ols4l$coefficients[c("treatconf.-interval:round2")]
ci231<-cbind(cie231,cice231)
event<-c("a (interval-lower education)")
ci231<-cbind(ci231,event)
ci231<-data.frame(ci231)
names(ci231)[3]<-paste("cice11")

cie241<-coefci(ols4l, parm=c("study_int"), vcov = vcovCL(ols4l, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice241<-ols4l$coefficients[c("study_int")]
ci241<-cbind(cie241,cice241)
event<-c("a (interval-education DDD)")
ci241<-cbind(ci241,event)
ci241<-data.frame(ci241)
names(ci241)[3]<-paste("cice11")

ciful<-rbind(ci11,ci31,ci21,ci41,ci51,ci61,ci71,ci81,ci91,ci101,ci111,ci121,ci131,ci141,ci151,ci161,ci171,ci181,ci191,ci201,ci211,ci221,ci231,ci241)
ciful$X2.5..<-as.numeric(ciful$X2.5..)
ciful$X97.5..<-as.numeric(ciful$X97.5..)
ciful$cice11<-as.numeric(ciful$cice11)
ciful$controls<-"1.) none"

#with demographic variables
cie11<-coefci(ols5, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols5, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice11<-ols5$coefficients[c("treatconf.-interval:round2")]
ci11<-cbind(cie11,cice11)
event<-c("b (interval-not female)")
ci11<-cbind(ci11,event)
ci11<-data.frame(ci11)

cie31<-coefci(ols5, parm=c("female_int"), vcov = vcovCL(ols5, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice31<-ols5$coefficients[c("female_int")]
ci31<-cbind(cie31,cice31)
event<-c("b (interval-female DDD)")
ci31<-cbind(ci31,event)
ci31<-data.frame(ci31)
names(ci31)[3]<-paste("cice11")

cie21<-coefci(ols5a, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols5a, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice21<-ols5a$coefficients[c("treatconf.-interval:round2")]
ci21<-cbind(cie21,cice21)
event<-c("a (interval-not female)")
ci21<-cbind(ci21,event)
ci21<-data.frame(ci21)
names(ci21)[3]<-paste("cice11")

cie41<-coefci(ols5a, parm=c("female_int"), vcov = vcovCL(ols5a, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice41<-ols5a$coefficients[c("female_int")]
ci41<-cbind(cie41,cice41)
event<-c("a (interval-female DDD)")
ci41<-cbind(ci41,event)
ci41<-data.frame(ci41)
names(ci41)[3]<-paste("cice11")


cie51<-coefci(ols5c, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols5c, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice51<-ols5c$coefficients[c("treatconf.-interval:round2")]
ci51<-cbind(cie51,cice51)
event<-c("b (interval-less accurate)")
ci51<-cbind(ci51,event)
ci51<-data.frame(ci51)
names(ci51)[3]<-paste("cice11")

cie61<-coefci(ols5c, parm=c("accu_int"), vcov = vcovCL(ols5c, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice61<-ols5c$coefficients[c("accu_int")]
ci61<-cbind(cie61,cice61)
event<-c("b (interval-accurate DDD)")
ci61<-cbind(ci61,event)
ci61<-data.frame(ci61)
names(ci61)[3]<-paste("cice11")


cie71<-coefci(ols5d, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols5d, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice71<-ols5d$coefficients[c("treatconf.-interval:round2")]
ci71<-cbind(cie71,cice71)
event<-c("a (interval-less accurate)")
ci71<-cbind(ci71,event)
ci71<-data.frame(ci71)
names(ci71)[3]<-paste("cice11")

cie81<-coefci(ols5d, parm=c("accu_int"), vcov = vcovCL(ols5d, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice81<-ols5d$coefficients[c("accu_int")]
ci81<-cbind(cie81,cice81)
event<-c("a (interval-accurate DDD)")
ci81<-cbind(ci81,event)
ci81<-data.frame(ci81)
names(ci81)[3]<-paste("cice11")

cie91<-coefci(ols5e, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols5e, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice91<-ols5e$coefficients[c("treatconf.-interval:round2")]
ci91<-cbind(cie91,cice91)
event<-c("b (interval-less credible)")
ci91<-cbind(ci91,event)
ci91<-data.frame(ci91)
names(ci91)[3]<-paste("cice11")

cie101<-coefci(ols5e, parm=c("cred_int"), vcov = vcovCL(ols5e, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice101<-ols5e$coefficients[c("cred_int")]
ci101<-cbind(cie101,cice101)
event<-c("b (interval-credible DDD)")
ci101<-cbind(ci101,event)
ci101<-data.frame(ci101)
names(ci101)[3]<-paste("cice11")

cie111<-coefci(ols5f, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols5f, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice111<-ols5f$coefficients[c("treatconf.-interval:round2")]
ci111<-cbind(cie111,cice111)
event<-c("a (interval-less credible)")
ci111<-cbind(ci111,event)
ci111<-data.frame(ci111)
names(ci111)[3]<-paste("cice11")

cie121<-coefci(ols5f, parm=c("cred_int"), vcov = vcovCL(ols5f, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice121<-ols5f$coefficients[c("cred_int")]
ci121<-cbind(cie121,cice121)
event<-c("a (interval-credible DDD)")
ci121<-cbind(ci121,event)
ci121<-data.frame(ci121)
names(ci121)[3]<-paste("cice11")

cie131<-coefci(ols5g, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols5g, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice131<-ols5g$coefficients[c("treatconf.-interval:round2")]
ci131<-cbind(cie131,cice131)
event<-c("b (interval-less forecast usage)")
ci131<-cbind(ci131,event)
ci131<-data.frame(ci131)
names(ci131)[3]<-paste("cice11")

cie141<-coefci(ols5g, parm=c("fore_int"), vcov = vcovCL(ols5g, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice141<-ols5g$coefficients[c("fore_int")]
ci141<-cbind(cie141,cice141)
event<-c("b (interval-forecast usage DDD)")
ci141<-cbind(ci141,event)
ci141<-data.frame(ci141)
names(ci141)[3]<-paste("cice11")

cie151<-coefci(ols5h, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols5h, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice151<-ols5h$coefficients[c("treatconf.-interval:round2")]
ci151<-cbind(cie151,cice151)
event<-c("a (interval-less forecast usage)")
ci151<-cbind(ci151,event)
ci151<-data.frame(ci151)
names(ci151)[3]<-paste("cice11")

cie161<-coefci(ols5h, parm=c("fore_int"), vcov = vcovCL(ols5h, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice161<-ols5h$coefficients[c("fore_int")]
ci161<-cbind(cie161,cice161)
event<-c("a (interval-forecast usage DDD)")
ci161<-cbind(ci161,event)
ci161<-data.frame(ci161)
names(ci161)[3]<-paste("cice11")

cie171<-coefci(ols5i, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols5i, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice171<-ols5i$coefficients[c("treatconf.-interval:round2")]
ci171<-cbind(cie171,cice171)
event<-c("b (interval-lower temperature)")
ci171<-cbind(ci171,event)
ci171<-data.frame(ci171)
names(ci171)[3]<-paste("cice11")

cie181<-coefci(ols5i, parm=c("temp_int"), vcov = vcovCL(ols5i, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice181<-ols5i$coefficients[c("temp_int")]
ci181<-cbind(cie181,cice181)
event<-c("b (interval-temperature DDD)")
ci181<-cbind(ci181,event)
ci181<-data.frame(ci181)
names(ci181)[3]<-paste("cice11")

cie191<-coefci(ols5j, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols5j, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice191<-ols5j$coefficients[c("treatconf.-interval:round2")]
ci191<-cbind(cie191,cice191)
event<-c("a (interval-lower temperature)")
ci191<-cbind(ci191,event)
ci191<-data.frame(ci191)
names(ci191)[3]<-paste("cice11")

cie201<-coefci(ols5j, parm=c("temp_int"), vcov = vcovCL(ols5j, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice201<-ols5j$coefficients[c("temp_int")]
ci201<-cbind(cie201,cice201)
event<-c("a (interval-temperature DDD)")
ci201<-cbind(ci201,event)
ci201<-data.frame(ci201)
names(ci201)[3]<-paste("cice11")

cie211<-coefci(ols5k, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols5k, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice211<-ols5k$coefficients[c("treatconf.-interval:round2")]
ci211<-cbind(cie211,cice211)
event<-c("b (interval-lower education)")
ci211<-cbind(ci211,event)
ci211<-data.frame(ci211)
names(ci211)[3]<-paste("cice11")

cie221<-coefci(ols5k, parm=c("study_int"), vcov = vcovCL(ols5k, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice221<-ols5k$coefficients[c("study_int")]
ci221<-cbind(cie221,cice221)
event<-c("b (interval-education DDD)")
ci221<-cbind(ci221,event)
ci221<-data.frame(ci221)
names(ci221)[3]<-paste("cice11")

cie231<-coefci(ols5l, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols5l, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice231<-ols5l$coefficients[c("treatconf.-interval:round2")]
ci231<-cbind(cie231,cice231)
event<-c("a (interval-lower education)")
ci231<-cbind(ci231,event)
ci231<-data.frame(ci231)
names(ci231)[3]<-paste("cice11")

cie241<-coefci(ols5l, parm=c("study_int"), vcov = vcovCL(ols5l, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice241<-ols5l$coefficients[c("study_int")]
ci241<-cbind(cie241,cice241)
event<-c("a (interval-education DDD)")
ci241<-cbind(ci241,event)
ci241<-data.frame(ci241)
names(ci241)[3]<-paste("cice11")


ciful2<-rbind(ci11,ci31,ci21,ci41,ci51,ci61,ci71,ci81,ci91,ci101,ci111,ci121,ci131,ci141,ci151,ci161,ci171,ci181,ci191,ci201,ci211,ci221,ci231,ci241)
ciful2$X2.5..<-as.numeric(ciful2$X2.5..)
ciful2$X97.5..<-as.numeric(ciful2$X97.5..)
ciful2$cice11<-as.numeric(ciful2$cice11)
ciful2$controls<-"2.) demographic"

ciful<-rbind(ciful,ciful2)


#with demographic variables + further controls
cie11<-coefci(ols6, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols6, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice11<-ols6$coefficients[c("treatconf.-interval:round2")]
ci11<-cbind(cie11,cice11)
event<-c("b (interval-not female)")
ci11<-cbind(ci11,event)
ci11<-data.frame(ci11)

cie31<-coefci(ols6, parm=c("female_int"), vcov = vcovCL(ols6, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice31<-ols6$coefficients[c("female_int")]
ci31<-cbind(cie31,cice31)
event<-c("b (interval-female DDD)")
ci31<-cbind(ci31,event)
ci31<-data.frame(ci31)
names(ci31)[3]<-paste("cice11")

cie21<-coefci(ols6a, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols6a, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice21<-ols6a$coefficients[c("treatconf.-interval:round2")]
ci21<-cbind(cie21,cice21)
event<-c("a (interval-not female)")
ci21<-cbind(ci21,event)
ci21<-data.frame(ci21)
names(ci21)[3]<-paste("cice11")

cie41<-coefci(ols6a, parm=c("female_int"), vcov = vcovCL(ols6a, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice41<-ols6a$coefficients[c("female_int")]
ci41<-cbind(cie41,cice41)
event<-c("a (interval-female DDD)")
ci41<-cbind(ci41,event)
ci41<-data.frame(ci41)
names(ci41)[3]<-paste("cice11")


cie51<-coefci(ols6c, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols6c, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice51<-ols6c$coefficients[c("treatconf.-interval:round2")]
ci51<-cbind(cie51,cice51)
event<-c("b (interval-less accurate)")
ci51<-cbind(ci51,event)
ci51<-data.frame(ci51)
names(ci51)[3]<-paste("cice11")

cie61<-coefci(ols6c, parm=c("accu_int"), vcov = vcovCL(ols6c, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice61<-ols6c$coefficients[c("accu_int")]
ci61<-cbind(cie61,cice61)
event<-c("b (interval-accurate DDD)")
ci61<-cbind(ci61,event)
ci61<-data.frame(ci61)
names(ci61)[3]<-paste("cice11")


cie71<-coefci(ols6d, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols6d, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice71<-ols6d$coefficients[c("treatconf.-interval:round2")]
ci71<-cbind(cie71,cice71)
event<-c("a (interval-less accurate)")
ci71<-cbind(ci71,event)
ci71<-data.frame(ci71)
names(ci71)[3]<-paste("cice11")

cie81<-coefci(ols6d, parm=c("accu_int"), vcov = vcovCL(ols6d, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice81<-ols6d$coefficients[c("accu_int")]
ci81<-cbind(cie81,cice81)
event<-c("a (interval-accurate DDD)")
ci81<-cbind(ci81,event)
ci81<-data.frame(ci81)
names(ci81)[3]<-paste("cice11")

cie91<-coefci(ols6e, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols6e, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice91<-ols6e$coefficients[c("treatconf.-interval:round2")]
ci91<-cbind(cie91,cice91)
event<-c("b (interval-less credible)")
ci91<-cbind(ci91,event)
ci91<-data.frame(ci91)
names(ci91)[3]<-paste("cice11")

cie101<-coefci(ols6e, parm=c("cred_int"), vcov = vcovCL(ols6e, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice101<-ols6e$coefficients[c("cred_int")]
ci101<-cbind(cie101,cice101)
event<-c("b (interval-credible DDD)")
ci101<-cbind(ci101,event)
ci101<-data.frame(ci101)
names(ci101)[3]<-paste("cice11")

cie111<-coefci(ols6f, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols6f, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice111<-ols6f$coefficients[c("treatconf.-interval:round2")]
ci111<-cbind(cie111,cice111)
event<-c("a (interval-less credible)")
ci111<-cbind(ci111,event)
ci111<-data.frame(ci111)
names(ci111)[3]<-paste("cice11")

cie121<-coefci(ols6f, parm=c("cred_int"), vcov = vcovCL(ols6f, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice121<-ols6f$coefficients[c("cred_int")]
ci121<-cbind(cie121,cice121)
event<-c("a (interval-credible DDD)")
ci121<-cbind(ci121,event)
ci121<-data.frame(ci121)
names(ci121)[3]<-paste("cice11")

cie131<-coefci(ols6g, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols6g, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice131<-ols6g$coefficients[c("treatconf.-interval:round2")]
ci131<-cbind(cie131,cice131)
event<-c("b (interval-less forecast usage)")
ci131<-cbind(ci131,event)
ci131<-data.frame(ci131)
names(ci131)[3]<-paste("cice11")

cie141<-coefci(ols6g, parm=c("fore_int"), vcov = vcovCL(ols6g, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice141<-ols6g$coefficients[c("fore_int")]
ci141<-cbind(cie141,cice141)
event<-c("b (interval-forecast usage DDD)")
ci141<-cbind(ci141,event)
ci141<-data.frame(ci141)
names(ci141)[3]<-paste("cice11")

cie151<-coefci(ols6h, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols6h, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice151<-ols6h$coefficients[c("treatconf.-interval:round2")]
ci151<-cbind(cie151,cice151)
event<-c("a (interval-less forecast usage)")
ci151<-cbind(ci151,event)
ci151<-data.frame(ci151)
names(ci151)[3]<-paste("cice11")

cie161<-coefci(ols6h, parm=c("fore_int"), vcov = vcovCL(ols6h, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice161<-ols6h$coefficients[c("fore_int")]
ci161<-cbind(cie161,cice161)
event<-c("a (interval-forecast usage DDD)")
ci161<-cbind(ci161,event)
ci161<-data.frame(ci161)
names(ci161)[3]<-paste("cice11")

cie171<-coefci(ols6i, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols6i, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice171<-ols6i$coefficients[c("treatconf.-interval:round2")]
ci171<-cbind(cie171,cice171)
event<-c("b (interval-lower temperature)")
ci171<-cbind(ci171,event)
ci171<-data.frame(ci171)
names(ci171)[3]<-paste("cice11")

cie181<-coefci(ols6i, parm=c("temp_int"), vcov = vcovCL(ols6i, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice181<-ols6i$coefficients[c("temp_int")]
ci181<-cbind(cie181,cice181)
event<-c("b (interval-temperature DDD)")
ci181<-cbind(ci181,event)
ci181<-data.frame(ci181)
names(ci181)[3]<-paste("cice11")

cie191<-coefci(ols6j, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols6j, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice191<-ols6j$coefficients[c("treatconf.-interval:round2")]
ci191<-cbind(cie191,cice191)
event<-c("a (interval-lower temperature)")
ci191<-cbind(ci191,event)
ci191<-data.frame(ci191)
names(ci191)[3]<-paste("cice11")

cie201<-coefci(ols6j, parm=c("temp_int"), vcov = vcovCL(ols6j, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice201<-ols6j$coefficients[c("temp_int")]
ci201<-cbind(cie201,cice201)
event<-c("a (interval-temperature DDD)")
ci201<-cbind(ci201,event)
ci201<-data.frame(ci201)
names(ci201)[3]<-paste("cice11")

cie211<-coefci(ols6k, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols6k, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice211<-ols6k$coefficients[c("treatconf.-interval:round2")]
ci211<-cbind(cie211,cice211)
event<-c("b (interval-lower education)")
ci211<-cbind(ci211,event)
ci211<-data.frame(ci211)
names(ci211)[3]<-paste("cice11")

cie221<-coefci(ols6k, parm=c("study_int"), vcov = vcovCL(ols6k, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice221<-ols6k$coefficients[c("study_int")]
ci221<-cbind(cie221,cice221)
event<-c("b (interval-education DDD)")
ci221<-cbind(ci221,event)
ci221<-data.frame(ci221)
names(ci221)[3]<-paste("cice11")

cie231<-coefci(ols6l, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols6l, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice231<-ols6l$coefficients[c("treatconf.-interval:round2")]
ci231<-cbind(cie231,cice231)
event<-c("a (interval-lower education)")
ci231<-cbind(ci231,event)
ci231<-data.frame(ci231)
names(ci231)[3]<-paste("cice11")

cie241<-coefci(ols6l, parm=c("study_int"), vcov = vcovCL(ols6l, cluster=~ful2$participant.label, type="HC1"),level=0.95)
cice241<-ols6l$coefficients[c("study_int")]
ci241<-cbind(cie241,cice241)
event<-c("a (interval-education DDD)")
ci241<-cbind(ci241,event)
ci241<-data.frame(ci241)
names(ci241)[3]<-paste("cice11")


ciful3<-rbind(ci11,ci31,ci21,ci41,ci51,ci61,ci71,ci81,ci91,ci101,ci111,ci121,ci131,ci141,ci151,ci161,ci171,ci181,ci191,ci201,ci211,ci221,ci231,ci241)
ciful3$X2.5..<-as.numeric(ciful3$X2.5..)
ciful3$X97.5..<-as.numeric(ciful3$X97.5..)
ciful3$cice11<-as.numeric(ciful3$cice11)
ciful3$controls<-"3.) demographic + further"

ciful<-rbind(ciful,ciful3)

ciful<-rbind(cifula,ciful)

ciful$controls<- with(ciful, factor(controls, levels=c("1.) none","2.) demographic", "3.) demographic + further")))


library(ggplot2)
ggplot(
  mapping = aes(
    y = ciful$event
  )
) +
  scale_y_discrete( limits=c("a (both-not female)","a (both-female DDD)", "b (both-not female)", "b (both-female DDD)","a (both-less accurate)","a (both-accurate DDD)", "b (both-less accurate)", "b (both-accurate DDD)","a (both-less credible)","a (both-credible DDD)", "b (both-less credible)", "b (both-credible DDD)","a (both-less forecast usage)","a (both-forecast usage DDD)", "b (both-less forecast usage)", "b (both-forecast usage DDD)","a (both-lower temperature)","a (both-temperature DDD)", "b (both-lower temperature)", "b (both-temperature DDD)","a (both-lower education)","a (both-education DDD)", "b (both-lower education)", "b (both-education DDD)", "a (interval-not female)","a (interval-female DDD)", "b (interval-not female)", "b (interval-female DDD)","a (interval-less accurate)","a (interval-accurate DDD)", "b (interval-less accurate)", "b (interval-accurate DDD)","a (interval-less credible)","a (interval-credible DDD)", "b (interval-less credible)", "b (interval-credible DDD)","a (interval-less forecast usage)","a (interval-forecast usage DDD)", "b (interval-less forecast usage)", "b (interval-forecast usage DDD)","a (interval-lower temperature)","a (interval-temperature DDD)", "b (interval-lower temperature)", "b (interval-temperature DDD)","a (interval-lower education)","a (interval-education DDD)", "b (interval-lower education)", "b (interval-education DDD)"))+
  geom_pointrange(aes(
    x = ciful$cice11,
    y = ciful$event,
    xmin = ciful$X2.5..,
    xmax = ciful$X97.5..,
    shape=factor(ciful$controls, levels=c("3.) demographic + further","2.) demographic", "1.) none"))
  ), 
  position = position_dodge(width = 0.4),
  
  # Optional decoration:
  fatten=5,
  alpha=.8
  ) +
  
  # Add a line marker for y = 0 (to see if the CI overlaps 0)
  geom_vline(xintercept = 0, color = "red", alpha = 0.2, show.legend=F) +
  # Additional decoration:
  theme_bw() +
  # guides(linetype=guide_legend(override.aes = list(shape = NA))) + 
  theme(
    plot.title = element_text(face = "bold", size = 12),
    legend.background = element_rect(fill = "white", size = 4, colour = "white"),
    #legend.justification = c(0, 1),
    axis.ticks = element_line(colour = "white", size = 0.1),
    panel.grid.major = element_line(colour = "white", size = 0.07),
    panel.grid.minor = element_blank(),
    legend.key.size = unit(1, "lines"),
    legend.box = 'horizontal',
    legend.position = "top"
  ) +
  labs(y = "Ambiguity Index", x = "Estimate",shape="Control variables")+
  scale_shape_manual(values = c("1.) none" = 15, "2.) demographic" = 16,"3.) demographic + further"=17))

ggsave("Ambiguity_regs_het_confirmation.pdf",height=9.5, width=7.2)


###Graph 3 (Contradiction DiDs)
# no control variables
cie11<-coefci(ols7, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols7, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice11<-ols7$coefficients[c("treatcontrad.-both:round2")]
ci11<-cbind(cie11,cice11)
event<-c("b (both-not female)")
ci11<-cbind(ci11,event)
ci11<-data.frame(ci11)

cie31<-coefci(ols7, parm=c("female_both"), vcov = vcovCL(ols7, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice31<-ols7$coefficients[c("female_both")]
ci31<-cbind(cie31,cice31)
event<-c("b (both-female DDD)")
ci31<-cbind(ci31,event)
ci31<-data.frame(ci31)
names(ci31)[3]<-paste("cice11")

cie21<-coefci(ols7a, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols7a, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice21<-ols7a$coefficients[c("treatcontrad.-both:round2")]
ci21<-cbind(cie21,cice21)
event<-c("a (both-not female)")
ci21<-cbind(ci21,event)
ci21<-data.frame(ci21)
names(ci21)[3]<-paste("cice11")

cie41<-coefci(ols7a, parm=c("female_both"), vcov = vcovCL(ols7a, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice41<-ols7a$coefficients[c("female_both")]
ci41<-cbind(cie41,cice41)
event<-c("a (both-female DDD)")
ci41<-cbind(ci41,event)
ci41<-data.frame(ci41)
names(ci41)[3]<-paste("cice11")


cie51<-coefci(ols7c, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols7c, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice51<-ols7c$coefficients[c("treatcontrad.-both:round2")]
ci51<-cbind(cie51,cice51)
event<-c("b (both-less accurate)")
ci51<-cbind(ci51,event)
ci51<-data.frame(ci51)
names(ci51)[3]<-paste("cice11")

cie61<-coefci(ols7c, parm=c("accu_both"), vcov = vcovCL(ols7c, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice61<-ols7c$coefficients[c("accu_both")]
ci61<-cbind(cie61,cice61)
event<-c("b (both-accurate DDD)")
ci61<-cbind(ci61,event)
ci61<-data.frame(ci61)
names(ci61)[3]<-paste("cice11")


cie71<-coefci(ols7d, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols7d, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice71<-ols7d$coefficients[c("treatcontrad.-both:round2")]
ci71<-cbind(cie71,cice71)
event<-c("a (both-less accurate)")
ci71<-cbind(ci71,event)
ci71<-data.frame(ci71)
names(ci71)[3]<-paste("cice11")

cie81<-coefci(ols7d, parm=c("accu_both"), vcov = vcovCL(ols7d, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice81<-ols7d$coefficients[c("accu_both")]
ci81<-cbind(cie81,cice81)
event<-c("a (both-accurate DDD)")
ci81<-cbind(ci81,event)
ci81<-data.frame(ci81)
names(ci81)[3]<-paste("cice11")

cie91<-coefci(ols7e, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols7e, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice91<-ols7e$coefficients[c("treatcontrad.-both:round2")]
ci91<-cbind(cie91,cice91)
event<-c("b (both-less credible)")
ci91<-cbind(ci91,event)
ci91<-data.frame(ci91)
names(ci91)[3]<-paste("cice11")

cie101<-coefci(ols7e, parm=c("cred_both"), vcov = vcovCL(ols7e, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice101<-ols7e$coefficients[c("cred_both")]
ci101<-cbind(cie101,cice101)
event<-c("b (both-credible DDD)")
ci101<-cbind(ci101,event)
ci101<-data.frame(ci101)
names(ci101)[3]<-paste("cice11")

cie111<-coefci(ols7f, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols7f, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice111<-ols7f$coefficients[c("treatcontrad.-both:round2")]
ci111<-cbind(cie111,cice111)
event<-c("a (both-less credible)")
ci111<-cbind(ci111,event)
ci111<-data.frame(ci111)
names(ci111)[3]<-paste("cice11")

cie121<-coefci(ols7f, parm=c("cred_both"), vcov = vcovCL(ols7f, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice121<-ols7f$coefficients[c("cred_both")]
ci121<-cbind(cie121,cice121)
event<-c("a (both-credible DDD)")
ci121<-cbind(ci121,event)
ci121<-data.frame(ci121)
names(ci121)[3]<-paste("cice11")

cie131<-coefci(ols7g, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols7g, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice131<-ols7g$coefficients[c("treatcontrad.-both:round2")]
ci131<-cbind(cie131,cice131)
event<-c("b (both-less forecast usage)")
ci131<-cbind(ci131,event)
ci131<-data.frame(ci131)
names(ci131)[3]<-paste("cice11")

cie141<-coefci(ols7g, parm=c("fore_both"), vcov = vcovCL(ols7g, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice141<-ols7g$coefficients[c("fore_both")]
ci141<-cbind(cie141,cice141)
event<-c("b (both-forecast usage DDD)")
ci141<-cbind(ci141,event)
ci141<-data.frame(ci141)
names(ci141)[3]<-paste("cice11")

cie151<-coefci(ols7h, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols7h, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice151<-ols7h$coefficients[c("treatcontrad.-both:round2")]
ci151<-cbind(cie151,cice151)
event<-c("a (both-less forecast usage)")
ci151<-cbind(ci151,event)
ci151<-data.frame(ci151)
names(ci151)[3]<-paste("cice11")

cie161<-coefci(ols7h, parm=c("fore_both"), vcov = vcovCL(ols7h, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice161<-ols7h$coefficients[c("fore_both")]
ci161<-cbind(cie161,cice161)
event<-c("a (both-forecast usage DDD)")
ci161<-cbind(ci161,event)
ci161<-data.frame(ci161)
names(ci161)[3]<-paste("cice11")

cie171<-coefci(ols7i, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols7i, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice171<-ols7i$coefficients[c("treatcontrad.-both:round2")]
ci171<-cbind(cie171,cice171)
event<-c("b (both-lower temperature)")
ci171<-cbind(ci171,event)
ci171<-data.frame(ci171)
names(ci171)[3]<-paste("cice11")

cie181<-coefci(ols7i, parm=c("temp_both"), vcov = vcovCL(ols7i, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice181<-ols7i$coefficients[c("temp_both")]
ci181<-cbind(cie181,cice181)
event<-c("b (both-temperature DDD)")
ci181<-cbind(ci181,event)
ci181<-data.frame(ci181)
names(ci181)[3]<-paste("cice11")

cie191<-coefci(ols7j, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols7j, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice191<-ols7j$coefficients[c("treatcontrad.-both:round2")]
ci191<-cbind(cie191,cice191)
event<-c("a (both-lower temperature)")
ci191<-cbind(ci191,event)
ci191<-data.frame(ci191)
names(ci191)[3]<-paste("cice11")

cie201<-coefci(ols7j, parm=c("temp_both"), vcov = vcovCL(ols7j, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice201<-ols7j$coefficients[c("temp_both")]
ci201<-cbind(cie201,cice201)
event<-c("a (both-temperature DDD)")
ci201<-cbind(ci201,event)
ci201<-data.frame(ci201)
names(ci201)[3]<-paste("cice11")

cie211<-coefci(ols7k, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols7k, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice211<-ols7k$coefficients[c("treatcontrad.-both:round2")]
ci211<-cbind(cie211,cice211)
event<-c("b (both-lower education)")
ci211<-cbind(ci211,event)
ci211<-data.frame(ci211)
names(ci211)[3]<-paste("cice11")

cie221<-coefci(ols7k, parm=c("study_both"), vcov = vcovCL(ols7k, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice221<-ols7k$coefficients[c("study_both")]
ci221<-cbind(cie221,cice221)
event<-c("b (both-education DDD)")
ci221<-cbind(ci221,event)
ci221<-data.frame(ci221)
names(ci221)[3]<-paste("cice11")

cie231<-coefci(ols7l, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols7l, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice231<-ols7l$coefficients[c("treatcontrad.-both:round2")]
ci231<-cbind(cie231,cice231)
event<-c("a (both-lower education)")
ci231<-cbind(ci231,event)
ci231<-data.frame(ci231)
names(ci231)[3]<-paste("cice11")

cie241<-coefci(ols7l, parm=c("study_both"), vcov = vcovCL(ols7l, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice241<-ols7l$coefficients[c("study_both")]
ci241<-cbind(cie241,cice241)
event<-c("a (both-education DDD)")
ci241<-cbind(ci241,event)
ci241<-data.frame(ci241)
names(ci241)[3]<-paste("cice11")

ciful<-rbind(ci11,ci31,ci21,ci41,ci51,ci61,ci71,ci81,ci91,ci101,ci111,ci121,ci131,ci141,ci151,ci161,ci171,ci181,ci191,ci201,ci211,ci221,ci231,ci241)
ciful$X2.5..<-as.numeric(ciful$X2.5..)
ciful$X97.5..<-as.numeric(ciful$X97.5..)
ciful$cice11<-as.numeric(ciful$cice11)
ciful$controls<-"1.) none"

#with demographic variables
cie11<-coefci(ols8, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols8, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice11<-ols8$coefficients[c("treatcontrad.-both:round2")]
ci11<-cbind(cie11,cice11)
event<-c("b (both-not female)")
ci11<-cbind(ci11,event)
ci11<-data.frame(ci11)

cie31<-coefci(ols8, parm=c("female_both"), vcov = vcovCL(ols8, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice31<-ols8$coefficients[c("female_both")]
ci31<-cbind(cie31,cice31)
event<-c("b (both-female DDD)")
ci31<-cbind(ci31,event)
ci31<-data.frame(ci31)
names(ci31)[3]<-paste("cice11")

cie21<-coefci(ols8a, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols8a, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice21<-ols8a$coefficients[c("treatcontrad.-both:round2")]
ci21<-cbind(cie21,cice21)
event<-c("a (both-not female)")
ci21<-cbind(ci21,event)
ci21<-data.frame(ci21)
names(ci21)[3]<-paste("cice11")

cie41<-coefci(ols8a, parm=c("female_both"), vcov = vcovCL(ols8a, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice41<-ols8a$coefficients[c("female_both")]
ci41<-cbind(cie41,cice41)
event<-c("a (both-female DDD)")
ci41<-cbind(ci41,event)
ci41<-data.frame(ci41)
names(ci41)[3]<-paste("cice11")


cie51<-coefci(ols8c, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols8c, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice51<-ols8c$coefficients[c("treatcontrad.-both:round2")]
ci51<-cbind(cie51,cice51)
event<-c("b (both-less accurate)")
ci51<-cbind(ci51,event)
ci51<-data.frame(ci51)
names(ci51)[3]<-paste("cice11")

cie61<-coefci(ols8c, parm=c("accu_both"), vcov = vcovCL(ols8c, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice61<-ols8c$coefficients[c("accu_both")]
ci61<-cbind(cie61,cice61)
event<-c("b (both-accurate DDD)")
ci61<-cbind(ci61,event)
ci61<-data.frame(ci61)
names(ci61)[3]<-paste("cice11")


cie71<-coefci(ols8d, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols8d, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice71<-ols8d$coefficients[c("treatcontrad.-both:round2")]
ci71<-cbind(cie71,cice71)
event<-c("a (both-less accurate)")
ci71<-cbind(ci71,event)
ci71<-data.frame(ci71)
names(ci71)[3]<-paste("cice11")

cie81<-coefci(ols8d, parm=c("accu_both"), vcov = vcovCL(ols8d, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice81<-ols8d$coefficients[c("accu_both")]
ci81<-cbind(cie81,cice81)
event<-c("a (both-accurate DDD)")
ci81<-cbind(ci81,event)
ci81<-data.frame(ci81)
names(ci81)[3]<-paste("cice11")

cie91<-coefci(ols8e, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols8e, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice91<-ols8e$coefficients[c("treatcontrad.-both:round2")]
ci91<-cbind(cie91,cice91)
event<-c("b (both-less credible)")
ci91<-cbind(ci91,event)
ci91<-data.frame(ci91)
names(ci91)[3]<-paste("cice11")

cie101<-coefci(ols8e, parm=c("cred_both"), vcov = vcovCL(ols8e, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice101<-ols8e$coefficients[c("cred_both")]
ci101<-cbind(cie101,cice101)
event<-c("b (both-credible DDD)")
ci101<-cbind(ci101,event)
ci101<-data.frame(ci101)
names(ci101)[3]<-paste("cice11")

cie111<-coefci(ols8f, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols8f, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice111<-ols8f$coefficients[c("treatcontrad.-both:round2")]
ci111<-cbind(cie111,cice111)
event<-c("a (both-less credible)")
ci111<-cbind(ci111,event)
ci111<-data.frame(ci111)
names(ci111)[3]<-paste("cice11")

cie121<-coefci(ols8f, parm=c("cred_both"), vcov = vcovCL(ols8f, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice121<-ols8f$coefficients[c("cred_both")]
ci121<-cbind(cie121,cice121)
event<-c("a (both-credible DDD)")
ci121<-cbind(ci121,event)
ci121<-data.frame(ci121)
names(ci121)[3]<-paste("cice11")

cie131<-coefci(ols8g, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols8g, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice131<-ols8g$coefficients[c("treatcontrad.-both:round2")]
ci131<-cbind(cie131,cice131)
event<-c("b (both-less forecast usage)")
ci131<-cbind(ci131,event)
ci131<-data.frame(ci131)
names(ci131)[3]<-paste("cice11")

cie141<-coefci(ols8g, parm=c("fore_both"), vcov = vcovCL(ols8g, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice141<-ols8g$coefficients[c("fore_both")]
ci141<-cbind(cie141,cice141)
event<-c("b (both-forecast usage DDD)")
ci141<-cbind(ci141,event)
ci141<-data.frame(ci141)
names(ci141)[3]<-paste("cice11")

cie151<-coefci(ols8h, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols8h, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice151<-ols8h$coefficients[c("treatcontrad.-both:round2")]
ci151<-cbind(cie151,cice151)
event<-c("a (both-less forecast usage)")
ci151<-cbind(ci151,event)
ci151<-data.frame(ci151)
names(ci151)[3]<-paste("cice11")

cie161<-coefci(ols8h, parm=c("fore_both"), vcov = vcovCL(ols8h, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice161<-ols8h$coefficients[c("fore_both")]
ci161<-cbind(cie161,cice161)
event<-c("a (both-forecast usage DDD)")
ci161<-cbind(ci161,event)
ci161<-data.frame(ci161)
names(ci161)[3]<-paste("cice11")

cie171<-coefci(ols8i, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols8i, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice171<-ols8i$coefficients[c("treatcontrad.-both:round2")]
ci171<-cbind(cie171,cice171)
event<-c("b (both-lower temperature)")
ci171<-cbind(ci171,event)
ci171<-data.frame(ci171)
names(ci171)[3]<-paste("cice11")

cie181<-coefci(ols8i, parm=c("temp_both"), vcov = vcovCL(ols8i, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice181<-ols8i$coefficients[c("temp_both")]
ci181<-cbind(cie181,cice181)
event<-c("b (both-temperature DDD)")
ci181<-cbind(ci181,event)
ci181<-data.frame(ci181)
names(ci181)[3]<-paste("cice11")

cie191<-coefci(ols8j, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols8j, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice191<-ols8j$coefficients[c("treatcontrad.-both:round2")]
ci191<-cbind(cie191,cice191)
event<-c("a (both-lower temperature)")
ci191<-cbind(ci191,event)
ci191<-data.frame(ci191)
names(ci191)[3]<-paste("cice11")

cie201<-coefci(ols8j, parm=c("temp_both"), vcov = vcovCL(ols8j, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice201<-ols8j$coefficients[c("temp_both")]
ci201<-cbind(cie201,cice201)
event<-c("a (both-temperature DDD)")
ci201<-cbind(ci201,event)
ci201<-data.frame(ci201)
names(ci201)[3]<-paste("cice11")

cie211<-coefci(ols8k, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols8k, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice211<-ols8k$coefficients[c("treatcontrad.-both:round2")]
ci211<-cbind(cie211,cice211)
event<-c("b (both-lower education)")
ci211<-cbind(ci211,event)
ci211<-data.frame(ci211)
names(ci211)[3]<-paste("cice11")

cie221<-coefci(ols8k, parm=c("study_both"), vcov = vcovCL(ols8k, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice221<-ols8k$coefficients[c("study_both")]
ci221<-cbind(cie221,cice221)
event<-c("b (both-education DDD)")
ci221<-cbind(ci221,event)
ci221<-data.frame(ci221)
names(ci221)[3]<-paste("cice11")

cie231<-coefci(ols8l, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols8l, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice231<-ols8l$coefficients[c("treatcontrad.-both:round2")]
ci231<-cbind(cie231,cice231)
event<-c("a (both-lower education)")
ci231<-cbind(ci231,event)
ci231<-data.frame(ci231)
names(ci231)[3]<-paste("cice11")

cie241<-coefci(ols8l, parm=c("study_both"), vcov = vcovCL(ols8l, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice241<-ols8l$coefficients[c("study_both")]
ci241<-cbind(cie241,cice241)
event<-c("a (both-education DDD)")
ci241<-cbind(ci241,event)
ci241<-data.frame(ci241)
names(ci241)[3]<-paste("cice11")


ciful3<-rbind(ci11,ci31,ci21,ci41,ci51,ci61,ci71,ci81,ci91,ci101,ci111,ci121,ci131,ci141,ci151,ci161,ci171,ci181,ci191,ci201,ci211,ci221,ci231,ci241)
ciful3$X2.5..<-as.numeric(ciful3$X2.5..)
ciful3$X97.5..<-as.numeric(ciful3$X97.5..)
ciful3$cice11<-as.numeric(ciful3$cice11)
ciful3$controls<-"2.) demographic"

ciful<-rbind(ciful,ciful3)


#with demographic variables + further controls
cie11<-coefci(ols9, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols9, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice11<-ols9$coefficients[c("treatcontrad.-both:round2")]
ci11<-cbind(cie11,cice11)
event<-c("b (both-not female)")
ci11<-cbind(ci11,event)
ci11<-data.frame(ci11)

cie31<-coefci(ols9, parm=c("female_both"), vcov = vcovCL(ols9, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice31<-ols9$coefficients[c("female_both")]
ci31<-cbind(cie31,cice31)
event<-c("b (both-female DDD)")
ci31<-cbind(ci31,event)
ci31<-data.frame(ci31)
names(ci31)[3]<-paste("cice11")

cie21<-coefci(ols9a, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols9a, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice21<-ols9a$coefficients[c("treatcontrad.-both:round2")]
ci21<-cbind(cie21,cice21)
event<-c("a (both-not female)")
ci21<-cbind(ci21,event)
ci21<-data.frame(ci21)
names(ci21)[3]<-paste("cice11")

cie41<-coefci(ols9a, parm=c("female_both"), vcov = vcovCL(ols9a, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice41<-ols9a$coefficients[c("female_both")]
ci41<-cbind(cie41,cice41)
event<-c("a (both-female DDD)")
ci41<-cbind(ci41,event)
ci41<-data.frame(ci41)
names(ci41)[3]<-paste("cice11")


cie51<-coefci(ols9c, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols9c, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice51<-ols9c$coefficients[c("treatcontrad.-both:round2")]
ci51<-cbind(cie51,cice51)
event<-c("b (both-less accurate)")
ci51<-cbind(ci51,event)
ci51<-data.frame(ci51)
names(ci51)[3]<-paste("cice11")

cie61<-coefci(ols9c, parm=c("accu_both"), vcov = vcovCL(ols9c, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice61<-ols9c$coefficients[c("accu_both")]
ci61<-cbind(cie61,cice61)
event<-c("b (both-accurate DDD)")
ci61<-cbind(ci61,event)
ci61<-data.frame(ci61)
names(ci61)[3]<-paste("cice11")


cie71<-coefci(ols9d, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols9d, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice71<-ols9d$coefficients[c("treatcontrad.-both:round2")]
ci71<-cbind(cie71,cice71)
event<-c("a (both-less accurate)")
ci71<-cbind(ci71,event)
ci71<-data.frame(ci71)
names(ci71)[3]<-paste("cice11")

cie81<-coefci(ols9d, parm=c("accu_both"), vcov = vcovCL(ols9d, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice81<-ols9d$coefficients[c("accu_both")]
ci81<-cbind(cie81,cice81)
event<-c("a (both-accurate DDD)")
ci81<-cbind(ci81,event)
ci81<-data.frame(ci81)
names(ci81)[3]<-paste("cice11")

cie91<-coefci(ols9e, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols9e, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice91<-ols9e$coefficients[c("treatcontrad.-both:round2")]
ci91<-cbind(cie91,cice91)
event<-c("b (both-less credible)")
ci91<-cbind(ci91,event)
ci91<-data.frame(ci91)
names(ci91)[3]<-paste("cice11")

cie101<-coefci(ols9e, parm=c("cred_both"), vcov = vcovCL(ols9e, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice101<-ols9e$coefficients[c("cred_both")]
ci101<-cbind(cie101,cice101)
event<-c("b (both-credible DDD)")
ci101<-cbind(ci101,event)
ci101<-data.frame(ci101)
names(ci101)[3]<-paste("cice11")

cie111<-coefci(ols9f, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols9f, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice111<-ols9f$coefficients[c("treatcontrad.-both:round2")]
ci111<-cbind(cie111,cice111)
event<-c("a (both-less credible)")
ci111<-cbind(ci111,event)
ci111<-data.frame(ci111)
names(ci111)[3]<-paste("cice11")

cie121<-coefci(ols9f, parm=c("cred_both"), vcov = vcovCL(ols9f, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice121<-ols9f$coefficients[c("cred_both")]
ci121<-cbind(cie121,cice121)
event<-c("a (both-credible DDD)")
ci121<-cbind(ci121,event)
ci121<-data.frame(ci121)
names(ci121)[3]<-paste("cice11")

cie131<-coefci(ols9g, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols9g, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice131<-ols9g$coefficients[c("treatcontrad.-both:round2")]
ci131<-cbind(cie131,cice131)
event<-c("b (both-less forecast usage)")
ci131<-cbind(ci131,event)
ci131<-data.frame(ci131)
names(ci131)[3]<-paste("cice11")

cie141<-coefci(ols9g, parm=c("fore_both"), vcov = vcovCL(ols9g, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice141<-ols9g$coefficients[c("fore_both")]
ci141<-cbind(cie141,cice141)
event<-c("b (both-forecast usage DDD)")
ci141<-cbind(ci141,event)
ci141<-data.frame(ci141)
names(ci141)[3]<-paste("cice11")

cie151<-coefci(ols9h, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols9h, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice151<-ols9h$coefficients[c("treatcontrad.-both:round2")]
ci151<-cbind(cie151,cice151)
event<-c("a (both-less forecast usage)")
ci151<-cbind(ci151,event)
ci151<-data.frame(ci151)
names(ci151)[3]<-paste("cice11")

cie161<-coefci(ols9h, parm=c("fore_both"), vcov = vcovCL(ols9h, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice161<-ols9h$coefficients[c("fore_both")]
ci161<-cbind(cie161,cice161)
event<-c("a (both-forecast usage DDD)")
ci161<-cbind(ci161,event)
ci161<-data.frame(ci161)
names(ci161)[3]<-paste("cice11")

cie171<-coefci(ols9i, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols9i, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice171<-ols9i$coefficients[c("treatcontrad.-both:round2")]
ci171<-cbind(cie171,cice171)
event<-c("b (both-lower temperature)")
ci171<-cbind(ci171,event)
ci171<-data.frame(ci171)
names(ci171)[3]<-paste("cice11")

cie181<-coefci(ols9i, parm=c("temp_both"), vcov = vcovCL(ols9i, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice181<-ols9i$coefficients[c("temp_both")]
ci181<-cbind(cie181,cice181)
event<-c("b (both-temperature DDD)")
ci181<-cbind(ci181,event)
ci181<-data.frame(ci181)
names(ci181)[3]<-paste("cice11")

cie191<-coefci(ols9j, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols9j, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice191<-ols9j$coefficients[c("treatcontrad.-both:round2")]
ci191<-cbind(cie191,cice191)
event<-c("a (both-lower temperature)")
ci191<-cbind(ci191,event)
ci191<-data.frame(ci191)
names(ci191)[3]<-paste("cice11")

cie201<-coefci(ols9j, parm=c("temp_both"), vcov = vcovCL(ols9j, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice201<-ols9j$coefficients[c("temp_both")]
ci201<-cbind(cie201,cice201)
event<-c("a (both-temperature DDD)")
ci201<-cbind(ci201,event)
ci201<-data.frame(ci201)
names(ci201)[3]<-paste("cice11")

cie211<-coefci(ols9k, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols9k, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice211<-ols9k$coefficients[c("treatcontrad.-both:round2")]
ci211<-cbind(cie211,cice211)
event<-c("b (both-lower education)")
ci211<-cbind(ci211,event)
ci211<-data.frame(ci211)
names(ci211)[3]<-paste("cice11")

cie221<-coefci(ols9k, parm=c("study_both"), vcov = vcovCL(ols9k, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice221<-ols9k$coefficients[c("study_both")]
ci221<-cbind(cie221,cice221)
event<-c("b (both-education DDD)")
ci221<-cbind(ci221,event)
ci221<-data.frame(ci221)
names(ci221)[3]<-paste("cice11")

cie231<-coefci(ols9l, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols9l, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice231<-ols9l$coefficients[c("treatcontrad.-both:round2")]
ci231<-cbind(cie231,cice231)
event<-c("a (both-lower education)")
ci231<-cbind(ci231,event)
ci231<-data.frame(ci231)
names(ci231)[3]<-paste("cice11")

cie241<-coefci(ols9l, parm=c("study_both"), vcov = vcovCL(ols9l, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice241<-ols9l$coefficients[c("study_both")]
ci241<-cbind(cie241,cice241)
event<-c("a (both-education DDD)")
ci241<-cbind(ci241,event)
ci241<-data.frame(ci241)
names(ci241)[3]<-paste("cice11")




ciful3<-rbind(ci11,ci31,ci21,ci41,ci51,ci61,ci71,ci81,ci91,ci101,ci111,ci121,ci131,ci141,ci151,ci161,ci171,ci181,ci191,ci201,ci211,ci221,ci231,ci241)
ciful3$X2.5..<-as.numeric(ciful3$X2.5..)
ciful3$X97.5..<-as.numeric(ciful3$X97.5..)
ciful3$cice11<-as.numeric(ciful3$cice11)
ciful3$controls<-"3.) demographic + further"

cifula<-rbind(ciful,ciful3)

#Graph 2: Interval estimates
# no control variables
cie11<-coefci(ols7, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols7, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice11<-ols7$coefficients[c("treatcontrad.-interval:round2")]
ci11<-cbind(cie11,cice11)
event<-c("b (interval-not female)")
ci11<-cbind(ci11,event)
ci11<-data.frame(ci11)

cie31<-coefci(ols7, parm=c("female_int"), vcov = vcovCL(ols7, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice31<-ols7$coefficients[c("female_int")]
ci31<-cbind(cie31,cice31)
event<-c("b (interval-female DDD)")
ci31<-cbind(ci31,event)
ci31<-data.frame(ci31)
names(ci31)[3]<-paste("cice11")

cie21<-coefci(ols7a, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols7a, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice21<-ols7a$coefficients[c("treatcontrad.-interval:round2")]
ci21<-cbind(cie21,cice21)
event<-c("a (interval-not female)")
ci21<-cbind(ci21,event)
ci21<-data.frame(ci21)
names(ci21)[3]<-paste("cice11")

cie41<-coefci(ols7a, parm=c("female_int"), vcov = vcovCL(ols7a, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice41<-ols7a$coefficients[c("female_int")]
ci41<-cbind(cie41,cice41)
event<-c("a (interval-female DDD)")
ci41<-cbind(ci41,event)
ci41<-data.frame(ci41)
names(ci41)[3]<-paste("cice11")


cie51<-coefci(ols7c, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols7c, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice51<-ols7c$coefficients[c("treatcontrad.-interval:round2")]
ci51<-cbind(cie51,cice51)
event<-c("b (interval-less accurate)")
ci51<-cbind(ci51,event)
ci51<-data.frame(ci51)
names(ci51)[3]<-paste("cice11")

cie61<-coefci(ols7c, parm=c("accu_int"), vcov = vcovCL(ols7c, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice61<-ols7c$coefficients[c("accu_int")]
ci61<-cbind(cie61,cice61)
event<-c("b (interval-accurate DDD)")
ci61<-cbind(ci61,event)
ci61<-data.frame(ci61)
names(ci61)[3]<-paste("cice11")


cie71<-coefci(ols7d, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols7d, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice71<-ols7d$coefficients[c("treatcontrad.-interval:round2")]
ci71<-cbind(cie71,cice71)
event<-c("a (interval-less accurate)")
ci71<-cbind(ci71,event)
ci71<-data.frame(ci71)
names(ci71)[3]<-paste("cice11")

cie81<-coefci(ols7d, parm=c("accu_int"), vcov = vcovCL(ols7d, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice81<-ols7d$coefficients[c("accu_int")]
ci81<-cbind(cie81,cice81)
event<-c("a (interval-accurate DDD)")
ci81<-cbind(ci81,event)
ci81<-data.frame(ci81)
names(ci81)[3]<-paste("cice11")

cie91<-coefci(ols7e, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols7e, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice91<-ols7e$coefficients[c("treatcontrad.-interval:round2")]
ci91<-cbind(cie91,cice91)
event<-c("b (interval-less credible)")
ci91<-cbind(ci91,event)
ci91<-data.frame(ci91)
names(ci91)[3]<-paste("cice11")

cie101<-coefci(ols7e, parm=c("cred_int"), vcov = vcovCL(ols7e, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice101<-ols7e$coefficients[c("cred_int")]
ci101<-cbind(cie101,cice101)
event<-c("b (interval-credible DDD)")
ci101<-cbind(ci101,event)
ci101<-data.frame(ci101)
names(ci101)[3]<-paste("cice11")

cie111<-coefci(ols7f, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols7f, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice111<-ols7f$coefficients[c("treatcontrad.-interval:round2")]
ci111<-cbind(cie111,cice111)
event<-c("a (interval-less credible)")
ci111<-cbind(ci111,event)
ci111<-data.frame(ci111)
names(ci111)[3]<-paste("cice11")

cie121<-coefci(ols7f, parm=c("cred_int"), vcov = vcovCL(ols7f, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice121<-ols7f$coefficients[c("cred_int")]
ci121<-cbind(cie121,cice121)
event<-c("a (interval-credible DDD)")
ci121<-cbind(ci121,event)
ci121<-data.frame(ci121)
names(ci121)[3]<-paste("cice11")

cie131<-coefci(ols7g, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols7g, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice131<-ols7g$coefficients[c("treatcontrad.-interval:round2")]
ci131<-cbind(cie131,cice131)
event<-c("b (interval-less forecast usage)")
ci131<-cbind(ci131,event)
ci131<-data.frame(ci131)
names(ci131)[3]<-paste("cice11")

cie141<-coefci(ols7g, parm=c("fore_int"), vcov = vcovCL(ols7g, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice141<-ols7g$coefficients[c("fore_int")]
ci141<-cbind(cie141,cice141)
event<-c("b (interval-forecast usage DDD)")
ci141<-cbind(ci141,event)
ci141<-data.frame(ci141)
names(ci141)[3]<-paste("cice11")

cie151<-coefci(ols7h, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols7h, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice151<-ols7h$coefficients[c("treatcontrad.-interval:round2")]
ci151<-cbind(cie151,cice151)
event<-c("a (interval-less forecast usage)")
ci151<-cbind(ci151,event)
ci151<-data.frame(ci151)
names(ci151)[3]<-paste("cice11")

cie161<-coefci(ols7h, parm=c("fore_int"), vcov = vcovCL(ols7h, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice161<-ols7h$coefficients[c("fore_int")]
ci161<-cbind(cie161,cice161)
event<-c("a (interval-forecast usage DDD)")
ci161<-cbind(ci161,event)
ci161<-data.frame(ci161)
names(ci161)[3]<-paste("cice11")

cie171<-coefci(ols7i, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols7i, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice171<-ols7i$coefficients[c("treatcontrad.-interval:round2")]
ci171<-cbind(cie171,cice171)
event<-c("b (interval-lower temperature)")
ci171<-cbind(ci171,event)
ci171<-data.frame(ci171)
names(ci171)[3]<-paste("cice11")

cie181<-coefci(ols7i, parm=c("temp_int"), vcov = vcovCL(ols7i, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice181<-ols7i$coefficients[c("temp_int")]
ci181<-cbind(cie181,cice181)
event<-c("b (interval-temperature DDD)")
ci181<-cbind(ci181,event)
ci181<-data.frame(ci181)
names(ci181)[3]<-paste("cice11")

cie191<-coefci(ols7j, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols7j, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice191<-ols7j$coefficients[c("treatcontrad.-interval:round2")]
ci191<-cbind(cie191,cice191)
event<-c("a (interval-lower temperature)")
ci191<-cbind(ci191,event)
ci191<-data.frame(ci191)
names(ci191)[3]<-paste("cice11")

cie201<-coefci(ols7j, parm=c("temp_int"), vcov = vcovCL(ols7j, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice201<-ols7j$coefficients[c("temp_int")]
ci201<-cbind(cie201,cice201)
event<-c("a (interval-temperature DDD)")
ci201<-cbind(ci201,event)
ci201<-data.frame(ci201)
names(ci201)[3]<-paste("cice11")

cie211<-coefci(ols7k, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols7k, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice211<-ols7k$coefficients[c("treatcontrad.-interval:round2")]
ci211<-cbind(cie211,cice211)
event<-c("b (interval-lower education)")
ci211<-cbind(ci211,event)
ci211<-data.frame(ci211)
names(ci211)[3]<-paste("cice11")

cie221<-coefci(ols7k, parm=c("study_int"), vcov = vcovCL(ols7k, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice221<-ols7k$coefficients[c("study_int")]
ci221<-cbind(cie221,cice221)
event<-c("b (interval-education DDD)")
ci221<-cbind(ci221,event)
ci221<-data.frame(ci221)
names(ci221)[3]<-paste("cice11")

cie231<-coefci(ols7l, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols7l, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice231<-ols7l$coefficients[c("treatcontrad.-interval:round2")]
ci231<-cbind(cie231,cice231)
event<-c("a (interval-lower education)")
ci231<-cbind(ci231,event)
ci231<-data.frame(ci231)
names(ci231)[3]<-paste("cice11")

cie241<-coefci(ols7l, parm=c("study_int"), vcov = vcovCL(ols7l, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice241<-ols7l$coefficients[c("study_int")]
ci241<-cbind(cie241,cice241)
event<-c("a (interval-education DDD)")
ci241<-cbind(ci241,event)
ci241<-data.frame(ci241)
names(ci241)[3]<-paste("cice11")



ciful<-rbind(ci11,ci31,ci21,ci41,ci51,ci61,ci71,ci81,ci91,ci101,ci111,ci121,ci131,ci141,ci151,ci161,ci171,ci181,ci191,ci201,ci211,ci221,ci231,ci241)
ciful$X2.5..<-as.numeric(ciful$X2.5..)
ciful$X97.5..<-as.numeric(ciful$X97.5..)
ciful$cice11<-as.numeric(ciful$cice11)
ciful$controls<-"1.) none"

#with demographic variables
cie11<-coefci(ols8, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols8, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice11<-ols8$coefficients[c("treatcontrad.-interval:round2")]
ci11<-cbind(cie11,cice11)
event<-c("b (interval-not female)")
ci11<-cbind(ci11,event)
ci11<-data.frame(ci11)

cie31<-coefci(ols8, parm=c("female_int"), vcov = vcovCL(ols8, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice31<-ols8$coefficients[c("female_int")]
ci31<-cbind(cie31,cice31)
event<-c("b (interval-female DDD)")
ci31<-cbind(ci31,event)
ci31<-data.frame(ci31)
names(ci31)[3]<-paste("cice11")

cie21<-coefci(ols8a, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols8a, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice21<-ols8a$coefficients[c("treatcontrad.-interval:round2")]
ci21<-cbind(cie21,cice21)
event<-c("a (interval-not female)")
ci21<-cbind(ci21,event)
ci21<-data.frame(ci21)
names(ci21)[3]<-paste("cice11")

cie41<-coefci(ols8a, parm=c("female_int"), vcov = vcovCL(ols8a, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice41<-ols8a$coefficients[c("female_int")]
ci41<-cbind(cie41,cice41)
event<-c("a (interval-female DDD)")
ci41<-cbind(ci41,event)
ci41<-data.frame(ci41)
names(ci41)[3]<-paste("cice11")


cie51<-coefci(ols8c, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols8c, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice51<-ols8c$coefficients[c("treatcontrad.-interval:round2")]
ci51<-cbind(cie51,cice51)
event<-c("b (interval-less accurate)")
ci51<-cbind(ci51,event)
ci51<-data.frame(ci51)
names(ci51)[3]<-paste("cice11")

cie61<-coefci(ols8c, parm=c("accu_int"), vcov = vcovCL(ols8c, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice61<-ols8c$coefficients[c("accu_int")]
ci61<-cbind(cie61,cice61)
event<-c("b (interval-accurate DDD)")
ci61<-cbind(ci61,event)
ci61<-data.frame(ci61)
names(ci61)[3]<-paste("cice11")


cie71<-coefci(ols8d, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols8d, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice71<-ols8d$coefficients[c("treatcontrad.-interval:round2")]
ci71<-cbind(cie71,cice71)
event<-c("a (interval-less accurate)")
ci71<-cbind(ci71,event)
ci71<-data.frame(ci71)
names(ci71)[3]<-paste("cice11")

cie81<-coefci(ols8d, parm=c("accu_int"), vcov = vcovCL(ols8d, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice81<-ols8d$coefficients[c("accu_int")]
ci81<-cbind(cie81,cice81)
event<-c("a (interval-accurate DDD)")
ci81<-cbind(ci81,event)
ci81<-data.frame(ci81)
names(ci81)[3]<-paste("cice11")

cie91<-coefci(ols8e, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols8e, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice91<-ols8e$coefficients[c("treatcontrad.-interval:round2")]
ci91<-cbind(cie91,cice91)
event<-c("b (interval-less credible)")
ci91<-cbind(ci91,event)
ci91<-data.frame(ci91)
names(ci91)[3]<-paste("cice11")

cie101<-coefci(ols8e, parm=c("cred_int"), vcov = vcovCL(ols8e, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice101<-ols8e$coefficients[c("cred_int")]
ci101<-cbind(cie101,cice101)
event<-c("b (interval-credible DDD)")
ci101<-cbind(ci101,event)
ci101<-data.frame(ci101)
names(ci101)[3]<-paste("cice11")

cie111<-coefci(ols8f, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols8f, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice111<-ols8f$coefficients[c("treatcontrad.-interval:round2")]
ci111<-cbind(cie111,cice111)
event<-c("a (interval-less credible)")
ci111<-cbind(ci111,event)
ci111<-data.frame(ci111)
names(ci111)[3]<-paste("cice11")

cie121<-coefci(ols8f, parm=c("cred_int"), vcov = vcovCL(ols8f, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice121<-ols8f$coefficients[c("cred_int")]
ci121<-cbind(cie121,cice121)
event<-c("a (interval-credible DDD)")
ci121<-cbind(ci121,event)
ci121<-data.frame(ci121)
names(ci121)[3]<-paste("cice11")

cie131<-coefci(ols8g, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols8g, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice131<-ols8g$coefficients[c("treatcontrad.-interval:round2")]
ci131<-cbind(cie131,cice131)
event<-c("b (interval-less forecast usage)")
ci131<-cbind(ci131,event)
ci131<-data.frame(ci131)
names(ci131)[3]<-paste("cice11")

cie141<-coefci(ols8g, parm=c("fore_int"), vcov = vcovCL(ols8g, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice141<-ols8g$coefficients[c("fore_int")]
ci141<-cbind(cie141,cice141)
event<-c("b (interval-forecast usage DDD)")
ci141<-cbind(ci141,event)
ci141<-data.frame(ci141)
names(ci141)[3]<-paste("cice11")

cie151<-coefci(ols8h, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols8h, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice151<-ols8h$coefficients[c("treatcontrad.-interval:round2")]
ci151<-cbind(cie151,cice151)
event<-c("a (interval-less forecast usage)")
ci151<-cbind(ci151,event)
ci151<-data.frame(ci151)
names(ci151)[3]<-paste("cice11")

cie161<-coefci(ols8h, parm=c("fore_int"), vcov = vcovCL(ols8h, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice161<-ols8h$coefficients[c("fore_int")]
ci161<-cbind(cie161,cice161)
event<-c("a (interval-forecast usage DDD)")
ci161<-cbind(ci161,event)
ci161<-data.frame(ci161)
names(ci161)[3]<-paste("cice11")

cie171<-coefci(ols8i, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols8i, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice171<-ols8i$coefficients[c("treatcontrad.-interval:round2")]
ci171<-cbind(cie171,cice171)
event<-c("b (interval-lower temperature)")
ci171<-cbind(ci171,event)
ci171<-data.frame(ci171)
names(ci171)[3]<-paste("cice11")

cie181<-coefci(ols8i, parm=c("temp_int"), vcov = vcovCL(ols8i, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice181<-ols8i$coefficients[c("temp_int")]
ci181<-cbind(cie181,cice181)
event<-c("b (interval-temperature DDD)")
ci181<-cbind(ci181,event)
ci181<-data.frame(ci181)
names(ci181)[3]<-paste("cice11")

cie191<-coefci(ols8j, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols8j, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice191<-ols8j$coefficients[c("treatcontrad.-interval:round2")]
ci191<-cbind(cie191,cice191)
event<-c("a (interval-lower temperature)")
ci191<-cbind(ci191,event)
ci191<-data.frame(ci191)
names(ci191)[3]<-paste("cice11")

cie201<-coefci(ols8j, parm=c("temp_int"), vcov = vcovCL(ols8j, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice201<-ols8j$coefficients[c("temp_int")]
ci201<-cbind(cie201,cice201)
event<-c("a (interval-temperature DDD)")
ci201<-cbind(ci201,event)
ci201<-data.frame(ci201)
names(ci201)[3]<-paste("cice11")

cie211<-coefci(ols8k, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols8k, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice211<-ols8k$coefficients[c("treatcontrad.-interval:round2")]
ci211<-cbind(cie211,cice211)
event<-c("b (interval-lower education)")
ci211<-cbind(ci211,event)
ci211<-data.frame(ci211)
names(ci211)[3]<-paste("cice11")

cie221<-coefci(ols8k, parm=c("study_int"), vcov = vcovCL(ols8k, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice221<-ols8k$coefficients[c("study_int")]
ci221<-cbind(cie221,cice221)
event<-c("b (interval-education DDD)")
ci221<-cbind(ci221,event)
ci221<-data.frame(ci221)
names(ci221)[3]<-paste("cice11")

cie231<-coefci(ols8l, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols8l, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice231<-ols8l$coefficients[c("treatcontrad.-interval:round2")]
ci231<-cbind(cie231,cice231)
event<-c("a (interval-lower education)")
ci231<-cbind(ci231,event)
ci231<-data.frame(ci231)
names(ci231)[3]<-paste("cice11")

cie241<-coefci(ols8l, parm=c("study_int"), vcov = vcovCL(ols8l, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice241<-ols8l$coefficients[c("study_int")]
ci241<-cbind(cie241,cice241)
event<-c("a (interval-education DDD)")
ci241<-cbind(ci241,event)
ci241<-data.frame(ci241)
names(ci241)[3]<-paste("cice11")


ciful3<-rbind(ci11,ci31,ci21,ci41,ci51,ci61,ci71,ci81,ci91,ci101,ci111,ci121,ci131,ci141,ci151,ci161,ci171,ci181,ci191,ci201,ci211,ci221,ci231,ci241)
ciful3$X2.5..<-as.numeric(ciful3$X2.5..)
ciful3$X97.5..<-as.numeric(ciful3$X97.5..)
ciful3$cice11<-as.numeric(ciful3$cice11)
ciful3$controls<-"2.) demographic"

ciful<-rbind(ciful,ciful3)


#with demographic variables + further controls
cie11<-coefci(ols9, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols9, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice11<-ols9$coefficients[c("treatcontrad.-interval:round2")]
ci11<-cbind(cie11,cice11)
event<-c("b (interval-not female)")
ci11<-cbind(ci11,event)
ci11<-data.frame(ci11)

cie31<-coefci(ols9, parm=c("female_int"), vcov = vcovCL(ols9, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice31<-ols9$coefficients[c("female_int")]
ci31<-cbind(cie31,cice31)
event<-c("b (interval-female DDD)")
ci31<-cbind(ci31,event)
ci31<-data.frame(ci31)
names(ci31)[3]<-paste("cice11")

cie21<-coefci(ols9a, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols9a, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice21<-ols9a$coefficients[c("treatcontrad.-interval:round2")]
ci21<-cbind(cie21,cice21)
event<-c("a (interval-not female)")
ci21<-cbind(ci21,event)
ci21<-data.frame(ci21)
names(ci21)[3]<-paste("cice11")

cie41<-coefci(ols9a, parm=c("female_int"), vcov = vcovCL(ols9a, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice41<-ols9a$coefficients[c("female_int")]
ci41<-cbind(cie41,cice41)
event<-c("a (interval-female DDD)")
ci41<-cbind(ci41,event)
ci41<-data.frame(ci41)
names(ci41)[3]<-paste("cice11")


cie51<-coefci(ols9c, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols9c, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice51<-ols9c$coefficients[c("treatcontrad.-interval:round2")]
ci51<-cbind(cie51,cice51)
event<-c("b (interval-less accurate)")
ci51<-cbind(ci51,event)
ci51<-data.frame(ci51)
names(ci51)[3]<-paste("cice11")

cie61<-coefci(ols9c, parm=c("accu_int"), vcov = vcovCL(ols9c, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice61<-ols9c$coefficients[c("accu_int")]
ci61<-cbind(cie61,cice61)
event<-c("b (interval-accurate DDD)")
ci61<-cbind(ci61,event)
ci61<-data.frame(ci61)
names(ci61)[3]<-paste("cice11")


cie71<-coefci(ols9d, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols9d, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice71<-ols9d$coefficients[c("treatcontrad.-interval:round2")]
ci71<-cbind(cie71,cice71)
event<-c("a (interval-less accurate)")
ci71<-cbind(ci71,event)
ci71<-data.frame(ci71)
names(ci71)[3]<-paste("cice11")

cie81<-coefci(ols9d, parm=c("accu_int"), vcov = vcovCL(ols9d, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice81<-ols9d$coefficients[c("accu_int")]
ci81<-cbind(cie81,cice81)
event<-c("a (interval-accurate DDD)")
ci81<-cbind(ci81,event)
ci81<-data.frame(ci81)
names(ci81)[3]<-paste("cice11")

cie91<-coefci(ols9e, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols9e, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice91<-ols9e$coefficients[c("treatcontrad.-interval:round2")]
ci91<-cbind(cie91,cice91)
event<-c("b (interval-less credible)")
ci91<-cbind(ci91,event)
ci91<-data.frame(ci91)
names(ci91)[3]<-paste("cice11")

cie101<-coefci(ols9e, parm=c("cred_int"), vcov = vcovCL(ols9e, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice101<-ols9e$coefficients[c("cred_int")]
ci101<-cbind(cie101,cice101)
event<-c("b (interval-credible DDD)")
ci101<-cbind(ci101,event)
ci101<-data.frame(ci101)
names(ci101)[3]<-paste("cice11")

cie111<-coefci(ols9f, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols9f, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice111<-ols9f$coefficients[c("treatcontrad.-interval:round2")]
ci111<-cbind(cie111,cice111)
event<-c("a (interval-less credible)")
ci111<-cbind(ci111,event)
ci111<-data.frame(ci111)
names(ci111)[3]<-paste("cice11")

cie121<-coefci(ols9f, parm=c("cred_int"), vcov = vcovCL(ols9f, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice121<-ols9f$coefficients[c("cred_int")]
ci121<-cbind(cie121,cice121)
event<-c("a (interval-credible DDD)")
ci121<-cbind(ci121,event)
ci121<-data.frame(ci121)
names(ci121)[3]<-paste("cice11")

cie131<-coefci(ols9g, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols9g, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice131<-ols9g$coefficients[c("treatcontrad.-interval:round2")]
ci131<-cbind(cie131,cice131)
event<-c("b (interval-less forecast usage)")
ci131<-cbind(ci131,event)
ci131<-data.frame(ci131)
names(ci131)[3]<-paste("cice11")

cie141<-coefci(ols9g, parm=c("fore_int"), vcov = vcovCL(ols9g, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice141<-ols9g$coefficients[c("fore_int")]
ci141<-cbind(cie141,cice141)
event<-c("b (interval-forecast usage DDD)")
ci141<-cbind(ci141,event)
ci141<-data.frame(ci141)
names(ci141)[3]<-paste("cice11")

cie151<-coefci(ols9h, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols9h, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice151<-ols9h$coefficients[c("treatcontrad.-interval:round2")]
ci151<-cbind(cie151,cice151)
event<-c("a (interval-less forecast usage)")
ci151<-cbind(ci151,event)
ci151<-data.frame(ci151)
names(ci151)[3]<-paste("cice11")

cie161<-coefci(ols9h, parm=c("fore_int"), vcov = vcovCL(ols9h, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice161<-ols9h$coefficients[c("fore_int")]
ci161<-cbind(cie161,cice161)
event<-c("a (interval-forecast usage DDD)")
ci161<-cbind(ci161,event)
ci161<-data.frame(ci161)
names(ci161)[3]<-paste("cice11")

cie171<-coefci(ols9i, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols9i, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice171<-ols9i$coefficients[c("treatcontrad.-interval:round2")]
ci171<-cbind(cie171,cice171)
event<-c("b (interval-lower temperature)")
ci171<-cbind(ci171,event)
ci171<-data.frame(ci171)
names(ci171)[3]<-paste("cice11")

cie181<-coefci(ols9i, parm=c("temp_int"), vcov = vcovCL(ols9i, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice181<-ols9i$coefficients[c("temp_int")]
ci181<-cbind(cie181,cice181)
event<-c("b (interval-temperature DDD)")
ci181<-cbind(ci181,event)
ci181<-data.frame(ci181)
names(ci181)[3]<-paste("cice11")

cie191<-coefci(ols9j, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols9j, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice191<-ols9j$coefficients[c("treatcontrad.-interval:round2")]
ci191<-cbind(cie191,cice191)
event<-c("a (interval-lower temperature)")
ci191<-cbind(ci191,event)
ci191<-data.frame(ci191)
names(ci191)[3]<-paste("cice11")

cie201<-coefci(ols9j, parm=c("temp_int"), vcov = vcovCL(ols9j, cluster=~ful3$participant.label, type="HC1"),level=0.95)
cice201<-ols9j$coefficients[c("temp_int")]
ci201<-cbind(cie201,cice201)
event<-c("a (interval-temperature DDD)")
ci201<-cbind(ci201,event)
ci201<-data.frame(ci201)
names(ci201)[3]<-paste("cice11")

ciful3<-rbind(ci11,ci31,ci21,ci41,ci51,ci61,ci71,ci81,ci91,ci101,ci111,ci121,ci131,ci141,ci151,ci161,ci171,ci181,ci191,ci201,ci211,ci221,ci231,ci241)
ciful3$X2.5..<-as.numeric(ciful3$X2.5..)
ciful3$X97.5..<-as.numeric(ciful3$X97.5..)
ciful3$cice11<-as.numeric(ciful3$cice11)
ciful3$controls<-"3.) demographic + further"

ciful<-rbind(ciful,ciful3)

ciful<-rbind(cifula,ciful)

ciful$controls<- with(ciful, factor(controls, levels=c("1.) none","2.) demographic", "3.) demographic + further")))


library(ggplot2)
ggplot(
  mapping = aes(
    y = ciful$event
  )
) +
  scale_y_discrete( limits=c("a (both-not female)","a (both-female DDD)", "b (both-not female)", "b (both-female DDD)","a (both-less accurate)","a (both-accurate DDD)", "b (both-less accurate)", "b (both-accurate DDD)","a (both-less credible)","a (both-credible DDD)", "b (both-less credible)", "b (both-credible DDD)","a (both-less forecast usage)","a (both-forecast usage DDD)", "b (both-less forecast usage)", "b (both-forecast usage DDD)","a (both-lower temperature)","a (both-temperature DDD)", "b (both-lower temperature)", "b (both-temperature DDD)","a (both-lower education)","a (both-education DDD)", "b (both-lower education)", "b (both-education DDD)", "a (interval-not female)","a (interval-female DDD)", "b (interval-not female)", "b (interval-female DDD)","a (interval-less accurate)","a (interval-accurate DDD)", "b (interval-less accurate)", "b (interval-accurate DDD)","a (interval-less credible)","a (interval-credible DDD)", "b (interval-less credible)", "b (interval-credible DDD)","a (interval-less forecast usage)","a (interval-forecast usage DDD)", "b (interval-less forecast usage)", "b (interval-forecast usage DDD)","a (interval-lower temperature)","a (interval-temperature DDD)", "b (interval-lower temperature)", "b (interval-temperature DDD)","a (interval-lower education)","a (interval-education DDD)", "b (interval-lower education)", "b (interval-education DDD)"))+
  geom_pointrange(aes(
    x = ciful$cice11,
    y = ciful$event,
    xmin = ciful$X2.5..,
    xmax = ciful$X97.5..,
    shape=factor(ciful$controls, levels=c("3.) demographic + further","2.) demographic", "1.) none"))
  ), 
  position = position_dodge(width = 0.4),
  
  # Optional decoration:
  fatten=5,
  alpha=.8
  ) +
  
  # Add a line marker for y = 0 (to see if the CI overlaps 0)
  geom_vline(xintercept = 0, color = "red", alpha = 0.2, show.legend=F) +
  # Additional decoration:
  theme_bw() +
  # guides(linetype=guide_legend(override.aes = list(shape = NA))) + 
  theme(
    plot.title = element_text(face = "bold", size = 12),
    legend.background = element_rect(fill = "white", size = 4, colour = "white"),
    #legend.justification = c(0, 1),
    axis.ticks = element_line(colour = "white", size = 0.1),
    panel.grid.major = element_line(colour = "white", size = 0.07),
    panel.grid.minor = element_blank(),
    legend.key.size = unit(1, "lines"),
    legend.box = 'horizontal',
    legend.position = "top"
  ) +
  labs(y = "Ambiguity Index", x = "Estimate",shape="Control variables")+
  scale_shape_manual(values = c("1.) none" = 15, "2.) demographic" = 16,"3.) demographic + further"=17))

ggsave("Ambiguity_regs_het_contradiction.pdf",height=9.5, width=7.2)

#####################################################################
####################Explanatory regression analysis for Part 1########
#######################################################################
#Control variables:
table(ful$Outro.1.player.Accuracy)
table(ful$Outro.1.player.Age)
table(ful$median_credibility)

table(ful$education_abi)
table(ful$education_fachabi)
table(ful$education_lehre)
table(ful$education_real)
table(ful$education_haupt)
table(ful$education_schueler)
table(ful$education_kein)
table(ful$education_andere)

table(ful$family_dist_marriage)
table(ful$family_dist_ssu)
table(ful$family_divorced)
table(ful$family_ssu)
table(ful$family_single)
table(ful$family_widowed)

table(ful$gender_female)
table(ful$gender_diverse)

table(ful$income_500)
table(ful$income_1000)
table(ful$income_2000)
table(ful$income_3000)
table(ful$income_4000)

table(ful$Outro.1.player.Kids)
table(ful$Outro.1.player.Risk_General)
table(ful$Outro.1.player.Risk_Weather)
table(ful$median_temp)
table(ful$median_usage)

table(ful$vac_not_yet)
table(ful$vac_no_interest)

#######################################
#####OLS Regs##########################
#######################################
library("car")
library("lmtest")
library("sandwich")
table(ful$round)
fulex<-subset(ful, ful$round==1)

reformulate(c("",list_dem), response="AA1")

exp1<-lm(reformulate(c("",list_dem), response="AA1"), data=fulex)
summary(exp1)
vif(exp1)

exp2<-lm(reformulate(c("",list_all), response="AA1"), data=fulex)
summary(exp2)
vif(exp2)



coeftest(exp2, vcov = vcovHC(exp2,type="HC1"))

exp4<-lm(reformulate(c("",list_dem), response="AA2"), data=fulex)
summary(exp4)
vif(exp4)

exp5<-lm(reformulate(c("",list_all), response="AA2"), data=fulex)
summary(exp5)
vif(exp5)



a1<-coeftest(exp1, vcov = vcovHC(exp1,type="HC1"))
a2<-coeftest(exp2, vcov = vcovHC(exp2,type="HC1"))
a4<-coeftest(exp4, vcov = vcovHC(exp4,type="HC1"))
a5<-coeftest(exp5, vcov = vcovHC(exp5,type="HC1"))

ses1<-list(a1[,2],a2[,2],a4[,2],a5[,2])
pvals1<-list(a1[,4],a2[,4],a4[,4],a5[,4])

library("stargazer")

stargazer(exp1,exp2,exp4,exp5, align=TRUE, se=ses1, p=pvals1,   title="Linear regressions: Explanatory analysis of Ambiguity Indices a and b",font.size = "scriptsize",type="latex", df=FALSE)




####################################################################################################
########################################## MAIN TABLES #############################################
############################################################################################################################################
#####OLS Regs##########################
#######################################
library("car")
library("lmtest")
library("sandwich")
ful$part2<-ifelse(ful$round==2,1,0)
table(ful$round2)

#Subsets
ful1<-subset(ful)
ful2<-subset(ful,ful$Intro.1.player.location=="Weiskirchen")
ful3<-subset(ful,ful$Intro.1.player.location=="Ilomantsi")
table(ful$Intro.1.player.treatment)
ful4<-subset(ful,ful$Intro.1.player.treatment=="best_guess")
ful5<-subset(ful,ful$Intro.1.player.treatment=="interval")
ful6<-subset(ful,ful$Intro.1.player.treatment=="both")


###########Regression dummys
ful1$contradiction<-ifelse(ful1$Intro.1.player.location=="Ilomantsi",1,0)
ful4$contradiction<-ifelse(ful4$Intro.1.player.location=="Ilomantsi",1,0)
ful5$contradiction<-ifelse(ful5$Intro.1.player.location=="Ilomantsi",1,0)
ful6$contradiction<-ifelse(ful6$Intro.1.player.location=="Ilomantsi",1,0)

table(ful1$contradiction)

##AA1##
#Confirmation,Contradiction
ols1<-lm(reformulate(c("contradiction*part2"), response="AA1"), data=ful1)
summary(ols1)

ols2<-lm(reformulate(c("Intro.1.player.treatment*part2"), response="AA1"), data=ful2)
summary(ols2)

ols3<-lm(reformulate(c("Intro.1.player.treatment*part2"), response="AA1"), data=ful3)
summary(ols3)

ols4<-lm(reformulate(c("contradiction*part2"), response="AA1"), data=ful4)
summary(ols4)

ols5<-lm(reformulate(c("contradiction*part2"), response="AA1"), data=ful5)
summary(ols5)

ols6<-lm(reformulate(c("contradiction*part2"), response="AA1"), data=ful6)
summary(ols6)

a1<-coeftest(ols1, vcovCL(ols1, cluster=~ful1$participant.label, type="HC1"))
a2<-coeftest(ols2, vcovCL(ols2, cluster=~ful2$participant.label, type="HC1"))
a4<-coeftest(ols3, vcovCL(ols3, cluster=~ful3$participant.label, type="HC1"))
a5<-coeftest(ols4, vcovCL(ols4, cluster=~ful4$participant.label, type="HC1"))
a6<-coeftest(ols5, vcovCL(ols5, cluster=~ful5$participant.label, type="HC1"))
a7<-coeftest(ols6, vcovCL(ols6, cluster=~ful6$participant.label, type="HC1"))

ses1<-list(a1[,2],a2[,2],a4[,2],a5[,2],a6[,2],a7[,2])
pvals1<-list(a1[,4],a2[,4],a4[,4],a5[,4],a6[,4],a7[,4])

library("stargazer")

stargazer(ols1,ols2,ols3,ols4,ols5,ols6, align=TRUE, se=ses1, p=pvals1,   title="Linear regressions: Treatment effects on ambiguity index b",font.size = "scriptsize",type="latex", df=FALSE)



##AA2##
#Confirmation,Contradiction
ols1a<-lm(reformulate(c("contradiction*part2"), response="AA2"), data=ful1)
summary(ols1a)

ols2a<-lm(reformulate(c("Intro.1.player.treatment*part2"), response="AA2"), data=ful2)
summary(ols2a)

ols3a<-lm(reformulate(c("Intro.1.player.treatment*part2"), response="AA2"), data=ful3)
summary(ols3a)

ols4a<-lm(reformulate(c("contradiction*part2"), response="AA2"), data=ful4)
summary(ols4a)

ols5a<-lm(reformulate(c("contradiction*part2"), response="AA2"), data=ful5)
summary(ols5a)

ols6a<-lm(reformulate(c("contradiction*part2"), response="AA2"), data=ful6)
summary(ols6a)

a1<-coeftest(ols1a, vcovCL(ols1a, cluster=~ful1$participant.label, type="HC1"))
a2<-coeftest(ols2a, vcovCL(ols2a, cluster=~ful2$participant.label, type="HC1"))
a4<-coeftest(ols3a, vcovCL(ols3a, cluster=~ful3$participant.label, type="HC1"))
a5<-coeftest(ols4a, vcovCL(ols4a, cluster=~ful4$participant.label, type="HC1"))
a6<-coeftest(ols5a, vcovCL(ols5a, cluster=~ful5$participant.label, type="HC1"))
a7<-coeftest(ols6a, vcovCL(ols6a, cluster=~ful6$participant.label, type="HC1"))

ses1<-list(a1[,2],a2[,2],a4[,2],a5[,2],a6[,2],a7[,2])
pvals1<-list(a1[,4],a2[,4],a4[,4],a5[,4],a6[,4],a7[,4])

library("stargazer")

stargazer(ols1a,ols2a,ols3a,ols4a,ols5a,ols6a, align=TRUE, se=ses1, p=pvals1,   title="Linear regressions: Treatment effects on ambiguity index a",font.size = "scriptsize",type="latex", df=FALSE)



#############for all events##################
##EE1##
#Confirmation,Contradiction
ols1<-lm(reformulate(c("contradiction*part2"), response="EE1"), data=ful1)
summary(ols1)

ols2<-lm(reformulate(c("Intro.1.player.treatment*part2"), response="EE1"), data=ful2)
summary(ols2)

ols3<-lm(reformulate(c("Intro.1.player.treatment*part2"), response="EE1"), data=ful3)
summary(ols3)

ols4<-lm(reformulate(c("contradiction*part2"), response="EE1"), data=ful4)
summary(ols4)

ols5<-lm(reformulate(c("contradiction*part2"), response="EE1"), data=ful5)
summary(ols5)

ols6<-lm(reformulate(c("contradiction*part2"), response="EE1"), data=ful6)
summary(ols6)

a1<-coeftest(ols1, vcovCL(ols1, cluster=~ful1$participant.label, type="HC1"))
a2<-coeftest(ols2, vcovCL(ols2, cluster=~ful2$participant.label, type="HC1"))
a4<-coeftest(ols3, vcovCL(ols3, cluster=~ful3$participant.label, type="HC1"))
a5<-coeftest(ols4, vcovCL(ols4, cluster=~ful4$participant.label, type="HC1"))
a6<-coeftest(ols5, vcovCL(ols5, cluster=~ful5$participant.label, type="HC1"))
a7<-coeftest(ols6, vcovCL(ols6, cluster=~ful6$participant.label, type="HC1"))

ses1<-list(a1[,2],a2[,2],a4[,2],a5[,2],a6[,2],a7[,2])
pvals1<-list(a1[,4],a2[,4],a4[,4],a5[,4],a6[,4],a7[,4])

library("stargazer")

stargazer(ols1,ols2,ols3,ols4,ols5,ols6, align=TRUE, se=ses1, p=pvals1,   title="Linear regressions: Treatment effects on E1",font.size = "scriptsize",type="latex", df=FALSE)



##EE2##
#Confirmation,Contradiction
ols1<-lm(reformulate(c("contradiction*part2"), response="EE2"), data=ful1)
summary(ols1)

ols2<-lm(reformulate(c("Intro.1.player.treatment*part2"), response="EE2"), data=ful2)
summary(ols2)

ols3<-lm(reformulate(c("Intro.1.player.treatment*part2"), response="EE2"), data=ful3)
summary(ols3)

ols4<-lm(reformulate(c("contradiction*part2"), response="EE2"), data=ful4)
summary(ols4)

ols5<-lm(reformulate(c("contradiction*part2"), response="EE2"), data=ful5)
summary(ols5)

ols6<-lm(reformulate(c("contradiction*part2"), response="EE2"), data=ful6)
summary(ols6)

a1<-coeftest(ols1, vcovCL(ols1, cluster=~ful1$participant.label, type="HC1"))
a2<-coeftest(ols2, vcovCL(ols2, cluster=~ful2$participant.label, type="HC1"))
a4<-coeftest(ols3, vcovCL(ols3, cluster=~ful3$participant.label, type="HC1"))
a5<-coeftest(ols4, vcovCL(ols4, cluster=~ful4$participant.label, type="HC1"))
a6<-coeftest(ols5, vcovCL(ols5, cluster=~ful5$participant.label, type="HC1"))
a7<-coeftest(ols6, vcovCL(ols6, cluster=~ful6$participant.label, type="HC1"))

ses1<-list(a1[,2],a2[,2],a4[,2],a5[,2],a6[,2],a7[,2])
pvals1<-list(a1[,4],a2[,4],a4[,4],a5[,4],a6[,4],a7[,4])

library("stargazer")

stargazer(ols1,ols2,ols3,ols4,ols5,ols6, align=TRUE, se=ses1, p=pvals1,   title="Linear regressions: Treatment effects on E2",font.size = "scriptsize",type="latex", df=FALSE)



##EE3##
#Confirmation,Contradiction
ols1<-lm(reformulate(c("contradiction*part2"), response="EE3"), data=ful1)
summary(ols1)

ols2<-lm(reformulate(c("Intro.1.player.treatment*part2"), response="EE3"), data=ful2)
summary(ols2)

ols3<-lm(reformulate(c("Intro.1.player.treatment*part2"), response="EE3"), data=ful3)
summary(ols3)

ols4<-lm(reformulate(c("contradiction*part2"), response="EE3"), data=ful4)
summary(ols4)

ols5<-lm(reformulate(c("contradiction*part2"), response="EE3"), data=ful5)
summary(ols5)

ols6<-lm(reformulate(c("contradiction*part2"), response="EE3"), data=ful6)
summary(ols6)

a1<-coeftest(ols1, vcovCL(ols1, cluster=~ful1$participant.label, type="HC1"))
a2<-coeftest(ols2, vcovCL(ols2, cluster=~ful2$participant.label, type="HC1"))
a4<-coeftest(ols3, vcovCL(ols3, cluster=~ful3$participant.label, type="HC1"))
a5<-coeftest(ols4, vcovCL(ols4, cluster=~ful4$participant.label, type="HC1"))
a6<-coeftest(ols5, vcovCL(ols5, cluster=~ful5$participant.label, type="HC1"))
a7<-coeftest(ols6, vcovCL(ols6, cluster=~ful6$participant.label, type="HC1"))

ses1<-list(a1[,2],a2[,2],a4[,2],a5[,2],a6[,2],a7[,2])
pvals1<-list(a1[,4],a2[,4],a4[,4],a5[,4],a6[,4],a7[,4])

library("stargazer")

stargazer(ols1,ols2,ols3,ols4,ols5,ols6, align=TRUE, se=ses1, p=pvals1,   title="Linear regressions: Treatment effects on E3",font.size = "scriptsize",type="latex", df=FALSE)



##EE12##
#Confirmation,Contradiction
ols1<-lm(reformulate(c("contradiction*part2"), response="EE12"), data=ful1)
summary(ols1)

ols2<-lm(reformulate(c("Intro.1.player.treatment*part2"), response="EE12"), data=ful2)
summary(ols2)

ols3<-lm(reformulate(c("Intro.1.player.treatment*part2"), response="EE12"), data=ful3)
summary(ols3)

ols4<-lm(reformulate(c("contradiction*part2"), response="EE12"), data=ful4)
summary(ols4)

ols5<-lm(reformulate(c("contradiction*part2"), response="EE12"), data=ful5)
summary(ols5)

ols6<-lm(reformulate(c("contradiction*part2"), response="EE12"), data=ful6)
summary(ols6)

a1<-coeftest(ols1, vcovCL(ols1, cluster=~ful1$participant.label, type="HC1"))
a2<-coeftest(ols2, vcovCL(ols2, cluster=~ful2$participant.label, type="HC1"))
a4<-coeftest(ols3, vcovCL(ols3, cluster=~ful3$participant.label, type="HC1"))
a5<-coeftest(ols4, vcovCL(ols4, cluster=~ful4$participant.label, type="HC1"))
a6<-coeftest(ols5, vcovCL(ols5, cluster=~ful5$participant.label, type="HC1"))
a7<-coeftest(ols6, vcovCL(ols6, cluster=~ful6$participant.label, type="HC1"))

ses1<-list(a1[,2],a2[,2],a4[,2],a5[,2],a6[,2],a7[,2])
pvals1<-list(a1[,4],a2[,4],a4[,4],a5[,4],a6[,4],a7[,4])

library("stargazer")

stargazer(ols1,ols2,ols3,ols4,ols5,ols6, align=TRUE, se=ses1, p=pvals1,   title="Linear regressions: Treatment effects on E12",font.size = "scriptsize",type="latex", df=FALSE)



##EE23##
#Confirmation,Contradiction
ols1<-lm(reformulate(c("contradiction*part2"), response="EE23"), data=ful1)
summary(ols1)

ols2<-lm(reformulate(c("Intro.1.player.treatment*part2"), response="EE23"), data=ful2)
summary(ols2)

ols3<-lm(reformulate(c("Intro.1.player.treatment*part2"), response="EE23"), data=ful3)
summary(ols3)

ols4<-lm(reformulate(c("contradiction*part2"), response="EE23"), data=ful4)
summary(ols4)

ols5<-lm(reformulate(c("contradiction*part2"), response="EE23"), data=ful5)
summary(ols5)

ols6<-lm(reformulate(c("contradiction*part2"), response="EE23"), data=ful6)
summary(ols6)

a1<-coeftest(ols1, vcovCL(ols1, cluster=~ful1$participant.label, type="HC1"))
a2<-coeftest(ols2, vcovCL(ols2, cluster=~ful2$participant.label, type="HC1"))
a4<-coeftest(ols3, vcovCL(ols3, cluster=~ful3$participant.label, type="HC1"))
a5<-coeftest(ols4, vcovCL(ols4, cluster=~ful4$participant.label, type="HC1"))
a6<-coeftest(ols5, vcovCL(ols5, cluster=~ful5$participant.label, type="HC1"))
a7<-coeftest(ols6, vcovCL(ols6, cluster=~ful6$participant.label, type="HC1"))

ses1<-list(a1[,2],a2[,2],a4[,2],a5[,2],a6[,2],a7[,2])
pvals1<-list(a1[,4],a2[,4],a4[,4],a5[,4],a6[,4],a7[,4])

library("stargazer")

stargazer(ols1,ols2,ols3,ols4,ols5,ols6, align=TRUE, se=ses1, p=pvals1,   title="Linear regressions: Treatment effects on E23",font.size = "scriptsize",type="latex", df=FALSE)


##EE13##
#Confirmation,Contradiction
ols1<-lm(reformulate(c("contradiction*part2"), response="EE13"), data=ful1)
summary(ols1)

ols2<-lm(reformulate(c("Intro.1.player.treatment*part2"), response="EE13"), data=ful2)
summary(ols2)

ols3<-lm(reformulate(c("Intro.1.player.treatment*part2"), response="EE13"), data=ful3)
summary(ols3)

ols4<-lm(reformulate(c("contradiction*part2"), response="EE13"), data=ful4)
summary(ols4)

ols5<-lm(reformulate(c("contradiction*part2"), response="EE13"), data=ful5)
summary(ols5)

ols6<-lm(reformulate(c("contradiction*part2"), response="EE13"), data=ful6)
summary(ols6)

a1<-coeftest(ols1, vcovCL(ols1, cluster=~ful1$participant.label, type="HC1"))
a2<-coeftest(ols2, vcovCL(ols2, cluster=~ful2$participant.label, type="HC1"))
a4<-coeftest(ols3, vcovCL(ols3, cluster=~ful3$participant.label, type="HC1"))
a5<-coeftest(ols4, vcovCL(ols4, cluster=~ful4$participant.label, type="HC1"))
a6<-coeftest(ols5, vcovCL(ols5, cluster=~ful5$participant.label, type="HC1"))
a7<-coeftest(ols6, vcovCL(ols6, cluster=~ful6$participant.label, type="HC1"))

ses1<-list(a1[,2],a2[,2],a4[,2],a5[,2],a6[,2],a7[,2])
pvals1<-list(a1[,4],a2[,4],a4[,4],a5[,4],a6[,4],a7[,4])

library("stargazer")

stargazer(ols1,ols2,ols3,ols4,ols5,ols6, align=TRUE, se=ses1, p=pvals1,   title="Linear regressions: Treatment effects on E13",font.size = "scriptsize",type="latex", df=FALSE)




#####################################################################################################################
#########################################Euclidyan distance##########################################################
#####################################################################################################################
library("car")
library("lmtest")
library("sandwich")

ful1<-subset(ful,ful$round==1)
ful2<-subset(ful,ful$round==2)


cols <- c("participant.label", "EE1", "EE2", "EE3", "EE12","EE23","EE13","AA1","AA2", "treat","Intro.1.player.location","Intro.1.player.treatment")
ful1 <- ful1[, cols]
ful2 <- ful2[, cols]

ful<-merge(ful1,ful2,by="participant.label")

ful$ed<-sqrt((ful$EE1.x-ful$EE1.y)^2+(ful$EE2.x-ful$EE2.y)^2+(ful$EE3.x-ful$EE3.y)^2+(ful$EE12.x-ful$EE12.y)^2+(ful$EE13.x-ful$EE13.y)^2+(ful$EE23.x-ful$EE23.y)^2)


#Subsets
ful1<-subset(ful)
ful2<-subset(ful,ful$Intro.1.player.location.x=="Weiskirchen")
ful3<-subset(ful,ful$Intro.1.player.location.x=="Ilomantsi")
table(ful$Intro.1.player.treatment)
ful4<-subset(ful,ful$Intro.1.player.treatment.x=="best_guess")
ful5<-subset(ful,ful$Intro.1.player.treatment.x=="interval")
ful6<-subset(ful,ful$Intro.1.player.treatment.x=="both")


###########Regression dummys
ful1$contradiction<-ifelse(ful1$Intro.1.player.location.x=="Ilomantsi",1,0)
ful4$contradiction<-ifelse(ful4$Intro.1.player.location.x=="Ilomantsi",1,0)
ful5$contradiction<-ifelse(ful5$Intro.1.player.location.x=="Ilomantsi",1,0)
ful6$contradiction<-ifelse(ful6$Intro.1.player.location.x=="Ilomantsi",1,0)

table(ful1$contradiction)



ols1<-lm(reformulate(c("contradiction"), response="ed"), data=ful1)
summary(ols1)

ols2<-lm(reformulate(c("Intro.1.player.treatment.x"), response="ed"), data=ful2)
summary(ols2)

ols3<-lm(reformulate(c("Intro.1.player.treatment.x"), response="ed"), data=ful3)
summary(ols3)

ols4<-lm(reformulate(c("contradiction"), response="ed"), data=ful4)
summary(ols4)

ols5<-lm(reformulate(c("contradiction"), response="ed"), data=ful5)
summary(ols5)

ols6<-lm(reformulate(c("contradiction"), response="ed"), data=ful6)
summary(ols6)



a1<-coeftest(ols1, vcov = vcovHC(ols1,type="HC1"))
a2<-coeftest(ols2, vcov = vcovHC(ols2,type="HC1"))
a3<-coeftest(ols3, vcov = vcovHC(ols3,type="HC1"))
a4<-coeftest(ols4, vcov = vcovHC(ols4,type="HC1"))
a5<-coeftest(ols5, vcov = vcovHC(ols5,type="HC1"))
a6<-coeftest(ols6, vcov = vcovHC(ols6,type="HC1"))

ses1<-list(a1[,2],a2[,2],a3[,2],a4[,2],a5[,2],a6[,2])
pvals1<-list(a1[,4],a2[,4],a3[,4],a4[,4],a5[,4],a6[,4])

library("stargazer")

stargazer(ols1,ols2,ols3,ols4,ols5,ols6, align=TRUE, se=ses1, p=pvals1,   title="Linear regressions: Treatment effects on Euclidian distance between vector of matching probabilities in part 1 vs.\ part 2.",font.size = "scriptsize",type="latex", df=FALSE)









######################################################################################################################################
############################### Weak and Strong Monotonocity: Regression on Treatments; Defeaults (49,5)#######################
######################################################################################################################################

#Weak Monotonicity
ful$weakmono<-ifelse(ful$MC<ful$MS,1,0)
table(ful$weakmono)
#strong Monotonocity: value between 0 and 6 depending how many monotonicites not fulfilled
ful$smono1<-ifelse(ful$EE1>ful$EE12,1,0)
ful$smono2<-ifelse(ful$EE1>ful$EE13,1,0)
ful$smono3<-ifelse(ful$EE2>ful$EE12,1,0)
ful$smono4<-ifelse(ful$EE2>ful$EE23,1,0)
ful$smono5<-ifelse(ful$EE3>ful$EE13,1,0)
ful$smono6<-ifelse(ful$EE3>ful$EE23,1,0)

ful$smono<-ful$smono1+ful$smono2+ful$smono3+ful$smono4+ful$smono5+ful$smono6
table(ful$smono)

#Defaults
ful$def1<-ifelse(ful$EE1==49.5,1,0)
ful$def2<-ifelse(ful$EE12==49.5,1,0)
ful$def3<-ifelse(ful$EE13==49.5,1,0)
ful$def4<-ifelse(ful$EE2==49.5,1,0)
ful$def5<-ifelse(ful$EE23==49.5,1,0)
ful$def6<-ifelse(ful$EE3==49.5,1,0)

ful$def<-ful$def1+ful$def2+ful$def3+ful$def4+ful$def5+ful$def6
table(ful$def)



ful$round2<-ifelse(ful$round==2,1,0)
table(ful$round2)

#Subsets
ful1<-subset(ful)
ful2<-subset(ful,ful$Intro.1.player.location=="Weiskirchen")
ful3<-subset(ful,ful$Intro.1.player.location=="Ilomantsi")

###########Regression dummys
ful1$contradiction<-ifelse(ful1$Intro.1.player.location=="Ilomantsi",1,0)
table(ful1$contradiction)

##weakmono##
#Confirmation,Contradiction
ols1<-lm(reformulate(c("contradiction*round2"), response="weakmono"), data=ful1)
summary(ols1)

#Confirmation
ols2<-lm(reformulate(c("treat*round2"), response="weakmono"), data=ful2)
summary(ols2)

#Contradiction
ols3<-lm(reformulate(c("treat*round2"), response="weakmono"), data=ful3)
summary(ols3)


##smono##
#Confirmation,Contradiction
ols4<-lm(reformulate(c("contradiction*round2"), response="smono"), data=ful1)
summary(ols4)

#Confirmation
ols5<-lm(reformulate(c("treat*round2"), response="smono"), data=ful2)
summary(ols5)

#Contradiction
ols6<-lm(reformulate(c("treat*round2"), response="smono"), data=ful3)
summary(ols6)

##def##
#Confirmation,Contradiction
ols7<-lm(reformulate(c("contradiction*round2"), response="def"), data=ful1)
summary(ols7)

#Confirmation
ols8<-lm(reformulate(c("treat*round2"), response="def"), data=ful2)
summary(ols8)

#Contradiction
ols9<-lm(reformulate(c("treat*round2"), response="def"), data=ful3)
summary(ols9)













####################################################
#####################SUR models#####################
####################################################
library("systemfit")
?systemfit
ful$round2<-ifelse(ful$round==2,1,0)
table(ful$round2)

#Subsets
ful1<-subset(ful)
ful2<-subset(ful,ful$Intro.1.player.location=="Weiskirchen")
ful3<-subset(ful,ful$Intro.1.player.location=="Ilomantsi")

###########Regression dummys
ful1$contradiction<-ifelse(ful1$Intro.1.player.location=="Ilomantsi",1,0)
table(ful1$contradiction)

##AA1 and AA2##
#Confirmation,Contradiction

r1<-AA1~contradiction*round2
r2<-AA2~contradiction*round2

fitsur1 <- systemfit(list(amb1 = r1, amb2 = r2),method="SUR", data=ful1)
summary(fitsur1)


r3<-AA1~contradiction*round2+age_53_69+age_35_52+education_studium+education_abi_fachabi+education_lehre+education_schueler_kein_andere+family_dist_marriage_ssu+family_divorced+family_single+family_widowed+gender_female+gender_diverse+income_0_1000+income_1001_3000+Outro.1.player.Kids
r4<-AA2~contradiction*round2+age_53_69+age_35_52+education_studium+education_abi_fachabi+education_lehre+education_schueler_kein_andere+family_dist_marriage_ssu+family_divorced+family_single+family_widowed+gender_female+gender_diverse+income_0_1000+income_1001_3000+Outro.1.player.Kids

fitsur2 <- systemfit(list(amb1 = r3, amb2 = r4),method="SUR", data=ful1)
summary(fitsur2)


r5<-AA1~contradiction*round2+age_53_69+age_35_52+education_studium+education_abi_fachabi+education_lehre+education_schueler_kein_andere+family_dist_marriage_ssu+family_divorced+family_single+family_widowed+gender_female+gender_diverse+income_0_1000+income_1001_3000+Outro.1.player.Kids+median_accuracy+median_credibility+median_risk_gen+median_risk_weat+median_temp+median_usage
r6<-AA2~contradiction*round2+age_53_69+age_35_52+education_studium+education_abi_fachabi+education_lehre+education_schueler_kein_andere+family_dist_marriage_ssu+family_divorced+family_single+family_widowed+gender_female+gender_diverse+income_0_1000+income_1001_3000+Outro.1.player.Kids+median_accuracy+median_credibility+median_risk_gen+median_risk_weat+median_temp+median_usage

fitsur3 <- systemfit(list(amb1 = r5, amb2 = r6),method="SUR", data=ful1)
summary(fitsur3)

#Confirmation
r7<-AA1~treat*round2
r8<-AA2~treat*round2
fitsur4 <- systemfit(list(amb1 = r7, amb2 = r8),method="SUR", data=ful2)
summary(fitsur4)


r9<-AA1~treat*round2+age_53_69+age_35_52+education_studium+education_abi_fachabi+education_lehre+education_schueler_kein_andere+family_dist_marriage_ssu+family_divorced+family_single+family_widowed+gender_female+gender_diverse+income_0_1000+income_1001_3000+Outro.1.player.Kids
r10<-AA2~treat*round2+age_53_69+age_35_52+education_studium+education_abi_fachabi+education_lehre+education_schueler_kein_andere+family_dist_marriage_ssu+family_divorced+family_single+family_widowed+gender_female+gender_diverse+income_0_1000+income_1001_3000+Outro.1.player.Kids
fitsur5 <- systemfit(list(amb1 = r9, amb2 = r10),method="SUR", data=ful2)
summary(fitsur5)

r11<-AA1~treat*round2+age_53_69+age_35_52+education_studium+education_abi_fachabi+education_lehre+education_schueler_kein_andere+family_dist_marriage_ssu+family_divorced+family_single+family_widowed+gender_female+gender_diverse+income_0_1000+income_1001_3000+Outro.1.player.Kids+median_accuracy+median_credibility+median_risk_gen+median_risk_weat+median_temp+median_usage
r12<-AA2~treat*round2+age_53_69+age_35_52+education_studium+education_abi_fachabi+education_lehre+education_schueler_kein_andere+family_dist_marriage_ssu+family_divorced+family_single+family_widowed+gender_female+gender_diverse+income_0_1000+income_1001_3000+Outro.1.player.Kids+median_accuracy+median_credibility+median_risk_gen+median_risk_weat+median_temp+median_usage
fitsur6 <- systemfit(list(amb1 = r11, amb2 = r12),method="SUR", data=ful2)
summary(fitsur6)


#Contradiction
r13<-AA1~treat*round2
r14<-AA2~treat*round2
fitsur7 <- systemfit(list(amb1 = r13, amb2 = r14),method="SUR", data=ful3)
summary(fitsur7)


r15<-AA1~treat*round2+age_53_69+age_35_52+education_studium+education_abi_fachabi+education_lehre+education_schueler_kein_andere+family_dist_marriage_ssu+family_divorced+family_single+family_widowed+gender_female+gender_diverse+income_0_1000+income_1001_3000+Outro.1.player.Kids
r16<-AA2~treat*round2+age_53_69+age_35_52+education_studium+education_abi_fachabi+education_lehre+education_schueler_kein_andere+family_dist_marriage_ssu+family_divorced+family_single+family_widowed+gender_female+gender_diverse+income_0_1000+income_1001_3000+Outro.1.player.Kids
fitsur8 <- systemfit(list(amb1 = r15, amb2 = r16),method="SUR", data=ful3)
summary(fitsur8)

ols8<-lm(AA1~, data=ful3)
summary(ols8)


r17<-AA1~treat*round2+age_53_69+age_35_52+education_studium+education_abi_fachabi+education_lehre+education_schueler_kein_andere+family_dist_marriage_ssu+family_divorced+family_single+family_widowed+gender_female+gender_diverse+income_0_1000+income_1001_3000+Outro.1.player.Kids+median_accuracy+median_credibility+median_risk_gen+median_risk_weat+median_temp+median_usage
r18<-AA2~treat*round2+age_53_69+age_35_52+education_studium+education_abi_fachabi+education_lehre+education_schueler_kein_andere+family_dist_marriage_ssu+family_divorced+family_single+family_widowed+gender_female+gender_diverse+income_0_1000+income_1001_3000+Outro.1.player.Kids+median_accuracy+median_credibility+median_risk_gen+median_risk_weat+median_temp+median_usage
fitsur9 <- systemfit(list(amb1 = r17, amb2 = r18),method="SUR", data=ful3)
summary(fitsur9)



#########################################
##############ZIP Codes###############
#######################################
library("car")
library("lmtest")
library("sandwich")
table(ful$round)
fulex<-subset(ful, ful$round==1)


exp1<-lm(reformulate(c("factor(Outro.1.player.ZIP2)"), response="AA1"), data=fulex)
summary(exp1)
vif(exp1)

exp2<-lm(reformulate(c("factor(Outro.1.player.ZIP2)",list_dem), response="AA1"), data=fulex)
summary(exp2)
vif(exp2)

exp3<-lm(reformulate(c("factor(Outro.1.player.ZIP2)",list_all), response="AA1"), data=fulex)
summary(exp3)
vif(exp3)



exp4<-lm(reformulate(c("factor(Outro.1.player.ZIP2)"), response="AA2"), data=fulex)
summary(exp4)
vif(exp4)

exp5<-lm(reformulate(c("factor(Outro.1.player.ZIP2)",list_dem), response="AA2"), data=fulex)
summary(exp5)
vif(exp5)

exp6<-lm(reformulate(c("factor(Outro.1.player.ZIP2)",list_all), response="AA2")
         , data=fulex)
summary(exp6)
vif(exp6)