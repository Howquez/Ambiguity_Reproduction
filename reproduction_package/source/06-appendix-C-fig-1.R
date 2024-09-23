################Analysis##################

data <- readRDS(file="../data/processed/full.Rda")

##################################################################
######Graphs (heterogeneous effects)###################
###################################################################

covariates_dem <- c("age_35_52",
                    "age_53_plus",
                    "female", 
                    "high_education",
                    "high_income",
                    "married",
                    "parentship")

covariates_all <- c(covariates_dem,
                    "high_temperature",
                    "high_usage",
                    "high_general_risk",
                    "high_weather_risk",
                    "high_accuracy",
                    "high_credibility")


data$round2<-ifelse(data$stage==2,1,0)
table(data$round2)


data$treat<-ifelse(data$communication=="point"&data$surprise==F,"conf.-best guess",NA)
data$treat<-ifelse(data$communication=="interval"&data$surprise==F,"conf.-interval",data$treat)
data$treat<-ifelse(data$communication=="both"&data$surprise==F,"conf.-both",data$treat)
data$treat<-ifelse(data$communication=="point"&data$surprise==T,"contrad.-best guess",data$treat)
data$treat<-ifelse(data$communication=="interval"&data$surprise==T,"contrad.-interval",data$treat)
data$treat<-ifelse(data$communication=="both"&data$surprise==T,"contrad.-both",data$treat)

#Interaction dummies for regressions

#female
table(data$female)
data$female_con<-ifelse(data$female==1&data$round2==1&data$surprise==T,1,0)
table(data$female_con)
data$female_both<-ifelse(data$female==1&data$round2==1&data$communication=="both",1,0)
table(data$female_both)
data$female_int<-ifelse(data$female==1&data$round2==1&data$communication=="interval",1,0)
table(data$female_int)

#Accuracy
table(data$high_accuracy)
data$accu_con<-ifelse(data$high_accuracy==T &data$round2==1&data$surprise==T,1,0)
table(data$accu_con)
data$accu_both<-ifelse(data$high_accuracy==T&data$round2==1&data$communication=="both",1,0)
table(data$accu_both)
data$accu_int<-ifelse(data$high_accuracy==T&data$round2==1&data$communication=="interval",1,0)
table(data$accu_int)

#Credibility
table(data$high_credibility)
data$cred_con<-ifelse(data$high_credibility==T &data$round2==1&data$surprise==T,1,0)
table(data$cred_con)
data$cred_both<-ifelse(data$high_credibility==T&data$round2==1&data$communication=="both",1,0)
table(data$cred_both)
data$cred_int<-ifelse(data$high_credibility==T&data$round2==1&data$communication=="interval",1,0)
table(data$cred_int)

#Forecast Usage
table(data$high_usage)

data$fore_con<-ifelse(data$high_usage==T &data$round2==1&data$surprise==T,1,0)
table(data$fore_con)
data$fore_both<-ifelse(data$high_usage==T&data$round2==1&data$communication=="both",1,0)
table(data$fore_both)
data$fore_int<-ifelse(data$high_usage==T&data$round2==1&data$communication=="interval",1,0)
table(data$fore_int)


#Temperature
table(data$high_temperature)


data$temp_con<-ifelse(data$high_temperature==T &data$round2==1&data$surprise==T,1,0)
table(data$temp_con)
data$temp_both<-ifelse(data$high_temperature==T&data$round2==1&data$communication=="both",1,0)
table(data$temp_both)
data$temp_int<-ifelse(data$high_temperature==T&data$round2==1&data$communication=="interval",1,0)
table(data$temp_int)

#education
table(data$high_education)
data$study_con<-ifelse(data$high_education==T&data$round2==1&data$surprise==T,1,0)
table(data$study_con)
data$study_both<-ifelse(data$high_education==T&data$round2==1&data$communication=="both",1,0)
table(data$study_both)
data$study_int<-ifelse(data$high_education==T&data$round2==1&data$communication=="interval",1,0)
table(data$study_int)


#Subsets
data1<-subset(data)
data2<-subset(data,data$surprise==F)
data3<-subset(data,data$surprise==T)

###########Regression dummys
data1$contradiction<-ifelse(data1$surprise==T,1,0)
table(data1$contradiction)




##b##female
#Confirmation,Contradiction
ols1<-lm(reformulate(c("contradiction*round2+female_con+female*round2+female*contradiction+female"), response="b"), data=data1)
summary(ols1)

ols2<-lm(reformulate(c("contradiction*round2+female_con+female*round2+female*contradiction+female",covariates_dem), response="b"), data=data1)
summary(ols2)

ols3<-lm(reformulate(c("contradiction*round2+female_con+female*round2+female*contradiction+female",covariates_all), response="b"), data=data1)
summary(ols3)


reformulate(c("treat*round2+female_both+female_int+female*round2+female*treat+female"), response="b")
#Confirmation
ols4<-lm(reformulate(c("treat*round2+female_both+female_int+female*round2+female*treat+female"), response="b"), data=data2)
summary(ols4)

ols5<-lm(reformulate(c("treat*round2+female_both+female_int+female*round2+female*treat+female",covariates_dem), response="b"), data=data2)
summary(ols5)

ols6<-lm(reformulate(c("treat*round2+female_both+female_int+female*round2+female*treat+female",covariates_all), response="b"), data=data2)
summary(ols6)

#Contradiction
ols7<-lm(reformulate(c("treat*round2+female_both+female_int+female*round2+female*treat+female"), response="b"), data=data3)
summary(ols7)

ols8<-lm(reformulate(c("treat*round2+female_both+female_int+female*round2+female*treat+female",covariates_dem), response="b"), data=data3)
summary(ols8)

ols9<-lm(reformulate(c("treat*round2+female_both+female_int+female*round2+female*treat+female",covariates_all), response="b"), data=data3)
summary(ols9)

##a##female
#Confirmation,Contradiction
ols1a<-lm(reformulate(c("contradiction*round2+female_con+female*round2+female*contradiction+female"),response="a"), data=data1)
summary(ols1a)

ols2a<-lm(reformulate(c("contradiction*round2+female_con+female*round2+female*contradiction+female",covariates_dem),response="a"), data=data1)
summary(ols2a)

ols3a<-lm(reformulate(c("contradiction*round2+female_con+female*round2+female*contradiction+female",covariates_all),response="a"), data=data1)
summary(ols3a)

#Confirmation
ols4a<-lm(reformulate(c("treat*round2+female_both+female_int+female*round2+female*treat+female"), response="a"), data=data2)
summary(ols4a)

ols5a<-lm(reformulate(c("treat*round2+female_both+female_int+female*round2+female*treat+female",covariates_dem), response="a"), data=data2)
summary(ols5a)

ols6a<-lm(reformulate(c("treat*round2+female_both+female_int+female*round2+female*treat+female",covariates_all), response="a"), data=data2)
summary(ols6a)

#Contradiction
ols7a<-lm(reformulate(c("treat*round2+female_both+female_int+female*round2+female*treat+female"), response="a"), data=data3)
summary(ols7a)

ols8a<-lm(reformulate(c("treat*round2+female_both+female_int+female*round2+female*treat+female",covariates_dem), response="a"), data=data3)
summary(ols8a)

ols9a<-lm(reformulate(c("treat*round2+female_both+female_int+female*round2+female*treat+female",covariates_all), response="a"), data=data3)
summary(ols9a)




##b##Accuracy
#Confirmation,Contradiction
ols1c<-lm(reformulate(c("contradiction*round2+accu_con+high_accuracy*contradiction+ high_accuracy*round2+high_accuracy"), response="b"), data=data1)
summary(ols1c)

ols2c<-lm(reformulate(c("contradiction*round2+accu_con+high_accuracy*contradiction+ high_accuracy*round2+high_accuracy",covariates_dem), response="b"), data=data1)
summary(ols2c)

ols3c<-lm(reformulate(c("contradiction*round2+accu_con+high_accuracy*contradiction+ high_accuracy*round2+high_accuracy",covariates_all), response="b"), data=data1)
summary(ols3c)

#Confirmation
ols4c<-lm(reformulate(c("treat*round2+accu_both+accu_int+high_accuracy*treat+ high_accuracy*round2+high_accuracy"), response="b"), data=data2)
summary(ols4c)

ols5c<-lm(reformulate(c("treat*round2+accu_both+accu_int+high_accuracy*treat+ high_accuracy*round2+high_accuracy",covariates_dem), response="b"), data=data2)
summary(ols5c)

ols6c<-lm(reformulate(c("treat*round2+accu_both+accu_int+high_accuracy*treat+ high_accuracy*round2+high_accuracy",covariates_all), response="b"), data=data2)
summary(ols6c)

#Contradiction
ols7c<-lm(reformulate(c("treat*round2+accu_both+accu_int+high_accuracy*treat+ high_accuracy*round2+high_accuracy"), response="b"), data=data3)
summary(ols7c)

ols8c<-lm(reformulate(c("treat*round2+accu_both+accu_int+high_accuracy*treat+ high_accuracy*round2+high_accuracy",covariates_dem), response="b"), data=data3)
summary(ols8c)

ols9c<-lm(reformulate(c("treat*round2+accu_both+accu_int+high_accuracy*treat+ high_accuracy*round2+high_accuracy",covariates_all), response="b"), data=data3)
summary(ols9c)

##a##Accuracy
#Confirmation,Contradiction
ols1d<-lm(reformulate(c("contradiction*round2+accu_con+high_accuracy*contradiction+ high_accuracy*round2+high_accuracy"), response="a"), data=data1)
summary(ols1d)

ols2d<-lm(reformulate(c("contradiction*round2+accu_con+high_accuracy*contradiction+ high_accuracy*round2+high_accuracy",covariates_dem), response="a"), data=data1)
summary(ols2d)

ols3d<-lm(reformulate(c("contradiction*round2+accu_con+high_accuracy*contradiction+ high_accuracy*round2+high_accuracy",covariates_all), response="a"), data=data1)
summary(ols3d)

#Confirmation
ols4d<-lm(reformulate(c("treat*round2+accu_both+accu_int+high_accuracy*treat+ high_accuracy*round2+high_accuracy"), response="a"), data=data2)
summary(ols4d)

ols5d<-lm(reformulate(c("treat*round2+accu_both+accu_int+high_accuracy*treat+ high_accuracy*round2+high_accuracy",covariates_dem), response="a"), data=data2)
summary(ols5d)

ols6d<-lm(reformulate(c("treat*round2+accu_both+accu_int+high_accuracy*treat+ high_accuracy*round2+high_accuracy",covariates_all), response="a"), data=data2)
summary(ols6d)

#Contradiction
ols7d<-lm(reformulate(c("treat*round2+accu_both+accu_int+high_accuracy*treat+ high_accuracy*round2+high_accuracy"), response="a"), data=data3)
summary(ols7d)

ols8d<-lm(reformulate(c("treat*round2+accu_both+accu_int+high_accuracy*treat+ high_accuracy*round2+high_accuracy",covariates_dem), response="a"), data=data3)
summary(ols8d)

ols9d<-lm(reformulate(c("treat*round2+accu_both+accu_int+high_accuracy*treat+ high_accuracy*round2+high_accuracy",covariates_all), response="a"), data=data3)
summary(ols9d)





##b##Credibility
#Confirmation,Contradiction
ols1e<-lm(reformulate(c("contradiction*round2+cred_con+contradiction*high_credibility+round2*high_credibility+high_credibility"), response="b"), data=data1)
summary(ols1e)

ols2e<-lm(reformulate(c("contradiction*round2+cred_con+contradiction*high_credibility+round2*high_credibility+high_credibility",covariates_dem), response="b"), data=data1)
summary(ols2e)

ols3e<-lm(reformulate(c("contradiction*round2+cred_con+contradiction*high_credibility+round2*high_credibility+high_credibility",covariates_all), response="b"), data=data1)
summary(ols3e)

#Confirmation
ols4e<-lm(reformulate(c("treat*round2+cred_both+cred_int+treat*high_credibility+round2*high_credibility+high_credibility"), response="b"), data=data2)
summary(ols4e)

ols5e<-lm(reformulate(c("treat*round2+cred_both+cred_int+treat*high_credibility+round2*high_credibility+high_credibility",covariates_dem), response="b"), data=data2)
summary(ols5e)

ols6e<-lm(reformulate(c("treat*round2+cred_both+cred_int+treat*high_credibility+round2*high_credibility+high_credibility",covariates_all), response="b"), data=data2)
summary(ols6e)

#Contradiction
ols7e<-lm(reformulate(c("treat*round2+cred_both+cred_int+treat*high_credibility+round2*high_credibility+high_credibility"), response="b"), data=data3)
summary(ols7e)

ols8e<-lm(reformulate(c("treat*round2+cred_both+cred_int+treat*high_credibility+round2*high_credibility+high_credibility",covariates_dem), response="b"), data=data3)
summary(ols8e)

ols9e<-lm(reformulate(c("treat*round2+cred_both+cred_int+treat*high_credibility+round2*high_credibility+high_credibility",covariates_all), response="b"), data=data3)
summary(ols9e)

##a##Credibility
#Confirmation,Contradiction
ols1f<-lm(reformulate(c("contradiction*round2+cred_con+contradiction*high_credibility+round2*high_credibility+high_credibility"), response="a"), data=data1)
summary(ols1f)

ols2f<-lm(reformulate(c("contradiction*round2+cred_con+contradiction*high_credibility+round2*high_credibility+high_credibility",covariates_dem), response="a"), data=data1)
summary(ols2f)

ols3f<-lm(reformulate(c("contradiction*round2+cred_con+contradiction*high_credibility+round2*high_credibility+high_credibility",covariates_all), response="a"), data=data1)
summary(ols3f)

#Confirmation
ols4f<-lm(reformulate(c("treat*round2+cred_both+cred_int+treat*high_credibility+round2*high_credibility+high_credibility"), response="a"), data=data2)
summary(ols4f)

ols5f<-lm(reformulate(c("treat*round2+cred_both+cred_int+treat*high_credibility+round2*high_credibility+high_credibility",covariates_dem), response="a"), data=data2)
summary(ols5f)

ols6f<-lm(reformulate(c("treat*round2+cred_both+cred_int+treat*high_credibility+round2*high_credibility+high_credibility",covariates_all), response="a"), data=data2)
summary(ols6f)

#Contradiction
ols7f<-lm(reformulate(c("treat*round2+cred_both+cred_int+treat*high_credibility+round2*high_credibility+high_credibility"), response="a"), data=data3)
summary(ols7f)

ols8f<-lm(reformulate(c("treat*round2+cred_both+cred_int+treat*high_credibility+round2*high_credibility+high_credibility",covariates_dem), response="a"), data=data3)
summary(ols8f)

ols9f<-lm(reformulate(c("treat*round2+cred_both+cred_int+treat*high_credibility+round2*high_credibility+high_credibility",covariates_all), response="a"), data=data3)
summary(ols9f)



##b##Forecast usage
#Confirmation,Contradiction
ols1g<-lm(reformulate(c("contradiction*round2+fore_con+contradiction*high_usage+round2*high_usage+high_usage"), response="b"), data=data1)
summary(ols1g)

ols2g<-lm(reformulate(c("contradiction*round2+fore_con+contradiction*high_usage+round2*high_usage+high_usage",covariates_dem), response="b"), data=data1)
summary(ols2g)

ols3g<-lm(reformulate(c("contradiction*round2+fore_con+contradiction*high_usage+round2*high_usage+high_usage",covariates_all), response="b"), data=data1)
summary(ols3g)

#Confirmation
ols4g<-lm(reformulate(c("treat*round2+fore_both+fore_int+treat*high_usage+round2*high_usage+high_usage"), response="b"), data=data2)
summary(ols4g)

ols5g<-lm(reformulate(c("treat*round2+fore_both+fore_int+treat*high_usage+round2*high_usage+high_usage",covariates_dem), response="b"), data=data2)
summary(ols5g)

ols6g<-lm(reformulate(c("treat*round2+fore_both+fore_int+treat*high_usage+round2*high_usage+high_usage",covariates_all), response="b"), data=data2)
summary(ols6g)

#Contradiction
ols7g<-lm(reformulate(c("treat*round2+fore_both+fore_int+treat*high_usage+round2*high_usage+high_usage"), response="b"), data=data3)
summary(ols7g)

ols8g<-lm(reformulate(c("treat*round2+fore_both+fore_int+treat*high_usage+round2*high_usage+high_usage",covariates_dem), response="b"), data=data3)
summary(ols8g)

ols9g<-lm(reformulate(c("treat*round2+fore_both+fore_int+treat*high_usage+round2*high_usage+high_usage",covariates_all), response="b"), data=data3)
summary(ols9g)

##a##Forecast Usage
#Confirmation,Contradiction
ols1h<-lm(reformulate(c("contradiction*round2+fore_con+contradiction*high_usage+round2*high_usage+high_usage"), response="a"), data=data1)
summary(ols1h)

ols2h<-lm(reformulate(c("contradiction*round2+fore_con+contradiction*high_usage+round2*high_usage+high_usage",covariates_dem), response="a"), data=data1)
summary(ols2h)

ols3h<-lm(reformulate(c("contradiction*round2+fore_con+contradiction*high_usage+round2*high_usage+high_usage",covariates_all), response="a"), data=data1)
summary(ols3h)

#Confirmation
ols4h<-lm(reformulate(c("treat*round2+fore_both+fore_int+treat*high_usage+round2*high_usage+high_usage"), response="a"), data=data2)
summary(ols4h)

ols5h<-lm(reformulate(c("treat*round2+fore_both+fore_int+treat*high_usage+round2*high_usage+high_usage",covariates_dem), response="a"), data=data2)
summary(ols5h)

ols6h<-lm(reformulate(c("treat*round2+fore_both+fore_int+treat*high_usage+round2*high_usage+high_usage",covariates_all), response="a"), data=data2)
summary(ols6h)

#Contradiction
ols7h<-lm(reformulate(c("treat*round2+fore_both+fore_int+treat*high_usage+round2*high_usage+high_usage"), response="a"), data=data3)
summary(ols7h)

ols8h<-lm(reformulate(c("treat*round2+fore_both+fore_int+treat*high_usage+round2*high_usage+high_usage",covariates_dem), response="a"), data=data3)
summary(ols8h)

ols9h<-lm(reformulate(c("treat*round2+fore_both+fore_int+treat*high_usage+round2*high_usage+high_usage",covariates_all), response="a"), data=data3)
summary(ols9h)




##b##Temperature
#Confirmation,Contradiction
ols1i<-lm(reformulate(c("contradiction*round2+temp_con+contradiction*high_temperature+round2*high_temperature+high_temperature"), response="b"), data=data1)
summary(ols1i)

ols2i<-lm(reformulate(c("contradiction*round2+temp_con+contradiction*high_temperature+round2*high_temperature+high_temperature",covariates_dem), response="b"), data=data1)
summary(ols2i)

ols3i<-lm(reformulate(c("contradiction*round2+temp_con+contradiction*high_temperature+round2*high_temperature+high_temperature",covariates_all), response="b"), data=data1)
summary(ols3i)

#Confirmation
ols4i<-lm(reformulate(c("treat*round2+temp_both+temp_int+treat*high_temperature+round2*high_temperature+high_temperature"), response="b"), data=data2)
summary(ols4i)

ols5i<-lm(reformulate(c("treat*round2+temp_both+temp_int+treat*high_temperature+round2*high_temperature+high_temperature",covariates_dem), response="b"), data=data2)
summary(ols5i)

ols6i<-lm(reformulate(c("treat*round2+temp_both+temp_int+treat*high_temperature+round2*high_temperature+high_temperature",covariates_all), response="b"), data=data2)
summary(ols6i)

#Contradiction
ols7i<-lm(reformulate(c("treat*round2+temp_both+temp_int+treat*high_temperature+round2*high_temperature+high_temperature"), response="b"), data=data3)
summary(ols7i)

ols8i<-lm(reformulate(c("treat*round2+temp_both+temp_int+treat*high_temperature+round2*high_temperature+high_temperature",covariates_dem), response="b"), data=data3)
summary(ols8i)

ols9i<-lm(reformulate(c("treat*round2+temp_both+temp_int+treat*high_temperature+round2*high_temperature+high_temperature",covariates_all), response="b"), data=data3)
summary(ols9i)

##a##Temperature
#Confirmation,Contradiction
ols1j<-lm(reformulate(c("contradiction*round2+temp_con+contradiction*high_temperature+round2*high_temperature+high_temperature"), response="a"), data=data1)
summary(ols1j)

ols2j<-lm(reformulate(c("contradiction*round2+temp_con+contradiction*high_temperature+round2*high_temperature+high_temperature",covariates_dem), response="a"), data=data1)
summary(ols2j)

ols3j<-lm(reformulate(c("contradiction*round2+temp_con+contradiction*high_temperature+round2*high_temperature+high_temperature",covariates_all), response="a"), data=data1)
summary(ols3j)

#Confirmation
ols4j<-lm(reformulate(c("treat*round2+temp_both+temp_int+treat*high_temperature+round2*high_temperature+high_temperature"), response="a"), data=data2)
summary(ols4j)

ols5j<-lm(reformulate(c("treat*round2+temp_both+temp_int+treat*high_temperature+round2*high_temperature+high_temperature",covariates_dem), response="a"), data=data2)
summary(ols5j)

ols6j<-lm(reformulate(c("treat*round2+temp_both+temp_int+treat*high_temperature+round2*high_temperature+high_temperature",covariates_all), response="a"), data=data2)
summary(ols6j)

#Contradiction
ols7j<-lm(reformulate(c("treat*round2+temp_both+temp_int+treat*high_temperature+round2*high_temperature+high_temperature"), response="a"), data=data3)
summary(ols7j)

ols8j<-lm(reformulate(c("treat*round2+temp_both+temp_int+treat*high_temperature+round2*high_temperature+high_temperature",covariates_dem), response="a"), data=data3)
summary(ols8j)

ols9j<-lm(reformulate(c("treat*round2+temp_both+temp_int+treat*high_temperature+round2*high_temperature+high_temperature",covariates_all), response="a"), data=data3)
summary(ols9j)



##b##high education
#Confirmation,Contradiction
ols1k<-lm(reformulate(c("contradiction*round2+study_con+high_education*round2+high_education*contradiction+high_education"), response="b"), data=data1)
summary(ols1k)

ols2k<-lm(reformulate(c("contradiction*round2+study_con+high_education*round2+high_education*contradiction+high_education",covariates_dem), response="b"), data=data1)
summary(ols2k)

ols3k<-lm(reformulate(c("contradiction*round2+study_con+high_education*round2+high_education*contradiction+high_education",covariates_all), response="b"), data=data1)
summary(ols3k)

#Confirmation
ols4k<-lm(reformulate(c("treat*round2+study_both+study_int+high_education*round2+high_education*treat+high_education"), response="b"), data=data2)
summary(ols4k)

ols5k<-lm(reformulate(c("treat*round2+study_both+study_int+high_education*round2+high_education*treat+high_education",covariates_dem), response="b"), data=data2)
summary(ols5k)

ols6k<-lm(reformulate(c("treat*round2+study_both+study_int+high_education*round2+high_education*treat+high_education",covariates_all), response="b"), data=data2)
summary(ols6k)

#Contradiction
ols7k<-lm(reformulate(c("treat*round2+study_both+study_int+high_education*round2+high_education*treat+high_education"), response="b"), data=data3)
summary(ols7k)

ols8k<-lm(reformulate(c("treat*round2+study_both+study_int+high_education*round2+high_education*treat+high_education",covariates_dem), response="b"), data=data3)
summary(ols8k)

ols9k<-lm(reformulate(c("treat*round2+study_both+study_int+high_education*round2+high_education*treat+high_education",covariates_all), response="b"), data=data3)
summary(ols9k)

##a##study
#Confirmation,Contradiction
ols1l<-lm(reformulate(c("contradiction*round2+study_con+high_education*round2+high_education*contradiction+high_education"), response="a"), data=data1)
summary(ols1l)

ols2l<-lm(reformulate(c("contradiction*round2+study_con+high_education*round2+high_education*contradiction+high_education",covariates_dem), response="a"), data=data1)
summary(ols2l)

ols3l<-lm(reformulate(c("contradiction*round2+study_con+high_education*round2+high_education*contradiction+high_education",covariates_all), response="a"), data=data1)
summary(ols3l)

#Confirmation
ols4l<-lm(reformulate(c("treat*round2+study_both+study_int+high_education*round2+high_education*treat+high_education"), response="a"), data=data2)
summary(ols4l)

ols5l<-lm(reformulate(c("treat*round2+study_both+study_int+high_education*round2+high_education*treat+high_education",covariates_dem), response="a"), data=data2)
summary(ols5l)

ols6l<-lm(reformulate(c("treat*round2+study_both+study_int+high_education*round2+high_education*treat+high_education",covariates_all), response="a"), data=data2)
summary(ols6l)

#Contradiction
ols7l<-lm(reformulate(c("treat*round2+study_both+study_int+high_education*round2+high_education*treat+high_education"), response="a"), data=data3)
summary(ols7l)

ols8l<-lm(reformulate(c("treat*round2+study_both+study_int+high_education*round2+high_education*treat+high_education",covariates_dem), response="a"), data=data3)
summary(ols8l)

ols9l<-lm(reformulate(c("treat*round2+study_both+study_int+high_education*round2+high_education*treat+high_education",covariates_all), response="a"), data=data3)
summary(ols9l)





#Graph 1: DiDs Contradiction in comparison to Confirmation
# no control variables
cie11<-coefci(ols1, parm=c("contradiction:round2"), vcov = vcovCL(ols1, cluster=~data1$participant.label, type="HC1"),level=0.95)
cice11<-ols1$coefficients[c("contradiction:round2")]
ci11<-cbind(cie11,cice11)
event<-c("b (not female)")
ci11<-cbind(ci11,event)
ci11<-data.frame(ci11)

cie31<-coefci(ols1, parm=c("female_con"), vcov = vcovCL(ols1, cluster=~data1$participant.label, type="HC1"),level=0.95)
cice31<-ols1$coefficients[c("female_con")]
ci31<-cbind(cie31,cice31)
event<-c("b (female DDD)")
ci31<-cbind(ci31,event)
ci31<-data.frame(ci31)
names(ci31)[3]<-paste("cice11")

cie21<-coefci(ols1a, parm=c("contradiction:round2"), vcov = vcovCL(ols1a, cluster=~data1$participant.label, type="HC1"),level=0.95)
cice21<-ols1a$coefficients[c("contradiction:round2")]
ci21<-cbind(cie21,cice21)
event<-c("a (not female)")
ci21<-cbind(ci21,event)
ci21<-data.frame(ci21)
names(ci21)[3]<-paste("cice11")

cie41<-coefci(ols1a, parm=c("female_con"), vcov = vcovCL(ols1a, cluster=~data1$participant.label, type="HC1"),level=0.95)
cice41<-ols1a$coefficients[c("female_con")]
ci41<-cbind(cie41,cice41)
event<-c("a (female DDD)")
ci41<-cbind(ci41,event)
ci41<-data.frame(ci41)
names(ci41)[3]<-paste("cice11")


cie51<-coefci(ols1c, parm=c("contradiction:round2"), vcov = vcovCL(ols1c, cluster=~data1$participant.label, type="HC1"),level=0.95)
cice51<-ols1c$coefficients[c("contradiction:round2")]
ci51<-cbind(cie51,cice51)
event<-c("b (less accurate)")
ci51<-cbind(ci51,event)
ci51<-data.frame(ci51)
names(ci51)[3]<-paste("cice11")

cie61<-coefci(ols1c, parm=c("accu_con"), vcov = vcovCL(ols1c, cluster=~data1$participant.label, type="HC1"),level=0.95)
cice61<-ols1c$coefficients[c("accu_con")]
ci61<-cbind(cie61,cice61)
event<-c("b (accurate DDD)")
ci61<-cbind(ci61,event)
ci61<-data.frame(ci61)
names(ci61)[3]<-paste("cice11")


cie71<-coefci(ols1d, parm=c("contradiction:round2"), vcov = vcovCL(ols1d, cluster=~data1$participant.label, type="HC1"),level=0.95)
cice71<-ols1d$coefficients[c("contradiction:round2")]
ci71<-cbind(cie71,cice71)
event<-c("a (less accurate)")
ci71<-cbind(ci71,event)
ci71<-data.frame(ci71)
names(ci71)[3]<-paste("cice11")

cie81<-coefci(ols1d, parm=c("accu_con"), vcov = vcovCL(ols1d, cluster=~data1$participant.label, type="HC1"),level=0.95)
cice81<-ols1d$coefficients[c("accu_con")]
ci81<-cbind(cie81,cice81)
event<-c("a (accurate DDD)")
ci81<-cbind(ci81,event)
ci81<-data.frame(ci81)
names(ci81)[3]<-paste("cice11")

cie91<-coefci(ols1e, parm=c("contradiction:round2"), vcov = vcovCL(ols1e, cluster=~data1$participant.label, type="HC1"),level=0.95)
cice91<-ols1e$coefficients[c("contradiction:round2")]
ci91<-cbind(cie91,cice91)
event<-c("b (less credible)")
ci91<-cbind(ci91,event)
ci91<-data.frame(ci91)
names(ci91)[3]<-paste("cice11")

cie101<-coefci(ols1e, parm=c("cred_con"), vcov = vcovCL(ols1e, cluster=~data1$participant.label, type="HC1"),level=0.95)
cice101<-ols1e$coefficients[c("cred_con")]
ci101<-cbind(cie101,cice101)
event<-c("b (credible DDD)")
ci101<-cbind(ci101,event)
ci101<-data.frame(ci101)
names(ci101)[3]<-paste("cice11")

cie111<-coefci(ols1f, parm=c("contradiction:round2"), vcov = vcovCL(ols1f, cluster=~data1$participant.label, type="HC1"),level=0.95)
cice111<-ols1f$coefficients[c("contradiction:round2")]
ci111<-cbind(cie111,cice111)
event<-c("a (less credible)")
ci111<-cbind(ci111,event)
ci111<-data.frame(ci111)
names(ci111)[3]<-paste("cice11")

cie121<-coefci(ols1f, parm=c("cred_con"), vcov = vcovCL(ols1f, cluster=~data1$participant.label, type="HC1"),level=0.95)
cice121<-ols1f$coefficients[c("cred_con")]
ci121<-cbind(cie121,cice121)
event<-c("a (credible DDD)")
ci121<-cbind(ci121,event)
ci121<-data.frame(ci121)
names(ci121)[3]<-paste("cice11")

cie131<-coefci(ols1g, parm=c("contradiction:round2"), vcov = vcovCL(ols1g, cluster=~data1$participant.label, type="HC1"),level=0.95)
cice131<-ols1g$coefficients[c("contradiction:round2")]
ci131<-cbind(cie131,cice131)
event<-c("b (less forecast usage)")
ci131<-cbind(ci131,event)
ci131<-data.frame(ci131)
names(ci131)[3]<-paste("cice11")

cie141<-coefci(ols1g, parm=c("fore_con"), vcov = vcovCL(ols1g, cluster=~data1$participant.label, type="HC1"),level=0.95)
cice141<-ols1g$coefficients[c("fore_con")]
ci141<-cbind(cie141,cice141)
event<-c("b (forecast usage DDD)")
ci141<-cbind(ci141,event)
ci141<-data.frame(ci141)
names(ci141)[3]<-paste("cice11")

cie151<-coefci(ols1h, parm=c("contradiction:round2"), vcov = vcovCL(ols1h, cluster=~data1$participant.label, type="HC1"),level=0.95)
cice151<-ols1h$coefficients[c("contradiction:round2")]
ci151<-cbind(cie151,cice151)
event<-c("a (less forecast usage)")
ci151<-cbind(ci151,event)
ci151<-data.frame(ci151)
names(ci151)[3]<-paste("cice11")

cie161<-coefci(ols1h, parm=c("fore_con"), vcov = vcovCL(ols1h, cluster=~data1$participant.label, type="HC1"),level=0.95)
cice161<-ols1h$coefficients[c("fore_con")]
ci161<-cbind(cie161,cice161)
event<-c("a (forecast usage DDD)")
ci161<-cbind(ci161,event)
ci161<-data.frame(ci161)
names(ci161)[3]<-paste("cice11")

cie171<-coefci(ols1i, parm=c("contradiction:round2"), vcov = vcovCL(ols1i, cluster=~data1$participant.label, type="HC1"),level=0.95)
cice171<-ols1i$coefficients[c("contradiction:round2")]
ci171<-cbind(cie171,cice171)
event<-c("b (lower temperature)")
ci171<-cbind(ci171,event)
ci171<-data.frame(ci171)
names(ci171)[3]<-paste("cice11")

cie181<-coefci(ols1i, parm=c("temp_con"), vcov = vcovCL(ols1i, cluster=~data1$participant.label, type="HC1"),level=0.95)
cice181<-ols1i$coefficients[c("temp_con")]
ci181<-cbind(cie181,cice181)
event<-c("b (temperature DDD)")
ci181<-cbind(ci181,event)
ci181<-data.frame(ci181)
names(ci181)[3]<-paste("cice11")

cie191<-coefci(ols1j, parm=c("contradiction:round2"), vcov = vcovCL(ols1j, cluster=~data1$participant.label, type="HC1"),level=0.95)
cice191<-ols1j$coefficients[c("contradiction:round2")]
ci191<-cbind(cie191,cice191)
event<-c("a (lower temperature)")
ci191<-cbind(ci191,event)
ci191<-data.frame(ci191)
names(ci191)[3]<-paste("cice11")

cie201<-coefci(ols1j, parm=c("temp_con"), vcov = vcovCL(ols1j, cluster=~data1$participant.label, type="HC1"),level=0.95)
cice201<-ols1j$coefficients[c("temp_con")]
ci201<-cbind(cie201,cice201)
event<-c("a (temperature DDD)")
ci201<-cbind(ci201,event)
ci201<-data.frame(ci201)
names(ci201)[3]<-paste("cice11")


cie211<-coefci(ols1k, parm=c("contradiction:round2"), vcov = vcovCL(ols1k, cluster=~data1$participant.label, type="HC1"),level=0.95)
cice211<-ols1k$coefficients[c("contradiction:round2")]
ci211<-cbind(cie211,cice211)
event<-c("b (lower education)")
ci211<-cbind(ci211,event)
ci211<-data.frame(ci211)
names(ci211)[3]<-paste("cice11")

cie221<-coefci(ols1k, parm=c("study_con"), vcov = vcovCL(ols1k, cluster=~data1$participant.label, type="HC1"),level=0.95)
cice221<-ols1k$coefficients[c("study_con")]
ci221<-cbind(cie221,cice221)
event<-c("b (education DDD)")
ci221<-cbind(ci221,event)
ci221<-data.frame(ci221)
names(ci221)[3]<-paste("cice11")

cie231<-coefci(ols1l, parm=c("contradiction:round2"), vcov = vcovCL(ols1l, cluster=~data1$participant.label, type="HC1"),level=0.95)
cice231<-ols1l$coefficients[c("contradiction:round2")]
ci231<-cbind(cie231,cice231)
event<-c("a (lower education)")
ci231<-cbind(ci231,event)
ci231<-data.frame(ci231)
names(ci231)[3]<-paste("cice11")

cie241<-coefci(ols1l, parm=c("study_con"), vcov = vcovCL(ols1l, cluster=~data1$participant.label, type="HC1"),level=0.95)
cice241<-ols1l$coefficients[c("study_con")]
ci241<-cbind(cie241,cice241)
event<-c("a (education DDD)")
ci241<-cbind(ci241,event)
ci241<-data.frame(ci241)
names(ci241)[3]<-paste("cice11")

cidata<-rbind(ci11,ci31,ci21,ci41,ci51,ci61,ci71,ci81,ci91,ci101,ci111,ci121,ci131,ci141,ci151,ci161,ci171,ci181,ci191,ci201,ci211,ci221,ci231,ci241)
cidata$X2.5..<-as.numeric(cidata$X2.5..)
cidata$X97.5..<-as.numeric(cidata$X97.5..)
cidata$cice11<-as.numeric(cidata$cice11)
cidata$controls<-"1.) none"

#with demographic variables
cie11<-coefci(ols2, parm=c("contradiction:round2"), vcov = vcovCL(ols2, cluster=~data1$participant.label, type="HC1"),level=0.95)
cice11<-ols2$coefficients[c("contradiction:round2")]
ci11<-cbind(cie11,cice11)
event<-c("b (not female)")
ci11<-cbind(ci11,event)
ci11<-data.frame(ci11)

cie31<-coefci(ols2, parm=c("female_con"), vcov = vcovCL(ols2, cluster=~data1$participant.label, type="HC1"),level=0.95)
cice31<-ols2$coefficients[c("female_con")]
ci31<-cbind(cie31,cice31)
event<-c("b (female DDD)")
ci31<-cbind(ci31,event)
ci31<-data.frame(ci31)
names(ci31)[3]<-paste("cice11")

cie21<-coefci(ols2a, parm=c("contradiction:round2"), vcov = vcovCL(ols2a, cluster=~data1$participant.label, type="HC1"),level=0.95)
cice21<-ols2a$coefficients[c("contradiction:round2")]
ci21<-cbind(cie21,cice21)
event<-c("a (not female)")
ci21<-cbind(ci21,event)
ci21<-data.frame(ci21)
names(ci21)[3]<-paste("cice11")

cie41<-coefci(ols2a, parm=c("female_con"), vcov = vcovCL(ols2a, cluster=~data1$participant.label, type="HC1"),level=0.95)
cice41<-ols2a$coefficients[c("female_con")]
ci41<-cbind(cie41,cice41)
event<-c("a (female DDD)")
ci41<-cbind(ci41,event)
ci41<-data.frame(ci41)
names(ci41)[3]<-paste("cice11")


cie51<-coefci(ols2c, parm=c("contradiction:round2"), vcov = vcovCL(ols2c, cluster=~data1$participant.label, type="HC1"),level=0.95)
cice51<-ols2c$coefficients[c("contradiction:round2")]
ci51<-cbind(cie51,cice51)
event<-c("b (less accurate)")
ci51<-cbind(ci51,event)
ci51<-data.frame(ci51)
names(ci51)[3]<-paste("cice11")

cie61<-coefci(ols2c, parm=c("accu_con"), vcov = vcovCL(ols2c, cluster=~data1$participant.label, type="HC1"),level=0.95)
cice61<-ols2c$coefficients[c("accu_con")]
ci61<-cbind(cie61,cice61)
event<-c("b (accurate DDD)")
ci61<-cbind(ci61,event)
ci61<-data.frame(ci61)
names(ci61)[3]<-paste("cice11")


cie71<-coefci(ols2d, parm=c("contradiction:round2"), vcov = vcovCL(ols2d, cluster=~data1$participant.label, type="HC1"),level=0.95)
cice71<-ols2d$coefficients[c("contradiction:round2")]
ci71<-cbind(cie71,cice71)
event<-c("a (less accurate)")
ci71<-cbind(ci71,event)
ci71<-data.frame(ci71)
names(ci71)[3]<-paste("cice11")

cie81<-coefci(ols2d, parm=c("accu_con"), vcov = vcovCL(ols2d, cluster=~data1$participant.label, type="HC1"),level=0.95)
cice81<-ols2d$coefficients[c("accu_con")]
ci81<-cbind(cie81,cice81)
event<-c("a (accurate DDD)")
ci81<-cbind(ci81,event)
ci81<-data.frame(ci81)
names(ci81)[3]<-paste("cice11")

cie91<-coefci(ols2e, parm=c("contradiction:round2"), vcov = vcovCL(ols2e, cluster=~data1$participant.label, type="HC1"),level=0.95)
cice91<-ols2e$coefficients[c("contradiction:round2")]
ci91<-cbind(cie91,cice91)
event<-c("b (less credible)")
ci91<-cbind(ci91,event)
ci91<-data.frame(ci91)
names(ci91)[3]<-paste("cice11")

cie101<-coefci(ols2e, parm=c("cred_con"), vcov = vcovCL(ols2e, cluster=~data1$participant.label, type="HC1"),level=0.95)
cice101<-ols2e$coefficients[c("cred_con")]
ci101<-cbind(cie101,cice101)
event<-c("b (credible DDD)")
ci101<-cbind(ci101,event)
ci101<-data.frame(ci101)
names(ci101)[3]<-paste("cice11")

cie111<-coefci(ols2f, parm=c("contradiction:round2"), vcov = vcovCL(ols2f, cluster=~data1$participant.label, type="HC1"),level=0.95)
cice111<-ols2f$coefficients[c("contradiction:round2")]
ci111<-cbind(cie111,cice111)
event<-c("a (less credible)")
ci111<-cbind(ci111,event)
ci111<-data.frame(ci111)
names(ci111)[3]<-paste("cice11")

cie121<-coefci(ols2f, parm=c("cred_con"), vcov = vcovCL(ols2f, cluster=~data1$participant.label, type="HC1"),level=0.95)
cice121<-ols2f$coefficients[c("cred_con")]
ci121<-cbind(cie121,cice121)
event<-c("a (credible DDD)")
ci121<-cbind(ci121,event)
ci121<-data.frame(ci121)
names(ci121)[3]<-paste("cice11")

cie131<-coefci(ols2g, parm=c("contradiction:round2"), vcov = vcovCL(ols2g, cluster=~data1$participant.label, type="HC1"),level=0.95)
cice131<-ols2g$coefficients[c("contradiction:round2")]
ci131<-cbind(cie131,cice131)
event<-c("b (less forecast usage)")
ci131<-cbind(ci131,event)
ci131<-data.frame(ci131)
names(ci131)[3]<-paste("cice11")

cie141<-coefci(ols2g, parm=c("fore_con"), vcov = vcovCL(ols2g, cluster=~data1$participant.label, type="HC1"),level=0.95)
cice141<-ols2g$coefficients[c("fore_con")]
ci141<-cbind(cie141,cice141)
event<-c("b (forecast usage DDD)")
ci141<-cbind(ci141,event)
ci141<-data.frame(ci141)
names(ci141)[3]<-paste("cice11")

cie151<-coefci(ols2h, parm=c("contradiction:round2"), vcov = vcovCL(ols2h, cluster=~data1$participant.label, type="HC1"),level=0.95)
cice151<-ols2h$coefficients[c("contradiction:round2")]
ci151<-cbind(cie151,cice151)
event<-c("a (less forecast usage)")
ci151<-cbind(ci151,event)
ci151<-data.frame(ci151)
names(ci151)[3]<-paste("cice11")

cie161<-coefci(ols2h, parm=c("fore_con"), vcov = vcovCL(ols2h, cluster=~data1$participant.label, type="HC1"),level=0.95)
cice161<-ols2h$coefficients[c("fore_con")]
ci161<-cbind(cie161,cice161)
event<-c("a (forecast usage DDD)")
ci161<-cbind(ci161,event)
ci161<-data.frame(ci161)
names(ci161)[3]<-paste("cice11")

cie171<-coefci(ols2i, parm=c("contradiction:round2"), vcov = vcovCL(ols2i, cluster=~data1$participant.label, type="HC1"),level=0.95)
cice171<-ols2i$coefficients[c("contradiction:round2")]
ci171<-cbind(cie171,cice171)
event<-c("b (lower temperature)")
ci171<-cbind(ci171,event)
ci171<-data.frame(ci171)
names(ci171)[3]<-paste("cice11")

cie181<-coefci(ols2i, parm=c("temp_con"), vcov = vcovCL(ols2i, cluster=~data1$participant.label, type="HC1"),level=0.95)
cice181<-ols2i$coefficients[c("temp_con")]
ci181<-cbind(cie181,cice181)
event<-c("b (temperature DDD)")
ci181<-cbind(ci181,event)
ci181<-data.frame(ci181)
names(ci181)[3]<-paste("cice11")

cie191<-coefci(ols2j, parm=c("contradiction:round2"), vcov = vcovCL(ols2j, cluster=~data1$participant.label, type="HC1"),level=0.95)
cice191<-ols2j$coefficients[c("contradiction:round2")]
ci191<-cbind(cie191,cice191)
event<-c("a (lower temperature)")
ci191<-cbind(ci191,event)
ci191<-data.frame(ci191)
names(ci191)[3]<-paste("cice11")

cie201<-coefci(ols2j, parm=c("temp_con"), vcov = vcovCL(ols2j, cluster=~data1$participant.label, type="HC1"),level=0.95)
cice201<-ols2j$coefficients[c("temp_con")]
ci201<-cbind(cie201,cice201)
event<-c("a (temperature DDD)")
ci201<-cbind(ci201,event)
ci201<-data.frame(ci201)
names(ci201)[3]<-paste("cice11")

cie211<-coefci(ols2k, parm=c("contradiction:round2"), vcov = vcovCL(ols2k, cluster=~data1$participant.label, type="HC1"),level=0.95)
cice211<-ols2k$coefficients[c("contradiction:round2")]
ci211<-cbind(cie211,cice211)
event<-c("b (lower education)")
ci211<-cbind(ci211,event)
ci211<-data.frame(ci211)
names(ci211)[3]<-paste("cice11")

cie221<-coefci(ols2k, parm=c("study_con"), vcov = vcovCL(ols2k, cluster=~data1$participant.label, type="HC1"),level=0.95)
cice221<-ols2k$coefficients[c("study_con")]
ci221<-cbind(cie221,cice221)
event<-c("b (education DDD)")
ci221<-cbind(ci221,event)
ci221<-data.frame(ci221)
names(ci221)[3]<-paste("cice11")

cie231<-coefci(ols2l, parm=c("contradiction:round2"), vcov = vcovCL(ols2l, cluster=~data1$participant.label, type="HC1"),level=0.95)
cice231<-ols2l$coefficients[c("contradiction:round2")]
ci231<-cbind(cie231,cice231)
event<-c("a (lower education)")
ci231<-cbind(ci231,event)
ci231<-data.frame(ci231)
names(ci231)[3]<-paste("cice11")

cie241<-coefci(ols2l, parm=c("study_con"), vcov = vcovCL(ols2l, cluster=~data1$participant.label, type="HC1"),level=0.95)
cice241<-ols2l$coefficients[c("study_con")]
ci241<-cbind(cie241,cice241)
event<-c("a (education DDD)")
ci241<-cbind(ci241,event)
ci241<-data.frame(ci241)
names(ci241)[3]<-paste("cice11")


cidata2<-rbind(ci11,ci31,ci21,ci41,ci51,ci61,ci71,ci81,ci91,ci101,ci111,ci121,ci131,ci141,ci151,ci161,ci171,ci181,ci191,ci201,ci211,ci221,ci231,ci241)
cidata2$X2.5..<-as.numeric(cidata2$X2.5..)
cidata2$X97.5..<-as.numeric(cidata2$X97.5..)
cidata2$cice11<-as.numeric(cidata2$cice11)
cidata2$controls<-"2.) demographic"

cidata<-rbind(cidata,cidata2)


#with demographic variables + further controls
cie11<-coefci(ols3, parm=c("contradiction:round2"), vcov = vcovCL(ols3, cluster=~data1$participant.label, type="HC1"),level=0.95)
cice11<-ols3$coefficients[c("contradiction:round2")]
ci11<-cbind(cie11,cice11)
event<-c("b (not female)")
ci11<-cbind(ci11,event)
ci11<-data.frame(ci11)

cie31<-coefci(ols3, parm=c("female_con"), vcov = vcovCL(ols3, cluster=~data1$participant.label, type="HC1"),level=0.95)
cice31<-ols3$coefficients[c("female_con")]
ci31<-cbind(cie31,cice31)
event<-c("b (female DDD)")
ci31<-cbind(ci31,event)
ci31<-data.frame(ci31)
names(ci31)[3]<-paste("cice11")

cie21<-coefci(ols3a, parm=c("contradiction:round2"), vcov = vcovCL(ols3a, cluster=~data1$participant.label, type="HC1"),level=0.95)
cice21<-ols3a$coefficients[c("contradiction:round2")]
ci21<-cbind(cie21,cice21)
event<-c("a (not female)")
ci21<-cbind(ci21,event)
ci21<-data.frame(ci21)
names(ci21)[3]<-paste("cice11")

cie41<-coefci(ols3a, parm=c("female_con"), vcov = vcovCL(ols3a, cluster=~data1$participant.label, type="HC1"),level=0.95)
cice41<-ols3a$coefficients[c("female_con")]
ci41<-cbind(cie41,cice41)
event<-c("a (female DDD)")
ci41<-cbind(ci41,event)
ci41<-data.frame(ci41)
names(ci41)[3]<-paste("cice11")


cie51<-coefci(ols3c, parm=c("contradiction:round2"), vcov = vcovCL(ols3c, cluster=~data1$participant.label, type="HC1"),level=0.95)
cice51<-ols3c$coefficients[c("contradiction:round2")]
ci51<-cbind(cie51,cice51)
event<-c("b (less accurate)")
ci51<-cbind(ci51,event)
ci51<-data.frame(ci51)
names(ci51)[3]<-paste("cice11")

cie61<-coefci(ols3c, parm=c("accu_con"), vcov = vcovCL(ols3c, cluster=~data1$participant.label, type="HC1"),level=0.95)
cice61<-ols3c$coefficients[c("accu_con")]
ci61<-cbind(cie61,cice61)
event<-c("b (accurate DDD)")
ci61<-cbind(ci61,event)
ci61<-data.frame(ci61)
names(ci61)[3]<-paste("cice11")


cie71<-coefci(ols3d, parm=c("contradiction:round2"), vcov = vcovCL(ols3d, cluster=~data1$participant.label, type="HC1"),level=0.95)
cice71<-ols3d$coefficients[c("contradiction:round2")]
ci71<-cbind(cie71,cice71)
event<-c("a (less accurate)")
ci71<-cbind(ci71,event)
ci71<-data.frame(ci71)
names(ci71)[3]<-paste("cice11")

cie81<-coefci(ols3d, parm=c("accu_con"), vcov = vcovCL(ols3d, cluster=~data1$participant.label, type="HC1"),level=0.95)
cice81<-ols3d$coefficients[c("accu_con")]
ci81<-cbind(cie81,cice81)
event<-c("a (accurate DDD)")
ci81<-cbind(ci81,event)
ci81<-data.frame(ci81)
names(ci81)[3]<-paste("cice11")

cie91<-coefci(ols3e, parm=c("contradiction:round2"), vcov = vcovCL(ols3e, cluster=~data1$participant.label, type="HC1"),level=0.95)
cice91<-ols3e$coefficients[c("contradiction:round2")]
ci91<-cbind(cie91,cice91)
event<-c("b (less credible)")
ci91<-cbind(ci91,event)
ci91<-data.frame(ci91)
names(ci91)[3]<-paste("cice11")

cie101<-coefci(ols3e, parm=c("cred_con"), vcov = vcovCL(ols3e, cluster=~data1$participant.label, type="HC1"),level=0.95)
cice101<-ols3e$coefficients[c("cred_con")]
ci101<-cbind(cie101,cice101)
event<-c("b (credible DDD)")
ci101<-cbind(ci101,event)
ci101<-data.frame(ci101)
names(ci101)[3]<-paste("cice11")

cie111<-coefci(ols3f, parm=c("contradiction:round2"), vcov = vcovCL(ols3f, cluster=~data1$participant.label, type="HC1"),level=0.95)
cice111<-ols3f$coefficients[c("contradiction:round2")]
ci111<-cbind(cie111,cice111)
event<-c("a (less credible)")
ci111<-cbind(ci111,event)
ci111<-data.frame(ci111)
names(ci111)[3]<-paste("cice11")

cie121<-coefci(ols3f, parm=c("cred_con"), vcov = vcovCL(ols3f, cluster=~data1$participant.label, type="HC1"),level=0.95)
cice121<-ols3f$coefficients[c("cred_con")]
ci121<-cbind(cie121,cice121)
event<-c("a (credible DDD)")
ci121<-cbind(ci121,event)
ci121<-data.frame(ci121)
names(ci121)[3]<-paste("cice11")

cie131<-coefci(ols3g, parm=c("contradiction:round2"), vcov = vcovCL(ols3g, cluster=~data1$participant.label, type="HC1"),level=0.95)
cice131<-ols3g$coefficients[c("contradiction:round2")]
ci131<-cbind(cie131,cice131)
event<-c("b (less forecast usage)")
ci131<-cbind(ci131,event)
ci131<-data.frame(ci131)
names(ci131)[3]<-paste("cice11")

cie141<-coefci(ols3g, parm=c("fore_con"), vcov = vcovCL(ols3g, cluster=~data1$participant.label, type="HC1"),level=0.95)
cice141<-ols3g$coefficients[c("fore_con")]
ci141<-cbind(cie141,cice141)
event<-c("b (forecast usage DDD)")
ci141<-cbind(ci141,event)
ci141<-data.frame(ci141)
names(ci141)[3]<-paste("cice11")

cie151<-coefci(ols3h, parm=c("contradiction:round2"), vcov = vcovCL(ols3h, cluster=~data1$participant.label, type="HC1"),level=0.95)
cice151<-ols3h$coefficients[c("contradiction:round2")]
ci151<-cbind(cie151,cice151)
event<-c("a (less forecast usage)")
ci151<-cbind(ci151,event)
ci151<-data.frame(ci151)
names(ci151)[3]<-paste("cice11")

cie161<-coefci(ols3h, parm=c("fore_con"), vcov = vcovCL(ols3h, cluster=~data1$participant.label, type="HC1"),level=0.95)
cice161<-ols3h$coefficients[c("fore_con")]
ci161<-cbind(cie161,cice161)
event<-c("a (forecast usage DDD)")
ci161<-cbind(ci161,event)
ci161<-data.frame(ci161)
names(ci161)[3]<-paste("cice11")

cie171<-coefci(ols3i, parm=c("contradiction:round2"), vcov = vcovCL(ols3i, cluster=~data1$participant.label, type="HC1"),level=0.95)
cice171<-ols3i$coefficients[c("contradiction:round2")]
ci171<-cbind(cie171,cice171)
event<-c("b (lower temperature)")
ci171<-cbind(ci171,event)
ci171<-data.frame(ci171)
names(ci171)[3]<-paste("cice11")

cie181<-coefci(ols3i, parm=c("temp_con"), vcov = vcovCL(ols3i, cluster=~data1$participant.label, type="HC1"),level=0.95)
cice181<-ols3i$coefficients[c("temp_con")]
ci181<-cbind(cie181,cice181)
event<-c("b (temperature DDD)")
ci181<-cbind(ci181,event)
ci181<-data.frame(ci181)
names(ci181)[3]<-paste("cice11")

cie191<-coefci(ols3j, parm=c("contradiction:round2"), vcov = vcovCL(ols3j, cluster=~data1$participant.label, type="HC1"),level=0.95)
cice191<-ols3j$coefficients[c("contradiction:round2")]
ci191<-cbind(cie191,cice191)
event<-c("a (lower temperature)")
ci191<-cbind(ci191,event)
ci191<-data.frame(ci191)
names(ci191)[3]<-paste("cice11")

cie201<-coefci(ols3j, parm=c("temp_con"), vcov = vcovCL(ols3j, cluster=~data1$participant.label, type="HC1"),level=0.95)
cice201<-ols3j$coefficients[c("temp_con")]
ci201<-cbind(cie201,cice201)
event<-c("a (temperature DDD)")
ci201<-cbind(ci201,event)
ci201<-data.frame(ci201)
names(ci201)[3]<-paste("cice11")

cie211<-coefci(ols3k, parm=c("contradiction:round2"), vcov = vcovCL(ols3k, cluster=~data1$participant.label, type="HC1"),level=0.95)
cice211<-ols3k$coefficients[c("contradiction:round2")]
ci211<-cbind(cie211,cice211)
event<-c("b (lower education)")
ci211<-cbind(ci211,event)
ci211<-data.frame(ci211)
names(ci211)[3]<-paste("cice11")

cie221<-coefci(ols3k, parm=c("study_con"), vcov = vcovCL(ols3k, cluster=~data1$participant.label, type="HC1"),level=0.95)
cice221<-ols3k$coefficients[c("study_con")]
ci221<-cbind(cie221,cice221)
event<-c("b (education DDD)")
ci221<-cbind(ci221,event)
ci221<-data.frame(ci221)
names(ci221)[3]<-paste("cice11")

cie231<-coefci(ols3l, parm=c("contradiction:round2"), vcov = vcovCL(ols3l, cluster=~data1$participant.label, type="HC1"),level=0.95)
cice231<-ols3l$coefficients[c("contradiction:round2")]
ci231<-cbind(cie231,cice231)
event<-c("a (lower education)")
ci231<-cbind(ci231,event)
ci231<-data.frame(ci231)
names(ci231)[3]<-paste("cice11")

cie241<-coefci(ols3l, parm=c("study_con"), vcov = vcovCL(ols3l, cluster=~data1$participant.label, type="HC1"),level=0.95)
cice241<-ols3l$coefficients[c("study_con")]
ci241<-cbind(cie241,cice241)
event<-c("a (education DDD)")
ci241<-cbind(ci241,event)
ci241<-data.frame(ci241)
names(ci241)[3]<-paste("cice11")


cidata3<-rbind(ci11,ci31,ci21,ci41,ci51,ci61,ci71,ci81,ci91,ci101,ci111,ci121,ci131,ci141,ci151,ci161,ci171,ci181,ci191,ci201,ci211,ci221,ci231,ci241)
cidata3$X2.5..<-as.numeric(cidata3$X2.5..)
cidata3$X97.5..<-as.numeric(cidata3$X97.5..)
cidata3$cice11<-as.numeric(cidata3$cice11)
cidata3$controls<-"3.) demographic + further"

cidata<-rbind(cidata,cidata3)


cidata$controls<- with(cidata, factor(controls, levels=c("1.) none","2.) demographic", "3.) demographic + further")))

p1 <- ggplot(
  mapping = aes(
    y = cidata$event
  )
) +
  scale_y_discrete( limits=c("a (not female)","a (female DDD)", "b (not female)", "b (female DDD)","a (less accurate)","a (accurate DDD)", "b (less accurate)", "b (accurate DDD)","a (less credible)","a (credible DDD)", "b (less credible)", "b (credible DDD)","a (less forecast usage)","a (forecast usage DDD)", "b (less forecast usage)", "b (forecast usage DDD)","a (lower temperature)","a (temperature DDD)", "b (lower temperature)", "b (temperature DDD)","a (lower education)","a (education DDD)", "b (lower education)", "b (education DDD)") )+
  geom_pointrange(aes(
    x = cidata$cice11,
    y = cidata$event,
    xmin = cidata$X2.5..,
    xmax = cidata$X97.5..,
    shape=factor(cidata$controls, levels=c("3.) demographic + further","2.) demographic", "1.) none"))
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



ggsave(plot = p1, 
       filename = "../rendered_documents/pdf/Ambiguity_regs_het_all.pdf",
       height = 6.5, 
       width = 6.7)


###Graph 2 (Confirmation DiDs)
# no control variables
cie11<-coefci(ols4, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols4, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice11<-ols4$coefficients[c("treatconf.-both:round2")]
ci11<-cbind(cie11,cice11)
event<-c("b (both-not female)")
ci11<-cbind(ci11,event)
ci11<-data.frame(ci11)

cie31<-coefci(ols4, parm=c("female_both"), vcov = vcovCL(ols4, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice31<-ols4$coefficients[c("female_both")]
ci31<-cbind(cie31,cice31)
event<-c("b (both-female DDD)")
ci31<-cbind(ci31,event)
ci31<-data.frame(ci31)
names(ci31)[3]<-paste("cice11")

cie21<-coefci(ols4a, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols4a, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice21<-ols4a$coefficients[c("treatconf.-both:round2")]
ci21<-cbind(cie21,cice21)
event<-c("a (both-not female)")
ci21<-cbind(ci21,event)
ci21<-data.frame(ci21)
names(ci21)[3]<-paste("cice11")

cie41<-coefci(ols4a, parm=c("female_both"), vcov = vcovCL(ols4a, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice41<-ols4a$coefficients[c("female_both")]
ci41<-cbind(cie41,cice41)
event<-c("a (both-female DDD)")
ci41<-cbind(ci41,event)
ci41<-data.frame(ci41)
names(ci41)[3]<-paste("cice11")


cie51<-coefci(ols4c, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols4c, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice51<-ols4c$coefficients[c("treatconf.-both:round2")]
ci51<-cbind(cie51,cice51)
event<-c("b (both-less accurate)")
ci51<-cbind(ci51,event)
ci51<-data.frame(ci51)
names(ci51)[3]<-paste("cice11")

cie61<-coefci(ols4c, parm=c("accu_both"), vcov = vcovCL(ols4c, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice61<-ols4c$coefficients[c("accu_both")]
ci61<-cbind(cie61,cice61)
event<-c("b (both-accurate DDD)")
ci61<-cbind(ci61,event)
ci61<-data.frame(ci61)
names(ci61)[3]<-paste("cice11")


cie71<-coefci(ols4d, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols4d, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice71<-ols4d$coefficients[c("treatconf.-both:round2")]
ci71<-cbind(cie71,cice71)
event<-c("a (both-less accurate)")
ci71<-cbind(ci71,event)
ci71<-data.frame(ci71)
names(ci71)[3]<-paste("cice11")

cie81<-coefci(ols4d, parm=c("accu_both"), vcov = vcovCL(ols4d, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice81<-ols4d$coefficients[c("accu_both")]
ci81<-cbind(cie81,cice81)
event<-c("a (both-accurate DDD)")
ci81<-cbind(ci81,event)
ci81<-data.frame(ci81)
names(ci81)[3]<-paste("cice11")

cie91<-coefci(ols4e, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols4e, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice91<-ols4e$coefficients[c("treatconf.-both:round2")]
ci91<-cbind(cie91,cice91)
event<-c("b (both-less credible)")
ci91<-cbind(ci91,event)
ci91<-data.frame(ci91)
names(ci91)[3]<-paste("cice11")

cie101<-coefci(ols4e, parm=c("cred_both"), vcov = vcovCL(ols4e, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice101<-ols4e$coefficients[c("cred_both")]
ci101<-cbind(cie101,cice101)
event<-c("b (both-credible DDD)")
ci101<-cbind(ci101,event)
ci101<-data.frame(ci101)
names(ci101)[3]<-paste("cice11")

cie111<-coefci(ols4f, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols4f, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice111<-ols4f$coefficients[c("treatconf.-both:round2")]
ci111<-cbind(cie111,cice111)
event<-c("a (both-less credible)")
ci111<-cbind(ci111,event)
ci111<-data.frame(ci111)
names(ci111)[3]<-paste("cice11")

cie121<-coefci(ols4f, parm=c("cred_both"), vcov = vcovCL(ols4f, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice121<-ols4f$coefficients[c("cred_both")]
ci121<-cbind(cie121,cice121)
event<-c("a (both-credible DDD)")
ci121<-cbind(ci121,event)
ci121<-data.frame(ci121)
names(ci121)[3]<-paste("cice11")

cie131<-coefci(ols4g, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols4g, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice131<-ols4g$coefficients[c("treatconf.-both:round2")]
ci131<-cbind(cie131,cice131)
event<-c("b (both-less forecast usage)")
ci131<-cbind(ci131,event)
ci131<-data.frame(ci131)
names(ci131)[3]<-paste("cice11")

cie141<-coefci(ols4g, parm=c("fore_both"), vcov = vcovCL(ols4g, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice141<-ols4g$coefficients[c("fore_both")]
ci141<-cbind(cie141,cice141)
event<-c("b (both-forecast usage DDD)")
ci141<-cbind(ci141,event)
ci141<-data.frame(ci141)
names(ci141)[3]<-paste("cice11")

cie151<-coefci(ols4h, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols4h, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice151<-ols4h$coefficients[c("treatconf.-both:round2")]
ci151<-cbind(cie151,cice151)
event<-c("a (both-less forecast usage)")
ci151<-cbind(ci151,event)
ci151<-data.frame(ci151)
names(ci151)[3]<-paste("cice11")

cie161<-coefci(ols4h, parm=c("fore_both"), vcov = vcovCL(ols4h, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice161<-ols4h$coefficients[c("fore_both")]
ci161<-cbind(cie161,cice161)
event<-c("a (both-forecast usage DDD)")
ci161<-cbind(ci161,event)
ci161<-data.frame(ci161)
names(ci161)[3]<-paste("cice11")

cie171<-coefci(ols4i, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols4i, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice171<-ols4i$coefficients[c("treatconf.-both:round2")]
ci171<-cbind(cie171,cice171)
event<-c("b (both-lower temperature)")
ci171<-cbind(ci171,event)
ci171<-data.frame(ci171)
names(ci171)[3]<-paste("cice11")

cie181<-coefci(ols4i, parm=c("temp_both"), vcov = vcovCL(ols4i, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice181<-ols4i$coefficients[c("temp_both")]
ci181<-cbind(cie181,cice181)
event<-c("b (both-temperature DDD)")
ci181<-cbind(ci181,event)
ci181<-data.frame(ci181)
names(ci181)[3]<-paste("cice11")

cie191<-coefci(ols4j, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols4j, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice191<-ols4j$coefficients[c("treatconf.-both:round2")]
ci191<-cbind(cie191,cice191)
event<-c("a (both-lower temperature)")
ci191<-cbind(ci191,event)
ci191<-data.frame(ci191)
names(ci191)[3]<-paste("cice11")

cie201<-coefci(ols4j, parm=c("temp_both"), vcov = vcovCL(ols4j, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice201<-ols4j$coefficients[c("temp_both")]
ci201<-cbind(cie201,cice201)
event<-c("a (both-temperature DDD)")
ci201<-cbind(ci201,event)
ci201<-data.frame(ci201)
names(ci201)[3]<-paste("cice11")

cie211<-coefci(ols4k, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols4k, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice211<-ols4k$coefficients[c("treatconf.-both:round2")]
ci211<-cbind(cie211,cice211)
event<-c("b (both-lower education)")
ci211<-cbind(ci211,event)
ci211<-data.frame(ci211)
names(ci211)[3]<-paste("cice11")

cie221<-coefci(ols4k, parm=c("study_both"), vcov = vcovCL(ols4k, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice221<-ols4k$coefficients[c("study_both")]
ci221<-cbind(cie221,cice221)
event<-c("b (both-education DDD)")
ci221<-cbind(ci221,event)
ci221<-data.frame(ci221)
names(ci221)[3]<-paste("cice11")

cie231<-coefci(ols4l, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols4l, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice231<-ols4l$coefficients[c("treatconf.-both:round2")]
ci231<-cbind(cie231,cice231)
event<-c("a (both-lower education)")
ci231<-cbind(ci231,event)
ci231<-data.frame(ci231)
names(ci231)[3]<-paste("cice11")

cie241<-coefci(ols4l, parm=c("study_both"), vcov = vcovCL(ols4l, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice241<-ols4l$coefficients[c("study_both")]
ci241<-cbind(cie241,cice241)
event<-c("a (both-education DDD)")
ci241<-cbind(ci241,event)
ci241<-data.frame(ci241)
names(ci241)[3]<-paste("cice11")

cidata<-rbind(ci11,ci31,ci21,ci41,ci51,ci61,ci71,ci81,ci91,ci101,ci111,ci121,ci131,ci141,ci151,ci161,ci171,ci181,ci191,ci201,ci211,ci221,ci231,ci241)
cidata$X2.5..<-as.numeric(cidata$X2.5..)
cidata$X97.5..<-as.numeric(cidata$X97.5..)
cidata$cice11<-as.numeric(cidata$cice11)
cidata$controls<-"1.) none"

#with demographic variables
cie11<-coefci(ols5, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols5, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice11<-ols5$coefficients[c("treatconf.-both:round2")]
ci11<-cbind(cie11,cice11)
event<-c("b (both-not female)")
ci11<-cbind(ci11,event)
ci11<-data.frame(ci11)

cie31<-coefci(ols5, parm=c("female_both"), vcov = vcovCL(ols5, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice31<-ols5$coefficients[c("female_both")]
ci31<-cbind(cie31,cice31)
event<-c("b (both-female DDD)")
ci31<-cbind(ci31,event)
ci31<-data.frame(ci31)
names(ci31)[3]<-paste("cice11")

cie21<-coefci(ols5a, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols5a, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice21<-ols5a$coefficients[c("treatconf.-both:round2")]
ci21<-cbind(cie21,cice21)
event<-c("a (both-not female)")
ci21<-cbind(ci21,event)
ci21<-data.frame(ci21)
names(ci21)[3]<-paste("cice11")

cie41<-coefci(ols5a, parm=c("female_both"), vcov = vcovCL(ols5a, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice41<-ols5a$coefficients[c("female_both")]
ci41<-cbind(cie41,cice41)
event<-c("a (both-female DDD)")
ci41<-cbind(ci41,event)
ci41<-data.frame(ci41)
names(ci41)[3]<-paste("cice11")


cie51<-coefci(ols5c, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols5c, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice51<-ols5c$coefficients[c("treatconf.-both:round2")]
ci51<-cbind(cie51,cice51)
event<-c("b (both-less accurate)")
ci51<-cbind(ci51,event)
ci51<-data.frame(ci51)
names(ci51)[3]<-paste("cice11")

cie61<-coefci(ols5c, parm=c("accu_both"), vcov = vcovCL(ols5c, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice61<-ols5c$coefficients[c("accu_both")]
ci61<-cbind(cie61,cice61)
event<-c("b (both-accurate DDD)")
ci61<-cbind(ci61,event)
ci61<-data.frame(ci61)
names(ci61)[3]<-paste("cice11")


cie71<-coefci(ols5d, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols5d, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice71<-ols5d$coefficients[c("treatconf.-both:round2")]
ci71<-cbind(cie71,cice71)
event<-c("a (both-less accurate)")
ci71<-cbind(ci71,event)
ci71<-data.frame(ci71)
names(ci71)[3]<-paste("cice11")

cie81<-coefci(ols5d, parm=c("accu_both"), vcov = vcovCL(ols5d, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice81<-ols5d$coefficients[c("accu_both")]
ci81<-cbind(cie81,cice81)
event<-c("a (both-accurate DDD)")
ci81<-cbind(ci81,event)
ci81<-data.frame(ci81)
names(ci81)[3]<-paste("cice11")

cie91<-coefci(ols5e, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols5e, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice91<-ols5e$coefficients[c("treatconf.-both:round2")]
ci91<-cbind(cie91,cice91)
event<-c("b (both-less credible)")
ci91<-cbind(ci91,event)
ci91<-data.frame(ci91)
names(ci91)[3]<-paste("cice11")

cie101<-coefci(ols5e, parm=c("cred_both"), vcov = vcovCL(ols5e, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice101<-ols5e$coefficients[c("cred_both")]
ci101<-cbind(cie101,cice101)
event<-c("b (both-credible DDD)")
ci101<-cbind(ci101,event)
ci101<-data.frame(ci101)
names(ci101)[3]<-paste("cice11")

cie111<-coefci(ols5f, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols5f, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice111<-ols5f$coefficients[c("treatconf.-both:round2")]
ci111<-cbind(cie111,cice111)
event<-c("a (both-less credible)")
ci111<-cbind(ci111,event)
ci111<-data.frame(ci111)
names(ci111)[3]<-paste("cice11")

cie121<-coefci(ols5f, parm=c("cred_both"), vcov = vcovCL(ols5f, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice121<-ols5f$coefficients[c("cred_both")]
ci121<-cbind(cie121,cice121)
event<-c("a (both-credible DDD)")
ci121<-cbind(ci121,event)
ci121<-data.frame(ci121)
names(ci121)[3]<-paste("cice11")

cie131<-coefci(ols5g, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols5g, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice131<-ols5g$coefficients[c("treatconf.-both:round2")]
ci131<-cbind(cie131,cice131)
event<-c("b (both-less forecast usage)")
ci131<-cbind(ci131,event)
ci131<-data.frame(ci131)
names(ci131)[3]<-paste("cice11")

cie141<-coefci(ols5g, parm=c("fore_both"), vcov = vcovCL(ols5g, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice141<-ols5g$coefficients[c("fore_both")]
ci141<-cbind(cie141,cice141)
event<-c("b (both-forecast usage DDD)")
ci141<-cbind(ci141,event)
ci141<-data.frame(ci141)
names(ci141)[3]<-paste("cice11")

cie151<-coefci(ols5h, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols5h, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice151<-ols5h$coefficients[c("treatconf.-both:round2")]
ci151<-cbind(cie151,cice151)
event<-c("a (both-less forecast usage)")
ci151<-cbind(ci151,event)
ci151<-data.frame(ci151)
names(ci151)[3]<-paste("cice11")

cie161<-coefci(ols5h, parm=c("fore_both"), vcov = vcovCL(ols5h, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice161<-ols5h$coefficients[c("fore_both")]
ci161<-cbind(cie161,cice161)
event<-c("a (both-forecast usage DDD)")
ci161<-cbind(ci161,event)
ci161<-data.frame(ci161)
names(ci161)[3]<-paste("cice11")

cie171<-coefci(ols5i, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols5i, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice171<-ols5i$coefficients[c("treatconf.-both:round2")]
ci171<-cbind(cie171,cice171)
event<-c("b (both-lower temperature)")
ci171<-cbind(ci171,event)
ci171<-data.frame(ci171)
names(ci171)[3]<-paste("cice11")

cie181<-coefci(ols5i, parm=c("temp_both"), vcov = vcovCL(ols5i, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice181<-ols5i$coefficients[c("temp_both")]
ci181<-cbind(cie181,cice181)
event<-c("b (both-temperature DDD)")
ci181<-cbind(ci181,event)
ci181<-data.frame(ci181)
names(ci181)[3]<-paste("cice11")

cie191<-coefci(ols5j, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols5j, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice191<-ols5j$coefficients[c("treatconf.-both:round2")]
ci191<-cbind(cie191,cice191)
event<-c("a (both-lower temperature)")
ci191<-cbind(ci191,event)
ci191<-data.frame(ci191)
names(ci191)[3]<-paste("cice11")

cie201<-coefci(ols5j, parm=c("temp_both"), vcov = vcovCL(ols5j, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice201<-ols5j$coefficients[c("temp_both")]
ci201<-cbind(cie201,cice201)
event<-c("a (both-temperature DDD)")
ci201<-cbind(ci201,event)
ci201<-data.frame(ci201)
names(ci201)[3]<-paste("cice11")

cie211<-coefci(ols5k, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols5k, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice211<-ols5k$coefficients[c("treatconf.-both:round2")]
ci211<-cbind(cie211,cice211)
event<-c("b (both-lower education)")
ci211<-cbind(ci211,event)
ci211<-data.frame(ci211)
names(ci211)[3]<-paste("cice11")

cie221<-coefci(ols5k, parm=c("study_both"), vcov = vcovCL(ols5k, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice221<-ols5k$coefficients[c("study_both")]
ci221<-cbind(cie221,cice221)
event<-c("b (both-education DDD)")
ci221<-cbind(ci221,event)
ci221<-data.frame(ci221)
names(ci221)[3]<-paste("cice11")

cie231<-coefci(ols5l, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols5l, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice231<-ols5l$coefficients[c("treatconf.-both:round2")]
ci231<-cbind(cie231,cice231)
event<-c("a (both-lower education)")
ci231<-cbind(ci231,event)
ci231<-data.frame(ci231)
names(ci231)[3]<-paste("cice11")

cie241<-coefci(ols5l, parm=c("study_both"), vcov = vcovCL(ols5l, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice241<-ols5l$coefficients[c("study_both")]
ci241<-cbind(cie241,cice241)
event<-c("a (both-education DDD)")
ci241<-cbind(ci241,event)
ci241<-data.frame(ci241)
names(ci241)[3]<-paste("cice11")

cidata2<-rbind(ci11,ci31,ci21,ci41,ci51,ci61,ci71,ci81,ci91,ci101,ci111,ci121,ci131,ci141,ci151,ci161,ci171,ci181,ci191,ci201,ci211,ci221,ci231,ci241)
cidata2$X2.5..<-as.numeric(cidata2$X2.5..)
cidata2$X97.5..<-as.numeric(cidata2$X97.5..)
cidata2$cice11<-as.numeric(cidata2$cice11)
cidata2$controls<-"2.) demographic"

cidata<-rbind(cidata,cidata2)


#with demographic variables + further controls
cie11<-coefci(ols6, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols6, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice11<-ols6$coefficients[c("treatconf.-both:round2")]
ci11<-cbind(cie11,cice11)
event<-c("b (both-not female)")
ci11<-cbind(ci11,event)
ci11<-data.frame(ci11)

cie31<-coefci(ols6, parm=c("female_both"), vcov = vcovCL(ols6, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice31<-ols6$coefficients[c("female_both")]
ci31<-cbind(cie31,cice31)
event<-c("b (both-female DDD)")
ci31<-cbind(ci31,event)
ci31<-data.frame(ci31)
names(ci31)[3]<-paste("cice11")

cie21<-coefci(ols6a, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols6a, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice21<-ols6a$coefficients[c("treatconf.-both:round2")]
ci21<-cbind(cie21,cice21)
event<-c("a (both-not female)")
ci21<-cbind(ci21,event)
ci21<-data.frame(ci21)
names(ci21)[3]<-paste("cice11")

cie41<-coefci(ols6a, parm=c("female_both"), vcov = vcovCL(ols6a, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice41<-ols6a$coefficients[c("female_both")]
ci41<-cbind(cie41,cice41)
event<-c("a (both-female DDD)")
ci41<-cbind(ci41,event)
ci41<-data.frame(ci41)
names(ci41)[3]<-paste("cice11")


cie51<-coefci(ols6c, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols6c, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice51<-ols6c$coefficients[c("treatconf.-both:round2")]
ci51<-cbind(cie51,cice51)
event<-c("b (both-less accurate)")
ci51<-cbind(ci51,event)
ci51<-data.frame(ci51)
names(ci51)[3]<-paste("cice11")

cie61<-coefci(ols6c, parm=c("accu_both"), vcov = vcovCL(ols6c, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice61<-ols6c$coefficients[c("accu_both")]
ci61<-cbind(cie61,cice61)
event<-c("b (both-accurate DDD)")
ci61<-cbind(ci61,event)
ci61<-data.frame(ci61)
names(ci61)[3]<-paste("cice11")


cie71<-coefci(ols6d, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols6d, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice71<-ols6d$coefficients[c("treatconf.-both:round2")]
ci71<-cbind(cie71,cice71)
event<-c("a (both-less accurate)")
ci71<-cbind(ci71,event)
ci71<-data.frame(ci71)
names(ci71)[3]<-paste("cice11")

cie81<-coefci(ols6d, parm=c("accu_both"), vcov = vcovCL(ols6d, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice81<-ols6d$coefficients[c("accu_both")]
ci81<-cbind(cie81,cice81)
event<-c("a (both-accurate DDD)")
ci81<-cbind(ci81,event)
ci81<-data.frame(ci81)
names(ci81)[3]<-paste("cice11")

cie91<-coefci(ols6e, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols6e, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice91<-ols6e$coefficients[c("treatconf.-both:round2")]
ci91<-cbind(cie91,cice91)
event<-c("b (both-less credible)")
ci91<-cbind(ci91,event)
ci91<-data.frame(ci91)
names(ci91)[3]<-paste("cice11")

cie101<-coefci(ols6e, parm=c("cred_both"), vcov = vcovCL(ols6e, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice101<-ols6e$coefficients[c("cred_both")]
ci101<-cbind(cie101,cice101)
event<-c("b (both-credible DDD)")
ci101<-cbind(ci101,event)
ci101<-data.frame(ci101)
names(ci101)[3]<-paste("cice11")

cie111<-coefci(ols6f, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols6f, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice111<-ols6f$coefficients[c("treatconf.-both:round2")]
ci111<-cbind(cie111,cice111)
event<-c("a (both-less credible)")
ci111<-cbind(ci111,event)
ci111<-data.frame(ci111)
names(ci111)[3]<-paste("cice11")

cie121<-coefci(ols6f, parm=c("cred_both"), vcov = vcovCL(ols6f, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice121<-ols6f$coefficients[c("cred_both")]
ci121<-cbind(cie121,cice121)
event<-c("a (both-credible DDD)")
ci121<-cbind(ci121,event)
ci121<-data.frame(ci121)
names(ci121)[3]<-paste("cice11")

cie131<-coefci(ols6g, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols6g, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice131<-ols6g$coefficients[c("treatconf.-both:round2")]
ci131<-cbind(cie131,cice131)
event<-c("b (both-less forecast usage)")
ci131<-cbind(ci131,event)
ci131<-data.frame(ci131)
names(ci131)[3]<-paste("cice11")

cie141<-coefci(ols6g, parm=c("fore_both"), vcov = vcovCL(ols6g, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice141<-ols6g$coefficients[c("fore_both")]
ci141<-cbind(cie141,cice141)
event<-c("b (both-forecast usage DDD)")
ci141<-cbind(ci141,event)
ci141<-data.frame(ci141)
names(ci141)[3]<-paste("cice11")

cie151<-coefci(ols6h, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols6h, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice151<-ols6h$coefficients[c("treatconf.-both:round2")]
ci151<-cbind(cie151,cice151)
event<-c("a (both-less forecast usage)")
ci151<-cbind(ci151,event)
ci151<-data.frame(ci151)
names(ci151)[3]<-paste("cice11")

cie161<-coefci(ols6h, parm=c("fore_both"), vcov = vcovCL(ols6h, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice161<-ols6h$coefficients[c("fore_both")]
ci161<-cbind(cie161,cice161)
event<-c("a (both-forecast usage DDD)")
ci161<-cbind(ci161,event)
ci161<-data.frame(ci161)
names(ci161)[3]<-paste("cice11")

cie171<-coefci(ols6i, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols6i, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice171<-ols6i$coefficients[c("treatconf.-both:round2")]
ci171<-cbind(cie171,cice171)
event<-c("b (both-lower temperature)")
ci171<-cbind(ci171,event)
ci171<-data.frame(ci171)
names(ci171)[3]<-paste("cice11")

cie181<-coefci(ols6i, parm=c("temp_both"), vcov = vcovCL(ols6i, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice181<-ols6i$coefficients[c("temp_both")]
ci181<-cbind(cie181,cice181)
event<-c("b (both-temperature DDD)")
ci181<-cbind(ci181,event)
ci181<-data.frame(ci181)
names(ci181)[3]<-paste("cice11")

cie191<-coefci(ols6j, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols6j, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice191<-ols6j$coefficients[c("treatconf.-both:round2")]
ci191<-cbind(cie191,cice191)
event<-c("a (both-lower temperature)")
ci191<-cbind(ci191,event)
ci191<-data.frame(ci191)
names(ci191)[3]<-paste("cice11")

cie201<-coefci(ols6j, parm=c("temp_both"), vcov = vcovCL(ols6j, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice201<-ols6j$coefficients[c("temp_both")]
ci201<-cbind(cie201,cice201)
event<-c("a (both-temperature DDD)")
ci201<-cbind(ci201,event)
ci201<-data.frame(ci201)
names(ci201)[3]<-paste("cice11")

cie211<-coefci(ols6k, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols6k, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice211<-ols6k$coefficients[c("treatconf.-both:round2")]
ci211<-cbind(cie211,cice211)
event<-c("b (both-lower education)")
ci211<-cbind(ci211,event)
ci211<-data.frame(ci211)
names(ci211)[3]<-paste("cice11")

cie221<-coefci(ols6k, parm=c("study_both"), vcov = vcovCL(ols6k, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice221<-ols6k$coefficients[c("study_both")]
ci221<-cbind(cie221,cice221)
event<-c("b (both-education DDD)")
ci221<-cbind(ci221,event)
ci221<-data.frame(ci221)
names(ci221)[3]<-paste("cice11")

cie231<-coefci(ols6l, parm=c("treatconf.-both:round2"), vcov = vcovCL(ols6l, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice231<-ols6l$coefficients[c("treatconf.-both:round2")]
ci231<-cbind(cie231,cice231)
event<-c("a (both-lower education)")
ci231<-cbind(ci231,event)
ci231<-data.frame(ci231)
names(ci231)[3]<-paste("cice11")

cie241<-coefci(ols6l, parm=c("study_both"), vcov = vcovCL(ols6l, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice241<-ols6l$coefficients[c("study_both")]
ci241<-cbind(cie241,cice241)
event<-c("a (both-education DDD)")
ci241<-cbind(ci241,event)
ci241<-data.frame(ci241)
names(ci241)[3]<-paste("cice11")


cidata3<-rbind(ci11,ci31,ci21,ci41,ci51,ci61,ci71,ci81,ci91,ci101,ci111,ci121,ci131,ci141,ci151,ci161,ci171,ci181,ci191,ci201,ci211,ci221,ci231,ci241)
cidata3$X2.5..<-as.numeric(cidata3$X2.5..)
cidata3$X97.5..<-as.numeric(cidata3$X97.5..)
cidata3$cice11<-as.numeric(cidata3$cice11)
cidata3$controls<-"3.) demographic + further"

cidataa<-rbind(cidata,cidata3)

#Graph 2: Interval estimates
# no control variables
cie11<-coefci(ols4, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols4, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice11<-ols4$coefficients[c("treatconf.-interval:round2")]
ci11<-cbind(cie11,cice11)
event<-c("b (interval-not female)")
ci11<-cbind(ci11,event)
ci11<-data.frame(ci11)

cie31<-coefci(ols4, parm=c("female_int"), vcov = vcovCL(ols4, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice31<-ols4$coefficients[c("female_int")]
ci31<-cbind(cie31,cice31)
event<-c("b (interval-female DDD)")
ci31<-cbind(ci31,event)
ci31<-data.frame(ci31)
names(ci31)[3]<-paste("cice11")

cie21<-coefci(ols4a, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols4a, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice21<-ols4a$coefficients[c("treatconf.-interval:round2")]
ci21<-cbind(cie21,cice21)
event<-c("a (interval-not female)")
ci21<-cbind(ci21,event)
ci21<-data.frame(ci21)
names(ci21)[3]<-paste("cice11")

cie41<-coefci(ols4a, parm=c("female_int"), vcov = vcovCL(ols4a, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice41<-ols4a$coefficients[c("female_int")]
ci41<-cbind(cie41,cice41)
event<-c("a (interval-female DDD)")
ci41<-cbind(ci41,event)
ci41<-data.frame(ci41)
names(ci41)[3]<-paste("cice11")


cie51<-coefci(ols4c, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols4c, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice51<-ols4c$coefficients[c("treatconf.-interval:round2")]
ci51<-cbind(cie51,cice51)
event<-c("b (interval-less accurate)")
ci51<-cbind(ci51,event)
ci51<-data.frame(ci51)
names(ci51)[3]<-paste("cice11")

cie61<-coefci(ols4c, parm=c("accu_int"), vcov = vcovCL(ols4c, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice61<-ols4c$coefficients[c("accu_int")]
ci61<-cbind(cie61,cice61)
event<-c("b (interval-accurate DDD)")
ci61<-cbind(ci61,event)
ci61<-data.frame(ci61)
names(ci61)[3]<-paste("cice11")


cie71<-coefci(ols4d, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols4d, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice71<-ols4d$coefficients[c("treatconf.-interval:round2")]
ci71<-cbind(cie71,cice71)
event<-c("a (interval-less accurate)")
ci71<-cbind(ci71,event)
ci71<-data.frame(ci71)
names(ci71)[3]<-paste("cice11")

cie81<-coefci(ols4d, parm=c("accu_int"), vcov = vcovCL(ols4d, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice81<-ols4d$coefficients[c("accu_int")]
ci81<-cbind(cie81,cice81)
event<-c("a (interval-accurate DDD)")
ci81<-cbind(ci81,event)
ci81<-data.frame(ci81)
names(ci81)[3]<-paste("cice11")

cie91<-coefci(ols4e, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols4e, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice91<-ols4e$coefficients[c("treatconf.-interval:round2")]
ci91<-cbind(cie91,cice91)
event<-c("b (interval-less credible)")
ci91<-cbind(ci91,event)
ci91<-data.frame(ci91)
names(ci91)[3]<-paste("cice11")

cie101<-coefci(ols4e, parm=c("cred_int"), vcov = vcovCL(ols4e, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice101<-ols4e$coefficients[c("cred_int")]
ci101<-cbind(cie101,cice101)
event<-c("b (interval-credible DDD)")
ci101<-cbind(ci101,event)
ci101<-data.frame(ci101)
names(ci101)[3]<-paste("cice11")

cie111<-coefci(ols4f, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols4f, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice111<-ols4f$coefficients[c("treatconf.-interval:round2")]
ci111<-cbind(cie111,cice111)
event<-c("a (interval-less credible)")
ci111<-cbind(ci111,event)
ci111<-data.frame(ci111)
names(ci111)[3]<-paste("cice11")

cie121<-coefci(ols4f, parm=c("cred_int"), vcov = vcovCL(ols4f, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice121<-ols4f$coefficients[c("cred_int")]
ci121<-cbind(cie121,cice121)
event<-c("a (interval-credible DDD)")
ci121<-cbind(ci121,event)
ci121<-data.frame(ci121)
names(ci121)[3]<-paste("cice11")

cie131<-coefci(ols4g, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols4g, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice131<-ols4g$coefficients[c("treatconf.-interval:round2")]
ci131<-cbind(cie131,cice131)
event<-c("b (interval-less forecast usage)")
ci131<-cbind(ci131,event)
ci131<-data.frame(ci131)
names(ci131)[3]<-paste("cice11")

cie141<-coefci(ols4g, parm=c("fore_int"), vcov = vcovCL(ols4g, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice141<-ols4g$coefficients[c("fore_int")]
ci141<-cbind(cie141,cice141)
event<-c("b (interval-forecast usage DDD)")
ci141<-cbind(ci141,event)
ci141<-data.frame(ci141)
names(ci141)[3]<-paste("cice11")

cie151<-coefci(ols4h, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols4h, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice151<-ols4h$coefficients[c("treatconf.-interval:round2")]
ci151<-cbind(cie151,cice151)
event<-c("a (interval-less forecast usage)")
ci151<-cbind(ci151,event)
ci151<-data.frame(ci151)
names(ci151)[3]<-paste("cice11")

cie161<-coefci(ols4h, parm=c("fore_int"), vcov = vcovCL(ols4h, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice161<-ols4h$coefficients[c("fore_int")]
ci161<-cbind(cie161,cice161)
event<-c("a (interval-forecast usage DDD)")
ci161<-cbind(ci161,event)
ci161<-data.frame(ci161)
names(ci161)[3]<-paste("cice11")

cie171<-coefci(ols4i, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols4i, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice171<-ols4i$coefficients[c("treatconf.-interval:round2")]
ci171<-cbind(cie171,cice171)
event<-c("b (interval-lower temperature)")
ci171<-cbind(ci171,event)
ci171<-data.frame(ci171)
names(ci171)[3]<-paste("cice11")

cie181<-coefci(ols4i, parm=c("temp_int"), vcov = vcovCL(ols4i, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice181<-ols4i$coefficients[c("temp_int")]
ci181<-cbind(cie181,cice181)
event<-c("b (interval-temperature DDD)")
ci181<-cbind(ci181,event)
ci181<-data.frame(ci181)
names(ci181)[3]<-paste("cice11")

cie191<-coefci(ols4j, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols4j, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice191<-ols4j$coefficients[c("treatconf.-interval:round2")]
ci191<-cbind(cie191,cice191)
event<-c("a (interval-lower temperature)")
ci191<-cbind(ci191,event)
ci191<-data.frame(ci191)
names(ci191)[3]<-paste("cice11")

cie201<-coefci(ols4j, parm=c("temp_int"), vcov = vcovCL(ols4j, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice201<-ols4j$coefficients[c("temp_int")]
ci201<-cbind(cie201,cice201)
event<-c("a (interval-temperature DDD)")
ci201<-cbind(ci201,event)
ci201<-data.frame(ci201)
names(ci201)[3]<-paste("cice11")

cie211<-coefci(ols4k, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols4k, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice211<-ols4k$coefficients[c("treatconf.-interval:round2")]
ci211<-cbind(cie211,cice211)
event<-c("b (interval-lower education)")
ci211<-cbind(ci211,event)
ci211<-data.frame(ci211)
names(ci211)[3]<-paste("cice11")

cie221<-coefci(ols4k, parm=c("study_int"), vcov = vcovCL(ols4k, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice221<-ols4k$coefficients[c("study_int")]
ci221<-cbind(cie221,cice221)
event<-c("b (interval-education DDD)")
ci221<-cbind(ci221,event)
ci221<-data.frame(ci221)
names(ci221)[3]<-paste("cice11")

cie231<-coefci(ols4l, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols4l, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice231<-ols4l$coefficients[c("treatconf.-interval:round2")]
ci231<-cbind(cie231,cice231)
event<-c("a (interval-lower education)")
ci231<-cbind(ci231,event)
ci231<-data.frame(ci231)
names(ci231)[3]<-paste("cice11")

cie241<-coefci(ols4l, parm=c("study_int"), vcov = vcovCL(ols4l, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice241<-ols4l$coefficients[c("study_int")]
ci241<-cbind(cie241,cice241)
event<-c("a (interval-education DDD)")
ci241<-cbind(ci241,event)
ci241<-data.frame(ci241)
names(ci241)[3]<-paste("cice11")

cidata<-rbind(ci11,ci31,ci21,ci41,ci51,ci61,ci71,ci81,ci91,ci101,ci111,ci121,ci131,ci141,ci151,ci161,ci171,ci181,ci191,ci201,ci211,ci221,ci231,ci241)
cidata$X2.5..<-as.numeric(cidata$X2.5..)
cidata$X97.5..<-as.numeric(cidata$X97.5..)
cidata$cice11<-as.numeric(cidata$cice11)
cidata$controls<-"1.) none"

#with demographic variables
cie11<-coefci(ols5, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols5, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice11<-ols5$coefficients[c("treatconf.-interval:round2")]
ci11<-cbind(cie11,cice11)
event<-c("b (interval-not female)")
ci11<-cbind(ci11,event)
ci11<-data.frame(ci11)

cie31<-coefci(ols5, parm=c("female_int"), vcov = vcovCL(ols5, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice31<-ols5$coefficients[c("female_int")]
ci31<-cbind(cie31,cice31)
event<-c("b (interval-female DDD)")
ci31<-cbind(ci31,event)
ci31<-data.frame(ci31)
names(ci31)[3]<-paste("cice11")

cie21<-coefci(ols5a, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols5a, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice21<-ols5a$coefficients[c("treatconf.-interval:round2")]
ci21<-cbind(cie21,cice21)
event<-c("a (interval-not female)")
ci21<-cbind(ci21,event)
ci21<-data.frame(ci21)
names(ci21)[3]<-paste("cice11")

cie41<-coefci(ols5a, parm=c("female_int"), vcov = vcovCL(ols5a, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice41<-ols5a$coefficients[c("female_int")]
ci41<-cbind(cie41,cice41)
event<-c("a (interval-female DDD)")
ci41<-cbind(ci41,event)
ci41<-data.frame(ci41)
names(ci41)[3]<-paste("cice11")


cie51<-coefci(ols5c, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols5c, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice51<-ols5c$coefficients[c("treatconf.-interval:round2")]
ci51<-cbind(cie51,cice51)
event<-c("b (interval-less accurate)")
ci51<-cbind(ci51,event)
ci51<-data.frame(ci51)
names(ci51)[3]<-paste("cice11")

cie61<-coefci(ols5c, parm=c("accu_int"), vcov = vcovCL(ols5c, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice61<-ols5c$coefficients[c("accu_int")]
ci61<-cbind(cie61,cice61)
event<-c("b (interval-accurate DDD)")
ci61<-cbind(ci61,event)
ci61<-data.frame(ci61)
names(ci61)[3]<-paste("cice11")


cie71<-coefci(ols5d, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols5d, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice71<-ols5d$coefficients[c("treatconf.-interval:round2")]
ci71<-cbind(cie71,cice71)
event<-c("a (interval-less accurate)")
ci71<-cbind(ci71,event)
ci71<-data.frame(ci71)
names(ci71)[3]<-paste("cice11")

cie81<-coefci(ols5d, parm=c("accu_int"), vcov = vcovCL(ols5d, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice81<-ols5d$coefficients[c("accu_int")]
ci81<-cbind(cie81,cice81)
event<-c("a (interval-accurate DDD)")
ci81<-cbind(ci81,event)
ci81<-data.frame(ci81)
names(ci81)[3]<-paste("cice11")

cie91<-coefci(ols5e, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols5e, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice91<-ols5e$coefficients[c("treatconf.-interval:round2")]
ci91<-cbind(cie91,cice91)
event<-c("b (interval-less credible)")
ci91<-cbind(ci91,event)
ci91<-data.frame(ci91)
names(ci91)[3]<-paste("cice11")

cie101<-coefci(ols5e, parm=c("cred_int"), vcov = vcovCL(ols5e, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice101<-ols5e$coefficients[c("cred_int")]
ci101<-cbind(cie101,cice101)
event<-c("b (interval-credible DDD)")
ci101<-cbind(ci101,event)
ci101<-data.frame(ci101)
names(ci101)[3]<-paste("cice11")

cie111<-coefci(ols5f, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols5f, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice111<-ols5f$coefficients[c("treatconf.-interval:round2")]
ci111<-cbind(cie111,cice111)
event<-c("a (interval-less credible)")
ci111<-cbind(ci111,event)
ci111<-data.frame(ci111)
names(ci111)[3]<-paste("cice11")

cie121<-coefci(ols5f, parm=c("cred_int"), vcov = vcovCL(ols5f, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice121<-ols5f$coefficients[c("cred_int")]
ci121<-cbind(cie121,cice121)
event<-c("a (interval-credible DDD)")
ci121<-cbind(ci121,event)
ci121<-data.frame(ci121)
names(ci121)[3]<-paste("cice11")

cie131<-coefci(ols5g, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols5g, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice131<-ols5g$coefficients[c("treatconf.-interval:round2")]
ci131<-cbind(cie131,cice131)
event<-c("b (interval-less forecast usage)")
ci131<-cbind(ci131,event)
ci131<-data.frame(ci131)
names(ci131)[3]<-paste("cice11")

cie141<-coefci(ols5g, parm=c("fore_int"), vcov = vcovCL(ols5g, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice141<-ols5g$coefficients[c("fore_int")]
ci141<-cbind(cie141,cice141)
event<-c("b (interval-forecast usage DDD)")
ci141<-cbind(ci141,event)
ci141<-data.frame(ci141)
names(ci141)[3]<-paste("cice11")

cie151<-coefci(ols5h, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols5h, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice151<-ols5h$coefficients[c("treatconf.-interval:round2")]
ci151<-cbind(cie151,cice151)
event<-c("a (interval-less forecast usage)")
ci151<-cbind(ci151,event)
ci151<-data.frame(ci151)
names(ci151)[3]<-paste("cice11")

cie161<-coefci(ols5h, parm=c("fore_int"), vcov = vcovCL(ols5h, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice161<-ols5h$coefficients[c("fore_int")]
ci161<-cbind(cie161,cice161)
event<-c("a (interval-forecast usage DDD)")
ci161<-cbind(ci161,event)
ci161<-data.frame(ci161)
names(ci161)[3]<-paste("cice11")

cie171<-coefci(ols5i, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols5i, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice171<-ols5i$coefficients[c("treatconf.-interval:round2")]
ci171<-cbind(cie171,cice171)
event<-c("b (interval-lower temperature)")
ci171<-cbind(ci171,event)
ci171<-data.frame(ci171)
names(ci171)[3]<-paste("cice11")

cie181<-coefci(ols5i, parm=c("temp_int"), vcov = vcovCL(ols5i, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice181<-ols5i$coefficients[c("temp_int")]
ci181<-cbind(cie181,cice181)
event<-c("b (interval-temperature DDD)")
ci181<-cbind(ci181,event)
ci181<-data.frame(ci181)
names(ci181)[3]<-paste("cice11")

cie191<-coefci(ols5j, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols5j, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice191<-ols5j$coefficients[c("treatconf.-interval:round2")]
ci191<-cbind(cie191,cice191)
event<-c("a (interval-lower temperature)")
ci191<-cbind(ci191,event)
ci191<-data.frame(ci191)
names(ci191)[3]<-paste("cice11")

cie201<-coefci(ols5j, parm=c("temp_int"), vcov = vcovCL(ols5j, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice201<-ols5j$coefficients[c("temp_int")]
ci201<-cbind(cie201,cice201)
event<-c("a (interval-temperature DDD)")
ci201<-cbind(ci201,event)
ci201<-data.frame(ci201)
names(ci201)[3]<-paste("cice11")

cie211<-coefci(ols5k, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols5k, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice211<-ols5k$coefficients[c("treatconf.-interval:round2")]
ci211<-cbind(cie211,cice211)
event<-c("b (interval-lower education)")
ci211<-cbind(ci211,event)
ci211<-data.frame(ci211)
names(ci211)[3]<-paste("cice11")

cie221<-coefci(ols5k, parm=c("study_int"), vcov = vcovCL(ols5k, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice221<-ols5k$coefficients[c("study_int")]
ci221<-cbind(cie221,cice221)
event<-c("b (interval-education DDD)")
ci221<-cbind(ci221,event)
ci221<-data.frame(ci221)
names(ci221)[3]<-paste("cice11")

cie231<-coefci(ols5l, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols5l, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice231<-ols5l$coefficients[c("treatconf.-interval:round2")]
ci231<-cbind(cie231,cice231)
event<-c("a (interval-lower education)")
ci231<-cbind(ci231,event)
ci231<-data.frame(ci231)
names(ci231)[3]<-paste("cice11")

cie241<-coefci(ols5l, parm=c("study_int"), vcov = vcovCL(ols5l, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice241<-ols5l$coefficients[c("study_int")]
ci241<-cbind(cie241,cice241)
event<-c("a (interval-education DDD)")
ci241<-cbind(ci241,event)
ci241<-data.frame(ci241)
names(ci241)[3]<-paste("cice11")


cidata2<-rbind(ci11,ci31,ci21,ci41,ci51,ci61,ci71,ci81,ci91,ci101,ci111,ci121,ci131,ci141,ci151,ci161,ci171,ci181,ci191,ci201,ci211,ci221,ci231,ci241)
cidata2$X2.5..<-as.numeric(cidata2$X2.5..)
cidata2$X97.5..<-as.numeric(cidata2$X97.5..)
cidata2$cice11<-as.numeric(cidata2$cice11)
cidata2$controls<-"2.) demographic"

cidata<-rbind(cidata,cidata2)


#with demographic variables + further controls
cie11<-coefci(ols6, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols6, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice11<-ols6$coefficients[c("treatconf.-interval:round2")]
ci11<-cbind(cie11,cice11)
event<-c("b (interval-not female)")
ci11<-cbind(ci11,event)
ci11<-data.frame(ci11)

cie31<-coefci(ols6, parm=c("female_int"), vcov = vcovCL(ols6, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice31<-ols6$coefficients[c("female_int")]
ci31<-cbind(cie31,cice31)
event<-c("b (interval-female DDD)")
ci31<-cbind(ci31,event)
ci31<-data.frame(ci31)
names(ci31)[3]<-paste("cice11")

cie21<-coefci(ols6a, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols6a, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice21<-ols6a$coefficients[c("treatconf.-interval:round2")]
ci21<-cbind(cie21,cice21)
event<-c("a (interval-not female)")
ci21<-cbind(ci21,event)
ci21<-data.frame(ci21)
names(ci21)[3]<-paste("cice11")

cie41<-coefci(ols6a, parm=c("female_int"), vcov = vcovCL(ols6a, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice41<-ols6a$coefficients[c("female_int")]
ci41<-cbind(cie41,cice41)
event<-c("a (interval-female DDD)")
ci41<-cbind(ci41,event)
ci41<-data.frame(ci41)
names(ci41)[3]<-paste("cice11")


cie51<-coefci(ols6c, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols6c, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice51<-ols6c$coefficients[c("treatconf.-interval:round2")]
ci51<-cbind(cie51,cice51)
event<-c("b (interval-less accurate)")
ci51<-cbind(ci51,event)
ci51<-data.frame(ci51)
names(ci51)[3]<-paste("cice11")

cie61<-coefci(ols6c, parm=c("accu_int"), vcov = vcovCL(ols6c, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice61<-ols6c$coefficients[c("accu_int")]
ci61<-cbind(cie61,cice61)
event<-c("b (interval-accurate DDD)")
ci61<-cbind(ci61,event)
ci61<-data.frame(ci61)
names(ci61)[3]<-paste("cice11")


cie71<-coefci(ols6d, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols6d, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice71<-ols6d$coefficients[c("treatconf.-interval:round2")]
ci71<-cbind(cie71,cice71)
event<-c("a (interval-less accurate)")
ci71<-cbind(ci71,event)
ci71<-data.frame(ci71)
names(ci71)[3]<-paste("cice11")

cie81<-coefci(ols6d, parm=c("accu_int"), vcov = vcovCL(ols6d, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice81<-ols6d$coefficients[c("accu_int")]
ci81<-cbind(cie81,cice81)
event<-c("a (interval-accurate DDD)")
ci81<-cbind(ci81,event)
ci81<-data.frame(ci81)
names(ci81)[3]<-paste("cice11")

cie91<-coefci(ols6e, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols6e, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice91<-ols6e$coefficients[c("treatconf.-interval:round2")]
ci91<-cbind(cie91,cice91)
event<-c("b (interval-less credible)")
ci91<-cbind(ci91,event)
ci91<-data.frame(ci91)
names(ci91)[3]<-paste("cice11")

cie101<-coefci(ols6e, parm=c("cred_int"), vcov = vcovCL(ols6e, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice101<-ols6e$coefficients[c("cred_int")]
ci101<-cbind(cie101,cice101)
event<-c("b (interval-credible DDD)")
ci101<-cbind(ci101,event)
ci101<-data.frame(ci101)
names(ci101)[3]<-paste("cice11")

cie111<-coefci(ols6f, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols6f, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice111<-ols6f$coefficients[c("treatconf.-interval:round2")]
ci111<-cbind(cie111,cice111)
event<-c("a (interval-less credible)")
ci111<-cbind(ci111,event)
ci111<-data.frame(ci111)
names(ci111)[3]<-paste("cice11")

cie121<-coefci(ols6f, parm=c("cred_int"), vcov = vcovCL(ols6f, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice121<-ols6f$coefficients[c("cred_int")]
ci121<-cbind(cie121,cice121)
event<-c("a (interval-credible DDD)")
ci121<-cbind(ci121,event)
ci121<-data.frame(ci121)
names(ci121)[3]<-paste("cice11")

cie131<-coefci(ols6g, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols6g, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice131<-ols6g$coefficients[c("treatconf.-interval:round2")]
ci131<-cbind(cie131,cice131)
event<-c("b (interval-less forecast usage)")
ci131<-cbind(ci131,event)
ci131<-data.frame(ci131)
names(ci131)[3]<-paste("cice11")

cie141<-coefci(ols6g, parm=c("fore_int"), vcov = vcovCL(ols6g, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice141<-ols6g$coefficients[c("fore_int")]
ci141<-cbind(cie141,cice141)
event<-c("b (interval-forecast usage DDD)")
ci141<-cbind(ci141,event)
ci141<-data.frame(ci141)
names(ci141)[3]<-paste("cice11")

cie151<-coefci(ols6h, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols6h, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice151<-ols6h$coefficients[c("treatconf.-interval:round2")]
ci151<-cbind(cie151,cice151)
event<-c("a (interval-less forecast usage)")
ci151<-cbind(ci151,event)
ci151<-data.frame(ci151)
names(ci151)[3]<-paste("cice11")

cie161<-coefci(ols6h, parm=c("fore_int"), vcov = vcovCL(ols6h, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice161<-ols6h$coefficients[c("fore_int")]
ci161<-cbind(cie161,cice161)
event<-c("a (interval-forecast usage DDD)")
ci161<-cbind(ci161,event)
ci161<-data.frame(ci161)
names(ci161)[3]<-paste("cice11")

cie171<-coefci(ols6i, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols6i, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice171<-ols6i$coefficients[c("treatconf.-interval:round2")]
ci171<-cbind(cie171,cice171)
event<-c("b (interval-lower temperature)")
ci171<-cbind(ci171,event)
ci171<-data.frame(ci171)
names(ci171)[3]<-paste("cice11")

cie181<-coefci(ols6i, parm=c("temp_int"), vcov = vcovCL(ols6i, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice181<-ols6i$coefficients[c("temp_int")]
ci181<-cbind(cie181,cice181)
event<-c("b (interval-temperature DDD)")
ci181<-cbind(ci181,event)
ci181<-data.frame(ci181)
names(ci181)[3]<-paste("cice11")

cie191<-coefci(ols6j, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols6j, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice191<-ols6j$coefficients[c("treatconf.-interval:round2")]
ci191<-cbind(cie191,cice191)
event<-c("a (interval-lower temperature)")
ci191<-cbind(ci191,event)
ci191<-data.frame(ci191)
names(ci191)[3]<-paste("cice11")

cie201<-coefci(ols6j, parm=c("temp_int"), vcov = vcovCL(ols6j, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice201<-ols6j$coefficients[c("temp_int")]
ci201<-cbind(cie201,cice201)
event<-c("a (interval-temperature DDD)")
ci201<-cbind(ci201,event)
ci201<-data.frame(ci201)
names(ci201)[3]<-paste("cice11")

cie211<-coefci(ols6k, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols6k, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice211<-ols6k$coefficients[c("treatconf.-interval:round2")]
ci211<-cbind(cie211,cice211)
event<-c("b (interval-lower education)")
ci211<-cbind(ci211,event)
ci211<-data.frame(ci211)
names(ci211)[3]<-paste("cice11")

cie221<-coefci(ols6k, parm=c("study_int"), vcov = vcovCL(ols6k, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice221<-ols6k$coefficients[c("study_int")]
ci221<-cbind(cie221,cice221)
event<-c("b (interval-education DDD)")
ci221<-cbind(ci221,event)
ci221<-data.frame(ci221)
names(ci221)[3]<-paste("cice11")

cie231<-coefci(ols6l, parm=c("treatconf.-interval:round2"), vcov = vcovCL(ols6l, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice231<-ols6l$coefficients[c("treatconf.-interval:round2")]
ci231<-cbind(cie231,cice231)
event<-c("a (interval-lower education)")
ci231<-cbind(ci231,event)
ci231<-data.frame(ci231)
names(ci231)[3]<-paste("cice11")

cie241<-coefci(ols6l, parm=c("study_int"), vcov = vcovCL(ols6l, cluster=~data2$participant.label, type="HC1"),level=0.95)
cice241<-ols6l$coefficients[c("study_int")]
ci241<-cbind(cie241,cice241)
event<-c("a (interval-education DDD)")
ci241<-cbind(ci241,event)
ci241<-data.frame(ci241)
names(ci241)[3]<-paste("cice11")


cidata3<-rbind(ci11,ci31,ci21,ci41,ci51,ci61,ci71,ci81,ci91,ci101,ci111,ci121,ci131,ci141,ci151,ci161,ci171,ci181,ci191,ci201,ci211,ci221,ci231,ci241)
cidata3$X2.5..<-as.numeric(cidata3$X2.5..)
cidata3$X97.5..<-as.numeric(cidata3$X97.5..)
cidata3$cice11<-as.numeric(cidata3$cice11)
cidata3$controls<-"3.) demographic + further"

cidata<-rbind(cidata,cidata3)

cidata<-rbind(cidataa,cidata)

cidata$controls<- with(cidata, factor(controls, levels=c("1.) none","2.) demographic", "3.) demographic + further")))


p2 <- ggplot(
  mapping = aes(
    y = cidata$event
  )
) +
  scale_y_discrete( limits=c("a (both-not female)","a (both-female DDD)", "b (both-not female)", "b (both-female DDD)","a (both-less accurate)","a (both-accurate DDD)", "b (both-less accurate)", "b (both-accurate DDD)","a (both-less credible)","a (both-credible DDD)", "b (both-less credible)", "b (both-credible DDD)","a (both-less forecast usage)","a (both-forecast usage DDD)", "b (both-less forecast usage)", "b (both-forecast usage DDD)","a (both-lower temperature)","a (both-temperature DDD)", "b (both-lower temperature)", "b (both-temperature DDD)","a (both-lower education)","a (both-education DDD)", "b (both-lower education)", "b (both-education DDD)", "a (interval-not female)","a (interval-female DDD)", "b (interval-not female)", "b (interval-female DDD)","a (interval-less accurate)","a (interval-accurate DDD)", "b (interval-less accurate)", "b (interval-accurate DDD)","a (interval-less credible)","a (interval-credible DDD)", "b (interval-less credible)", "b (interval-credible DDD)","a (interval-less forecast usage)","a (interval-forecast usage DDD)", "b (interval-less forecast usage)", "b (interval-forecast usage DDD)","a (interval-lower temperature)","a (interval-temperature DDD)", "b (interval-lower temperature)", "b (interval-temperature DDD)","a (interval-lower education)","a (interval-education DDD)", "b (interval-lower education)", "b (interval-education DDD)"))+
  geom_pointrange(aes(
    x = cidata$cice11,
    y = cidata$event,
    xmin = cidata$X2.5..,
    xmax = cidata$X97.5..,
    shape=factor(cidata$controls, levels=c("3.) demographic + further","2.) demographic", "1.) none"))
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

ggsave(plot = p2, 
       filename = "../rendered_documents/pdf/Ambiguity_regs_het_confirmation.pdf",
       height = 9.5,
       width = 7.2)


###Graph 3 (Contradiction DiDs)
# no control variables
cie11<-coefci(ols7, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols7, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice11<-ols7$coefficients[c("treatcontrad.-both:round2")]
ci11<-cbind(cie11,cice11)
event<-c("b (both-not female)")
ci11<-cbind(ci11,event)
ci11<-data.frame(ci11)

cie31<-coefci(ols7, parm=c("female_both"), vcov = vcovCL(ols7, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice31<-ols7$coefficients[c("female_both")]
ci31<-cbind(cie31,cice31)
event<-c("b (both-female DDD)")
ci31<-cbind(ci31,event)
ci31<-data.frame(ci31)
names(ci31)[3]<-paste("cice11")

cie21<-coefci(ols7a, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols7a, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice21<-ols7a$coefficients[c("treatcontrad.-both:round2")]
ci21<-cbind(cie21,cice21)
event<-c("a (both-not female)")
ci21<-cbind(ci21,event)
ci21<-data.frame(ci21)
names(ci21)[3]<-paste("cice11")

cie41<-coefci(ols7a, parm=c("female_both"), vcov = vcovCL(ols7a, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice41<-ols7a$coefficients[c("female_both")]
ci41<-cbind(cie41,cice41)
event<-c("a (both-female DDD)")
ci41<-cbind(ci41,event)
ci41<-data.frame(ci41)
names(ci41)[3]<-paste("cice11")


cie51<-coefci(ols7c, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols7c, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice51<-ols7c$coefficients[c("treatcontrad.-both:round2")]
ci51<-cbind(cie51,cice51)
event<-c("b (both-less accurate)")
ci51<-cbind(ci51,event)
ci51<-data.frame(ci51)
names(ci51)[3]<-paste("cice11")

cie61<-coefci(ols7c, parm=c("accu_both"), vcov = vcovCL(ols7c, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice61<-ols7c$coefficients[c("accu_both")]
ci61<-cbind(cie61,cice61)
event<-c("b (both-accurate DDD)")
ci61<-cbind(ci61,event)
ci61<-data.frame(ci61)
names(ci61)[3]<-paste("cice11")


cie71<-coefci(ols7d, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols7d, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice71<-ols7d$coefficients[c("treatcontrad.-both:round2")]
ci71<-cbind(cie71,cice71)
event<-c("a (both-less accurate)")
ci71<-cbind(ci71,event)
ci71<-data.frame(ci71)
names(ci71)[3]<-paste("cice11")

cie81<-coefci(ols7d, parm=c("accu_both"), vcov = vcovCL(ols7d, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice81<-ols7d$coefficients[c("accu_both")]
ci81<-cbind(cie81,cice81)
event<-c("a (both-accurate DDD)")
ci81<-cbind(ci81,event)
ci81<-data.frame(ci81)
names(ci81)[3]<-paste("cice11")

cie91<-coefci(ols7e, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols7e, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice91<-ols7e$coefficients[c("treatcontrad.-both:round2")]
ci91<-cbind(cie91,cice91)
event<-c("b (both-less credible)")
ci91<-cbind(ci91,event)
ci91<-data.frame(ci91)
names(ci91)[3]<-paste("cice11")

cie101<-coefci(ols7e, parm=c("cred_both"), vcov = vcovCL(ols7e, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice101<-ols7e$coefficients[c("cred_both")]
ci101<-cbind(cie101,cice101)
event<-c("b (both-credible DDD)")
ci101<-cbind(ci101,event)
ci101<-data.frame(ci101)
names(ci101)[3]<-paste("cice11")

cie111<-coefci(ols7f, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols7f, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice111<-ols7f$coefficients[c("treatcontrad.-both:round2")]
ci111<-cbind(cie111,cice111)
event<-c("a (both-less credible)")
ci111<-cbind(ci111,event)
ci111<-data.frame(ci111)
names(ci111)[3]<-paste("cice11")

cie121<-coefci(ols7f, parm=c("cred_both"), vcov = vcovCL(ols7f, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice121<-ols7f$coefficients[c("cred_both")]
ci121<-cbind(cie121,cice121)
event<-c("a (both-credible DDD)")
ci121<-cbind(ci121,event)
ci121<-data.frame(ci121)
names(ci121)[3]<-paste("cice11")

cie131<-coefci(ols7g, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols7g, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice131<-ols7g$coefficients[c("treatcontrad.-both:round2")]
ci131<-cbind(cie131,cice131)
event<-c("b (both-less forecast usage)")
ci131<-cbind(ci131,event)
ci131<-data.frame(ci131)
names(ci131)[3]<-paste("cice11")

cie141<-coefci(ols7g, parm=c("fore_both"), vcov = vcovCL(ols7g, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice141<-ols7g$coefficients[c("fore_both")]
ci141<-cbind(cie141,cice141)
event<-c("b (both-forecast usage DDD)")
ci141<-cbind(ci141,event)
ci141<-data.frame(ci141)
names(ci141)[3]<-paste("cice11")

cie151<-coefci(ols7h, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols7h, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice151<-ols7h$coefficients[c("treatcontrad.-both:round2")]
ci151<-cbind(cie151,cice151)
event<-c("a (both-less forecast usage)")
ci151<-cbind(ci151,event)
ci151<-data.frame(ci151)
names(ci151)[3]<-paste("cice11")

cie161<-coefci(ols7h, parm=c("fore_both"), vcov = vcovCL(ols7h, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice161<-ols7h$coefficients[c("fore_both")]
ci161<-cbind(cie161,cice161)
event<-c("a (both-forecast usage DDD)")
ci161<-cbind(ci161,event)
ci161<-data.frame(ci161)
names(ci161)[3]<-paste("cice11")

cie171<-coefci(ols7i, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols7i, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice171<-ols7i$coefficients[c("treatcontrad.-both:round2")]
ci171<-cbind(cie171,cice171)
event<-c("b (both-lower temperature)")
ci171<-cbind(ci171,event)
ci171<-data.frame(ci171)
names(ci171)[3]<-paste("cice11")

cie181<-coefci(ols7i, parm=c("temp_both"), vcov = vcovCL(ols7i, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice181<-ols7i$coefficients[c("temp_both")]
ci181<-cbind(cie181,cice181)
event<-c("b (both-temperature DDD)")
ci181<-cbind(ci181,event)
ci181<-data.frame(ci181)
names(ci181)[3]<-paste("cice11")

cie191<-coefci(ols7j, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols7j, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice191<-ols7j$coefficients[c("treatcontrad.-both:round2")]
ci191<-cbind(cie191,cice191)
event<-c("a (both-lower temperature)")
ci191<-cbind(ci191,event)
ci191<-data.frame(ci191)
names(ci191)[3]<-paste("cice11")

cie201<-coefci(ols7j, parm=c("temp_both"), vcov = vcovCL(ols7j, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice201<-ols7j$coefficients[c("temp_both")]
ci201<-cbind(cie201,cice201)
event<-c("a (both-temperature DDD)")
ci201<-cbind(ci201,event)
ci201<-data.frame(ci201)
names(ci201)[3]<-paste("cice11")

cie211<-coefci(ols7k, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols7k, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice211<-ols7k$coefficients[c("treatcontrad.-both:round2")]
ci211<-cbind(cie211,cice211)
event<-c("b (both-lower education)")
ci211<-cbind(ci211,event)
ci211<-data.frame(ci211)
names(ci211)[3]<-paste("cice11")

cie221<-coefci(ols7k, parm=c("study_both"), vcov = vcovCL(ols7k, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice221<-ols7k$coefficients[c("study_both")]
ci221<-cbind(cie221,cice221)
event<-c("b (both-education DDD)")
ci221<-cbind(ci221,event)
ci221<-data.frame(ci221)
names(ci221)[3]<-paste("cice11")

cie231<-coefci(ols7l, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols7l, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice231<-ols7l$coefficients[c("treatcontrad.-both:round2")]
ci231<-cbind(cie231,cice231)
event<-c("a (both-lower education)")
ci231<-cbind(ci231,event)
ci231<-data.frame(ci231)
names(ci231)[3]<-paste("cice11")

cie241<-coefci(ols7l, parm=c("study_both"), vcov = vcovCL(ols7l, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice241<-ols7l$coefficients[c("study_both")]
ci241<-cbind(cie241,cice241)
event<-c("a (both-education DDD)")
ci241<-cbind(ci241,event)
ci241<-data.frame(ci241)
names(ci241)[3]<-paste("cice11")

cidata<-rbind(ci11,ci31,ci21,ci41,ci51,ci61,ci71,ci81,ci91,ci101,ci111,ci121,ci131,ci141,ci151,ci161,ci171,ci181,ci191,ci201,ci211,ci221,ci231,ci241)
cidata$X2.5..<-as.numeric(cidata$X2.5..)
cidata$X97.5..<-as.numeric(cidata$X97.5..)
cidata$cice11<-as.numeric(cidata$cice11)
cidata$controls<-"1.) none"

#with demographic variables
cie11<-coefci(ols8, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols8, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice11<-ols8$coefficients[c("treatcontrad.-both:round2")]
ci11<-cbind(cie11,cice11)
event<-c("b (both-not female)")
ci11<-cbind(ci11,event)
ci11<-data.frame(ci11)

cie31<-coefci(ols8, parm=c("female_both"), vcov = vcovCL(ols8, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice31<-ols8$coefficients[c("female_both")]
ci31<-cbind(cie31,cice31)
event<-c("b (both-female DDD)")
ci31<-cbind(ci31,event)
ci31<-data.frame(ci31)
names(ci31)[3]<-paste("cice11")

cie21<-coefci(ols8a, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols8a, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice21<-ols8a$coefficients[c("treatcontrad.-both:round2")]
ci21<-cbind(cie21,cice21)
event<-c("a (both-not female)")
ci21<-cbind(ci21,event)
ci21<-data.frame(ci21)
names(ci21)[3]<-paste("cice11")

cie41<-coefci(ols8a, parm=c("female_both"), vcov = vcovCL(ols8a, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice41<-ols8a$coefficients[c("female_both")]
ci41<-cbind(cie41,cice41)
event<-c("a (both-female DDD)")
ci41<-cbind(ci41,event)
ci41<-data.frame(ci41)
names(ci41)[3]<-paste("cice11")


cie51<-coefci(ols8c, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols8c, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice51<-ols8c$coefficients[c("treatcontrad.-both:round2")]
ci51<-cbind(cie51,cice51)
event<-c("b (both-less accurate)")
ci51<-cbind(ci51,event)
ci51<-data.frame(ci51)
names(ci51)[3]<-paste("cice11")

cie61<-coefci(ols8c, parm=c("accu_both"), vcov = vcovCL(ols8c, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice61<-ols8c$coefficients[c("accu_both")]
ci61<-cbind(cie61,cice61)
event<-c("b (both-accurate DDD)")
ci61<-cbind(ci61,event)
ci61<-data.frame(ci61)
names(ci61)[3]<-paste("cice11")


cie71<-coefci(ols8d, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols8d, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice71<-ols8d$coefficients[c("treatcontrad.-both:round2")]
ci71<-cbind(cie71,cice71)
event<-c("a (both-less accurate)")
ci71<-cbind(ci71,event)
ci71<-data.frame(ci71)
names(ci71)[3]<-paste("cice11")

cie81<-coefci(ols8d, parm=c("accu_both"), vcov = vcovCL(ols8d, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice81<-ols8d$coefficients[c("accu_both")]
ci81<-cbind(cie81,cice81)
event<-c("a (both-accurate DDD)")
ci81<-cbind(ci81,event)
ci81<-data.frame(ci81)
names(ci81)[3]<-paste("cice11")

cie91<-coefci(ols8e, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols8e, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice91<-ols8e$coefficients[c("treatcontrad.-both:round2")]
ci91<-cbind(cie91,cice91)
event<-c("b (both-less credible)")
ci91<-cbind(ci91,event)
ci91<-data.frame(ci91)
names(ci91)[3]<-paste("cice11")

cie101<-coefci(ols8e, parm=c("cred_both"), vcov = vcovCL(ols8e, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice101<-ols8e$coefficients[c("cred_both")]
ci101<-cbind(cie101,cice101)
event<-c("b (both-credible DDD)")
ci101<-cbind(ci101,event)
ci101<-data.frame(ci101)
names(ci101)[3]<-paste("cice11")

cie111<-coefci(ols8f, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols8f, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice111<-ols8f$coefficients[c("treatcontrad.-both:round2")]
ci111<-cbind(cie111,cice111)
event<-c("a (both-less credible)")
ci111<-cbind(ci111,event)
ci111<-data.frame(ci111)
names(ci111)[3]<-paste("cice11")

cie121<-coefci(ols8f, parm=c("cred_both"), vcov = vcovCL(ols8f, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice121<-ols8f$coefficients[c("cred_both")]
ci121<-cbind(cie121,cice121)
event<-c("a (both-credible DDD)")
ci121<-cbind(ci121,event)
ci121<-data.frame(ci121)
names(ci121)[3]<-paste("cice11")

cie131<-coefci(ols8g, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols8g, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice131<-ols8g$coefficients[c("treatcontrad.-both:round2")]
ci131<-cbind(cie131,cice131)
event<-c("b (both-less forecast usage)")
ci131<-cbind(ci131,event)
ci131<-data.frame(ci131)
names(ci131)[3]<-paste("cice11")

cie141<-coefci(ols8g, parm=c("fore_both"), vcov = vcovCL(ols8g, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice141<-ols8g$coefficients[c("fore_both")]
ci141<-cbind(cie141,cice141)
event<-c("b (both-forecast usage DDD)")
ci141<-cbind(ci141,event)
ci141<-data.frame(ci141)
names(ci141)[3]<-paste("cice11")

cie151<-coefci(ols8h, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols8h, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice151<-ols8h$coefficients[c("treatcontrad.-both:round2")]
ci151<-cbind(cie151,cice151)
event<-c("a (both-less forecast usage)")
ci151<-cbind(ci151,event)
ci151<-data.frame(ci151)
names(ci151)[3]<-paste("cice11")

cie161<-coefci(ols8h, parm=c("fore_both"), vcov = vcovCL(ols8h, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice161<-ols8h$coefficients[c("fore_both")]
ci161<-cbind(cie161,cice161)
event<-c("a (both-forecast usage DDD)")
ci161<-cbind(ci161,event)
ci161<-data.frame(ci161)
names(ci161)[3]<-paste("cice11")

cie171<-coefci(ols8i, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols8i, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice171<-ols8i$coefficients[c("treatcontrad.-both:round2")]
ci171<-cbind(cie171,cice171)
event<-c("b (both-lower temperature)")
ci171<-cbind(ci171,event)
ci171<-data.frame(ci171)
names(ci171)[3]<-paste("cice11")

cie181<-coefci(ols8i, parm=c("temp_both"), vcov = vcovCL(ols8i, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice181<-ols8i$coefficients[c("temp_both")]
ci181<-cbind(cie181,cice181)
event<-c("b (both-temperature DDD)")
ci181<-cbind(ci181,event)
ci181<-data.frame(ci181)
names(ci181)[3]<-paste("cice11")

cie191<-coefci(ols8j, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols8j, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice191<-ols8j$coefficients[c("treatcontrad.-both:round2")]
ci191<-cbind(cie191,cice191)
event<-c("a (both-lower temperature)")
ci191<-cbind(ci191,event)
ci191<-data.frame(ci191)
names(ci191)[3]<-paste("cice11")

cie201<-coefci(ols8j, parm=c("temp_both"), vcov = vcovCL(ols8j, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice201<-ols8j$coefficients[c("temp_both")]
ci201<-cbind(cie201,cice201)
event<-c("a (both-temperature DDD)")
ci201<-cbind(ci201,event)
ci201<-data.frame(ci201)
names(ci201)[3]<-paste("cice11")

cie211<-coefci(ols8k, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols8k, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice211<-ols8k$coefficients[c("treatcontrad.-both:round2")]
ci211<-cbind(cie211,cice211)
event<-c("b (both-lower education)")
ci211<-cbind(ci211,event)
ci211<-data.frame(ci211)
names(ci211)[3]<-paste("cice11")

cie221<-coefci(ols8k, parm=c("study_both"), vcov = vcovCL(ols8k, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice221<-ols8k$coefficients[c("study_both")]
ci221<-cbind(cie221,cice221)
event<-c("b (both-education DDD)")
ci221<-cbind(ci221,event)
ci221<-data.frame(ci221)
names(ci221)[3]<-paste("cice11")

cie231<-coefci(ols8l, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols8l, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice231<-ols8l$coefficients[c("treatcontrad.-both:round2")]
ci231<-cbind(cie231,cice231)
event<-c("a (both-lower education)")
ci231<-cbind(ci231,event)
ci231<-data.frame(ci231)
names(ci231)[3]<-paste("cice11")

cie241<-coefci(ols8l, parm=c("study_both"), vcov = vcovCL(ols8l, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice241<-ols8l$coefficients[c("study_both")]
ci241<-cbind(cie241,cice241)
event<-c("a (both-education DDD)")
ci241<-cbind(ci241,event)
ci241<-data.frame(ci241)
names(ci241)[3]<-paste("cice11")


cidata3<-rbind(ci11,ci31,ci21,ci41,ci51,ci61,ci71,ci81,ci91,ci101,ci111,ci121,ci131,ci141,ci151,ci161,ci171,ci181,ci191,ci201,ci211,ci221,ci231,ci241)
cidata3$X2.5..<-as.numeric(cidata3$X2.5..)
cidata3$X97.5..<-as.numeric(cidata3$X97.5..)
cidata3$cice11<-as.numeric(cidata3$cice11)
cidata3$controls<-"2.) demographic"

cidata<-rbind(cidata,cidata3)


#with demographic variables + further controls
cie11<-coefci(ols9, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols9, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice11<-ols9$coefficients[c("treatcontrad.-both:round2")]
ci11<-cbind(cie11,cice11)
event<-c("b (both-not female)")
ci11<-cbind(ci11,event)
ci11<-data.frame(ci11)

cie31<-coefci(ols9, parm=c("female_both"), vcov = vcovCL(ols9, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice31<-ols9$coefficients[c("female_both")]
ci31<-cbind(cie31,cice31)
event<-c("b (both-female DDD)")
ci31<-cbind(ci31,event)
ci31<-data.frame(ci31)
names(ci31)[3]<-paste("cice11")

cie21<-coefci(ols9a, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols9a, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice21<-ols9a$coefficients[c("treatcontrad.-both:round2")]
ci21<-cbind(cie21,cice21)
event<-c("a (both-not female)")
ci21<-cbind(ci21,event)
ci21<-data.frame(ci21)
names(ci21)[3]<-paste("cice11")

cie41<-coefci(ols9a, parm=c("female_both"), vcov = vcovCL(ols9a, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice41<-ols9a$coefficients[c("female_both")]
ci41<-cbind(cie41,cice41)
event<-c("a (both-female DDD)")
ci41<-cbind(ci41,event)
ci41<-data.frame(ci41)
names(ci41)[3]<-paste("cice11")


cie51<-coefci(ols9c, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols9c, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice51<-ols9c$coefficients[c("treatcontrad.-both:round2")]
ci51<-cbind(cie51,cice51)
event<-c("b (both-less accurate)")
ci51<-cbind(ci51,event)
ci51<-data.frame(ci51)
names(ci51)[3]<-paste("cice11")

cie61<-coefci(ols9c, parm=c("accu_both"), vcov = vcovCL(ols9c, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice61<-ols9c$coefficients[c("accu_both")]
ci61<-cbind(cie61,cice61)
event<-c("b (both-accurate DDD)")
ci61<-cbind(ci61,event)
ci61<-data.frame(ci61)
names(ci61)[3]<-paste("cice11")


cie71<-coefci(ols9d, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols9d, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice71<-ols9d$coefficients[c("treatcontrad.-both:round2")]
ci71<-cbind(cie71,cice71)
event<-c("a (both-less accurate)")
ci71<-cbind(ci71,event)
ci71<-data.frame(ci71)
names(ci71)[3]<-paste("cice11")

cie81<-coefci(ols9d, parm=c("accu_both"), vcov = vcovCL(ols9d, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice81<-ols9d$coefficients[c("accu_both")]
ci81<-cbind(cie81,cice81)
event<-c("a (both-accurate DDD)")
ci81<-cbind(ci81,event)
ci81<-data.frame(ci81)
names(ci81)[3]<-paste("cice11")

cie91<-coefci(ols9e, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols9e, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice91<-ols9e$coefficients[c("treatcontrad.-both:round2")]
ci91<-cbind(cie91,cice91)
event<-c("b (both-less credible)")
ci91<-cbind(ci91,event)
ci91<-data.frame(ci91)
names(ci91)[3]<-paste("cice11")

cie101<-coefci(ols9e, parm=c("cred_both"), vcov = vcovCL(ols9e, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice101<-ols9e$coefficients[c("cred_both")]
ci101<-cbind(cie101,cice101)
event<-c("b (both-credible DDD)")
ci101<-cbind(ci101,event)
ci101<-data.frame(ci101)
names(ci101)[3]<-paste("cice11")

cie111<-coefci(ols9f, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols9f, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice111<-ols9f$coefficients[c("treatcontrad.-both:round2")]
ci111<-cbind(cie111,cice111)
event<-c("a (both-less credible)")
ci111<-cbind(ci111,event)
ci111<-data.frame(ci111)
names(ci111)[3]<-paste("cice11")

cie121<-coefci(ols9f, parm=c("cred_both"), vcov = vcovCL(ols9f, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice121<-ols9f$coefficients[c("cred_both")]
ci121<-cbind(cie121,cice121)
event<-c("a (both-credible DDD)")
ci121<-cbind(ci121,event)
ci121<-data.frame(ci121)
names(ci121)[3]<-paste("cice11")

cie131<-coefci(ols9g, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols9g, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice131<-ols9g$coefficients[c("treatcontrad.-both:round2")]
ci131<-cbind(cie131,cice131)
event<-c("b (both-less forecast usage)")
ci131<-cbind(ci131,event)
ci131<-data.frame(ci131)
names(ci131)[3]<-paste("cice11")

cie141<-coefci(ols9g, parm=c("fore_both"), vcov = vcovCL(ols9g, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice141<-ols9g$coefficients[c("fore_both")]
ci141<-cbind(cie141,cice141)
event<-c("b (both-forecast usage DDD)")
ci141<-cbind(ci141,event)
ci141<-data.frame(ci141)
names(ci141)[3]<-paste("cice11")

cie151<-coefci(ols9h, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols9h, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice151<-ols9h$coefficients[c("treatcontrad.-both:round2")]
ci151<-cbind(cie151,cice151)
event<-c("a (both-less forecast usage)")
ci151<-cbind(ci151,event)
ci151<-data.frame(ci151)
names(ci151)[3]<-paste("cice11")

cie161<-coefci(ols9h, parm=c("fore_both"), vcov = vcovCL(ols9h, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice161<-ols9h$coefficients[c("fore_both")]
ci161<-cbind(cie161,cice161)
event<-c("a (both-forecast usage DDD)")
ci161<-cbind(ci161,event)
ci161<-data.frame(ci161)
names(ci161)[3]<-paste("cice11")

cie171<-coefci(ols9i, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols9i, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice171<-ols9i$coefficients[c("treatcontrad.-both:round2")]
ci171<-cbind(cie171,cice171)
event<-c("b (both-lower temperature)")
ci171<-cbind(ci171,event)
ci171<-data.frame(ci171)
names(ci171)[3]<-paste("cice11")

cie181<-coefci(ols9i, parm=c("temp_both"), vcov = vcovCL(ols9i, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice181<-ols9i$coefficients[c("temp_both")]
ci181<-cbind(cie181,cice181)
event<-c("b (both-temperature DDD)")
ci181<-cbind(ci181,event)
ci181<-data.frame(ci181)
names(ci181)[3]<-paste("cice11")

cie191<-coefci(ols9j, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols9j, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice191<-ols9j$coefficients[c("treatcontrad.-both:round2")]
ci191<-cbind(cie191,cice191)
event<-c("a (both-lower temperature)")
ci191<-cbind(ci191,event)
ci191<-data.frame(ci191)
names(ci191)[3]<-paste("cice11")

cie201<-coefci(ols9j, parm=c("temp_both"), vcov = vcovCL(ols9j, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice201<-ols9j$coefficients[c("temp_both")]
ci201<-cbind(cie201,cice201)
event<-c("a (both-temperature DDD)")
ci201<-cbind(ci201,event)
ci201<-data.frame(ci201)
names(ci201)[3]<-paste("cice11")

cie211<-coefci(ols9k, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols9k, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice211<-ols9k$coefficients[c("treatcontrad.-both:round2")]
ci211<-cbind(cie211,cice211)
event<-c("b (both-lower education)")
ci211<-cbind(ci211,event)
ci211<-data.frame(ci211)
names(ci211)[3]<-paste("cice11")

cie221<-coefci(ols9k, parm=c("study_both"), vcov = vcovCL(ols9k, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice221<-ols9k$coefficients[c("study_both")]
ci221<-cbind(cie221,cice221)
event<-c("b (both-education DDD)")
ci221<-cbind(ci221,event)
ci221<-data.frame(ci221)
names(ci221)[3]<-paste("cice11")

cie231<-coefci(ols9l, parm=c("treatcontrad.-both:round2"), vcov = vcovCL(ols9l, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice231<-ols9l$coefficients[c("treatcontrad.-both:round2")]
ci231<-cbind(cie231,cice231)
event<-c("a (both-lower education)")
ci231<-cbind(ci231,event)
ci231<-data.frame(ci231)
names(ci231)[3]<-paste("cice11")

cie241<-coefci(ols9l, parm=c("study_both"), vcov = vcovCL(ols9l, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice241<-ols9l$coefficients[c("study_both")]
ci241<-cbind(cie241,cice241)
event<-c("a (both-education DDD)")
ci241<-cbind(ci241,event)
ci241<-data.frame(ci241)
names(ci241)[3]<-paste("cice11")




cidata3<-rbind(ci11,ci31,ci21,ci41,ci51,ci61,ci71,ci81,ci91,ci101,ci111,ci121,ci131,ci141,ci151,ci161,ci171,ci181,ci191,ci201,ci211,ci221,ci231,ci241)
cidata3$X2.5..<-as.numeric(cidata3$X2.5..)
cidata3$X97.5..<-as.numeric(cidata3$X97.5..)
cidata3$cice11<-as.numeric(cidata3$cice11)
cidata3$controls<-"3.) demographic + further"

cidataa<-rbind(cidata,cidata3)

#Graph 2: Interval estimates
# no control variables
cie11<-coefci(ols7, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols7, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice11<-ols7$coefficients[c("treatcontrad.-interval:round2")]
ci11<-cbind(cie11,cice11)
event<-c("b (interval-not female)")
ci11<-cbind(ci11,event)
ci11<-data.frame(ci11)

cie31<-coefci(ols7, parm=c("female_int"), vcov = vcovCL(ols7, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice31<-ols7$coefficients[c("female_int")]
ci31<-cbind(cie31,cice31)
event<-c("b (interval-female DDD)")
ci31<-cbind(ci31,event)
ci31<-data.frame(ci31)
names(ci31)[3]<-paste("cice11")

cie21<-coefci(ols7a, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols7a, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice21<-ols7a$coefficients[c("treatcontrad.-interval:round2")]
ci21<-cbind(cie21,cice21)
event<-c("a (interval-not female)")
ci21<-cbind(ci21,event)
ci21<-data.frame(ci21)
names(ci21)[3]<-paste("cice11")

cie41<-coefci(ols7a, parm=c("female_int"), vcov = vcovCL(ols7a, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice41<-ols7a$coefficients[c("female_int")]
ci41<-cbind(cie41,cice41)
event<-c("a (interval-female DDD)")
ci41<-cbind(ci41,event)
ci41<-data.frame(ci41)
names(ci41)[3]<-paste("cice11")


cie51<-coefci(ols7c, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols7c, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice51<-ols7c$coefficients[c("treatcontrad.-interval:round2")]
ci51<-cbind(cie51,cice51)
event<-c("b (interval-less accurate)")
ci51<-cbind(ci51,event)
ci51<-data.frame(ci51)
names(ci51)[3]<-paste("cice11")

cie61<-coefci(ols7c, parm=c("accu_int"), vcov = vcovCL(ols7c, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice61<-ols7c$coefficients[c("accu_int")]
ci61<-cbind(cie61,cice61)
event<-c("b (interval-accurate DDD)")
ci61<-cbind(ci61,event)
ci61<-data.frame(ci61)
names(ci61)[3]<-paste("cice11")


cie71<-coefci(ols7d, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols7d, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice71<-ols7d$coefficients[c("treatcontrad.-interval:round2")]
ci71<-cbind(cie71,cice71)
event<-c("a (interval-less accurate)")
ci71<-cbind(ci71,event)
ci71<-data.frame(ci71)
names(ci71)[3]<-paste("cice11")

cie81<-coefci(ols7d, parm=c("accu_int"), vcov = vcovCL(ols7d, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice81<-ols7d$coefficients[c("accu_int")]
ci81<-cbind(cie81,cice81)
event<-c("a (interval-accurate DDD)")
ci81<-cbind(ci81,event)
ci81<-data.frame(ci81)
names(ci81)[3]<-paste("cice11")

cie91<-coefci(ols7e, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols7e, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice91<-ols7e$coefficients[c("treatcontrad.-interval:round2")]
ci91<-cbind(cie91,cice91)
event<-c("b (interval-less credible)")
ci91<-cbind(ci91,event)
ci91<-data.frame(ci91)
names(ci91)[3]<-paste("cice11")

cie101<-coefci(ols7e, parm=c("cred_int"), vcov = vcovCL(ols7e, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice101<-ols7e$coefficients[c("cred_int")]
ci101<-cbind(cie101,cice101)
event<-c("b (interval-credible DDD)")
ci101<-cbind(ci101,event)
ci101<-data.frame(ci101)
names(ci101)[3]<-paste("cice11")

cie111<-coefci(ols7f, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols7f, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice111<-ols7f$coefficients[c("treatcontrad.-interval:round2")]
ci111<-cbind(cie111,cice111)
event<-c("a (interval-less credible)")
ci111<-cbind(ci111,event)
ci111<-data.frame(ci111)
names(ci111)[3]<-paste("cice11")

cie121<-coefci(ols7f, parm=c("cred_int"), vcov = vcovCL(ols7f, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice121<-ols7f$coefficients[c("cred_int")]
ci121<-cbind(cie121,cice121)
event<-c("a (interval-credible DDD)")
ci121<-cbind(ci121,event)
ci121<-data.frame(ci121)
names(ci121)[3]<-paste("cice11")

cie131<-coefci(ols7g, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols7g, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice131<-ols7g$coefficients[c("treatcontrad.-interval:round2")]
ci131<-cbind(cie131,cice131)
event<-c("b (interval-less forecast usage)")
ci131<-cbind(ci131,event)
ci131<-data.frame(ci131)
names(ci131)[3]<-paste("cice11")

cie141<-coefci(ols7g, parm=c("fore_int"), vcov = vcovCL(ols7g, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice141<-ols7g$coefficients[c("fore_int")]
ci141<-cbind(cie141,cice141)
event<-c("b (interval-forecast usage DDD)")
ci141<-cbind(ci141,event)
ci141<-data.frame(ci141)
names(ci141)[3]<-paste("cice11")

cie151<-coefci(ols7h, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols7h, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice151<-ols7h$coefficients[c("treatcontrad.-interval:round2")]
ci151<-cbind(cie151,cice151)
event<-c("a (interval-less forecast usage)")
ci151<-cbind(ci151,event)
ci151<-data.frame(ci151)
names(ci151)[3]<-paste("cice11")

cie161<-coefci(ols7h, parm=c("fore_int"), vcov = vcovCL(ols7h, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice161<-ols7h$coefficients[c("fore_int")]
ci161<-cbind(cie161,cice161)
event<-c("a (interval-forecast usage DDD)")
ci161<-cbind(ci161,event)
ci161<-data.frame(ci161)
names(ci161)[3]<-paste("cice11")

cie171<-coefci(ols7i, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols7i, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice171<-ols7i$coefficients[c("treatcontrad.-interval:round2")]
ci171<-cbind(cie171,cice171)
event<-c("b (interval-lower temperature)")
ci171<-cbind(ci171,event)
ci171<-data.frame(ci171)
names(ci171)[3]<-paste("cice11")

cie181<-coefci(ols7i, parm=c("temp_int"), vcov = vcovCL(ols7i, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice181<-ols7i$coefficients[c("temp_int")]
ci181<-cbind(cie181,cice181)
event<-c("b (interval-temperature DDD)")
ci181<-cbind(ci181,event)
ci181<-data.frame(ci181)
names(ci181)[3]<-paste("cice11")

cie191<-coefci(ols7j, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols7j, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice191<-ols7j$coefficients[c("treatcontrad.-interval:round2")]
ci191<-cbind(cie191,cice191)
event<-c("a (interval-lower temperature)")
ci191<-cbind(ci191,event)
ci191<-data.frame(ci191)
names(ci191)[3]<-paste("cice11")

cie201<-coefci(ols7j, parm=c("temp_int"), vcov = vcovCL(ols7j, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice201<-ols7j$coefficients[c("temp_int")]
ci201<-cbind(cie201,cice201)
event<-c("a (interval-temperature DDD)")
ci201<-cbind(ci201,event)
ci201<-data.frame(ci201)
names(ci201)[3]<-paste("cice11")

cie211<-coefci(ols7k, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols7k, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice211<-ols7k$coefficients[c("treatcontrad.-interval:round2")]
ci211<-cbind(cie211,cice211)
event<-c("b (interval-lower education)")
ci211<-cbind(ci211,event)
ci211<-data.frame(ci211)
names(ci211)[3]<-paste("cice11")

cie221<-coefci(ols7k, parm=c("study_int"), vcov = vcovCL(ols7k, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice221<-ols7k$coefficients[c("study_int")]
ci221<-cbind(cie221,cice221)
event<-c("b (interval-education DDD)")
ci221<-cbind(ci221,event)
ci221<-data.frame(ci221)
names(ci221)[3]<-paste("cice11")

cie231<-coefci(ols7l, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols7l, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice231<-ols7l$coefficients[c("treatcontrad.-interval:round2")]
ci231<-cbind(cie231,cice231)
event<-c("a (interval-lower education)")
ci231<-cbind(ci231,event)
ci231<-data.frame(ci231)
names(ci231)[3]<-paste("cice11")

cie241<-coefci(ols7l, parm=c("study_int"), vcov = vcovCL(ols7l, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice241<-ols7l$coefficients[c("study_int")]
ci241<-cbind(cie241,cice241)
event<-c("a (interval-education DDD)")
ci241<-cbind(ci241,event)
ci241<-data.frame(ci241)
names(ci241)[3]<-paste("cice11")



cidata<-rbind(ci11,ci31,ci21,ci41,ci51,ci61,ci71,ci81,ci91,ci101,ci111,ci121,ci131,ci141,ci151,ci161,ci171,ci181,ci191,ci201,ci211,ci221,ci231,ci241)
cidata$X2.5..<-as.numeric(cidata$X2.5..)
cidata$X97.5..<-as.numeric(cidata$X97.5..)
cidata$cice11<-as.numeric(cidata$cice11)
cidata$controls<-"1.) none"

#with demographic variables
cie11<-coefci(ols8, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols8, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice11<-ols8$coefficients[c("treatcontrad.-interval:round2")]
ci11<-cbind(cie11,cice11)
event<-c("b (interval-not female)")
ci11<-cbind(ci11,event)
ci11<-data.frame(ci11)

cie31<-coefci(ols8, parm=c("female_int"), vcov = vcovCL(ols8, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice31<-ols8$coefficients[c("female_int")]
ci31<-cbind(cie31,cice31)
event<-c("b (interval-female DDD)")
ci31<-cbind(ci31,event)
ci31<-data.frame(ci31)
names(ci31)[3]<-paste("cice11")

cie21<-coefci(ols8a, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols8a, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice21<-ols8a$coefficients[c("treatcontrad.-interval:round2")]
ci21<-cbind(cie21,cice21)
event<-c("a (interval-not female)")
ci21<-cbind(ci21,event)
ci21<-data.frame(ci21)
names(ci21)[3]<-paste("cice11")

cie41<-coefci(ols8a, parm=c("female_int"), vcov = vcovCL(ols8a, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice41<-ols8a$coefficients[c("female_int")]
ci41<-cbind(cie41,cice41)
event<-c("a (interval-female DDD)")
ci41<-cbind(ci41,event)
ci41<-data.frame(ci41)
names(ci41)[3]<-paste("cice11")


cie51<-coefci(ols8c, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols8c, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice51<-ols8c$coefficients[c("treatcontrad.-interval:round2")]
ci51<-cbind(cie51,cice51)
event<-c("b (interval-less accurate)")
ci51<-cbind(ci51,event)
ci51<-data.frame(ci51)
names(ci51)[3]<-paste("cice11")

cie61<-coefci(ols8c, parm=c("accu_int"), vcov = vcovCL(ols8c, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice61<-ols8c$coefficients[c("accu_int")]
ci61<-cbind(cie61,cice61)
event<-c("b (interval-accurate DDD)")
ci61<-cbind(ci61,event)
ci61<-data.frame(ci61)
names(ci61)[3]<-paste("cice11")


cie71<-coefci(ols8d, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols8d, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice71<-ols8d$coefficients[c("treatcontrad.-interval:round2")]
ci71<-cbind(cie71,cice71)
event<-c("a (interval-less accurate)")
ci71<-cbind(ci71,event)
ci71<-data.frame(ci71)
names(ci71)[3]<-paste("cice11")

cie81<-coefci(ols8d, parm=c("accu_int"), vcov = vcovCL(ols8d, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice81<-ols8d$coefficients[c("accu_int")]
ci81<-cbind(cie81,cice81)
event<-c("a (interval-accurate DDD)")
ci81<-cbind(ci81,event)
ci81<-data.frame(ci81)
names(ci81)[3]<-paste("cice11")

cie91<-coefci(ols8e, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols8e, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice91<-ols8e$coefficients[c("treatcontrad.-interval:round2")]
ci91<-cbind(cie91,cice91)
event<-c("b (interval-less credible)")
ci91<-cbind(ci91,event)
ci91<-data.frame(ci91)
names(ci91)[3]<-paste("cice11")

cie101<-coefci(ols8e, parm=c("cred_int"), vcov = vcovCL(ols8e, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice101<-ols8e$coefficients[c("cred_int")]
ci101<-cbind(cie101,cice101)
event<-c("b (interval-credible DDD)")
ci101<-cbind(ci101,event)
ci101<-data.frame(ci101)
names(ci101)[3]<-paste("cice11")

cie111<-coefci(ols8f, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols8f, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice111<-ols8f$coefficients[c("treatcontrad.-interval:round2")]
ci111<-cbind(cie111,cice111)
event<-c("a (interval-less credible)")
ci111<-cbind(ci111,event)
ci111<-data.frame(ci111)
names(ci111)[3]<-paste("cice11")

cie121<-coefci(ols8f, parm=c("cred_int"), vcov = vcovCL(ols8f, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice121<-ols8f$coefficients[c("cred_int")]
ci121<-cbind(cie121,cice121)
event<-c("a (interval-credible DDD)")
ci121<-cbind(ci121,event)
ci121<-data.frame(ci121)
names(ci121)[3]<-paste("cice11")

cie131<-coefci(ols8g, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols8g, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice131<-ols8g$coefficients[c("treatcontrad.-interval:round2")]
ci131<-cbind(cie131,cice131)
event<-c("b (interval-less forecast usage)")
ci131<-cbind(ci131,event)
ci131<-data.frame(ci131)
names(ci131)[3]<-paste("cice11")

cie141<-coefci(ols8g, parm=c("fore_int"), vcov = vcovCL(ols8g, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice141<-ols8g$coefficients[c("fore_int")]
ci141<-cbind(cie141,cice141)
event<-c("b (interval-forecast usage DDD)")
ci141<-cbind(ci141,event)
ci141<-data.frame(ci141)
names(ci141)[3]<-paste("cice11")

cie151<-coefci(ols8h, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols8h, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice151<-ols8h$coefficients[c("treatcontrad.-interval:round2")]
ci151<-cbind(cie151,cice151)
event<-c("a (interval-less forecast usage)")
ci151<-cbind(ci151,event)
ci151<-data.frame(ci151)
names(ci151)[3]<-paste("cice11")

cie161<-coefci(ols8h, parm=c("fore_int"), vcov = vcovCL(ols8h, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice161<-ols8h$coefficients[c("fore_int")]
ci161<-cbind(cie161,cice161)
event<-c("a (interval-forecast usage DDD)")
ci161<-cbind(ci161,event)
ci161<-data.frame(ci161)
names(ci161)[3]<-paste("cice11")

cie171<-coefci(ols8i, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols8i, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice171<-ols8i$coefficients[c("treatcontrad.-interval:round2")]
ci171<-cbind(cie171,cice171)
event<-c("b (interval-lower temperature)")
ci171<-cbind(ci171,event)
ci171<-data.frame(ci171)
names(ci171)[3]<-paste("cice11")

cie181<-coefci(ols8i, parm=c("temp_int"), vcov = vcovCL(ols8i, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice181<-ols8i$coefficients[c("temp_int")]
ci181<-cbind(cie181,cice181)
event<-c("b (interval-temperature DDD)")
ci181<-cbind(ci181,event)
ci181<-data.frame(ci181)
names(ci181)[3]<-paste("cice11")

cie191<-coefci(ols8j, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols8j, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice191<-ols8j$coefficients[c("treatcontrad.-interval:round2")]
ci191<-cbind(cie191,cice191)
event<-c("a (interval-lower temperature)")
ci191<-cbind(ci191,event)
ci191<-data.frame(ci191)
names(ci191)[3]<-paste("cice11")

cie201<-coefci(ols8j, parm=c("temp_int"), vcov = vcovCL(ols8j, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice201<-ols8j$coefficients[c("temp_int")]
ci201<-cbind(cie201,cice201)
event<-c("a (interval-temperature DDD)")
ci201<-cbind(ci201,event)
ci201<-data.frame(ci201)
names(ci201)[3]<-paste("cice11")

cie211<-coefci(ols8k, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols8k, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice211<-ols8k$coefficients[c("treatcontrad.-interval:round2")]
ci211<-cbind(cie211,cice211)
event<-c("b (interval-lower education)")
ci211<-cbind(ci211,event)
ci211<-data.frame(ci211)
names(ci211)[3]<-paste("cice11")

cie221<-coefci(ols8k, parm=c("study_int"), vcov = vcovCL(ols8k, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice221<-ols8k$coefficients[c("study_int")]
ci221<-cbind(cie221,cice221)
event<-c("b (interval-education DDD)")
ci221<-cbind(ci221,event)
ci221<-data.frame(ci221)
names(ci221)[3]<-paste("cice11")

cie231<-coefci(ols8l, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols8l, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice231<-ols8l$coefficients[c("treatcontrad.-interval:round2")]
ci231<-cbind(cie231,cice231)
event<-c("a (interval-lower education)")
ci231<-cbind(ci231,event)
ci231<-data.frame(ci231)
names(ci231)[3]<-paste("cice11")

cie241<-coefci(ols8l, parm=c("study_int"), vcov = vcovCL(ols8l, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice241<-ols8l$coefficients[c("study_int")]
ci241<-cbind(cie241,cice241)
event<-c("a (interval-education DDD)")
ci241<-cbind(ci241,event)
ci241<-data.frame(ci241)
names(ci241)[3]<-paste("cice11")


cidata3<-rbind(ci11,ci31,ci21,ci41,ci51,ci61,ci71,ci81,ci91,ci101,ci111,ci121,ci131,ci141,ci151,ci161,ci171,ci181,ci191,ci201,ci211,ci221,ci231,ci241)
cidata3$X2.5..<-as.numeric(cidata3$X2.5..)
cidata3$X97.5..<-as.numeric(cidata3$X97.5..)
cidata3$cice11<-as.numeric(cidata3$cice11)
cidata3$controls<-"2.) demographic"

cidata<-rbind(cidata,cidata3)


#with demographic variables + further controls
cie11<-coefci(ols9, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols9, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice11<-ols9$coefficients[c("treatcontrad.-interval:round2")]
ci11<-cbind(cie11,cice11)
event<-c("b (interval-not female)")
ci11<-cbind(ci11,event)
ci11<-data.frame(ci11)

cie31<-coefci(ols9, parm=c("female_int"), vcov = vcovCL(ols9, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice31<-ols9$coefficients[c("female_int")]
ci31<-cbind(cie31,cice31)
event<-c("b (interval-female DDD)")
ci31<-cbind(ci31,event)
ci31<-data.frame(ci31)
names(ci31)[3]<-paste("cice11")

cie21<-coefci(ols9a, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols9a, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice21<-ols9a$coefficients[c("treatcontrad.-interval:round2")]
ci21<-cbind(cie21,cice21)
event<-c("a (interval-not female)")
ci21<-cbind(ci21,event)
ci21<-data.frame(ci21)
names(ci21)[3]<-paste("cice11")

cie41<-coefci(ols9a, parm=c("female_int"), vcov = vcovCL(ols9a, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice41<-ols9a$coefficients[c("female_int")]
ci41<-cbind(cie41,cice41)
event<-c("a (interval-female DDD)")
ci41<-cbind(ci41,event)
ci41<-data.frame(ci41)
names(ci41)[3]<-paste("cice11")


cie51<-coefci(ols9c, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols9c, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice51<-ols9c$coefficients[c("treatcontrad.-interval:round2")]
ci51<-cbind(cie51,cice51)
event<-c("b (interval-less accurate)")
ci51<-cbind(ci51,event)
ci51<-data.frame(ci51)
names(ci51)[3]<-paste("cice11")

cie61<-coefci(ols9c, parm=c("accu_int"), vcov = vcovCL(ols9c, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice61<-ols9c$coefficients[c("accu_int")]
ci61<-cbind(cie61,cice61)
event<-c("b (interval-accurate DDD)")
ci61<-cbind(ci61,event)
ci61<-data.frame(ci61)
names(ci61)[3]<-paste("cice11")


cie71<-coefci(ols9d, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols9d, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice71<-ols9d$coefficients[c("treatcontrad.-interval:round2")]
ci71<-cbind(cie71,cice71)
event<-c("a (interval-less accurate)")
ci71<-cbind(ci71,event)
ci71<-data.frame(ci71)
names(ci71)[3]<-paste("cice11")

cie81<-coefci(ols9d, parm=c("accu_int"), vcov = vcovCL(ols9d, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice81<-ols9d$coefficients[c("accu_int")]
ci81<-cbind(cie81,cice81)
event<-c("a (interval-accurate DDD)")
ci81<-cbind(ci81,event)
ci81<-data.frame(ci81)
names(ci81)[3]<-paste("cice11")

cie91<-coefci(ols9e, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols9e, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice91<-ols9e$coefficients[c("treatcontrad.-interval:round2")]
ci91<-cbind(cie91,cice91)
event<-c("b (interval-less credible)")
ci91<-cbind(ci91,event)
ci91<-data.frame(ci91)
names(ci91)[3]<-paste("cice11")

cie101<-coefci(ols9e, parm=c("cred_int"), vcov = vcovCL(ols9e, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice101<-ols9e$coefficients[c("cred_int")]
ci101<-cbind(cie101,cice101)
event<-c("b (interval-credible DDD)")
ci101<-cbind(ci101,event)
ci101<-data.frame(ci101)
names(ci101)[3]<-paste("cice11")

cie111<-coefci(ols9f, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols9f, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice111<-ols9f$coefficients[c("treatcontrad.-interval:round2")]
ci111<-cbind(cie111,cice111)
event<-c("a (interval-less credible)")
ci111<-cbind(ci111,event)
ci111<-data.frame(ci111)
names(ci111)[3]<-paste("cice11")

cie121<-coefci(ols9f, parm=c("cred_int"), vcov = vcovCL(ols9f, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice121<-ols9f$coefficients[c("cred_int")]
ci121<-cbind(cie121,cice121)
event<-c("a (interval-credible DDD)")
ci121<-cbind(ci121,event)
ci121<-data.frame(ci121)
names(ci121)[3]<-paste("cice11")

cie131<-coefci(ols9g, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols9g, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice131<-ols9g$coefficients[c("treatcontrad.-interval:round2")]
ci131<-cbind(cie131,cice131)
event<-c("b (interval-less forecast usage)")
ci131<-cbind(ci131,event)
ci131<-data.frame(ci131)
names(ci131)[3]<-paste("cice11")

cie141<-coefci(ols9g, parm=c("fore_int"), vcov = vcovCL(ols9g, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice141<-ols9g$coefficients[c("fore_int")]
ci141<-cbind(cie141,cice141)
event<-c("b (interval-forecast usage DDD)")
ci141<-cbind(ci141,event)
ci141<-data.frame(ci141)
names(ci141)[3]<-paste("cice11")

cie151<-coefci(ols9h, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols9h, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice151<-ols9h$coefficients[c("treatcontrad.-interval:round2")]
ci151<-cbind(cie151,cice151)
event<-c("a (interval-less forecast usage)")
ci151<-cbind(ci151,event)
ci151<-data.frame(ci151)
names(ci151)[3]<-paste("cice11")

cie161<-coefci(ols9h, parm=c("fore_int"), vcov = vcovCL(ols9h, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice161<-ols9h$coefficients[c("fore_int")]
ci161<-cbind(cie161,cice161)
event<-c("a (interval-forecast usage DDD)")
ci161<-cbind(ci161,event)
ci161<-data.frame(ci161)
names(ci161)[3]<-paste("cice11")

cie171<-coefci(ols9i, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols9i, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice171<-ols9i$coefficients[c("treatcontrad.-interval:round2")]
ci171<-cbind(cie171,cice171)
event<-c("b (interval-lower temperature)")
ci171<-cbind(ci171,event)
ci171<-data.frame(ci171)
names(ci171)[3]<-paste("cice11")

cie181<-coefci(ols9i, parm=c("temp_int"), vcov = vcovCL(ols9i, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice181<-ols9i$coefficients[c("temp_int")]
ci181<-cbind(cie181,cice181)
event<-c("b (interval-temperature DDD)")
ci181<-cbind(ci181,event)
ci181<-data.frame(ci181)
names(ci181)[3]<-paste("cice11")

cie191<-coefci(ols9j, parm=c("treatcontrad.-interval:round2"), vcov = vcovCL(ols9j, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice191<-ols9j$coefficients[c("treatcontrad.-interval:round2")]
ci191<-cbind(cie191,cice191)
event<-c("a (interval-lower temperature)")
ci191<-cbind(ci191,event)
ci191<-data.frame(ci191)
names(ci191)[3]<-paste("cice11")

cie201<-coefci(ols9j, parm=c("temp_int"), vcov = vcovCL(ols9j, cluster=~data3$participant.label, type="HC1"),level=0.95)
cice201<-ols9j$coefficients[c("temp_int")]
ci201<-cbind(cie201,cice201)
event<-c("a (interval-temperature DDD)")
ci201<-cbind(ci201,event)
ci201<-data.frame(ci201)
names(ci201)[3]<-paste("cice11")

cidata3<-rbind(ci11,ci31,ci21,ci41,ci51,ci61,ci71,ci81,ci91,ci101,ci111,ci121,ci131,ci141,ci151,ci161,ci171,ci181,ci191,ci201,ci211,ci221,ci231,ci241)
cidata3$X2.5..<-as.numeric(cidata3$X2.5..)
cidata3$X97.5..<-as.numeric(cidata3$X97.5..)
cidata3$cice11<-as.numeric(cidata3$cice11)
cidata3$controls<-"3.) demographic + further"

cidata<-rbind(cidata,cidata3)

cidata<-rbind(cidataa,cidata)

cidata$controls<- with(cidata, factor(controls, levels=c("1.) none","2.) demographic", "3.) demographic + further")))


p3 <- ggplot(
  mapping = aes(
    y = cidata$event
  )
) +
  scale_y_discrete( limits=c("a (both-not female)","a (both-female DDD)", "b (both-not female)", "b (both-female DDD)","a (both-less accurate)","a (both-accurate DDD)", "b (both-less accurate)", "b (both-accurate DDD)","a (both-less credible)","a (both-credible DDD)", "b (both-less credible)", "b (both-credible DDD)","a (both-less forecast usage)","a (both-forecast usage DDD)", "b (both-less forecast usage)", "b (both-forecast usage DDD)","a (both-lower temperature)","a (both-temperature DDD)", "b (both-lower temperature)", "b (both-temperature DDD)","a (both-lower education)","a (both-education DDD)", "b (both-lower education)", "b (both-education DDD)", "a (interval-not female)","a (interval-female DDD)", "b (interval-not female)", "b (interval-female DDD)","a (interval-less accurate)","a (interval-accurate DDD)", "b (interval-less accurate)", "b (interval-accurate DDD)","a (interval-less credible)","a (interval-credible DDD)", "b (interval-less credible)", "b (interval-credible DDD)","a (interval-less forecast usage)","a (interval-forecast usage DDD)", "b (interval-less forecast usage)", "b (interval-forecast usage DDD)","a (interval-lower temperature)","a (interval-temperature DDD)", "b (interval-lower temperature)", "b (interval-temperature DDD)","a (interval-lower education)","a (interval-education DDD)", "b (interval-lower education)", "b (interval-education DDD)"))+
  geom_pointrange(aes(
    x = cidata$cice11,
    y = cidata$event,
    xmin = cidata$X2.5..,
    xmax = cidata$X97.5..,
    shape=factor(cidata$controls, levels=c("3.) demographic + further","2.) demographic", "1.) none"))
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

ggsave(plot = p3, 
       filename = "../rendered_documents/pdf/Ambiguity_regs_het_contradiction.pdf",
       height = 9.5, 
       width = 7.2)
