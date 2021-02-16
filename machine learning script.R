#machine learning stuff

library(caret)

#change reference groups in variables
its1$ager<-relevel(its1$ager, ref = 4)
its1$ethnicr<-relevel(its1$ethnicr, ref = 2)
its1$notify_breachr<-relevel(its1$notify_breachr, ref =2)

inTrain<-createDataPartition(its1$idtheft,p=.6, list=FALSE)
#put 60% in training (inTrain)
training <- its1[inTrain,]
#put 40% in test(not inTrain)
testing <- its1[-inTrain,]

#check for predictors with no variability in training data
nzv<-nearZeroVar(training, saveMetrics = TRUE)
nzv

#train logistic regression model using all predictors except sex
modFit <- glm(idtheft ~ incomer+ager+ethnicr+prevent_total+OUTSIDE_PAST_YEARR+notify_breachr
                ,data=training,family="binomial")
#get model estimates
summary(modFit)
#Odds ratio
exp(modFit$coeff)
#graph of model residuals
plot(modFit)
#get variance inflation factors
vif(modFit)
#confidence intervals
exp(confint(modFit))
#anova- used for putting factors in and out of model
anova(modFit, test = "Chisq")

