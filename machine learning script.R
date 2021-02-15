#machine learning stuff
#create datafile with only complete cases (no NAs)
its_complete<-its1[complete.cases(its1),]
library(caret)

inTrain<-createDataPartition(its1$idtheft,p=.6, list=FALSE)
#put 60% in training (inTrain)
training <- its1[inTrain,]
#put 40% in test(not inTrain)
testing <- its1[-inTrain,]

#check for predictors with no variability in training data
nzv<-nearZeroVar(training, saveMetrics = TRUE)
nzv

#train logistic regression model using all predictors
modFit <- glm(idtheft ~ .,data=training,family="binomial")
#get model estimates
summary(modFit)
#Odds ratio
exp(modFit$coeff)
#confidence intervals
exp(confint(modFit))
#anova- used for putting factors in and out of model
anova(modFit, test = "Chisq")


#adjust model to exclude sex because its p value is close to 1
modFit_adjust<-glm(idtheft ~incomer+ager+ethnicr+prevent_total+OUTSIDE_PAST_YEARR+ notify_breachr
                   ,data=training,family="binomial")
#get model estimates
summary(modFit_adjust)
#Odds ratio
exp(modFit_adjust$coeff)
#confidence intervals
exp(confint(modFit_adjust))
#anova- used for putting factors in and out of model
anova(modFit_adjust, test = "Chisq")

#nested model selection
anova(modFit, modFit_adjust)

