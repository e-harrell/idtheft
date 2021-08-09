#WILL NEED TO CHANGE OUTCOME VARIABLE FOR PROJECT TO PREDICTING OVERALL IDENTITY THEFT
#full script for project
#adding libraries
library(reshape2)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(caret)
library(caTools)
library(pROC)


#Loading data
#read in  R data from 2016 ITS downloaded from ICPSR website:
#https://www.icpsr.umich.edu/web/NACJD/studies/36829 and saved in data folder
unzip("./data/ICPSR_36829-V2.zip", exdir="./data")
#have to increase R memory to load data
memory.limit(50000)
#read in data from data folder
load(file="./data/ICPSR_36829/DS0001/36829-0001-Data.rda")
#create data file with shorter name
data<-da36829.0001
#remove original data
rm(da36829.0001)

#Data wrangling and cleaning
#Reducing the dataset- There were 125,165 total persons in the 2016 ITS.
#Just looking at the completed  telephone and personal interviews leaves 
#96,130 interviews or observations in the dataset.
#Get interview status for all cases 

x<-cbind(Count=table(data$VS008),Percentage=round(prop.table(table(data$VS008))*100))
rbind(x,Total = c(nrow(data),100))

#select only cases with ITS interviews
data<-data[data$VS008 != levels(data$VS008)[3],]
x<-cbind(Count=table(data$VS008),Percentage=round(prop.table(table(data$VS008))*100))
rbind(x,Total = c(nrow(data),100))

rm(x)

#Create smaller dataset with only variables needed for analysis
#outcome variable
#past year ID theft variables and bank and CC account variables
pastyearbankacct<-data$VS010
existing_bank<-data$VS012
currentccacct<-data$VS014
pastyearccacct<-data$VS016
existing_credit_card<-data$VS017
other_existing_accts<-data$VS019
open_new_acct<-data$VS041
personal_info<-data$VS063

#predictor variables
#demographic variables
sex<-data$V3018
race<-data$V3023A
hispanic<-data$V3024A
age<-data$V3014
income<-data$V2026A

#prior to past year ID theft variable
OUTSIDE_PAST_YEAR<-data$VS306

#preventative behavior variables
CHCKD_CR_PAST_YR<-data$VS343
CHNG_PASSWORDS<-data$VS345
PURCHASE_IDTHFT_INS<-data$VS347
SHRED_DOCS<-data$VS349
VERIFY_CHARGES<-data$VS351
PROTECT_COMPUTER<-data$VS353
PURCHASE_IDTHFT_PROT<-data$VS355

#data breach variables
notify_breach<-data$VS358

#combine individual vectors into dataset 
its<-cbind(data.frame(sex,race,hispanic,income, age,
                      pastyearbankacct, existing_bank, currentccacct, 
                      pastyearccacct, existing_credit_card, 
                      other_existing_accts, open_new_acct, personal_info,
                      OUTSIDE_PAST_YEAR, CHCKD_CR_PAST_YR, CHNG_PASSWORDS, 
                      PURCHASE_IDTHFT_INS, SHRED_DOCS,VERIFY_CHARGES, 
                      PROTECT_COMPUTER, PURCHASE_IDTHFT_PROT, 
                      notify_breach))

#remove individual vector variables 
rm(sex,race,hispanic,income,age,
     pastyearbankacct, existing_bank, currentccacct, pastyearccacct, 
   existing_credit_card, other_existing_accts, open_new_acct,personal_info,
   OUTSIDE_PAST_YEAR,  CHCKD_CR_PAST_YR, CHNG_PASSWORDS, 
   PURCHASE_IDTHFT_INS, SHRED_DOCS, 
   VERIFY_CHARGES, PROTECT_COMPUTER, PURCHASE_IDTHFT_PROT, 
   notify_breach)

#look at dataset
summary(its)
str(its)

#recode variables
#outcome variable 
#past year identity theft
its$idtheft<-"No past year ID theft"
its$idtheft[its$existing_bank=="(01) Yes"| its$existing_credit_card=="(01) Yes"| 
              its$other_existing_accts=="(01) Yes"| its$personal_info== "(01) Yes" |
              its$open_new_acct=="(01) Yes"]<-"Past year ID theft"
its$idtheft<-as.factor(its$idtheft)
table(its$idtheft, its$existing_bank)
table(its$idtheft, its$existing_credit_card)
table(its$idtheft, its$other_existing_accts)
table(its$idtheft, its$personal_info)
table(its$idtheft, its$open_new_acct)
#look at levels
levels(its$idtheft)

#predictor variables
#gender
its$sexr<-recode_factor(its$sex, '(1) Male'='Male', '(2) Female'='Female',
                        '(8) Residue'='Unknown')
table(its$sex,its$sexr)
#look at levels
levels(its$sexr)
#remove unused level
its$sexr<-droplevels(its$sexr)
levels(its$sexr)

#race
its$racer<-recode_factor(its$race, '(01) White only'='White',
      "(02) Black only"="Black",
      "(03) Am Ind/AK native only"='Other',
      "(04) Asian only"="Other",
      "(05) Hawaiian/Pacific IS only"="Other",
      "(06) White-Black"="2 or more races",
      "(07) White-Amer Ind"="2 or more races", 
      "(08) White-Asian"="2 or more races",
      "(09) White-Hawaiian"="2 or more races",
      "(10) Black-Amer Ind"="2 or more races",
      "(11) Black-Asian"="2 or more races",
      "(12) Black-Hawaiian/Pacific Ils"="2 or more races",
      "(13) American Indian-Asian"="Other",
      "(14) Asian-Hawaiian/Pacific Ils"="Other",
      "(15) White-Black-American Ind"="2 or more races",
      "(16) White-Black-Asian"="2 or more races",
      "(17) White-Amer Ind-Asian"="2 or more races",
      "(18) White-Asian-Hawaiian"="2 or more races",
      "(19) 2 or 3 races" ="2 or more races",
      "(20) 4 or 5 races"= "2 or more races",
      '(98) Residue' = 'Unknown')           
table(its$race,its$racer)

#Hispanic origin
its$hispanicr<-recode_factor(its$hispanic, "(1) Yes"="Hispanic",  
                             "(2) No"="NonHispanic","(8) Residue"="Unknown")
table(its$hispanic,its$hispanicr)

#race & Hispanic origin
its$ethnicr[its$racer=="White" & its$hispanicr=="NonHispanic"]<-"White"
its$ethnicr[its$racer=="Black" & its$hispanicr=="NonHispanic"]<-"Black"
its$ethnicr[its$hispanicr=="Hispanic"]<-"Hispanic"
its$ethnicr[its$racer=="Other" & its$hispanicr=="NonHispanic"]<-"Other race"
its$ethnicr[its$racer=="2 or more races" & its$hispanicr=="NonHispanic"]<-"2 or more races"
its$ethnicr<-as.factor(its$ethnicr)
table(its$hispanicr,its$racer)
table(its$ethnicr,its$hispanicr)
table(its$ethnicr,its$racer)
#look at levels
levels(its$ethnicr)
#reorder levels
its$ethnicr<-factor(its$ethnicr, levels = c("White","Black","Hispanic",
                                "Other race", "2 or more races"))
levels(its$ethnicr)

#age
its$ager[its$age>=16 & its$age<=17]<-"Age 16 to 17"
its$ager[its$age>=18 & its$age<=24]<-"Age 18 to 24"
its$ager[its$age>=25 & its$age<=34]<-"Age 25 to 34"
its$ager[its$age>=35 & its$age<=49]<-"Age 35 to 49"
its$ager[its$age>=50 & its$age<=64]<-"Age 50 to 64"
its$ager[its$age>=65]<-"Age 65 or older"
its$ager<-as.factor(its$ager)
table(its$age,its$ager)
#look at levels
levels(its$ager)

#income
its$incomer<-recode_factor(its$income,  "(01) Less than $5,000"="$24,999 or less", 
    "(02) $5,000 to $7,499"="$24,999 or less",
    "(03) $7,500 to $9,999"="$24,999 or less",
    "(04) $10,000 to $12,499"="$24,999 or less",
    "(05) $12,500 to $14,999"="$24,999 or less",
    "(06) $15,000 to $17,499"="$24,999 or less",
    "(07) $17,500 to $19,999"="$24,999 or less",
    "(08) $20,000 to $24,999"="$24,999 or less",
    "(09) $25,000 to $29,999"="$25,000–$49,999",
    "(10) $30,000 to $34,999"="$25,000–$49,999", 
    "(11) $35,000 to $39,999"="$25,000–$49,999",
    "(12) $40,000 to $49,999"="$25,000–$49,999",
    "(13) $50,000 to $74,999"="$50,000–$74,999",
    "(14) $75,000 and over"="$75,000 or more")
table(its$income,its$incomer)
#look at levels
levels(its$incomer)

#prior to past year ID theft 
its$OUTSIDE_PAST_YEARR<-recode_factor(its$OUTSIDE_PAST_YEAR,
                  "(02) No"="No prior to past year ID theft",
                  "(01) Yes" ="Prior to past year ID theft",
                  "(08) Residue"="Unknown",
                  "(98) Refused"="Unknown",
                  "(99) Don't know"="Unknown")
table(its$OUTSIDE_PAST_YEARR,its$OUTSIDE_PAST_YEAR)
#look at levels
levels(its$OUTSIDE_PAST_YEARR)
#reorder levels
its$OUTSIDE_PAST_YEARR<-factor(its$OUTSIDE_PAST_YEARR,
                                levels = c("Unknown","No prior to past year ID theft",
                                           "Prior to past year ID theft")) 
levels(its$OUTSIDE_PAST_YEARR)

#preventative behavior variable
its$prevent_total<-"Unknown"
its$prevent_total[its$CHCKD_CR_PAST_YR=="(01) Yes"|its$CHNG_PASSWORDS=="(01) Yes"|
                    its$PURCHASE_IDTHFT_INS=="(01) Yes"|its$SHRED_DOCS=="(01) Yes"|
                    its$VERIFY_CHARGES=="(01) Yes"| its$PROTECT_COMPUTER=="(01) Yes"|
                    its$PURCHASE_IDTHFT_PROT=="(01) Yes"]<-"At least one preventative behavior"
its$prevent_total[its$CHCKD_CR_PAST_YR=="(02) No" & its$CHNG_PASSWORDS=="(02) No"&
                    its$PURCHASE_IDTHFT_INS=="(02) No" & its$SHRED_DOCS=="(02) No"&
                    its$VERIFY_CHARGES=="(02) No" & its$PROTECT_COMPUTER=="(02) No"&
                    its$PURCHASE_IDTHFT_PROT=="(02) No"]<-"No preventative behaviors"
its$prevent_total<-as.factor(its$prevent_total)
table(its$prevent_total,its$CHCKD_CR_PAST_YR)
table(its$prevent_total,its$CHNG_PASSWORDS)
table(its$prevent_total,its$PURCHASE_IDTHFT_INS)
table(its$prevent_total,its$SHRED_DOCS)
table(its$prevent_total,its$VERIFY_CHARGES)
table(its$prevent_total,its$PROTECT_COMPUTER)
table(its$prevent_total,its$PURCHASE_IDTHFT_PROT)
table(its$prevent_total)
#look at levels
levels(its$prevent_total)
#reorder levels
its$prevent_total<-factor(its$prevent_total,levels = c("Unknown",
                                                       "No preventative behaviors",
                                                       "At least one preventative behavior"))
levels(its$prevent_total)

#data breach variable
its$notify_breachr<-recode_factor(its$notify_breach,
                                  '(01) Yes'='Data breach victim',
                                  '(02) No'='Not a data breach victim',
                                  "(08) Residue"="Unknown",
                                  "(98) Refused"="Unknown",
                                  "(99) Don't know"="Unknown")
table(its$notify_breach,its$notify_breachr)
#look at levels
levels(its$notify_breachr)
#reorder levels
its$notify_breachr<-factor(its$notify_breachr,levels=c('Unknown','Not a data breach victim',
                                                       'Data breach victim'))
levels(its$notify_breachr)

#exploratory data analysis
#bar charts
#Outcome variable
#Past year identity theft - Eleven percent (11%) of the sample reported at least one 
#type of identity theft (misuse of an existing account, misuse of personal information to 
#open new account or misuse of personal information for other fraudulent
#purposes) in the past year while 89% of the sample reported no identity theft.

x<-cbind(Count=table(its$idtheft),Percentage=round(prop.table(table(its$idtheft))*100))
rbind(x,Total = c(nrow(its),100))
      
ggplot(data = its, aes(x = idtheft, y=..prop.., group=1)) + 
  geom_bar(stat = "count", fill="lightgreen") +
  stat_count(geom = "text", colour = "black", size = 3.5,
             aes(label = paste0(round(..prop..*100,2),"%")),
             position=position_stack(vjust=0.5)) +
  ggtitle("Identity Theft in the Past 12 Months")+
  xlab("Past Year ID theft") + theme_clean()+
  ylab("Percent") + scale_y_continuous(labels=scales:: percent_format())

#Predictors- 
#Annual Household Income-One in five (21%) respondents were in households
#with annual incomes of $24,999 or less. Another fourth (26%) of the sample
#were in household with annual incomes of $25,000 to $49,999. Eighteen percent
#(18%) of the sample were in households with annual incomes of $50,000 to $74,999.
#The reminder of the sample (35%) were in household with annual incomes of
#at least $75,000.  Identity theft affected a higher percentage of those in households
#with higher annual incomes. Fifteen percent (15%) of persons in households
#with annual incomes of at least $75,000 reported experiencing identity theft
#in the past 12 months compared to 6.5% of those in household with annual incomes
#of $24,999 or less.

x<-cbind(Count=table(its$incomer),Percentage=round(prop.table(table(its$incomer))*100))
rbind(x,Total = c(nrow(its),100))

ggplot(its, aes(x=incomer, fill=idtheft))+
  geom_bar(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..]), position="stack") +
  geom_text(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..], 
                 label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..]) ),
            stat="count", position=position_dodge(0), vjust=1.5)+
  ylab('Percent') + xlab('Annual Household Income')+
  ggtitle("Annual Household Income by Past Year Identity Theft") +
  scale_fill_manual(values = c("blue", "lightblue"))+
  scale_y_continuous(labels = scales::percent)+
  theme_clean()+labs(fill = "Past Year Identity Theft")

#Race/Hispanic origin-Seventy-one percent (71%) of the sample were White while
#one in ten (10%) respondents were Black. Hispanics
#accounted for 13% of respondents. Persons who were of another
#race accounted for 5% of the sample. Persons of 2 or more races 
#accounted for 1% of the sample. Identity theft appeared to
#account for a larger proportion of Whites (12%) and persons
#of 2 or more races (14%) than Blacks (8%), Hispanics (7%) and
#persons of other races (9%).

x<-cbind(Count=table(its$ethnicr),Percentage=round(prop.table(table(its$ethnicr))*100))
rbind(x,Total = c(nrow(its),100))

ggplot(its, aes(x=ethnicr, fill=idtheft))+
  geom_bar(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..]), position="stack") +
  geom_text(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..], 
                 label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..]) ),
            stat="count", position=position_dodge(0), vjust=1.5)+
  ylab('Percent') + xlab('Race/Hispanic origin')+
  ggtitle("Race/Hispanic Origin by Past Year Identity Theft") +
  scale_fill_manual(values = c("purple", "violet"))+
  scale_y_continuous(labels = scales::percent)+
  theme_clean()+labs(fill = "Past Year Identity Theft")

#Age-Twenty-eight(28%) of the sample was age 50 to 64 while nearly one in four (24%) were
#age 35 to 49. 23% of the sample was age 65 or older. The remainder of the
#sample was under the age of 35.Thirteen percent (13%) of persons age 35 to 49 reported
#past year identity theft, compared to 7% of persons age 18 to 34 and 9% of persons age 65
#or older.

x<-cbind(Count=table(its$ager),Percentage=round(prop.table(table(its$ager))*100))
rbind(x,Total = c(nrow(its),100))

ggplot(its, aes(x=ager, fill=idtheft))+
  geom_bar(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..]), position="stack") +
  geom_text(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..], 
                 label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..]) ),
            stat="count", position=position_dodge(0), vjust=1.5)+
  ylab('Percent') + xlab('Age')+ggtitle("Age by Past Year Identity Theft") +
  scale_fill_manual(values = c("darkred", "pink"))+
  scale_y_continuous(labels = scales::percent)+
  theme_clean()+labs(fill = "Past Year Identity Theft")


#Gender-More than half of the sample (53%) was female while the
#remainder (47%) was male. Past year identity theft was experienced by
#11% of males and a similar percentage of females.

x<-cbind(Count=table(its$sexr),Percentage=round(prop.table(table(its$sexr))*100))
rbind(x,Total = c(nrow(its),100))

ggplot(its, aes(x=sexr, fill=idtheft))+
  geom_bar(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..]), position="stack") +
  geom_text(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..], 
                 label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..]) ),
            stat="count", position=position_dodge(0), vjust=1.5)+
  ylab('Percent') + xlab('Gender')+ggtitle("Gender by Past Year Identity Theft") +
  scale_fill_manual(values = c("darkorange", "yellow"))+
  scale_y_continuous(labels = scales::percent)+
  theme_clean()+labs(fill = "Past Year Identity Theft")

#Use of Preventative Behaviors-Nearly nine out of ten respondents
#(88%) used at least one of the preventative behaviors measured (checked bank
#or credit card statements, shredded or destroyed documents with financial
#information, checked credit report, changed passwords on financial accounts,
#used identity-theft security program on computer, purchased identity theft 
#insurance or credit monitoring service, purchased identity-theft protection)
#in the past 12 months.Eighteen percent of persons who did not know if they 
#had used a preventative behavior in the past 12 months reported past year 
#identity theft compared to 12% of those who had used at least 1 
#preventative behavior in the past 12 months. Unusually, 2% of those
#who reported no preventative behaviors reported identity theft in the past
#year.

x<-cbind(Count=table(its$prevent_total),Percentage=round(prop.table(table(its$prevent_total))*100))
rbind(x,Total = c(nrow(its),100))

ggplot(its, aes(x=prevent_total, fill=idtheft))+
  geom_bar(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..]), position="stack") +
  geom_text(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..], 
                 label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..]) ),
            stat="count", position=position_dodge(0), vjust=1.5)+
  ylab('Percent') + xlab('Use of Preventative Behaviors')+
  ggtitle("Use of At Least One Preventative Behavior in the Past Year by Past Year Identity Theft") +
  scale_fill_manual(values = c("green", "lightgreen"))+
  scale_y_continuous(labels = scales::percent)+
  theme_clean()+labs(fill = "Past Year Identity Theft")

#Identity theft prior to the past year- Thirteen percent (13%) of the sample experienced identity
#theft (misuse of an existing account, misuse of personal information to 
#create new account or misuse of personal information for other fraudulent
#purposes) prior to the 12 months prior to their ITS interview. 
#The majority of the sample (87%) did not experience it. About one in five
#(19%) respondents who experienced identity theft prior to the past year
#also experienced identity theft in the past 12 months. About 10% of those who
#did not have identity theft prior to the past 12 months experienced identity theft
#in the past 12 months.

x<-cbind(Count=table(its$OUTSIDE_PAST_YEARR),Percentage=round(prop.table(table(its$OUTSIDE_PAST_YEARR))*100))
rbind(x,Total = c(nrow(its),100))

ggplot(its, aes(x=OUTSIDE_PAST_YEARR, fill=idtheft))+
  geom_bar(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..]), position="stack") +
  geom_text(color = "white",aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..], 
                 label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..]) ),
            stat="count", position=position_dodge(0), vjust=1.5)+
  ylab('Percent') + xlab('ID theft prior to the past year')+
  ggtitle("Identity Theft Prior to the Past Year by Past Year Identity Theft") +
  scale_fill_manual(values = c("brown", "tan"))+
  scale_y_continuous(labels = scales::percent)+
  theme_clean()+labs(fill = "Past Year Identity Theft")
  
#Notified of exposure due to data breach- Eleven percent (11%) of the sample 
#reported that they were notified that their personal information was exposed 
#during a data breach. The majority of the sample (88%) reported that 
#they were not notified that their personal information was exposed 
#during a data breach. Of those who were notified that their
#information was exposed during a data breach, one in five (21%)
#reported being victims of identity theft in the past year. This is 
#compared to 9% of those who were not notified that their personal 
#information was exposed in a data breach being victims of
#past year identity theft.

x<-cbind(Count=table(its$notify_breachr),Percentage=round(prop.table(table(its$notify_breachr))*100))
rbind(x,Total = c(nrow(its),100))

ggplot(its, aes(x=notify_breachr, fill=idtheft))+
  geom_bar(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..]), position="stack") +
  geom_text(color = "white",aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..], 
                 label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..]) ),
            stat="count", position=position_dodge(0), vjust=1.5)+
  ylab('Percent') + xlab('Notified of data breach in the past year')+
  ggtitle("Notified of Personal Information Exposed in Data Breach in the Past Year by Past Year Identity Theft") +
  scale_fill_manual(values = c("black", "darkgray"))+
  scale_y_continuous(labels = scales::percent)+
  theme_clean()+labs(fill = "Past Year Identity Theft")

#remove vector used to make tables
rm(x)

#creating datasets without missing data
#separate out variables to be used
idtheft<-its$idtheft
incomer<-its$incomer
ager<-its$ager
ethnicr<-its$ethnicr
prevent_total<-its$prevent_total
OUTSIDE_PAST_YEARR<-its$OUTSIDE_PAST_YEARR
notify_breachr<-its$notify_breachr   
sexr<-its$sexr
#change unknown category to NA in variables with unknown category
levels(prevent_total)
levels(prevent_total)[levels(prevent_total)== "Unknown"]<-NA
levels(prevent_total)
levels(OUTSIDE_PAST_YEARR)
levels(OUTSIDE_PAST_YEARR)[levels(OUTSIDE_PAST_YEARR)== "Unknown"]<-NA
levels(OUTSIDE_PAST_YEARR)
levels(notify_breachr)
levels(notify_breachr)[levels(notify_breachr)== "Unknown"]<-NA
levels(notify_breachr)

#combine individual variables into a dataset  
its_clean<-cbind(data.frame(idtheft,incomer,ager,ethnicr,prevent_total,OUTSIDE_PAST_YEARR,notify_breachr,sexr))

#get number of incomplete & complete cases (no NAs)-
#About 1% of the sample was an incomplete case (having a NA value 
#on at least 1 variable). Removing incomplete cases left
#95,516 cases in the dataset.
incompletecases<-sum(!complete.cases(its_clean))
completecases<-sum(complete.cases(its_clean))
natable<-cbind(Number=c(incompletecases,completecases),Percent=round(prop.table(c(incompletecases,completecases))*100))
row.names(natable)<-c('Incomplete cases','Complete cases')
rbind(natable,Total=c(nrow(its_clean),100))

#remove cases with NAs
its_clean<-its_clean[complete.cases(its_clean),]

#check cleaned dataset with only complete cases
summary(its_clean)

#remove vectors used to create table and data with complete cases 
rm(incompletecases,completecases,natable, 
   ager,ethnicr,idtheft,incomer,notify_breachr,OUTSIDE_PAST_YEARR,
   prevent_total,sexr)

#correlation
#create separate dataset with all numeric data
its_clean_num<-its_clean
#make variables numeric and start at zero
table(its_clean_num$sexr)
its_clean_num$sexr<-as.numeric(its_clean_num$sexr)-1
table(its_clean_num$sexr)

table(its_clean_num$ethnicr)
its_clean_num$ethnicr<-as.numeric(its_clean_num$ethnicr)-1
table(its_clean_num$ethnicr)

table(its_clean_num$ager)
its_clean_num$ager<-as.numeric(its_clean_num$ager)-1
table(its_clean_num$ager)

table(its_clean_num$incomer)
its_clean_num$incomer<-as.numeric(its_clean_num$incomer)-1
table(its_clean_num$incomer)

table(its_clean_num$idtheft)
its_clean_num$idtheft<-as.numeric(its_clean_num$idtheft)-1
table(its_clean_num$idtheft)

table(its_clean_num$OUTSIDE_PAST_YEARR)
its_clean_num$OUTSIDE_PAST_YEARR<-as.numeric(its_clean_num$OUTSIDE_PAST_YEARR)-1
table(its_clean_num$OUTSIDE_PAST_YEARR)

table(its_clean_num$prevent_total)
its_clean_num$prevent_total<-as.numeric(its_clean_num$prevent_total)-1
table(its_clean_num$prevent_total)

table(its_clean_num$notify_breachr)
its_clean_num$notify_breachr<-as.numeric(its_clean_num$notify_breachr)-1
table(its_clean_num$notify_breachr)

#correlations-Looking at Pearson correlations of the variables,
#the predictors appear to be relatively independent of 
#each other with no moderate or strong correlations in the dataset.

#create correlation matrix
its_clean_cor<-round(cor(its_clean_num),2)
#create correlation heatmap
melted_its_clean_cor<-melt(its_clean_cor)
head(melted_its_clean_cor)
#replace values with full names of variables
melted_its_clean_cor$Var1<-recode_factor(melted_its_clean_cor$Var1,
                                         "idtheft"="Past year ID theft",
                                         "incomer"="Annual household income",
                                         "ager"="Age",
                                         "ethnicr"="Race/Hispanic origin",
                                         "prevent_total"="Preventative behaviors",
                                         "OUTSIDE_PAST_YEARR"="ID theft prior to past year",
                                         "notify_breachr"="Data breach victim",
                                         "sexr"="Sex")

melted_its_clean_cor$Var2<-recode_factor(melted_its_clean_cor$Var2,
                                         "idtheft"="Past year ID theft",
                                         "incomer"="Annual household income",
                                         "ager"="Age",
                                         "ethnicr"="Race/Hispanic origin",
                                         "prevent_total"="Preventative behaviors",
                                         "OUTSIDE_PAST_YEARR"="ID theft prior to past year",
                                         "notify_breachr"="Data breach victim",
                                         "sexr"="Sex")

ggplot(data = melted_its_clean_cor, aes(x=Var1, y=Var2, fill=value)) + 
  ggtitle("Correlation Matrix of Variables")+geom_tile()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
  legend.title = element_blank(),axis.title=element_blank())+
  geom_text(aes(label=value), color="white")

#remove items for correlation
rm(its_clean_cor,melted_its_clean_cor)

#Chi-square
# Multiple chi-square analyses were run on the dataset with only completed cases. 
#They show that  between past year identity theft was dependent on most of the predictors
#(p < 0.05) with the exception of sex (p > 0.05).
chisq.test(its_clean$idtheft,its_clean$incomer)
chisq.test(its_clean$idtheft,its_clean$ager)
chisq.test(its_clean$idtheft,its_clean$sexr)
chisq.test(its_clean$idtheft,its_clean$ethnicr)
chisq.test(its_clean$idtheft,its_clean$prevent_total)
chisq.test(its_clean$idtheft,its_clean$OUTSIDE_PAST_YEARR)
chisq.test(its_clean$idtheft,its_clean$notify_breachr)

#Machine learning with Logistic regression
#creating train and test data
set.seed(1234)
sampleSplit<-sample.split(its_clean$idtheft,SplitRatio=0.7)
#train dataset-70% of cases
trainSet<-its_clean[sampleSplit==TRUE,]
#test dataset-30% of cases
testSet<-its_clean[sampleSplit==FALSE,]

#train model-no gender since it wasn't correlated with past year ID theft
model<-glm(idtheft~incomer+ager+ethnicr+OUTSIDE_PAST_YEARR+prevent_total+notify_breachr
             ,data=trainSet,family="binomial")
#look at model
summary(model)

#predict the probability of identity theft in test data
model.probs<-predict(model,testSet,type = 'response')

#create a vector of class predictions based on whether 
#the predicted probability of being a victim of identity theft
#is greater than or less than 0.11. 
#(using 0.11 cause 11% of persons reported ID theft)
model.pred=rep ("No past year ID theft",28654)
model.pred[model.probs >.11]="Past Year ID theft"

#predicting in test data
probabs<-predict(model,testSet,type='response')
#use .11 as cutoff since 11% of data reported ID theft
preds<-ifelse(probabs>0.11,1,0)
confusionMatrix(factor(preds),factor(as.numeric(testSet$idtheft)-1))

#since preventative behaviors was giving contrary results in the EDA &
#logistic regression model, another model was run without it
#creating train and test data
set.seed(123)
sampleSplit<-sample.split(its_clean$idtheft,SplitRatio=0.7)
#train dataset-70% of cases
trainSet<-its_clean[sampleSplit==TRUE,]
#test dataset-30% of cases
testSet<-its_clean[sampleSplit==FALSE,]

#train model-no gender or preventative behaviors
model<-glm(idtheft~incomer+ager+ethnicr+OUTSIDE_PAST_YEARR+notify_breachr
           ,data=trainSet,family="binomial")
#look at model
summary(model)

#odds ratios for model
exp(model$coeff)

#predict the probability of identity theft in test data
model.probs<-predict(model,testSet,type = 'response')

#create a vector of class predictions based on whether 
#the predicted probability of being a victim of identity theft
#is greater than or less than 0.11. 
#(using 0.11 cause 11% of persons reported ID theft)

#predicting in test data
probabs<-predict(model,testSet,type='response')
#use .11 as cutoff since 11% of data reported ID theft
preds<-ifelse(probabs>0.11,1,0)
confusionMatrix(factor(preds),factor(as.numeric(testSet$idtheft)-1))

#ROC curve
myroc<-roc(testSet$idtheft,probabs)
#the area under the ROC curve is 67% which indicates a poor model.
auc(myroc)
ggroc(myroc, color="black",linetype=2, size=1) + ggtitle("ROC curve")

