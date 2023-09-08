## start of document ##


# setting up 

setwd("C:/Users/janeb/Box/FYP/Aleks_data/Aleks_data")

library(tidyverse)
library(foreign)
library(stargazer)
library(ggcorrplot)
# install.packages("glmnet")
library(glmnet)
library(plm)
library(lmtest)

# importing, collating, and cleaning datasets used

# 3 files required:
# study1toanalyze.dta
# study2final.dta
# study308052013merged.dta



# study 1
# 9 rows per person, 64 participants, Rice sample

study1 <- read.dta("study1toanalyze.dta")

study1_9rowpp <- subset(study1, blocknum == 2:10)

nrow(study1_9rowpp)
576/9
colnames(study1_9rowpp)
table(study1_9rowpp$ideouncertain)
rm(study1)

# this code is to be run after all of the data cleaning, 
# so that the 9 row pp data is also updated with data cleaning changes made below.
# This way, no further data cleaning will be required for the 9 row pp analysis.  
# Study 1, 1 row pp, 64 participants
# study1_1rowpp <- study1_9rowpp[!duplicated(study1_9rowpp$subject), ]
# nrow(study1_1rowpp)





# Study 2 - MTurk, 46 people

# 1 row pp, 46 people
study2_1rowpp <- read.dta("study2final.dta")
nrow(study2_1rowpp)



# Study 3 - MTurk, 17 people
# 1 row pp

study3_1rowpp <- read.dta("study308052013merged.dta")
nrow(study3_1rowpp)

table(study3_1rowpp$age)



# all studies have female, race_white, ideouncertain, pid

# study 3 need to make "age"
study3_1rowpp$age <- study3_1rowpp$age_response
table(study3_1rowpp$age)
table(study3_1rowpp$age_response)



# selfideology to 100 in 1 and 3, to 103 in 2 - standardize to 1-100 
# and convert to self-ideology extremity (by -50 and retain absolute value)
# now all range 1-50, /50 to get a variable ranging 0-1

table(study1_9rowpp$selfideology)
table(study2_1rowpp$selfideology)
table(study3_1rowpp$selfideology)

study1_9rowpp$selfidext <- abs(study1_9rowpp$selfideology - 50)/50
table(study1_9rowpp$selfidext)

sum(table(study1_9rowpp$selfideology))
sum(table(study1_9rowpp$selfidext))

study3_1rowpp$selfidext <- abs(study3_1rowpp$selfideology - 50)/50
table(study3_1rowpp$selfidext)

study2_1rowpp$selfidext <- abs((study2_1rowpp$selfideology/103*100) - 50)/50
table(study2_1rowpp$selfidext)

# add unique identifiers to each row - will be necessary for later analysis with 9 rows pp as 2 subject numbers 
# appear in 2 studies and want to be able to distinguish them

study1_9rowpp$akstudy<- paste("Study1")
study2_1rowpp$akstudy <- paste("Study2")
study3_1rowpp$akstudy <- paste("Study3")

study1_9rowpp$uniquesubject<- paste(study1_9rowpp$akstudy, study1_9rowpp$subject)
study2_1rowpp$uniquesubject <- paste(study2_1rowpp$akstudy, study2_1rowpp$subject)
study3_1rowpp$uniquesubject <- paste(study3_1rowpp$akstudy, study3_1rowpp$subject)



# check I have all the variables I need

table(study1_9rowpp$age)
sum(table(study1_9rowpp$age))
table(study2_1rowpp$age)
sum(table(study2_1rowpp$age))
table(study3_1rowpp$age)
sum(table(study3_1rowpp$age))

table(study1_9rowpp$female)
sum(table(study1_9rowpp$female))
table(study2_1rowpp$female)
sum(table(study2_1rowpp$female))
table(study3_1rowpp$female)
sum(table(study3_1rowpp$female))

table(study1_9rowpp$ideouncertain)
sum(table(study1_9rowpp$ideouncertain))
table(study2_1rowpp$ideouncertain)
sum(table(study2_1rowpp$ideouncertain))
table(study3_1rowpp$ideouncertain)
sum(table(study3_1rowpp$ideouncertain))

table(study1_9rowpp$race_white)
sum(table(study1_9rowpp$race_white))
table(study2_1rowpp$race_white)
sum(table(study2_1rowpp$race_white))
table(study3_1rowpp$race_white)
sum(table(study3_1rowpp$race_white))

table(study1_9rowpp$selfidext)
sum(table(study1_9rowpp$selfidext))
table(study2_1rowpp$selfidext)
sum(table(study2_1rowpp$selfidext))
table(study3_1rowpp$selfidext)
sum(table(study3_1rowpp$selfidext))


# pid - take absolute value to convert to measure of extremity (i.e. distance from center (0))
# then /3 to scale to 0-1
table(study1_9rowpp$pid)
study1_9rowpp$pidscaled <- abs(study1_9rowpp$pid)/3
table(study1_9rowpp$pidscaled)
sum(table(study1_9rowpp$pidscaled))

table(study2_1rowpp$pid)
study2_1rowpp$pidscaled <- abs(study2_1rowpp$pid)/3
table(study2_1rowpp$pidscaled)
sum(table(study2_1rowpp$pidscaled))


table(study3_1rowpp$pid)
study3_1rowpp$pidscaled <- abs(study3_1rowpp$pid)/3
table(study3_1rowpp$pidscaled)
sum(table(study1_9rowpp$pidscaled))



# make attitude variables indexing across all 9 issues, scaled to 0-1

# study 1

study1_9rowpp <- study1_9rowpp %>%
  mutate(allissuescaresum = rowSums(select(., c(issue1_care, issue2_care, issue3_care, 
                                                issue4_care, issue5_care, issue6_care, 
                                                issue7_care, issue8_care, issue9_care))))

study1_9rowpp$allissuescare <- study1_9rowpp$allissuescaresum / 900

table(study1_9rowpp$allissuescaresum)
table(study1_9rowpp$allissuescare)
sum(table(study1_9rowpp$allissuescare))


study1_9rowpp <- study1_9rowpp %>%
  mutate(allissuesfeelsum = rowSums(select(., c(issue1_feel, issue2_feel, issue3_feel, 
                                                issue4_feel, issue5_feel, issue6_feel, 
                                                issue7_feel, issue8_feel, issue9_feel))))

study1_9rowpp$allissuesfeel<- study1_9rowpp$allissuesfeelsum / 900

table(study1_9rowpp$allissuesfeelsum)
table(study1_9rowpp$allissuesfeel)
sum(table(study1_9rowpp$allissuesfeel))

study1_9rowpp <- study1_9rowpp %>%
  mutate(allissuesthinksum = rowSums(select(., c(issue1_think, issue2_think, issue3_think, 
                                                issue4_think, issue5_think, issue6_think, 
                                                issue7_think, issue8_think, issue9_think))))

study1_9rowpp$allissuesthink<- study1_9rowpp$allissuesthinksum / 900

table(study1_9rowpp$allissuesthinksum)
table(study1_9rowpp$allissuesthink)
sum(table(study1_9rowpp$allissuesthink))



study1_9rowpp <- study1_9rowpp %>%
  mutate(allissuescertainsum = rowSums(select(., c(issue1_certain, issue2_certain, issue3_certain, 
                                                 issue4_certain, issue5_certain, issue6_certain, 
                                                 issue7_certain, issue8_certain, issue9_certain))))

study1_9rowpp$allissuescertain<- study1_9rowpp$allissuescertainsum / 900

table(study1_9rowpp$allissuescertainsum)
table(study1_9rowpp$allissuescertain)
sum(table(study1_9rowpp$allissuescertain))

# study 2

study2_1rowpp <- study2_1rowpp %>%
  mutate(allissuescaresum = rowSums(select(., c(issue1_care, issue2_care, issue3_care, 
                                                issue4_care, issue5_care, issue6_care, 
                                                issue7_care, issue8_care, issue9_care))))

study2_1rowpp$allissuescare <- study2_1rowpp$allissuescaresum / 900

table(study2_1rowpp$allissuescaresum)
table(study2_1rowpp$allissuescare)
sum(table(study2_1rowpp$allissuescare))


study2_1rowpp <- study2_1rowpp %>%
  mutate(allissuesfeelsum = rowSums(select(., c(issue1_feel, issue2_feel, issue3_feel, 
                                                issue4_feel, issue5_feel, issue6_feel, 
                                                issue7_feel, issue8_feel, issue9_feel))))

study2_1rowpp$allissuesfeel<- study2_1rowpp$allissuesfeelsum / 900

table(study2_1rowpp$allissuesfeelsum)
table(study2_1rowpp$allissuesfeel)
sum(table(study2_1rowpp$allissuesfeel))

study2_1rowpp <- study2_1rowpp %>%
  mutate(allissuesthinksum = rowSums(select(., c(issue1_think, issue2_think, issue3_think, 
                                                 issue4_think, issue5_think, issue6_think, 
                                                 issue7_think, issue8_think, issue9_think))))

study2_1rowpp$allissuesthink<- study2_1rowpp$allissuesthinksum / 900

table(study2_1rowpp$allissuesthinksum)
table(study2_1rowpp$allissuesthink)
sum(table(study2_1rowpp$allissuesthink))



study2_1rowpp <- study2_1rowpp %>%
  mutate(allissuescertainsum = rowSums(select(., c(issue1_certain, issue2_certain, issue3_certain, 
                                                   issue4_certain, issue5_certain, issue6_certain, 
                                                   issue7_certain, issue8_certain, issue9_certain))))

study2_1rowpp$allissuescertain<- study2_1rowpp$allissuescertainsum / 900

table(study2_1rowpp$allissuescertainsum)
table(study2_1rowpp$allissuescertain)
sum(table(study2_1rowpp$allissuescertain))




# study 3

study3_1rowpp <- study3_1rowpp %>%
  mutate(allissuescaresum = rowSums(select(., c(issue1_care, issue2_care, issue3_care, 
                                                issue4_care, issue5_care, issue6_care, 
                                                issue7_care, issue8_care, issue9_care))))

study3_1rowpp$allissuescare <- study3_1rowpp$allissuescaresum / 900

table(study3_1rowpp$allissuescaresum)
table(study3_1rowpp$allissuescare)
sum(table(study3_1rowpp$allissuescare))


study3_1rowpp <- study3_1rowpp %>%
  mutate(allissuesfeelsum = rowSums(select(., c(issue1_feel, issue2_feel, issue3_feel, 
                                                issue4_feel, issue5_feel, issue6_feel, 
                                                issue7_feel, issue8_feel, issue9_feel))))

study3_1rowpp$allissuesfeel<- study3_1rowpp$allissuesfeelsum / 900

table(study3_1rowpp$allissuesfeelsum)
table(study3_1rowpp$allissuesfeel)
sum(table(study3_1rowpp$allissuesfeel))

study3_1rowpp <- study3_1rowpp %>%
  mutate(allissuesthinksum = rowSums(select(., c(issue1_think, issue2_think, issue3_think, 
                                                 issue4_think, issue5_think, issue6_think, 
                                                 issue7_think, issue8_think, issue9_think))))

study3_1rowpp$allissuesthink<- study3_1rowpp$allissuesthinksum / 900

table(study3_1rowpp$allissuesthinksum)
table(study3_1rowpp$allissuesthink)
sum(table(study3_1rowpp$allissuesthink))



study3_1rowpp <- study3_1rowpp %>%
  mutate(allissuescertainsum = rowSums(select(., c(issue1_certain, issue2_certain, issue3_certain, 
                                                   issue4_certain, issue5_certain, issue6_certain, 
                                                   issue7_certain, issue8_certain, issue9_certain))))

study3_1rowpp$allissuescertain<- study3_1rowpp$allissuescertainsum / 900

table(study3_1rowpp$allissuescertainsum)
table(study3_1rowpp$allissuescertain)
sum(table(study3_1rowpp$allissuescertain))


# finally, run the code to subset study 1 to 1 row pp
# Study 1, 1 row pp, 64 participants
study1_1rowpp <- study1_9rowpp[!duplicated(study1_9rowpp$subject), ]
nrow(study1_1rowpp)

table(study1_1rowpp$uniquesubject)
sum(table(study1_1rowpp$uniquesubject))

table(study1_9rowpp$uniquesubject)
sum(table(study1_9rowpp$uniquesubject))



# combine 1 row pp datasets

all1rowpp <- bind_rows(study1_1rowpp, study2_1rowpp, study3_1rowpp)
nrow(all1rowpp)
64+46+17

table(all1rowpp$uniquesubject)

# for ease of later data analysis, option to export 'all1rowpp' data object so just that can be imported next time
# write.csv(all1rowpp, "all1rowpp.csv")

# also option to export data for 9 row pp analysis
# write.csv(study1_1rowpp, "study1_1rowpp.csv")
# write.csv(study2_1rowpp, "study2_1rowpp.csv")
# write.csv(study3_1rowpp, "study3_1rowpp.csv")

### end of data collation and cleaning ####


## ------------------------------------------------------------------------------------------

## Descriptive sample stats

table(all1rowpp$race_white)
80/127*100
# 63% white and 37% non-white.

table(all1rowpp$age)
# age 25 or younger
10+13+8+15+8+1+5
60/127*100
# 47% of the sample is aged 25 or younger, with the oldest participants in their 60s

table(all1rowpp$female)
56/127*100
# The sample is 44% female, 56% male. 

table(all1rowpp$pid)
# Rs are -ve
12+9+12
33/127*100
# 26% Rs

# Ds +ve
27+26+23
76/127*100
# 60% Ds

# indeps = 0
18/127*100
# 14% of sample

# Democrats are over-represented in the sample (60%), 
# but with a reasonable number of Republicans (26%) and independents (14%) also represented.  


# look at bivariate relationships 

par(mfrow = c(2, 2))
plot(all1rowpp$pidscaled, all1rowpp$allissuescertain)
abline(lm(all1rowpp$pidscaled ~ all1rowpp$allissuescertain))

plot(all1rowpp$pidscaled, all1rowpp$allissuescare)
abline(lm(all1rowpp$pidscaled ~ all1rowpp$allissuescare))

plot(all1rowpp$pidscaled, all1rowpp$allissuesfeel)
abline(lm(all1rowpp$pidscaled ~ all1rowpp$allissuesfeel))

plot(all1rowpp$pidscaled, all1rowpp$allissuesthink)
abline(lm(all1rowpp$pidscaled ~ all1rowpp$allissuesthink))

# interestingly, certain has strongest slope, but it only starts at about 0.3, so the very 
# low strength partisans don't seem to be influenced by certainty, which is in keeping 
# with what I'd expected based on Converse's packages theory - i.e. you have to be some
# amount partisan to receive the packages and assimilate them.  

summary(lm(all1rowpp$pidscaled ~ all1rowpp$allissuescertain))
summary(lm(all1rowpp$pidscaled ~ all1rowpp$allissuescare))
summary(lm(all1rowpp$pidscaled ~ all1rowpp$allissuesfeel))
summary(lm(all1rowpp$pidscaled ~ all1rowpp$allissuesthink))


certbiv <- lm(all1rowpp$pidscaled ~ all1rowpp$allissuescertain)
carebiv <- lm(all1rowpp$pidscaled ~ all1rowpp$allissuescare)
feelbiv <- lm(all1rowpp$pidscaled ~ all1rowpp$allissuesfeel)
thinkbiv <- lm(all1rowpp$pidscaled ~ all1rowpp$allissuesthink)

stargazer (certbiv, carebiv, feelbiv, thinkbiv, type = "html", out = "bivregs")



## ------------------------------------------------------------------------------------------

### Analysis of data, 1 row pp ###

# use object all1rowpp, either as above, or import from saved object last time
# setwd("C:/Users/janeb/Box/FYP/Aleks_data/Aleks_data")
# all1rowpp <- read.csv("all1rowpp.csv")


# first, some data checks

# heatmap of correlations

aaa <- subset(all1rowpp, select = c(allissuescare, allissuesfeel, allissuescertain, allissuesthink,
                                  ideouncertain, pidscaled, race_white, age, female))

colnames(aaa) <- c("Issues - Care", "Issues - Feel", "Issues - Certain", "Issues - Think",
                   "Uncertain Ideology", "Strength of Partisan ID",
                   "Race - White", "Age", "Female")



correlation_matrix <- round(cor(aaa, method = c("pearson"), 
                                use = "complete.obs"),2)
corrp.mat <- cor_pmat(aaa)

ggcorrplot(correlation_matrix, method = "square",
           type = "upper", lab = TRUE) +
  labs(title = "Pearson Correlation Matrix of Variables of Interest", 
       subtitle = "One Observation per Subject")


# tests for normality
shapiro.test(dataset$allissuescare) # yes normally distributed then...
shapiro.test(dataset$allissuesfeel) # yes normally distributed then...
shapiro.test(dataset$allissuescertain) # yes normally distributed then...
shapiro.test(dataset$allissuesthink) # yes normally distributed then...


# with each attitude measure only
pidmodcert <- lm(pidscaled ~ allissuescertain +
               ideouncertain + race_white + age + female, all1rowpp)
pidmodcare <- lm(pidscaled ~ allissuescare +
                 ideouncertain + race_white + age + female, all1rowpp)
pidmodfeel <- lm(pidscaled ~ allissuesfeel +
                 ideouncertain + race_white + age + female, all1rowpp)
pidmodthink <- lm(pidscaled ~ allissuesthink +
                 ideouncertain + race_white + age + female, all1rowpp)

stargazer(pidmodcert, pidmodcare, pidmodfeel, pidmodthink, type = "text")
# on their own, none is significant

# regression with all 4 variables
pidmodall <- lm(pidscaled ~ allissuescare + allissuesfeel + allissuescertain + allissuesthink +
               ideouncertain + race_white + age + female, all1rowpp)

stargazer(pidmodall, type = "text")
# together only suggestive evidence that certainty matters, but OLS is unreliable due to multicollinearity anyway...


# try a general attitude measure
all1rowpp <- all1rowpp %>%
  mutate(averageatt = rowSums(select(., c(allissuescare, allissuesfeel, allissuescertain, allissuesthink))))

all1rowpp$averageatt <- all1rowpp$averageatt / 4
sum(table(all1rowpp$averageatt))

pidmodav <- lm(pidscaled ~ averageatt +
               ideouncertain + race_white + age + female, all1rowpp)

stargazer(pidmodav, type = "text")
# so even an average of attitudes is not relating to partisan identity strength at the 1 row pp level


# export for paper
# stargazer(pidmodcert, pidmodcare, pidmodfeel, pidmodthink, type = "html", out = "1rowregs_sep")
# stargazer(pidmodav, pidmodall, type = "html", out = "1rowregs_combined")
stargazer(pidmodcert, pidmodcare, pidmodfeel, pidmodthink, type = "text")
stargazer(pidmodav, pidmodall, type = "text")

# I did think about matching...but I don't think this is helpful in this case as the dimensions are so 
# strongly correlated
lm1 <- lm(allissuescertain ~ allissuescare + allissuesfeel + allissuesthink,
          data = all1rowpp)
# stargazer(lm1, type = "html", out = "whycantmatch")
stargazer(lm1, type = "text")
# caring and certainty not related, but 100% feeling = 50% certain, and 100% thinking = 30% certain.
# also, since this is not even the correct data, I don't think it's going to add much to my analysis...


### End of analysis of data, 1 row pp ###

## ------------------------------------------------------------------------------------------

### Analysis of data, 9 row pp ###

# use object all1rowpp, either as above, or import from saved object last time
# setwd("C:/Users/janeb/Box/FYP/Aleks_data/Aleks_data")
# all1rowpp <- read.csv("all1rowpp.csv")

# make data long, with 1 row per participant per issue

datalong <-reshape(all1rowpp, direction='long', 
        varying=c('issue1_care', 'issue1_feel', 'issue1_certain', 'issue1_think', 
                  'issue2_care', 'issue2_feel', 'issue2_certain', 'issue2_think', 
                  'issue3_care', 'issue3_feel', 'issue3_certain', 'issue3_think', 
                  'issue4_care', 'issue4_feel', 'issue4_certain', 'issue4_think', 
                  'issue5_care', 'issue5_feel', 'issue5_certain', 'issue5_think', 
                  'issue6_care', 'issue6_feel', 'issue6_certain', 'issue6_think', 
                  'issue7_care', 'issue7_feel', 'issue7_certain', 'issue7_think', 
                  'issue8_care', 'issue8_feel', 'issue8_certain', 'issue8_think', 
                  'issue9_care', 'issue9_feel', 'issue9_certain', 'issue9_think'), 
        timevar='issnumjb',
        times=c('issue1', 'issue2', 'issue3', 'issue4', 'issue5', 'issue6', 'issue7', 'issue8', 'issue9'),
        v.names=c('care', 'feel', 'think', 'certain'),
        idvar= 'uniquesubject')

# rescale attitude dimensions, which are 1-100

table(datalong$certain)
datalong$certain <- datalong$certain / 100
datalong$care <- datalong$care / 100
datalong$feel <- datalong$feel / 100
datalong$think <- datalong$think / 100

# make average attitude variable
datalong$averageatt <- (datalong$care + datalong$feel + datalong$certain + datalong$think)/4

# can now export this for future use
# write.csv(datalong, "all9rowpp.csv")
# if importing back in for analysis:
# datalong <- read.csv("all9rowpp.csv)


# basic analysis

shapiro.test(datalong$certain) # NOT normally distributed...
shapiro.test(datalong$care) # NOT normally distributed...
shapiro.test(datalong$feel) # NOT normally distributed...
shapiro.test(datalong$think) # NOT normally distributed...
shapiro.test(datalong$averageatt) # NOT normally distributed



# check collinearity of 9 rows pp data, re: the 4 attitudinal variables esp

# heatmap of corrs

aaa <- subset(datalong, select = c(care, feel, certain, think,
                                  ideouncertain, pidscaled, race_white, age, female))

colnames(aaa) <- c("Issue - Care", "Issue - Feel", "Issue - Certain", "Issue - Think",
                   "Uncertain Ideology", "Strength of Partisan ID", "Race - White", "Age", "Female")



correlation_matrix <- round(cor(aaa, method = c("pearson"), 
                                use = "complete.obs"),2)
corrp.mat <- cor_pmat(aaa)

ggcorrplot(correlation_matrix, method = "square",
           type = "upper", lab = TRUE) +
  labs(title = "Pearson Correlation Matrix of Variables of Interest", 
       subtitle = "Nine Observations per Subject (One per Policy Issue)")


# subset data by issue
issue1data <- subset(datalong, datalong$issnumjb == "issue1")
issue2data <- subset(datalong, datalong$issnumjb == "issue2")
issue3data <- subset(datalong, datalong$issnumjb == "issue3")
issue4data <- subset(datalong, datalong$issnumjb == "issue4")
issue5data <- subset(datalong, datalong$issnumjb == "issue5")
issue6data <- subset(datalong, datalong$issnumjb == "issue6")
issue7data <- subset(datalong, datalong$issnumjb == "issue7")
issue8data <- subset(datalong, datalong$issnumjb == "issue8")
issue9data <- subset(datalong, datalong$issnumjb == "issue9")



# then the regressions but with only certainty in them...

pidmod1 <- lm(pidscaled ~ certain +
                ideouncertain + race_white + age + female, data = issue1data)

pidmod2 <- lm(pidscaled ~ certain +
                ideouncertain + race_white + age + female, data = issue2data)

pidmod3 <- lm(pidscaled ~ certain +
                ideouncertain + race_white + age + female, data = issue3data)

pidmod4 <- lm(pidscaled ~ certain +
                ideouncertain + race_white + age + female, data = issue4data)

pidmod5 <- lm(pidscaled ~ certain +
                ideouncertain + race_white + age + female, data = issue5data)

pidmod6 <- lm(pidscaled ~ certain +
                ideouncertain + race_white + age + female, data = issue6data)

pidmod7 <- lm(pidscaled ~ certain +
                ideouncertain + race_white + age + female, data = issue7data)

pidmod8 <- lm(pidscaled ~ certain +
                ideouncertain + race_white + age + female, data = issue8data)

pidmod9 <- lm(pidscaled ~ certain +
                ideouncertain + race_white + age + female, data = issue9data)


stargazer(pidmod1, pidmod2, pidmod3, type = "text")
stargazer(pidmod4, pidmod5, pidmod6, type = "text")
stargazer(pidmod7, pidmod8, pidmod9, type = "text")


stargazer(pidmod1, pidmod2, pidmod3, type = "html", out = "table_pidcert_iss13")
stargazer(pidmod4, pidmod5, pidmod6, type = "html", out = "table_pidcert_iss46")
stargazer(pidmod7, pidmod8, pidmod9, type = "html", out = "table_pidcert_iss79")



# then the regressions but with only care

pidmod1 <- lm(pidscaled ~ care +
                ideouncertain + race_white + age + female, data = issue1data)

pidmod2 <- lm(pidscaled ~ care +
                ideouncertain + race_white + age + female, data = issue2data)

pidmod3 <- lm(pidscaled ~ care +
                ideouncertain + race_white + age + female, data = issue3data)

pidmod4 <- lm(pidscaled ~ care +
                ideouncertain + race_white + age + female, data = issue4data)

pidmod5 <- lm(pidscaled ~ care +
                ideouncertain + race_white + age + female, data = issue5data)

pidmod6 <- lm(pidscaled ~ care +
                ideouncertain + race_white + age + female, data = issue6data)

pidmod7 <- lm(pidscaled ~ care +
                ideouncertain + race_white + age + female, data = issue7data)

pidmod8 <- lm(pidscaled ~ care +
                ideouncertain + race_white + age + female, data = issue8data)

pidmod9 <- lm(pidscaled ~ care +
                ideouncertain + race_white + age + female, data = issue9data)

stargazer(pidmod1, pidmod2, pidmod3, type = "text")
stargazer(pidmod4, pidmod5, pidmod6, type = "text")
stargazer(pidmod7, pidmod8, pidmod9, type = "text")


stargazer(pidmod1, pidmod2, pidmod3, type = "html", out = "table_pidcare_iss13")
stargazer(pidmod4, pidmod5, pidmod6, type = "html", out = "table_pidcare_iss46")
stargazer(pidmod7, pidmod8, pidmod9, type = "html", out = "table_pidcare_iss79")



# then the regressions but with only feel

pidmod1 <- lm(pidscaled ~ feel +
                ideouncertain + race_white + age + female, data = issue1data)

pidmod2 <- lm(pidscaled ~ feel +
                ideouncertain + race_white + age + female, data = issue2data)

pidmod3 <- lm(pidscaled ~ feel +
                ideouncertain + race_white + age + female, data = issue3data)

pidmod4 <- lm(pidscaled ~ feel +
                ideouncertain + race_white + age + female, data = issue4data)

pidmod5 <- lm(pidscaled ~ feel +
                ideouncertain + race_white + age + female, data = issue5data)

pidmod6 <- lm(pidscaled ~ feel +
                ideouncertain + race_white + age + female, data = issue6data)

pidmod7 <- lm(pidscaled ~ feel +
                ideouncertain + race_white + age + female, data = issue7data)

pidmod8 <- lm(pidscaled ~ feel +
                ideouncertain + race_white + age + female, data = issue8data)

pidmod9 <- lm(pidscaled ~ feel +
                ideouncertain + race_white + age + female, data = issue9data)

stargazer(pidmod1, pidmod2, pidmod3, type = "text")
stargazer(pidmod4, pidmod5, pidmod6, type = "text")
stargazer(pidmod7, pidmod8, pidmod9, type = "text")


stargazer(pidmod1, pidmod2, pidmod3, type = "html", out = "table_pidfeel_iss13")
stargazer(pidmod4, pidmod5, pidmod6, type = "html", out = "table_pidfeel_iss46")
stargazer(pidmod7, pidmod8, pidmod9, type = "html", out = "table_pidfeel_iss79")


# then the regressions but with only think

pidmod1 <- lm(pidscaled ~ think +
                ideouncertain + race_white + age + female, data = issue1data)

pidmod2 <- lm(pidscaled ~ think +
                ideouncertain + race_white + age + female, data = issue2data)

pidmod3 <- lm(pidscaled ~ think +
                ideouncertain + race_white + age + female, data = issue3data)

pidmod4 <- lm(pidscaled ~ think +
                ideouncertain + race_white + age + female, data = issue4data)

pidmod5 <- lm(pidscaled ~ think +
                ideouncertain + race_white + age + female, data = issue5data)

pidmod6 <- lm(pidscaled ~ think +
                ideouncertain + race_white + age + female, data = issue6data)

pidmod7 <- lm(pidscaled ~ think +
                ideouncertain + race_white + age + female, data = issue7data)

pidmod8 <- lm(pidscaled ~ think +
                ideouncertain + race_white + age + female, data = issue8data)

pidmod9 <- lm(pidscaled ~ think +
                ideouncertain + race_white + age + female, data = issue9data)

stargazer(pidmod1, pidmod2, pidmod3, type = "text")
stargazer(pidmod4, pidmod5, pidmod6, type = "text")
stargazer(pidmod7, pidmod8, pidmod9, type = "text")


stargazer(pidmod1, pidmod2, pidmod3, type = "html", out = "table_pidthink_iss13")
stargazer(pidmod4, pidmod5, pidmod6, type = "html", out = "table_pidthink_iss46")
stargazer(pidmod7, pidmod8, pidmod9, type = "html", out = "table_pidthink_iss79")


# then the regressions but with only average att

pidmod1 <- lm(pidscaled ~ averageatt +
                ideouncertain + race_white + age + female, data = issue1data)

pidmod2 <- lm(pidscaled ~ averageatt +
                ideouncertain + race_white + age + female, data = issue2data)

pidmod3 <- lm(pidscaled ~ averageatt +
                ideouncertain + race_white + age + female, data = issue3data)

pidmod4 <- lm(pidscaled ~ averageatt +
                ideouncertain + race_white + age + female, data = issue4data)

pidmod5 <- lm(pidscaled ~ averageatt +
                ideouncertain + race_white + age + female, data = issue5data)

pidmod6 <- lm(pidscaled ~ averageatt +
                ideouncertain + race_white + age + female, data = issue6data)

pidmod7 <- lm(pidscaled ~ averageatt +
                ideouncertain + race_white + age + female, data = issue7data)

pidmod8 <- lm(pidscaled ~ averageatt +
                ideouncertain + race_white + age + female, data = issue8data)

pidmod9 <- lm(pidscaled ~ averageatt +
                ideouncertain + race_white + age + female, data = issue9data)

stargazer(pidmod1, pidmod2, pidmod3, type = "text")
stargazer(pidmod4, pidmod5, pidmod6, type = "text")
stargazer(pidmod7, pidmod8, pidmod9, type = "text")


stargazer(pidmod1, pidmod2, pidmod3, type = "html", out = "table_pidavatt_iss13")
stargazer(pidmod4, pidmod5, pidmod6, type = "html", out = "table_pidavatt_iss46")
stargazer(pidmod7, pidmod8, pidmod9, type = "html", out = "table_pidavatt_iss79")


# pooled with clustered SEs, by attitude dimension

dataset <- pdata.frame(datalong, index=c("uniquesubject", "issnumjb"))

pooledpidcert <- plm(pidscaled ~ certain +
                   ideouncertain + race_white + age + female, data = dataset, model = "pooling")

summary(pooledpidcert)
# suggestive evidence for certainty in pooled model without SEs. 
coeftest(pooledpidcert, vcov = vcovHC(pooledpid, type = "HC2", cluster = "group"))
# no evidence supporting certainty when clustered SEs applied.




pooledpidcare <- plm(pidscaled ~ care +
                   ideouncertain + race_white + age + female, data = dataset, model = "pooling")

summary(pooledpidcare)
# no evidence supporting care




pooledpidfeel <- plm(pidscaled ~ feel +
                       ideouncertain + race_white + age + female, data = dataset, model = "pooling")

summary(pooledpidfeel)
# no evidence supporting feel


pooledpidthink <- plm(pidscaled ~ think +
                       ideouncertain + race_white + age + female, data = dataset, model = "pooling")

summary(pooledpidthink)
# no evidence supporting think



### End of analysis of data, 9 row pp ###


##
##

## End of document ###

