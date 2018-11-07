library(tidyverse)

neg <- read_csv("LIWC_neg.csv")
neut <- read_csv("LIWC_neut.csv")
pos <- read_csv("LIWC_pos.csv")

#install.packages("Stack")
library(Stack)
pos_neut <- Stack(pos, neut)
fin_combined <- Stack(pos_neut, neg)

fin_combined$`Source (B)`[fin_combined$`Source (B)`=="neutral"] <- "Neutral"
fin_combined$`Source (B)`[fin_combined$`Source (B)`=="Neutral?"] <- "Neutral"
fin_combined$`Source (B)`[fin_combined$`Source (B)`=="negative"] <- "Negative"
fin_combined$`Source (B)`[fin_combined$`Source (B)`=="positive"] <- "Positive"

fin_combined$`Source (B)`<-factor(fin_combined$`Source (B)`,
                                  levels = c("Neutral", "Negative", "Positive"))

Vax_opinion <- fin_combined$`Source (B)`
summary(fin_combined)

#First Logistic Regression Model:

#load in the nnet library to do multinomial regression
library(nnet)
library(arm)

#Vax_opinion is outcome variable of interest
table(Vax_opinion)

#Show all variables in LIWC data set:
ls(fin_combined)

#A random selection of LIWC variables (centered continuous variables):

Analytic.c <- fin_combined$Analytic - mean(fin_combined$Analytic)
Authentic.c <- fin_combined$Authentic - mean(fin_combined$Authentic)
Clout.c <- fin_combined$Clout - mean(fin_combined$Clout)
Dash.c <- fin_combined$Dash - mean(fin_combined$Dash)
Exclam.c <- fin_combined$Exclam - mean(fin_combined$Exclam)
Dic.c <- fin_combined$Dic - mean(fin_combined$Dic)
AllPunc.c <- fin_combined$AllPunc - mean(fin_combined$AllPunc)
WC.c <- fin_combined$WC - mean(fin_combined$WC)

#Fit a multinomial regression:
Multinomreg = multinom(Vax_opinion ~ Analytic.c
                       + Authentic.c
                       + Clout.c
                       + Dash.c
                       + Exclam.c
                       + Dic.c
                       + AllPunc.c
                       + WC.c, data = fin_combined)
summary(Multinomreg)

#exponentiate to get to interpretations in terms of multiplicative factors for odds:
exp(coef(Multinomreg))

#get 95% CIs:
confint(Multinomreg)
exp(confint(Multinomreg))

#Prepare Training and Test Data (Not completed as of 11/7/2018):

set.seed(100)
trainingRows <- sample(1:nrow(fin_combined), 0.7*nrow(fin_combined))
training <- fin_combined[trainingRows, ]
test <- fin_combined[-trainingRows, ]

predicted_scores <- predict(Multinomreg, test, "probs") # predict on new data

predicted_class <- predict(Multinomreg, test)

table(predicted_class, test$`Source (B)`)

mean(as.character(predicted_class) != as.character(test$`Source (B)`))
