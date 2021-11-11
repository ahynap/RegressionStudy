library(tidyverse)
library(dplyr)
library(ggplot2)
library(caret)

credit <- read.csv("credit-approval.csv", sep=",")
head(credit)
summary(credit)

table(credit$A1, credit$A16)

credit$A16 <- factor(credit$A16)
model <- glm(A16 ~ . , data=credit, family = "binomial")
summary(model)

res <- predict(model, credit, type='response')
res

res_c <- factor(ifelse(res>0.2,"+","-"))
res_c

confusionMatrix(res_c,credit$A16,mode = "prec_recall",positive = "+")





