#Reference, similar model: https://towardsdatascience.com/implementing-and-interpreting-ordinal-logistic-regression-1ee699274cf5

#load clean data -----
setwd('C:/Users/PC Tuan/Desktop/Github/potential-potato/data') #depend on your pc
df_load <- read.csv('clean_googleplaystore.csv')
#Situation: suppose a programer is working on an app, he would like to know
#if it would be worth his time to build sth will become useful( high number of Installs)
#selected column and fix Installs columns----
library(tidyverse)
library(dplyr)
df_selected <- df_load %>%
                select(c(2,5,6,9)) %>%
                drop_na(.)

df_selected$Installs <- as.character(df_selected$Installs)
df_selected$Installs <- substr(df_selected$Installs,1,nchar(df_selected$Installs)-1)
df_selected$Installs<- as.numeric(gsub(",", "", df_selected$Installs))
df_selected$Installs <- as.factor(df_selected$Installs)
summary(df_selected)

# graph of Installs columns----
library(ggplot2)
ggplot(df_selected,aes(Installs))+
  geom_bar()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

#Ordinal Logistic Regression----
library(MASS)
model_fit <-polr(Installs~Rating+Type+Genres, data = df_selected)
summary(model_fit)

#Better format to view result
summary_table <- coef(summary(model_fit))
pval <- pnorm(abs(summary_table[, "t value"]),lower.tail = FALSE)* 2
summary_table <- cbind(summary_table, "p value" = round(pval,3))

#filtering out those with p value less than 0.05 (means having impact on model)
summary_table_filtered <- as_data_frame(summary_table, rownames = 'id')
summary_table_filtered <- summary_table_filtered %>%
                            filter(`p value` <= 0.05)
print.data.frame(summary_table_filtered)
#Comments on coeffiecient: only rating have positive effect on number of installs, if the app is paid or belong
# to these genres below will have negative impact on its popularity
#Comments on intercept: take 1|5 as example: the odd of log that the app will have only 1 person
#installs the app versus the odd of log many people try the app


#Predict value----
#let's predict the probability of popularity that an app developer create a app with following info
new_app <- data.frame('Rating'=4,'Type'='Free','Genres'='Educational')
round(predict(model_fit,new_app,type = "p"), 3)
#Comments: the app may have 20% (highest chance) to get 1 million downloads
#another app
new_app_2 <- data.frame('Rating'=3.5,'Type'='Paid','Genres'='Racing')
round(predict(model_fit,new_app_2,type = "p"), 3)
#Comments: this app have 17% (highest) to get at least 10 thounsand downloads