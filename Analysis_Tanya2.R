###########################################################################
#
#   R code to reproduce the analysis reported in 
#   course project 
#   "Phonological Perception Skills in 5-6-years-old Preschool Children"
#   Written by Tatyana Eremicheva
#   email: eremichevat22@gmail.com
#   Please email me if you see any errors or have any questions
#   last update: 25.05.2022
#
###########################################################################

rm(list = ls())
library(psych)
library(tidyverse)
library(dplyr)
library(stringr)
library(gdata)
library(sjPlot)
library(brms)
library(glmmTMB)
library(MASS)
install.packages("effects")
library(effects)

if(!require(psych)){install.packages('psych')}

install.packages("xlsx", dep = T)
library("xlsx")

################################################################################
# Загрузка файла с результатами всех тестов по всем участникам из выборки
################################################################################

setwd("/Users/oksana/Desktop/курсовая/2 курс")

data = read_excel("./2021_ReadingPredictors_summary_data.xlsx")

data$gender <- as.factor(data$gender)
data$age <- as.factor(data$age)


data$RuTOPP_1_PhonDiscr <- as.numeric(data$RuTOPP_1_PhonDiscr)
data$RuTOPP_2_LexDec <- as.numeric(data$RuTOPP_2_LexDec)
data$RuTOPP_4_PhonDet <- as.numeric(data$RuTOPP_4_PhonDet)

#######данные описательной статистики (рассчет среднего)

tapply(X = data$RuTOPP_1_PhonDiscr, INDEX = data$gender, FUN = mean)
tapply(X = data$RuTOPP_2_LexDec, INDEX = data$gender, FUN = mean)
tapply(X = data$RuTOPP_4_PhonDet, INDEX = data$gender, FUN = mean, na.rm = TRUE)


#######данные описательной статистики (рассчет стандартного отклонения)

tapply(X = data$RuTOPP_1_PhonDiscr, INDEX = data$gender, FUN = sd)
tapply(X = data$RuTOPP_2_LexDec, INDEX = data$gender, FUN = sd)
tapply(X = data$RuTOPP_4_PhonDet, INDEX = data$gender, FUN = sd, na.rm = TRUE)


####################оценка нормальности распределения

hist(data$RuTOPP_1_PhonDiscr)
shapiro.test(data$RuTOPP_1_PhonDiscr)
#не норм распределение

hist(data$RuTOPP_2_LexDec)
shapiro.test(data$RuTOPP_2_LexDec)
#не норм распределение

hist(data$RuTOPP_4_PhonDet)
shapiro.test(data$RuTOPP_4_PhonDet)
#не норм распределение

#########проверка статистической значимости

wilcox.test(data$RuTOPP_1_PhonDiscr,data$RuTOPP_2_LexDec, paired = F, exact = F, conf.int = TRUE)
#p-value > 0.05 значит они статистически не значимы
wilcox.test(data$RuTOPP_1_PhonDiscr,data$RuTOPP_4_PhonDet, paired = F, exact = F, conf.int = TRUE)
#p-value значительно < 0.05 значит они статистически значимы
wilcox.test(data$RuTOPP_2_LexDec,data$RuTOPP_4_PhonDet, paired = F, exact = F, conf.int = TRUE)
#p-value значительно < 0.05 значит они статистически значимы


##############построение боксплотов###############

boxplot(data$RuTOPP_1_PhonDiscr, data$RuTOPP_2_LexDec, data$RuTOPP_4_PhonDet)

#############средняя скорость реакции
data$RuTOPP_1_PhonDiscr_Reaction <- as.numeric(data$RuTOPP_1_PhonDiscr_Reaction)
data$RuTOPP_2_LexDec_Reaction <- as.numeric(data$RuTOPP_2_LexDec_Reaction)
data$RuTOPP_4_PhonDet_Reaction <- as.numeric(data$RuTOPP_4_PhonDet_Reaction)

#######рассчет средней скорости по каждому субтесту

tapply(X = data$RuTOPP_1_PhonDiscr_Reaction, INDEX = data$gender, FUN = mean)
tapply(X = data$RuTOPP_2_LexDec_Reaction, INDEX = data$gender, FUN = mean)
tapply(X = data$RuTOPP_4_PhonDet_Reaction, INDEX = data$gender, FUN = mean, na.rm = TRUE)


#######рассчет стандартного отклонения у средней скорости по каждому субтесту

tapply(X = data$RuTOPP_1_PhonDiscr_Reaction, INDEX = data$gender, FUN = sd)
tapply(X = data$RuTOPP_2_LexDec_Reaction, INDEX = data$gender, FUN = sd)
tapply(X = data$RuTOPP_4_PhonDet_Reaction, INDEX = data$gender, FUN = sd, na.rm = TRUE)

#######################описательная статистика и средние значения для усеченной выборки 

data4 = read_excel("./ParentsDiagnosis.xlsx")

data4$gender <- as.factor(data4$gender)
data4$age <- as.factor(data4$age)


data4$RuTOPP_1_PhonDiscr <- as.numeric(data4$RuTOPP_1_PhonDiscr)
data4$RuTOPP_2_LexDec <- as.numeric(data4$RuTOPP_2_LexDec)
data4$RuTOPP_4_PhonDet <- as.numeric(data4$RuTOPP_4_PhonDet)

#######данные описательной статистики (рассчет среднего) для усеченной выборки

tapply(X = data4$RuTOPP_1_PhonDiscr, INDEX = data4$gender, FUN = mean)
tapply(X = data4$RuTOPP_2_LexDec, INDEX = data4$gender, FUN = mean)
tapply(X = data4$RuTOPP_4_PhonDet, INDEX = data4$gender, FUN = mean, na.rm = TRUE)


#######данные описательной статистики (рассчет стандартного отклонения) для усеченной выборки

tapply(X = data4$RuTOPP_1_PhonDiscr, INDEX = data4$gender, FUN = sd)
tapply(X = data4$RuTOPP_2_LexDec, INDEX = data4$gender, FUN = sd)
tapply(X = data4$RuTOPP_4_PhonDet, INDEX = data4$gender, FUN = sd, na.rm = TRUE)



####################оценка нормальности распределения для усеченной выборки

hist(data4$RuTOPP_1_PhonDiscr)
shapiro.test(data4$RuTOPP_1_PhonDiscr)
#норм распределение

hist(data4$RuTOPP_2_LexDec)
shapiro.test(data4$RuTOPP_2_LexDec)
#не норм распределение

hist(data4$RuTOPP_4_PhonDet)
shapiro.test(data4$RuTOPP_4_PhonDet)
#норм распределение

#########проверка статистической значимости для усеченной выборки

t.test(data4$RuTOPP_1_PhonDiscr,data4$RuTOPP_4_PhonDet, var.equal = F)
#p-value > 0.05 значит они статистически не значимы
wilcox.test(data4$RuTOPP_1_PhonDiscr,data4$RuTOPP_2_LexDec, paired = F, exact = F, conf.int = TRUE)
#p-value > 0.05 значит они статистически не значимы
wilcox.test(data4$RuTOPP_2_LexDec,data4$RuTOPP_4_PhonDet, paired = F, exact = F, conf.int = TRUE)
#p-value > 0.05 значит они статистически не значимы


boxplot(data4$RuTOPP_1_PhonDiscr, data4$RuTOPP_2_LexDec, data4$RuTOPP_4_PhonDet)

#############средняя скорость реакции для усеченной выборки
data4$RuTOPP_1_PhonDiscr_Reaction <- as.numeric(data4$RuTOPP_1_PhonDiscr_Reaction)
data4$RuTOPP_2_LexDec_Reaction <- as.numeric(data4$RuTOPP_2_LexDec_Reaction)
data4$RuTOPP_4_PhonDet_Reaction <- as.numeric(data4$RuTOPP_4_PhonDet_Reaction)

#######рассчет средней скорости по каждому субтесту для усеченной выборки

tapply(X = data4$RuTOPP_1_PhonDiscr_Reaction, INDEX = data4$gender, FUN = mean)
tapply(X = data4$RuTOPP_2_LexDec_Reaction, INDEX = data4$gender, FUN = mean)
tapply(X = data4$RuTOPP_4_PhonDet_Reaction, INDEX = data4$gender, FUN = mean, na.rm = TRUE)


#######рассчет стандартного отклонения у средней скорости по каждому субтесту
#######для усеченной выборки

tapply(X = data4$RuTOPP_1_PhonDiscr_Reaction, INDEX = data4$gender, FUN = sd)
tapply(X = data4$RuTOPP_2_LexDec_Reaction, INDEX = data4$gender, FUN = sd)
tapply(X = data4$RuTOPP_4_PhonDet_Reaction, INDEX = data4$gender, FUN = sd, na.rm = TRUE)


#проверяем, есть ли зависимость между возрастом, полом участников и результатами субтестов
lm.1 <- lm("RuTOPP_1_PhonDiscr ~ gender + age", data = data)
summary(lm.1)
#переменные не значимы

lm.2 <- lm("RuTOPP_2_LexDec ~ gender + age", data = data)
summary(lm.2)
#переменные не значимы

lm.3 <- lm("RuTOPP_4_PhonDet ~ gender + age", data = data)
summary(lm.3)
#переменные не значимы

########################корреляции по двум тестам##############

cor.test(data$RuTOPP_1_PhonDiscr, data$RuTOPP_4_PhonDet, method = 'spearman', use = "complete.obs", paired = F, exact = F, conf.int = TRUE)
#связь между результатами тестов не очень сильная и положительная 
#чем больше результаты в тесте на дискриминацию фонем, 
#тем больше результаты в тесте на наличие звука в слове

ggplot(data, mapping = aes(x = RuTOPP_1_PhonDiscr, y = RuTOPP_4_PhonDet)) + 
  geom_point(mapping = aes(color = age)) + 
  geom_smooth()

cor.test(data$RuTOPP_2_LexDec, data$RuTOPP_4_PhonDet, method = 'spearman', use = "complete.obs", paired = F, exact = F, conf.int = TRUE)
#связь между результатами тестов слабая и отрицательная 
#чем больше результаты в тесте на лексический доступ, 
#тем меньше результаты в тесте на наличие звука в слове

ggplot(data, mapping = aes(x = RuTOPP_2_LexDec, y = RuTOPP_4_PhonDet)) + 
  geom_point(mapping = aes(color = age)) + 
  geom_smooth()

cor.test(data$RuTOPP_2_LexDec, data$RuTOPP_1_PhonDiscr, method = 'spearman', paired = F, exact = F, conf.int = TRUE)
#связь между результатами тестов не очень сильная и положительная 
#чем больше результаты в тесте на дискриминацию фонем, 
#тем больше результаты в тесте на лексический доступ

ggplot(data, mapping = aes(x = RuTOPP_2_LexDec, y = RuTOPP_1_PhonDiscr)) + 
  geom_point(mapping = aes(color = age)) + 
  geom_smooth()

###########разные группы проб в тесте на наличие звука в слове

data2 = read_excel("./Dyslexia8_Sound_In_Word_result.xlsx")

data2 %>% 
  dplyr::count(Accuracy, Palatalization, name = 'Freq') %>% 
  ggplot(aes(x = Accuracy, y = Freq, fill = Palatalization)) +
  geom_bar(stat = "identity", position = position_dodge())+
  geom_text(aes(label = Freq), vjust=-0.25, color="black",
            position = position_dodge(0.9), size=3.5)+
  scale_fill_brewer(palette="Paired")+
  theme_minimal()

#########################разные группы проб в тесте на дискриминацию фонем

data6 = read_excel("./PhonDiscr_results.xlsx")

data6 %>% 
  dplyr::count(Accuracy, Type_of_stimuli, name = 'Freq') %>% 
  ggplot(aes(x = Accuracy, y = Freq, fill = Type_of_stimuli)) +
  geom_bar(stat = "identity", position = position_dodge())+
  geom_text(aes(label = Freq), vjust=-0.25, color="black",
            position = position_dodge(0.9), size=3.5)+
  scale_fill_brewer(palette="Paired")+
  theme_minimal()

########################разные группы проб в тесте на лексическое решение
data7 = read_excel("./LexDec_results.xlsx")

data7 %>% 
  dplyr::count(Accuracy, Class_of_word, name = 'Freq') %>% 
  ggplot(aes(x = Accuracy, y = Freq, fill = Class_of_word)) +
  geom_bar(stat = "identity", position = position_dodge())+
  geom_text(aes(label = Freq), vjust=-0.25, color="black",
            position = position_dodge(0.9), size=3.5)+
  scale_fill_brewer(palette="Greens")+
  theme_minimal()

data7 %>% 
  dplyr::count(Accuracy, Pseudoword_formation, name = 'Freq') %>% 
  ggplot(aes(x = Accuracy, y = Freq, fill = Pseudoword_formation)) +
  geom_bar(stat = "identity", position = position_dodge())+
  geom_text(aes(label = Freq), vjust=-0.25, color="black",
            position = position_dodge(0.9), size=3)+
  scale_fill_brewer(palette="Paired")+
  theme_minimal()


######################################################
# The end
######################################################




