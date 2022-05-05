###########################################################################
#
#   R code to reproduce the analysis reported in 
#   course project 
#   "Phonological Perception Skills in 5-6-years-old Preschool Children"
#   Written by Tatyana Eremicheva
#   email: eremichevat22@gmail.com
#   Please email me if you see any errors or have any questions
#   last update: 01.05.2022
#
###########################################################################

rm(list = ls())
library(psych)
library(tidyverse)
library(dplyr)
library(stringr)
library(gdata)
library(apaTables)
library(sjPlot)
library(brms)
library(glmmTMB)
library(MASS)
library(lme4)
install.packages("effects")
library(effects)

if(!require(psych)){install.packages('psych')}
if(!require(apaTables)){install.packages('apaTables')}

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

####попытка рассчитать корреляцию Спирмена####
x = data$RuTOPP_1_PhonDiscr
y = data$RuTOPP_4_PhonDet
result = cor(x, y, method = "spearman")
cat("Spearman correlation coefficient is:", result)
#######данные описательной статистики (рассчет среднего)

tapply(X = data$RuTOPP_1_PhonDiscr, INDEX = data$gender, FUN = mean)
tapply(X = data$RuTOPP_2_LexDec, INDEX = data$gender, FUN = mean)
tapply(X = data$RuTOPP_4_PhonDet, INDEX = data$gender, FUN = mean, na.rm = TRUE)


#######данные описательной статистики (рассчет стандартного отклонения)

tapply(X = data$RuTOPP_1_PhonDiscr, INDEX = data$gender, FUN = sd)
tapply(X = data$RuTOPP_2_LexDec, INDEX = data$gender, FUN = sd)
tapply(X = data$RuTOPP_4_PhonDet, INDEX = data$gender, FUN = sd, na.rm = TRUE)


##########построение гистограмм для каждого из тестов###########

hist(data$RuTOPP_1_PhonDiscr)
hist(data$RuTOPP_2_LexDec)
hist(data$RuTOPP_4_PhonDet)

##################################

shapiro.test(data$RuTOPP_1_PhonDiscr)
shapiro.test(data$RuTOPP_2_LexDec)
shapiro.test(data$RuTOPP_4_PhonDet)


t.test(data$RuTOPP_1_PhonDiscr,data$RuTOPP_2_LexDec, paired = T)
t.test(data$RuTOPP_1_PhonDiscr,data$RuTOPP_4_PhonDet, paired = T)
t.test(data$RuTOPP_2_LexDec,data$RuTOPP_4_PhonDet, paired = T)



wilcox.test(data$RuTOPP_1_PhonDiscr,data$RuTOPP_4_PhonDet, paired = T, exact = F, conf.int = TRUE)
wilcox.test(data$RuTOPP_1_PhonDiscr,data$RuTOPP_2_LexDec, paired = T, exact = F, conf.int = TRUE)
wilcox.test(data$RuTOPP_2_LexDec,data$RuTOPP_4_PhonDet, paired = T, exact = F)

##############построение боксплотов###############

boxplot(data$RuTOPP_1_PhonDiscr, data$RuTOPP_2_LexDec, data$RuTOPP_4_PhonDet)




#проверяем, есть ли зависимость между возрастом, гендером и результатами субтестов
lm.1 <- lm("RuTOPP_1_PhonDiscr ~ age + gender", data = data)
summary(lm.1)

lm.2 <- lm("RuTOPP_2_LexDec ~ age + gender", data = data)
summary(lm.2)

lm.3 <- lm("RuTOPP_4_PhonDet ~ age + gender", data = data)
summary(lm.3)


tab_model(lm.1, title = "RuTOPP_1_PhonDiscr ~ age + gender")
plot(allEffects(lm.1))

tab_model(lm.2, title = "RuTOPP_2_LexDec ~ age + gender")
plot(allEffects(lm.2))

tab_model(lm.3, title = "RuTOPP_4_PhonDet ~ age + gender")
plot(allEffects(lm.3))



library(apaTables)
apa.cor.table(data, filename = "Predictors_T.doc", show.conf.interval = T)
#Relrvant values for the paper were extracted from this table.

########################корреляции по двум тестам##############

ggplot(data) + 
  geom_point(mapping = aes(x = RuTOPP_1_PhonDiscr, y = RuTOPP_4_PhonDet, color = age))

ggplot(data) + 
  geom_point(mapping = aes(x = RuTOPP_2_LexDec, y = RuTOPP_4_PhonDet, color = gender))

ggplot(data, mapping = aes(x = RuTOPP_2_LexDec, y = RuTOPP_4_PhonDet)) + 
  geom_point(mapping = aes(color = age)) + 
  geom_smooth()

ggplot(data, mapping = aes(x = RuTOPP_2_LexDec, y = RuTOPP_1_PhonDiscr)) + 
  geom_point(mapping = aes(color = age)) + 
  geom_smooth()

################графики по результатам с разной сложностью#############

data1 = read_excel("./Dyslexia8_Sound_In_Word_Difficulty.xlsx")

bar <- ggplot(data1) + 
  geom_bar(
    mapping = aes(x = Difficulty1, fill = Difficulty1), 
    show.legend = FALSE,
    width = 1
  ) + 
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)

bar + coord_flip()
bar + coord_polar()

bar <- ggplot(data1) + 
  geom_bar(
    mapping = aes(x = Difficulty2, fill = Difficulty2), 
    show.legend = FALSE,
    width = 1
  ) + 
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)

bar + coord_flip()
bar + coord_polar()

bar <- ggplot(data1) + 
  geom_bar(
    mapping = aes(x = Difficulty3, fill = Difficulty3), 
    show.legend = FALSE,
    width = 1
  ) + 
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)

bar + coord_flip()
bar + coord_polar()

###########разные группы проб в тесте на наличие звука в слове

data2 = read_excel("./Dyslexia8_Sound_In_Word_result.xlsx")

ggplot(data2) + 
  geom_bar(mapping = aes(x = ID, fill = Accuracy), position = "dodge")


ggplot(data2) + 
  geom_bar(mapping = aes(x = Accuracy, fill = Place_in_word), position = "dodge")

ggplot(data2) + 
  geom_bar(mapping = aes(x = Accuracy, fill = Class_of_sound), position = "dodge")

ggplot(data2) + 
  geom_bar(mapping = aes(x = Accuracy, fill = Phonation), position = "dodge")

ggplot(data2) + 
  geom_bar(mapping = aes(x = Accuracy, fill = Palatalization), position = "dodge")

##################################разные группы проб в тесте на дискриминацию фонем

data6 = read_excel("./PhonDiscr_results.xlsx")

ggplot(data6) + 
  geom_bar(mapping = aes(x = Accuracy, fill = Type_of_stimuli), position = "dodge")
  
##################################разные группы проб в тесте на лексическое решение
data7 = read_excel("./LexDec_results.xlsx")

ggplot(data7) + 
  geom_bar(mapping = aes(x = Accuracy, fill = Pseudoword_formation), position = "dodge")

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



hist(data4$RuTOPP_1_PhonDiscr)
hist(data4$RuTOPP_2_LexDec)
hist(data4$RuTOPP_4_PhonDet)

lm.4 <- lm("RuTOPP_1_PhonDiscr ~ age + gender", data = data4)
summary(lm.4)

tab_model(lm.4, title = "RuTOPP_1_PhonDiscr ~ age + gender")
plot(allEffects(lm.4))

boxplot(data4$RuTOPP_1_PhonDiscr, data4$RuTOPP_2_LexDec, data4$RuTOPP_4_PhonDet)

##############результаты для разных групп проб по каждому субтесту для усеченной выборки

data5 = read_excel("./ParentsDiagnosis_PhonDet_full.xlsx")

ggplot(data5) + 
  geom_bar(mapping = aes(x = Accuracy, fill = Place_in_word), position = "dodge")

ggplot(data5) + 
  geom_bar(mapping = aes(x = Accuracy, fill = Class_of_sound), position = "dodge")

ggplot(data5) + 
  geom_bar(mapping = aes(x = Accuracy, fill = Phonation), position = "dodge")

ggplot(data5) + 
  geom_bar(mapping = aes(x = Accuracy, fill = Palatalization), position = "dodge")
##################################

data8 = read_excel("./ParentsDiagnosis_LexDec.xlsx")

ggplot(data8) + 
  geom_bar(mapping = aes(x = Accuracy, fill = Pseudoword_formation), position = "dodge")

ggplot(data8) + 
  geom_bar(mapping = aes(x = Accuracy, fill = Class_of_word), position = "dodge")

##################################

data9 = read_excel("./ParentsDiagnosis_PhonDiscr.xlsx")

ggplot(data9) + 
  geom_bar(mapping = aes(x = Accuracy, fill = Type_of_stimuli), position = "dodge")

##################################сравнение результатов каждого участника из усеченной выборки
ggplot(data5) + 
  geom_bar(mapping = aes(x = Accuracy, fill = Palatalization), position = "dodge") +
  facet_wrap(~ID) + 
  theme_minimal(base_size = 9) +
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1))

##################################
ggplot(data9) + 
  geom_bar(mapping = aes(x = Accuracy, fill = Type_of_stimuli), position = "dodge") +
  facet_wrap(~ID) + 
  theme_minimal(base_size = 9) +
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1))

##################################

ggplot(data8) + 
  geom_bar(mapping = aes(x = Accuracy, fill = Pseudoword_formation), position = "dodge") +
  facet_wrap(~ID) + 
  theme_minimal(base_size = 9) +
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1))


######################################################
# The end
######################################################




