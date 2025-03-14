library(tidyverse)
library(dplyr)


df <- read.csv("Titanicv2.csv")


str(df)
summary(df)


#Cual es el porcentaje de problacion por clase?
df %>% count(Pclass) %>% mutate(porcentaje = (n/sum(n))*100)

#Porcentaje de hombres y mujeres en la poblacion
df %>% count(Sex) %>% mutate(porcentaje = (n/sum(n))*100)

#Medidas de tendencia central de edad
mean(na.omit(df$Age))
median(na.omit(df$Age))
max(na.omit(df$Age))
min(na.omit(df$Age))

#Cual fue el lugar donde embarcaron mas pasajeros
df %>% group_by(Embarked) %>% tally() %>% mutate(porcentaje = (n/sum(n))*100)

#Porcentaje de supervivientes
survivors <- df %>% count(Survived) %>% mutate(porcentaje = (n/sum(n))*100)
survivors


df_survivors <- df %>% filter(Survived =="Yes")

#Total de supervivientes
df_survivors %>% nrow()

#sexo de los supervivientes
df_survivors %>% count(Sex)

#clases supervivientes
df_survivors %>%  count(Pclass) %>% mutate(porcentaje = (n/sum(n))*100)

#Procedencia de los supervivientes

df_survivors %>% group_by(Embarked) %>% tally() %>% mutate(porcentaje = (n/sum(n))*100)


#Medidas de tendencia central para los supervivientes

mean(na.omit(df_survivors$Age))
median(na.omit(df_survivors$Age))
max(na.omit(df_survivors$Age))
min(na.omit(df_survivors$Age))

#Graficas de barras de edades
df %>%   ggplot(aes(x=Pclass)) + 
  geom_bar()

df_survivors %>% ggplot(aes(x=Pclass)) + 
  geom_bar()

df %>%   ggplot(aes(x=Sex, y=Age)) +
  geom_boxplot()

df %>% ggplot(aes(x=Pclass, y=Age, color = Sex)) +
  geom_point(position="jitter")

df_survivors %>%   ggplot(aes(x=Pclass, y=Age)) +
  geom_violin(scale = "count")

df %>% ggplot(aes(x=Pclass, fill = Survived)) + geom_bar()


