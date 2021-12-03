library(tidyverse)
library(corrplot)
library(lares)
library(stats)
ObesityDataSet <- read.csv("ObesityDataSet_raw_and_data_sinthetic.csv") %>%
  mutate(Weight_Level = as_factor(NObeyesdad), Gender = as_factor(Gender), 
         Family_History = as_factor(family_history_with_overweight),
         FAVC = as_factor(FAVC), CAEC = as_factor(CAEC),
         SMOKE = as_factor(SMOKE), SCC = as_factor(SCC),
         CALC = as_factor(CALC), MTRANS = as_factor(MTRANS),
         Diagnosis = ifelse(NObeyesdad == "Normal_Weight", 0,
                        ifelse(NObeyesdad == "Insufficient_Weight", 0, 1))) %>%
  select(1:4, Family_History, 6:16, Weight_Level, Diagnosis) %>% na.omit()

tib <- as_tibble(table(ObesityDataSet$Diagnosis, ObesityDataSet$Gender, ObesityDataSet$Family_History), .name_repair = make.names)

obesity <- ObesityDataSet %>% select(1:16,18)
corr_cross(obesity)
corr_cross(obesity, type =2)
corr_cross(obesity, type =2, contains = "Diagnosis")
png(file="test.png", width=1200, height=1600)
x
x <-corr_cross(obesity, contains = "Diagnosis")

cor <- cor(ObesityDataSet[sapply(ObesityDataSet, is.numeric)])
corrplot::corrplot(cor, method="number")

corrplot(O)
O <- read.csv("ObesityDataSet_raw_and_data_sinthetic.csv")%>% na.omit()

png(file="ob2w.png", width=1200, height=1600)
GGally::ggpairs(ObesityDataSet)





O <- read.csv("ObesityDataSet_raw_and_data_sinthetic.csv") %>%
  mutate(Weight_Level = as.numeric(as_factor(NObeyesdad)), Gender = as.numeric(as_factor(Gender)), 
         Family_History = as.numeric(as_factor(family_history_with_overweight)),
         FAVC = as.numeric(as_factor(FAVC)), CAEC = as.numeric(as_factor(CAEC)),
         SMOKE = as.numeric(as_factor(SMOKE)), SCC = as.numeric(as_factor(SCC)),
         CALC = as.numeric(as_factor(CALC)), MTRANS = as.numeric(as_factor(MTRANS)),
         Diagnosis = ifelse(NObeyesdad == "Normal_Weight", 0,
                            ifelse(NObeyesdad == "Insufficient_Weight", 0, 1))) %>%
  select(1:4, Family_History, 6:16, Weight_Level, Diagnosis) %>% na.omit()

select(1:4, Family_History, 6:16, Weight_Level, Diagnosis)


png(file="pairsplot.png", width=1000, height=1200)
GGally::ggpairs(O)

summary(O)


corrplot::corrplot(O) 
corrplot::corrplot(O, is.corr = FALSE, method = "square")
corrplot()
# Base plot
g <- ggplot(data = ObesityDataSet, aes(x= Weight_Level)) + 
  xlab("Diagnosis by Gender") + ylab("Density")

# Filled, stacked histogram with density of shares by weekend level
g + geom_count(aes(x = Weight_Level, fill = Gender)), postion = dodge) 

  labs(title = "Density of Shares: Weekday vs. Weekend") + 
  scale_fill_discrete(name = "Weekday or Weekend?",
                      labels = c("Weekday", "Weekend"))
corrplot::corrplot(O)
