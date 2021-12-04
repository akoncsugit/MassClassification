library(tidyverse)
library(corrplot) 
library(naniar)
# library(logistf)

# library(lares)
# library(stats)

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



################################ KEEP THIS CODE ################################

g <- ggplot(ObesityDataSet, aes(as.factor(Diagnosis)))

## one discrete variable
d <- g + geom_bar() 
d + theme_light() 
d + theme_minimal()


# One diescrete filled by another
df <- ggplot(ObesityDataSet, aes(as.factor(Diagnosis), fill = Family_History)) 
df + geom_bar(position = "dodge")

## two discreate variables
d2 <- ggplot(ObesityDataSet, aes(as.factor(Diagnosis), Gender))
d2 + geom_count(aes(shape = Gender))


## continous one variable
c <- ggplot(ObesityDataSet, aes(Weight))
c + geom_histogram(binwidth = 5)

## two continous variables
c2 <- ggplot(ObesityDataSet, aes(Weight, Age))

## filled by diagnosis
c2 + geom_jitter(height = 2, width = 2, aes(color = factor(Diagnosis), alpha = Height))
## alpha is opacity, color, shape, size options




### one discrete and one continous variable
dc <- ggplot(ObesityDataSet, aes(as.factor(Diagnosis), Age))

## Family History
dc + geom_boxplot(aes(fill = as.factor(Diagnosis))) + facet_wrap(~Family_History)

ObFitData <- ObesityDataSet %>% select(1:16, 18) %>% 
  mutate(Diagnosis = as_factor(Diagnosis))
fit <- glm(Diagnosis ~ ., data = ObFitData, family = "binomial")
fit
## remove intercept term with -1
summary(fit)


#Plot of Quality vs Total sulfur dioxide
data <- glmPlot %>% group_by(`total sulfur dioxide`) %>% summarise(propGood = mean(qualityBin), n = n())
ggplot(data, aes(x = `total sulfur dioxide`, y = propGood, size = n)) + geom_point(stat = "identity")+
  ggtitle("Quality vs Total sulfur dioxide")



glmFit1 <- 



data <- ObFitData %>% filter(Diagnosis == 0)



