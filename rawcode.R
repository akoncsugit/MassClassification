library(tidyverse)
library(boot)
library(class)
library(tree)
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
library(shinythemes)

mammo_mass <- read_csv("mammographic_masses.csv",
                        col_names = c("BI_RADS", "Age", "numShape", "numMargin",
                                    "numDensity", "numSeverity"), na = "?") %>%
  mutate("ID" = c(1:961),
         "Shape" = cut(numShape, breaks = 4, labels = c("round", "oval",
                                                       "lobular", "irregular")),
         "Margin" = cut(numMargin, breaks = 5,
                        labels = c("circumscribed","microlobulated", "obscured",
                                   "ill-defined", "spiculated")),
         "Density" = cut(numDensity,
                         breaks = 4, labels = c("high", "iso", "low",
                                                "fat-containing"),
                                                ordered_result = TRUE),
         "Severity" = cut(numSeverity, breaks = 2,
                          labels = c("benign", "maligant")),
  )

View(mammo_mass)
split <-mammo_mass %>% na.omit()


table(split$Density, split$Shape)
table(split$Density, split$Margin)
table(split$Shape, split$Margin)


set.seed(998)

inTraining <- sample(seq_len(nrow(split)), size = floor(0.7 * nrow(split)))

# This will be needed later on when we start modeling
train<- split[inTraining,] 
test<- split[-inTraining,]

trainSubset <- train %>% select(Age, Shape, Margin, Density, Severity)
testSubset <- test %>% select(Age, Shape, Margin, Density, Severity)




cor <- cor(trainSubset[sapply(trainSubset, is.numeric)])
attributes(cor)$dimnames[[1]] <- c("Age", "Severity")
attributes(cor)$dimnames[[2]] <- c("Age", "Severity")
cor
corrplot::corrplot(cor)


# Base plot
g <- ggplot(data = m, aes(x= Severity)) + 
  xlab("Severity") + ylab("Density")



################################ KEEP THIS CODE ################################

g <- ggplot(m, aes(Severity))

## one discrete variable
d <- g + geom_bar() 
d + theme_light() 
d + theme_minimal()


# One diescrete filled by another
df <- ggplot(m, aes(Age, fill = Severity))
df + geom_bar(position = "dodge")

## two discreate variables
d2 <- ggplot(ObesityDataSet, aes(as.factor(Diagnosis), Gender))
d2 + geom_count(aes(shape = Gender))


## continous one variable
c <- ggplot(m, aes(Age))
c + geom_histogram(binwidth = 5)


### one discrete and one continous variable
dc <- ggplot(m, aes(Severity, Age))

## Family History
dc + geom_boxplot(aes(fill = Severity)) + facet_wrap(~Shape)





#Plot of Quality vs Total sulfur dioxide
data <- maszzz %>% group_by(Age) %>% summarise(ProportMaligant = mean(numSeverity), n = n())
ggplot(data, aes(x = Age, y = ProportMaligant, size = n)) + geom_point(stat = "identity")+
  ggtitle("QTitle")

table(m$Shape, m$Density, m$Severity)

mean(m$Age)
summary(m)




# ## About Page
# ∗ Describe the purpose of the app
# ∗ Briefly discuss the data and its source - providing a link to more information about the data
# ∗ Tell the user the purpose of each tab (page) of the app
# ∗ Include a picture related to the data (for instance, if the data was about the world wildlife
#                                          fund, you might include a picture of their logo)
# ## Data Exploration Page
# ∗ Create numerical and graphical summaries
# ∗ Change the type of plot and type of summary reported
# ∗ Change the variables and filter the rows to change the data in the plots/summaries


## Model Page
### Modeling Info Tab

### Model Fitting Tab
trCtrl <- trainControl(method = "repeatedcv", number = 5, repeats = 3)
glmFit <- train(Severity ~ Shape + Margin + Density + Age + Shape:Age +
                  Margin:Age + Density:Age + I(Age^2) + Shape:I(Age^2) +
                  Margin:I(Age^2) + Density:I(Age^2), data = trainSubset,
                method = "glm", family = "binomial", 
                trControl = trCtrl, preProcess = c("center", "scale"))

class <-  train(Severity ~ ., data = trainSubset, method = "rpart",
                trControl=trCtrl, preProcess = c("center", "scale"),
                tuneGrid = data.frame(cp = seq(0, 0.1, 0.001)))

rpart.plot(class$finalModel, box.palette="GnRd", nn=TRUE)

rfFit <- train(Severity ~ ., data = trainSubset, method = "rf",
               trControl=trCtrl, preProcess = c("center", "scale"),
               tuneGrid = data.frame(mtry = 1:15))


predglm <- predict(glmFit, newdata = testSubset)
postResample(predglm, testSubset$Severity)
confusionMatrix(glmFit, newdata = test)

predclass<-predict(class, newdata = testSubset)
postResample(predclass, testSubset$Severity)
confusionMatrix(class, newdata = testSubset)
confuClass <- confusionMatrix(predclass, testSubset$Severity)

predrf <- predict(rfFit, newdata = testSubset)
postResample(predrf, testSubset$Severity)
confuRF <- confusionMatrix(predrf, testSubset$Severity)

Results <-data.frame("kNN" = kNN$overall, "Classification Tree" = class$overall, "Bagged Tree" = bag$overall,
                     "Random Forest" = rf$overall, "Boosted Tree" = boost$overall)


plot(varImp(glmFit), top = 10)
plot(varImp(rfFit), top = 10)
plot(varImp(class), top = 10)


### Prediction Tab
newpt <- data.frame(Age = 25, Shape = "oval", Margin = "microlobulated",
                    Density = "iso")

predict(rfFit, newpt)
predict(glmFit, newpt)
predict(class, newpt)


# ## Data Page
# A Data page. The user should be able to
# ∗ Scroll through the data set
# ∗ Subset this data set (rows and columns)
# ∗ Save the (possibly subsetted) data as a file (.csv is fine but whatever you’d like)