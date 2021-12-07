library(tidyverse)
library(boot)
library(class)
library(tree)
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
library(shinythemes)

set.seed(998)
raw <- read_csv("mammographic_masses.csv",
                        col_names = c("BI-RADS", "Age", "numShape", "numMargin",
                                    "numDensity", "numSeverity"), na = "?") %>%
  mutate(
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
                          labels = c("benign", "maligant"))
  ) %>% select(1:2, 7:10)


split <- raw %>% na.omit() %>% select(2:6)








inTraining <- sample(seq_len(nrow(split)), size = floor(0.7 * nrow(split)))

# This will be needed later on when we start modeling
train<- split[inTraining,] 
test<- split[-inTraining,]




split %>% filter(Age == 50) %>% filter(Shape !="round")


table(raw$Density, raw$Shape, raw$Margin)


split used for graphs



## Proportion Maligant by Age
## Default Plot
## Check box to appear



################################ KEEP THIS CODE ################################



## one discrete variable
g <- ggplot(data, aes(Severity, Age))
d <- g + geom_bar(stat = "identity") 
d + theme_light() 
d + theme_minimal()


# One diescrete filled by another
df <- ggplot(trainSubset, aes(Age, fill = Severity))
df + geom_bar(position = "dodge") 

## two discreate variables
d2 <- ggplot(trainSubset, aes(Density, Margin, Severity))
d2 + geom_count(aes(color = Severity))


## continous one variable
c <- ggplot(trainSubset, aes(Age))
x <-c + geom_histogram(binwidth = 5) + aes(fill = Severity)
c + geom_histogram(binwidth = 5) + aes(fill = Shape)

x+ scale_fill_brewer(palette="Set2")
x
change

c + geom_histogram(aes(y = stat(count) / sum(count), binwidth = 5)) + aes(fill = Severity)


### one discrete and one continous variable
dc <- ggplot(trainSubset, aes(Severity, Age))

## Shape
dc + geom_boxplot(aes(fill = Severity)) + facet_wrap(~Shape)





#one-way, two-way, three way
#Select variables

table(trainSubset$Shape, trainSubset$Density, trainSubset$Severity)

mean(trainSubset$Age)
summary(trainSubset)




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

## number selector 3, 5, 10 folds
## cross validation repeated or not radio buttons
trCtrl <- trainControl(method = "repeatedcv", number = 5, repeats = 3)

#Variable selector 
glmFit <- train(Severity ~ Shape + Margin + Density + Age + Shape:Age +
                  Margin:Age + Density:Age + I(Age^2) + Shape:I(Age^2) +
                  Margin:I(Age^2) + Density:I(Age^2), data = train,
                method = "glm", family = "binomial", 
                trControl = trCtrl, preProcess = c("center", "scale"))

#Default is . or variable selector
class <-  train(Severity ~ ., data = train, method = "rpart",
                trControl=trCtrl, preProcess = c("center", "scale"),
                tuneGrid = data.frame(cp = seq(0, 0.5, 0.001)))
plot(class)

#Color selection #Save plot
rpart.plot(class$finalModel, box.palette="GnRd", nn=TRUE)

# mtry selection number slider both ends
rfFit <- train(Severity ~ ., data = train, method = "rf",
               trControl=trCtrl, preProcess = c("center", "scale"),
               tuneGrid = data.frame(mtry = 1:15))


predglm <- predict(glmFit, newdata = test)
a <- postResample(predglm, testSubset$Severity)


predclass<-predict(class, newdata = test)
b<- postResample(predclass, test$Severity)




predrf <- predict(rfFit, newdata = test)
c <- postResample(predrf, testSubset$Severity)



confuGLM <- confusionMatrix(glmFit, newdata = testSubset)
confuClass <- confusionMatrix(class, newdata = testSubset)
confuRF <- confusionMatrix(rfFit, newdata = testSubset)




a
b
c


Results <-data.frame("GLM" = confuGLM$table, "Classification Tree" = confuClass$table,
                     "Random Forest" = confuRF$table)


plot(varImp(glmFit), top = 10)
plot(varImp(rfFit), top = 10)
plot(varImp(class), top = 10)


### Prediction Tab
newpt <- data.frame(Age = 25, Shape = "oval", Margin = "microlobulated",
                    Density = "iso")

upload csv please save the csv column names 
screen shoot and include image example
read in csv
save as data frame
predict and report back as tible with added column Predicted Severity

allow for individual patient selection

newpt <- c(Age = input$predAge, Shape= input$predShape,
           Margin = input$predMargin, Density = input$predDens)

a <- predict(glmFit, newpt)
b <- predict(class, newpt)
c <- predict(rfFit, newpt)

data.frame(`GLM prediction` = a, `Classification Tree prediction` = b, 
           `Random Forest Prediction` = c)
              # ## Data Page
# A Data page. The user should be able to
# ∗ Scroll through the data set
# ∗ Subset this data set (rows and columns)
# ∗ Save the (possibly subsetted) data as a file (.csv is fine but whatever you’d like)

allow viewing of full mammo_mass file
describe each row
allow for subsetting and and filtering
save created data set as csv