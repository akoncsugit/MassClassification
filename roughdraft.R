newCols <- dummyVars(~Shape + Margin + Density, data = split) %>%
  predict(newdata = split)
# add those and remove originals
mybcKNN<- cbind(split, newCols) %>% mutate("Maligant" = as.factor(numSeverity)) %>%
  select(-BI_RADS, -numShape,-numMargin, -numDensity, -numSeverity, -Shape,
         -Margin, -Density, -Severity)

set.seed(101)
splitForTrain<- createDataPartition(y = mybcKNN$Maligant, p = 0.8, list = FALSE)
trainKnn <- mybcKNN[splitForTrain, ]
testKnn <- mybcKNN[-splitForTrain, ]

subTrainKNN <- testKnn %>% select(-ID)
subTestKNN <- testKnn %>% select(-ID)


kNNFit <- train(Maligant ~ ., subTrainKNN, method = "knn",
                trControl = trainControl(method = "repeatedcv", number = 10,
                                         repeats = 3),
                preProcess = c("center", "scale"),
                tuneGrid = data.frame(k = 1:40))
kNNFit
confusionMatrix(kNNFit, newdata = subTestKNN)


library(tidyverse)
library(class)
library(tree)
library(caret)
library(rpart)
library(rpart.plot)


mammo_mass <- read_csv("mammographic_masses.csv",
                        col_names = c("BI-RADS", "Age", "numShape", "numMargin",
                                    "numDensity", "numSeverity"), na = "?") %>%
  mutate("Shape" = cut(numShape, breaks = 4, labels = c("round", "oval",
                                                       "lobular", "irregular")),
         "Margin" = cut(numMargin, breaks = 5,
                        labels = c("circumscribed","microlobulated", "obscured",
                                   "ill-defined", "spiculated")),
         "Density" = cut(numDensity,
                         breaks = 4, labels = c("high", "iso", "low",
                                                "fat-containing"),
                                                ordered_result = TRUE),
         "Severity" = cut(numSeverity, breaks = 2,
                          labels = c("benign", "maligant")))
View(mammo_masses)
maszzz <-mammo_mass %>% na.omit()
m <- mammo_mass %>% na.omit() %>% select(Age, Shape, Margin, Density, Severity)
n <- mammo_mass %>% na.omit() %>% select(Age, numSeverity)
summary(glm(Severity ~ (Shape+Margin+Age)^2 + Density + I(Age^2),
            family = binomial, data = m))
summary(glm(Severity ~ I(Age^2),family = binomial, data = m))
summary(glm(Severity ~ Shape*Age + I(Age^2),family = binomial, data = m))


cor <- cor(n[sapply(n, is.numeric)])
attributes(cor)$dimnames[[1]] <- c("Age", "Severity")
attributes(cor)$dimnames[[2]] <- c("Age", "Severity")
cor
corrplot::corrplot(cor, )


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


## two discreate variables
d2 <- ggplot(ObesityDataSet, aes(as.factor(Diagnosis), Gender))
d2 + geom_count(aes(shape = Gender))

###
### HISTOGRAM

if histogram(
c <- ggplot(m, aes(Age))
if color select (
c + geom_histogram(binwidth = 5, aes(fill = Age)) + scale_fill_brewer(palette="Set2")
)

c + geom_histogram(binwidth = 5, aes(fill = Shape))
)
#### 
#### BOX

if box (
  dc <- ggplot(m, aes(Severity, Age)) 
  if color select (
    if facet wrap(
    dc + geom_boxplot(aes(fill = Shape)) +
      facet_wrap(~Margin) + scale_fill_brewer(palette="Set2")
    ) else (
      dc + geom_boxplot(aes(fill = Shape)) + scale_fill_brewer(palette="Set2")
    )
  ) else (
    if facet wrap(
      dc + geom_boxplot(aes(fill = Shape)) + facet_wrap(~Margin)
    ) else (
      dc + geom_boxplot(aes(fill = Shape)) 
    )
  )
)   


###
### BAR
if bar (
df <- ggplot(m, aes(Shape, fill = Severity))

if color select (
  df + geom_bar(position = "dodge") + scale_fill_brewer(palette="Set2")
) else (
  df + geom_bar(position = "dodge")
)
)


### one discrete and one continous variable
dc <- ggplot(m, aes(Severity, Age))

## Family History
dc + geom_boxplot()
aes(fill = Severity)) + facet_wrap(~Shape)


## remove intercept term with -1
summary(fit)


#Plot of Quality vs Total sulfur dioxide
data <- maszzz %>% group_by(Age) %>% summarise(ProportMaligant = mean(numSeverity), n = n())
ggplot(data, aes(x = Age, y = ProportMaligant, size = n)) + geom_point(stat = "identity")+
  ggtitle("QTitle")

table(m$Shape, m$Density, m$Severity)

mean(m$Age)
summary(m)



### Add kNN ###



# Create a decision tree model
tree <- rpart(Severity~., data=m, cp=0.0001)
# Visualize the decision tree with rpart.plot
rpart.plot(giniFit, box.palette="GnRd", nn=TRUE)
rpart.plot(tree, box.palette="RdBu", shadow.col="gray", nn=TRUE)

# caret :  method = "rpart"

#option for repeated vs not, number = fold, repeats = )

trCtrl <- trainControl(method = "repeatedcv", number = 5, repeats = 3)

rfFit <- train(Severity ~.^2, data = m, method = "rf",
               trControl=trCtrl,
               tuneGrid = data.frame(mtry = 1:3))

class <-  train(Severity ~., data = m, method = "rpart",
                trControl=trCtrl)

rpart.plot(class$finalModel, box.palette="GnRd", nn=TRUE)
 

### Save for other projects 
###http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/116-mfa-multiple-factor-analysis-in-r-essentials/###
# u <- m %>% select(Age, Shape, Margin, Density)
# library(FactoMineR)
# mf <-MFA(u, group = c(1,1,1,1), type = c("c", "n", "n", "n"))
# print(mf)
# library(factoextra)
# eig.val <- get_eigenvalue(mf)
# head(eig.val)
# fviz_screeplot(mf)
# group <- get_mfa_var(mf, "group")
# group
# fviz_mfa_var(mf, "group")
