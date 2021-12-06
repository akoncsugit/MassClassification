library(shiny)
library(shinythemes)
library(shinyWidgets)
library(markdown)
library(rlang)
library(tidyverse)
library(boot)
library(class)
library(tree)
library(caret)
library(rpart)
library(rpart.plot)

server <- function(session, input, output) {
  
  ### about page
  ### data page
  input$switchRemoveNA
  input$sliderAgeFilter
  input$selectShapeFilter
  selectMarginFilter
  pickDensityFilter
  pickerSubset
  actionData
  dataDownload  #need download handler
  
  render tableOutput "myTable"
  
  ### graph page
  switchColors
  color
  plotType
  fill
  histBins
  maxBins
  xaxis
  facet
  
  
  downloadPlot #download handler plot
  
  output$sumPlot <- renderPlot ({
    if(input$plotType == "Histogram") {
      hist <- ggplot(graphData, aes(Age))
      if(input$switchColors) {
        hist + geom_histogram(binwidth = input$histBins, aes(fill = Age)) +
          scale_fill_brewer(palette = input$color)
      } else {
        hist + geom_histogram(binwidth = input$histBins, aes(fill = Age)) 
      }
    } else if (input$plotType == "Bar"){
      bar <- ggplot(m, aes(input$ fill = Severity))
      
    }
  })
  
  
  
  
  ## numeric page
  
  render plot "sumPlot"
  
  conPick
  conTab # action button
  
  render table "con"
  render output "summary"
  
  ## model info

  
  
  ## model fit

  # Train/Test data split
  moddat <- eventReactive(input$predbutton, {
    ratio <- input$percent/100
    index <- sample(seq_len(nrow(split)), size = floor(ratio*nrow(split)))
    train <- split[index,]
    test <- split[-index,]
    return(list(Train=train, Test=test))
  })

  form <- eventReactive(input$predbutton, {
      n <- length(input$modVar)
      temp <- paste0(input$modVar,c(rep("+",n-1),""))
      temp <- paste0(temp, collapse = "")
      return(formula(paste0('~', temp)))
  })
  
  train <- moddat()[["Train"]]
  test  <- moddat()[["Test"]]
  
  trCtrl <- eventReactive(input$predbutton, {
    trainControl(method = "repeatedcv", number = input$kfolds, repeats = 3)
  })
  
  
  glmFit <- eventReactive(input$predbutton, {
    glmFit <- train(Severity ~ form, data = train, method = "glm", family = "binomial",
                    trControl = trCtrl())
    })
  
  class <- eventReactive(input$predbutton,{
    class<- train(Severity ~ form, data = train, method = "rpart", trControl=trCtrl(),
                  tuneGrid = data.frame(cp = seq(0, input$sliderCP, 0.001)))
  })
  
  rfFit <- eventReactive(input$predbutton, {
    rfFit <-train(Severity ~ form, data = train, method = "rf", trControl=trCtrl(),
          tuneGrid = data.frame(mtry = 1:input$mtrys))
  })
 
  
  output$summaryGLM <- renderPrint({
    if (input$predbutton){
      summary(glmFit())
    }
  })
  
  output$resultsRF <- renderPrint({
    if (input$predbutton){
    rfFit()
    }
  })
  
  output$resultsRF <- renderPrint({
    if (input$predbutton){
    class()
    }
  })
  
  output$varImp <- renderPlot({
    plot(varImp(rfFit()), top = 10)
  })
  
  output$classPlot <- renderPlot({
    rpart.plot(class()$finalModel, box.palette="GnRd", nn=TRUE)
  })
  
  output$confuGLM<- renderPrint({
    confusionMatrix(glmFit(), newdata = test)
  })
  
  output$confuClass <- renderPrint({
    confusionMatrix(class(), newdata = test)
  })
  
  output$confuRF <- renderPrint({
    confusionMatrix(rfFit(), newdata = test)
  })

  

  output$fitResults <- renderDataTable({
    data.frame("GLM" = confuGLM()$table, "Classification Tree" = confuClass()$table,
               "Random Forest" = confuRF()$table)
  })
  


  
  newpt <- c(Age = input$predAge, Shape= input$predShape,
             Margin = input$predMargin, Density = input$predDens)


  output$title <- renderUI({
    h1(tools::toTitleCase(paste0("Predictions for a mass from a ", input$predAge,
                                 " year old patient of shape: ",
                                 input$predShape, ", density: ", input$predDens, ", and margin: ",
                                 input$predMarg, ".")))
  })
  
  output$predResults <- renderDataTable({
    data.frame(`GLM prediction` = predict(glmFit(), newpt),
               `Classification Tree prediction` = predict(class(), newpt),
               `Random Forest Prediction` = predict(rfFit(), newpt)
               )
  })
  
 }
