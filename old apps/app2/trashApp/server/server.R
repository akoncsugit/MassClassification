########################################################################;
#Author:  Ashley Ko                                                     ;
#Title: ST558 Project 3 server                                          ;
#Program Purpose: To complete ST558 project 3 requirements              ;
#Date: 2021/12/05                                                       ;
########################################################################;


# Required packages
library(caret)
library(rpart.plot) 

# Establishes app server
server <- function(session, input, output) {
  
  # Data Page
  # Gets raw data
  getRawData <- reactive({ raw })

  # Reactive which filters and subsets raw data using conditonal logic
  filsubDat <- reactive({
    if(input$filtVar == "Severity") {
      if(is.null(input$pickerSubset)) {
        getRawData() %>% filter(Severity == input$filSeverity)
        } else {
          getRawData() %>% filter(Severity == input$filSeverity) %>%
            select(`BI-RADS`, input$pickerSubset, Severity)
          }
      } else if(input$filtVar == "Margin"){
        if(is.null(input$pickerSubset)) {
          getRawData() %>% filter(Margin == input$filMargin)
          } else {
            getRawData() %>% filter(Margin == input$filMargin) %>%
              select(`BI-RADS`, input$pickerSubset, Severity)
            }
        } else if(input$filtVar == "Shape"){
          if(is.null(input$pickerSubset)) {
            getRawData() %>% filter(Shape == input$filShape)
            } else {
              getRawData() %>% filter(Shape == input$filShape) %>%
                select(`BI-RADS`, input$pickerSubset, Severity)
            }
        } else if(input$filtVar == "Density") { 
          if(is.null(input$pickerSubset)) {
          getRawData() %>% filter(Density == input$filDens)
        } else {
          getRawData() %>% filter(Density == input$filDens) %>%
            select(`BI-RADS`, input$pickerSubset, Severity)
        }
      } else{
        if(is.null(input$pickerSubset)) {
          getRawData()
      } else {
        getRawData() %>% select(`BI-RADS`, input$pickerSubset, Severity)
      }
    }
  })

  # Calls filsubDat to filter and subset data which is returned for display
  output$myTable <- renderDataTable({
    filsubDat()
  })

  # Download handler for download data button saves data as csv
  output$dataDownload <- downloadHandler(
    filename = function(){
      paste0("dataframe", ".csv")
    },
    content = function(file){
      write.csv(filsubDat(), file)
    }
  )
  

  # Data Exploration Page

  # Creates plot using conditional logic and user input
  output$sumPlot <- renderPlot ({
    if(input$plotType == "Histogram") {
      hist <- ggplot(split, aes(Age)) +  labs(title = "Histogram of Age")
      if(input$switchColors){
        hist + geom_histogram(binwidth = input$histBins, aes(fill = input$fill)) +
          scale_fill_brewer(palette = input$color) + theme(legend.position="none")
      } else {
        hist + geom_histogram(binwidth = input$histBins, aes(fill = Age)) +
          theme(legend.position="none")
      }
    } else if (input$plotType == "Bar") {
      bar <- ggplot(split, aes_string(input$xaxis)) +
        labs(title = tools::toTitleCase(paste0("Bar Plot of ", input$xaxis,
                                               " (fill: ", input$fill, ")")))
      if(input$switchColors){
        bar + geom_bar(position = "dodge", aes_string(fill = input$fill)) +
          scale_fill_brewer(palette = input$color) 
      } else {
        bar + geom_bar()
      }
    } else if(input$plotType == "Box") {
      box <- ggplot(split, aes_string(input$xaxis, "Age"))+
        geom_boxplot(aes_string(fill = input$fill)) + facet_wrap(formula(paste("~", input$facet))) +
        labs(title = tools::toTitleCase(paste0("Box Plot of Age by ", input$xaxis, " (fill: ",
                                               input$fill, ", facet: ", input$facet, ")")))
      if(input$switchColors){
        box + scale_fill_brewer(palette = input$color)
      } else {
        box
      }
    }
  })


 # Creates contigency table using input and returns it to ui
 output$conTab <- renderPrint({
   if(input$editConTab == "One-Way") {
     if(input$oneWay == "Severity"){
       table("Severity" = split$Severity)
     } else if(input$oneWay == "Margin"){
       table("Margin" = split$Margin)
     } else if(input$oneWay == "Density"){
       table("Density" = split$Density)
     } else if(input$oneWay == "Shape"){
       table("Shape" = split$Shape)
     }
   } else if (input$editConTab == "Two-Way") {
     if(input$twoWay == "Severity|Margin"){
       table("Severity" = split$Severity, "Margin" = split$Margin)
     } else if(input$twoWay == "Severity|Density"){
       table("Severity" = split$Severity, "Density" = split$Density)
     } else if(input$twoWay == "Severity|Shape"){
       table("Severity" = split$Severity, "Shape" = split$Shape)
     }
   } else if (input$editConTab == "Three-Way") {
     if (input$threeWay == "Margin|Density|Severity"){
       table("Margin" = split$Margin,"Density" = split$Density, "Severity" = split$Severity)
     } else if(input$threeWay == "Margin|Shape|Severity"){
       table("Margin" = split$Margin,"Shape" = split$Shape, "Severity" = split$Severity)
     } else if(input$threeWay == "Shape|Density|Severity"){
       table("Shape" = split$Shape, "Density" = split$Density, "Severity" = split$Severity)
     } else if (input$threeWay == "Shape|Margin|Density"){
       table("Shape" = split$Shape, "Margin" = split$Margin, "Density" = split$Density)
     }
   }
 })

 # Returns basic data summary to ui
 output$generalSum<- renderPrint({
   summary(raw)
 })

  # Modeling page

  # Creates Train/Test data split based on user input
  moddat <- reactive({
    index <- sample(seq_len(nrow(split)), size = floor(input$percent*nrow(split)))
    train <- split[index,]
    test <- split[-index,]
    return(list(Train=train, Test=test))
  })

  # Generates model call from user input
  form <- reactive({
    formula((gsub(" ", "+", paste("Severity", paste0(input$modVar, collapse = " "), sep = "~"))))
  })

  # Train control function based on user input
  trCtrl <- reactive({
    trainControl(method = "repeatedcv", number = input$kfolds, repeats = 3)
  })

  # Fits GLM
  glmFit <- reactive({
   train(form(), data = moddat()[["Train"]], method = "glm", family = "binomial",
         trControl = trCtrl())
    })

  # Fits Classification Tree
  class <- reactive({
  train(form(), data =  moddat()[["Train"]], method = "rpart", trControl=trCtrl(),
        tuneGrid = data.frame(cp = seq(0, input$sliderCP, 0.001)))
  })

  # Fits Random Forest 
  rfFit <- reactive({
    train(form(), data =  moddat()[["Train"]], method = "rf", trControl=trCtrl(),
          tuneGrid = data.frame(mtry = 1:input$mtrys))
  })

  # Uses conditional logic to output fit results
  output$fitSummary <- renderPrint({
    if(input$modSelect == "GLM") {
      summary(glmFit())
    } else if(input$modSelect == "Classification Tree") {
      class()
    } else if(input$modSelect == "Random Forest"){
      rfFit()
    }
  })

  # Dynamic text to render above contigency table
  output$confuName <- renderUI({
    h6(tools::toTitleCase(paste0("Test Prediction Confusion Matrix for the ",
                                 input$resultsSelect, " model.")))
  })

  # Creats confusion matrix based on conditional logic and user input
  output$confusion <- renderPrint({
    if(input$resultsSelect == "Generalized Linear"){
      confusionMatrix(glmFit(), newdata = moddat()[["Test"]])
    } else if (input$resultsSelect == "Classification Tree") {
      confusionMatrix(class(), newdata = moddat()[["Test"]])
    } else if(input$resultsSelect == "Random Forest"){
      confusionMatrix(rfFit(), newdata = moddat()[["Test"]])
    }
  })

  # Creates classification plot and sends to ui
  output$treePlot <- renderPlot({
    rpart.plot(class()$finalModel, box.palette="GnRd", nn=TRUE)
  })

  # Creates variable importance plot for random forest model and sends to ui
  output$varImpPlot <- renderPlot({
    plot(varImp(rfFit()), top = 10)
  })

  # Generates a dynamic title for prediction page
  output$ptInfo <- renderUI({
    h3(strong(paste0("Predictions for a mass from a ", input$predAge,
                                 " year old patient with shape: ",
                                 input$predShape, ", density: ", input$predDens, ", and margin: ",
                                 input$predMarg, ".")))
  })

  # Returns prediction from glm based on user input
  output$predResGLM <- renderPrint({
    predict(glmFit(),  data.frame(Age = input$predAge, Shape = input$predShape,
                                  Margin = input$predMarg, Density = input$predDens))
  })

  # Returns prediction from classification tree based on user input
  output$predResCla<- renderPrint({
    predict(class(), data.frame(Age = input$predAge, Shape = input$predShape,
                                Margin = input$predMarg, Density = input$predDens))
  })

  # Returns prediction from random forest based on user input
  output$predResRF <- renderPrint({
    predict(rfFit(), data.frame(Age = input$predAge, Shape= input$predShape,
                             Margin = input$predMarg, Density = input$predDens))
  })
 
 }
