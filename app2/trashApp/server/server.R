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

  getRawData <- reactive({ raw })
  
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

  output$myTable <- renderDataTable({
    filsubDat()
  })

  output$dataDownload <- downloadHandler(
    filename = function(){
      paste0("dataframe", ".csv")
    },
    content = function(file){
      write.csv(filsubDat(), file)
    }
  )
  

  
  ### graph page
  # switchColors
  # color
  # plotType
  # fill
  # histBins
  # maxBins
  # xaxis
  # facet
  # 
  # 
  # downloadPlot #download handler plot
  
  # output$sumPlot <- renderPlot ({
  #   if(input$plotType == "Histogram") {
  #     hist <- ggplot(graphData, aes(Age))
  #     if(input$switchColors) {
  #       hist + geom_histogram(binwidth = input$histBins, aes(fill = Age)) +
  #         scale_fill_brewer(palette = input$color)
  #     } else {
  #       hist + geom_histogram(binwidth = input$histBins, aes(fill = Age)) 
  #     }
  #   } else if (input$plotType == "Bar"){
  #     bar <- ggplot(m, aes(input$ fill = Severity))
  #     
  #   }
  # })
  
  
  
  
  # ## numeric page
  # 
  # render plot "sumPlot"
  # 
  # conPick
  # conTab # action button
  # 
  # render table "con"
  # render output "summary"
  # 
  # ## model info
  # 
  # 
  # 
  # ## model fit

  # Train/Test data split
  moddat <- reactive({
    index <- sample(seq_len(nrow(split)), size = floor(input$percent*nrow(split)))
    train <- split[index,]
    test <- split[-index,]
    return(list(Train=train, Test=test))
  })

  form <- reactive({
    formula((gsub(" ", "+", paste("Severity", paste0(input$modVar, collapse = " "), sep = "~"))))
  })
  

  
  trCtrl <- reactive({
    trainControl(method = "repeatedcv", number = input$kfolds, repeats = 3)
  })
  

  glmFit <- reactive({
   train(form(), data = moddat()[["Train"]], method = "glm", family = "binomial",
         trControl = trCtrl())
    })
  
  class <- reactive({
  train(form(), data =  moddat()[["Train"]], method = "rpart", trControl=trCtrl(),
        tuneGrid = data.frame(cp = seq(0, input$sliderCP, 0.001)))
  })
  
  rfFit <- reactive({
    train(form(), data =  moddat()[["Train"]], method = "rf", trControl=trCtrl(),
          tuneGrid = data.frame(mtry = 1:input$mtrys))
  })


  output$fitSummary <- renderPrint({
    if(input$modSelect == "GLM") {
      summary(glmFit())
    } else if(input$modSelect == "Classification Tree") {
      class()
    } else if(input$modSelect == "Random Forest"){
      rfFit()
    }
  })
  
  # output$confusion <- renderPrint({
  #   if(input$resultsSelect == "GLM"){
  #     if(input$glmResults == "Prediction Confusion Matrix"){
  #       confusionMatrix(glmFit(), newdata = moddat()[["Test"]])
  #     } else if(input$cglmResults == "Accuracy"){
  #       glmFit()
  #     }
  #   } else if(input$resultsSelect == "Classification Tree") {
  #     if(input$classResults == "Prediction Confusion Matrix") {
  #       confusionMatrix(class(), newdata = moddat()[["Test"]])
  #     }
  #   }else if(input$resultsSelect == "Random Forest") {
  #     confusionMatrix(rfFit(), newdata = moddat()[["Test"]])
  #   }
  # })
  # 
  
  output$confuName <- renderUI({
    h6(tools::toTitleCase(paste0("Test Prediction Confusion Matrix for the ",
                                 input$resultsSelect, " model.")))
  })
  
  
  output$confusion <- renderPrint({
    if(input$resultsSelect == "GLM"){
      confusionMatrix(glmFit(), newdata = moddat()[["Test"]])
    } else if (input$resultsSelect == "Classification Tree") {
      confusionMatrix(class(), newdata = moddat()[["Test"]])
    } else if(input$resultsSelect == "Random Forest"){
      confusionMatrix(rfFit(), newdata = moddat()[["Test"]])
    }
  })

  
  
  output$treePlot <- renderPlot({
    rpart.plot(class()$finalModel, box.palette="GnRd", nn=TRUE)
  })
  output$varImpPlot <- renderPlot({
    plot(varImp(rfFit()), top = 10)
  })
  

  output$ptInfo <- renderUI({
    h3(strong(paste0("Predictions for a mass from a ", input$predAge,
                                 " year old patient with shape: ",
                                 input$predShape, ", density: ", input$predDens, ", and margin: ",
                                 input$predMarg, ".")))
  })
  
  
  output$predResGLM <- renderPrint({
    predict(glmFit(),  data.frame(Age = input$predAge, Shape = input$predShape,
                                  Margin = input$predMarg, Density = input$predDens))
  })
  
  output$predResCla<- renderPrint({
    predict(class(), data.frame(Age = input$predAge, Shape = input$predShape,
                                Margin = input$predMarg, Density = input$predDens))
  })
  
  output$predResRF <- renderPrint({
    predict(rfFit(), data.frame(Age = input$predAge, Shape= input$predShape,
                             Margin = input$predMarg, Density = input$predDens))
  })
  


 }
