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
    index <- sample(seq_len(nrow(split)), size = floor(.7*nrow(split)))
    train <- split[index,]
    test <- split[-index,]
    return(list(Train=train, Test=test))
  })

  form <- reactive({

  })
  
  

  
  trCtrl <- reactive({
    trainControl(method = "repeatedcv", number = 3, repeats = 2)
  })
  
  # glmFit <- train(form = Severity ~ ., data = train, method = "glm", family = "binomial",
  #                 trControl = trCtrl)
  # 
  glmFit <- reactive({
   train(Severity ~ input$modVar, data = moddat()[["Train"]], method = "glm", family = "binomial",
         trControl = trCtrl())
    })
  
  # class <- eventReactive(input$actionFit,{
  #   class<- train(Severity ~ form, data = train(), method = "rpart", trControl=trCtrl(),
  #                 tuneGrid = data.frame(cp = seq(0, input$sliderCP, 0.001)))
  # })
  
  # rfFit <- eventReactive(input$actionFit, {
  #   rfFit <-train(Severity ~ form, data = train(), method = "rf", trControl=trCtrl(),
  #         tuneGrid = data.frame(mtry = 1:input$mtrys))
  # })
  # 
  
  output$summaryGLM <- renderPrint({
    summary(glmFit())
    # if (input$predbutton){
    #   summary(glmFit())
    # }
  })
  
  # output$resultsRF <- renderPrint({
  #   if (input$predbutton){
  #   rfFit()
  #   }
  # })
  # 
  # output$resultsRF <- renderPrint({
  #   if (input$predbutton){
  #   class()
  #   }
  # })
  # 
  # output$varImp <- renderPlot({
  #   plot(varImp(rfFit()), top = 10)
  # })
  # 
  # output$classPlot <- renderPlot({
  #   rpart.plot(class()$finalModel, box.palette="GnRd", nn=TRUE)
  # })
  # 
  # output$confuGLM<- renderPrint({
  #   confusionMatrix(glmFit(), newdata = test())
  # })
  # 
  # output$confuClass <- renderPrint({
  #   confusionMatrix(class(), newdata = test())
  # })
  # 
  # output$confuRF <- renderPrint({
  #   confusionMatrix(rfFit(), newdata = test())
  # })
  # 
  # 
  # output$fitResults <- renderDataTable({
  #   data.frame("GLM" = confuGLM()$table, "Classification Tree" = confuClass()$table,
  #              "Random Forest" = confuRF()$table)
  # })
  
  newpt <- reactive({ c(Age = input$predAge, Shape= input$predShape,
                        Margin = input$predMargin, Density = input$predDens)})
    


  output$ptInfo <- renderUI({
    h3(paste0("Predictions for a mass from a ", input$predAge,
                                 " year old patient of shape: ",
                                 input$predShape, ", density: ", input$predDens, ", and margin: ",
                                 input$predMarg, "."))
  })
  
  output$predResults <- renderDataTable({
    data.frame(`GLM prediction` = predict(glmFit(), newpt()),
               `Classification Tree prediction` = predict(class(), newpt()),
               `Random Forest Prediction` = predict(rfFit(), newpt())
               )
  })
  
 }
