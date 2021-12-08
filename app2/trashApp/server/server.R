########################################################################;
#Author:  Ashley Ko                                                     ;
#Title: ST558 Project 3 server                                          ;
#Program Purpose: To complete ST558 project 3 requirements              ;
#Date: 2021/12/05                                                       ;
########################################################################;


# Required packages
library(caret)
library(rpart.plot) 


server <- function(session, input, output) {
  
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
  


  # ### graph page
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
  # 
  # t + labs(x = "New x axis label", y = "New y axis label",
  

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

 output$generalSum<- renderPrint({
   summary(raw)
 })

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
  

  
  output$confuName <- renderUI({
    h6(tools::toTitleCase(paste0("Test Prediction Confusion Matrix for the ",
                                 input$resultsSelect, " model.")))
  })
  
  
  output$confusion <- renderPrint({
    if(input$resultsSelect == "Generalized Linear"){
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
