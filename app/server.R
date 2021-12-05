library(shinydashboard)
library(markdown)
library(shinythemes)
library(shinyWidgets)

server <-function(input, output, session) {
    output$bar <- renderPlot ({
        g + geom_bar()
        #theme <- paste)
        # if(theme == "gray", )
        # theme_gray() 
        # theme_bw()
        # theme_linedraw()
        # theme_light()
        # theme_dark() 
        # theme_minimal()
        # theme_classic()
        # theme_void()
    })
    
    output$plot <- renderPlot({
        plot(cars, type=input$plotType)
    })
    
    output$summary <- renderPrint({
        summary(cars)
    })
    
    output$table <- DT::renderDataTable({
        DT::datatable(cars)
    })
}