library(shinydashboard)
library(markdown)
library(shinythemes)
library(shinyWidgets)

server <-function(input, output, session) {
    output$bar <- renderPlot ({
        g <- ggplot(data, aes(Severity))
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

    output$downloadPlot <- downloadHandler(
        filename = "plot.png" ,
            content = function(file) {
                png(file = file)
                p2()
                dev.off()
            })
    
    output$table <- renderPrint({

        table(input$tablePicker)
    })
    output$summmary <- renderPrint({
        
        summary(data)
    })
    
    output$downloadData <- downloadHandler(
          filename = function() {
            paste('data-', Sys.Date(), '.csv', sep='')
          },
          content = function(con) {
            write.csv(data, con)
          }
        )
    }
}