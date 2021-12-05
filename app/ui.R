library(shinydashboard)
library(markdown)
library(shinythemes)

# Define UI for random distribution app ----
ui <-  navbarPage("Navbar!",
                  theme = shinytheme("superhero"),
                  tabPanel("About",
                           sidebarLayout(
                             sidebarPanel(
                               radioButtons("plotType", "Plot type",
                                            c("Scatter"="p", "Line"="l")
                                            )
                               ),
                             mainPanel()
                             )
                           ),
                  tabPanel("Data Exploration",
                           sidebarLayout(
                             sidebarPanel(
                               radioButtons("plotType", "Plot type",
                                            c("Scatter"="p", "Line"="l")
                                            )
                               ),
                             mainPanel()
                             )
                           ),
                  tabPanel("Model",
                           sidebarLayout(
                             sidebarPanel(
                               radioButtons("plotType", "Plot type",
                                            c("Scatter"="p", "Line"="l")
                                            )
                               ),
                             mainPanel(
                               tabsetPanel(
                                 tabPanel("Model", 
                                          plotOutput("plot")),
                                 tabPanel("Model Fitting"),
                                 tabPanel("Prediction")
                                 ),
                               )
                             )
                           ),
                  tabPanel("Data",
                           sidebarLayout(
                               sidebarPanel(
                                   radioButtons("plotType", "Plot type",
                                                c("Scatter"="p", "Line"="l")
                                   )
                               ),
                               mainPanel(
                                   tabsetPanel(
                                       tabPanel("Tab 1", 
                                                plotOutput("plot")),
                                       tabPanel("Tab 2")
                                       )
                                   )
                               )
                           )
                  )