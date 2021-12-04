library(shinydashboard)
library(markdown)
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
                                        mainPanel(
                                            tabsetPanel(
                                                tabPanel("Tab 1", 
                                                         plotOutput("plot")),
                                                tabPanel("Tab 2")
                                            ),
                                            
                                        )
                                    )
                           ),
                           tabPanel("Data Exploration",
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
                                            ),
                                            
                                        )
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
                                   ),
                                   
                               )
                           )
                  ),

                           navbarMenu("More",
                                      tabPanel("Table",
                                               DT::dataTableOutput("table")
                                      ),
                                      tabPanel("About",
                                               fluidRow(
                                                   column(6,
                                                          
                                                   ),
                                                   column(3,
                                                          img(class="img-polaroid",
                                                              src=paste0("http://upload.wikimedia.org/",
                                                                         "wikipedia/commons/9/92/",
                                                                         "1919_Ford_Model_T_Highboy_Coupe.jpg")),
                                                          tags$small(
                                                              "Source: Photographed at the Bay State Antique ",
                                                              "Automobile Club's July 10, 2005 show at the ",
                                                              "Endicott Estate in Dedham, MA by ",
                                                              a(href="http://commons.wikimedia.org/wiki/User:Sfoskett",
                                                                "User:Sfoskett")
                                                          )
                                                   )
                                               )
                                      )
                           )
                )