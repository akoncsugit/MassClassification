library(shinydashboard)
library(markdown)
library(shinythemes)
library(shinyWidgets)

# Define UI for random distribution app ----
ui <-navbarPage("Navbar!", theme = shinytheme("paper"),
                tabPanel("About",includeMarkdown("about.md")
                         ),

                  tabPanel("Data Exploration",
                           tabsetPanel(
                             tabPanel("Graphical Summary",
                                      p("p creates a paragraph of text."),
                                      plotOutput("bar"),
                                      dropdown(
                                        
                                        tags$h6("List of Input"),
                                        switchInput(
                                          inputId = "selectTheme",
                                          label = "Select Theme?", 
                                          onLabel = "Yes",
                                          offLabel = "No",
                                        ),
                                        pickerInput(inputId = 'xcol2',
                                                    label = 'X Variable',
                                                    choices = names(iris),
                                                    options = list(`style` = "btn-info")),
                                        
                                        pickerInput(inputId = 'ycol2',
                                                    label = 'Y Variable',
                                                    choices = names(iris),
                                                    selected = names(iris)[[2]],
                                                    options = list(`style` = "btn-warning")),
                                        
                                        # sliderInput(inputId = 'clusters2',
                                        #             label = 'Cluster count',
                                        #             value = 3,
                                        #             min = 1, max = 9),
                                        
                                        prettyRadioButtons(
                                          inputId = "Id037",
                                          label = "Choose:", 
                                          choices = c("Click me !", "Me !", "Or me !"),
                                          inline = TRUE, 
                                          status = "danger",
                                          fill = TRUE
                                        ),
                                        
                                        style = "unite", icon = icon("gear"),
                                        status = "danger", width = "300px",
                                        animate = animateOptions(
                                          enter = animations$fading_entrances$fadeInLeftBig,
                                          exit = animations$fading_exits$fadeOutRightBig
                                        )
                                      ),
                                      
                                      plotOutput("bar"),
                                      includeMarkdown("about.md")
                                      ),
                             tabPanel("Numeric Summary")
                           )
                           


                               # p("p creates a paragraph of text."),
                               # p("A new p() command starts a new paragraph. Supply a style attribute to change the format of the entire paragraph.", style = "font-family: 'times'; font-si16pt"),
                               # strong("strong() makes bold text."),
                               # em("em() creates italicized (i.e, emphasized) text."),
                               # br(),
                               # code("code displays your text similar to computer code"),
                               # div("div creates segments of text with a similar style. This division of text is all blue because I passed the argument 'style = color:blue' to div", style = "color:blue"),
                               # br(),
                               # p("span does the same thing as div, but it works with",
                               #   span("groups of words", style = "color:blue"),
                               #   "that appear inside a paragraph.")

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
                                          withMathJax(helpText("Some math here $$\\alpha+\\beta$$")),
                                          includeMarkdown("about.md")),
                                 tabPanel("Model Fitting",
                                          plotOutput("plot")),
                                 tabPanel("Prediction")
                                 ),
                               )
                             )
                           ),
# tabPanel("Test",
#          tabsetPanel("Chicken",
#            tabPanel(
#              sidebarLayout(
#                sidebarPanel(
#                  radioButtons("plotType", "Plot type", c("Scatter"="p", "Line"="l")
#                )
#                ),
#                mainPanel(includeMarkdown("about.md"))
#              )
#            )
#          ),
#            tabsetPanel("Bread",
#                        tabPanel(
#                          sidebarLayout(
#                            sidebarPanel(
#                              radioButtons("plotType", "Plot type", c("HI"="p", "LO"="l")
#                              )
#                            ),
#                            mainPanel(includeMarkdown("about.md"))
#                          )
#                        )
#            )
# ),
# 





                  tabPanel("Data",
                           sidebarLayout(
                               sidebarPanel(
                                   radioButtons("plotType", "Hi",
                                                c("Scatter"="p", "Line"="l")
                                   )
                               ),
                               mainPanel()
                               )
                           )

)
