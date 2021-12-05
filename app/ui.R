library(shinydashboard)
library(markdown)
library(shinythemes)

# Define UI for random distribution app ----
ui <-navbarPage("Navbar!", theme = shinytheme("paper"),
                tabPanel("About",includeMarkdown("about.md")
                         ),

                  tabPanel("Data Exploration",
                           sidebarLayout(
                             sidebarPanel(
                               radioButtons("plotType", "Plot type",
                                            c("Scatter"="p", "Line"="l")
                                            )
                               ),
                             mainPanel(
                               p("p creates a paragraph of text."),
                               p("p creates a paragraph of text."),
                               p("A new p() command starts a new paragraph. Supply a style attribute to change the format of the entire paragraph.", style = "font-family: 'times'; font-si16pt"),
                               strong("strong() makes bold text."),
                               em("em() creates italicized (i.e, emphasized) text."),
                               br(),
                               code("code displays your text similar to computer code"),
                               div("div creates segments of text with a similar style. This division of text is all blue because I passed the argument 'style = color:blue' to div", style = "color:blue"),
                               br(),
                               p("span does the same thing as div, but it works with",
                                 span("groups of words", style = "color:blue"),
                                 "that appear inside a paragraph.")
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
                                          withMathJax(helpText("Some math here $$\\alpha+\\beta$$")),
                                          includeMarkdown("about.md")),
                                 tabPanel("Model Fitting",
                                          plotOutput("plot")),
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
                               mainPanel()
                               )
                           )

)
