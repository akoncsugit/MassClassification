##########################################
####   Shiny ui                       ####
##########################################
library(shinyWidgets)
library(shiny)
library(shinyWidgets)
library(shiny)
library(plotly)
library(shinythemes)
library(DT)
library(rsconnect)
# ------------------
# Main title section
# ------------------

ui <- navbarPage(
  "Mammographic Masses",
  theme = shinytheme("cosmo"),
  tabPanel(
    "About",
    fluidPage(includeMarkdown("about.md"))

),
    

  tabPanel("Data Exploration",
    tabsetPanel(
      type ="tabs",
      tabPanel("Graphic Summary",
               tags$br(),
               sidebarLayout(
                 sidebarPanel(
                   h3("Data by Year"),
                   tags$br(),
                   prettySwitch("switchTheme", label = "Select plot theme?", value = FALSE),

                   pickerInput(
                     inputId = "plotThemePicker",
                     label = "Select ggplot theme", 
                     choices = c("gray", "bw", "linedraw", "light", 
                                 "dark", "minimal", "classic", "void")
                   ),
                   prettySwitch("switchColors", label = "Select plot color palette?", value = FALSE),
                   pickerInput(
                     inputId = "plotColorPicker",
                     label = "Select color palate", 
                     choices = c("Accent", "Dark2", "Paired", "Set1", 
                                 "Set2", "Set3", "Pastel1", "Pastel2")
                   ),
                   prettyRadioButtons(
                     inputId = "buttonPlotType",
                     label = "Select plot type:", 
                     choices = c("Bar Plot", "2-D Count", "Histogram", "Box Plot"),
                     inline = TRUE, 
                     status = "info",
                     fill = FALSE,
                   ),
                   # awesomeRadio(
                   #   inputId = "radioBar",
                   #   label = "Select variable for x-axis", 
                   #   choices = c("Severity", "Shape", "Margin", "Density"),
                   #   selected = "A",
                   #   inline = FALSE, 
                   #   checkbox = FALSE
                   # )
                 ),
                 
                 mainPanel(
                   fluidPage(
                     tags$h2("Dropdown Button"),
                     br(),
                     dropdown(
                       
                       tags$h3("Graph Options"),
                       
                       pickerInput(inputId = 'xVarPicker',
                                   label = 'X Variable',
                                   choices = c("Severity", "Margin", "Shape", "Density"),
                                   options = list(`style` = "btn-error")),
                       
                       pickerInput(inputId = 'fillVarPicker',
                                   label = 'Fill Variable',
                                   choices = c("None", "Severity", "Margin", "Shape", "Density"),
                                   selected = "None",
                                   options = list(`style` = "btn-error")),

                       pickerInput(inputId = 'facetVarPicker',
                                   label = 'Facet Variable',
                                   choices = c("None", "Severity", "Margin", "Shape", "Density"),
                                   selected = "None",
                                   options = list(`style` = "btn-error")),

                       sliderInput(inputId = 'binSlider',
                                   label = 'Binwidth Selector',
                                   value = 5,
                                   min = 0, max = 100),
                       
                       style = "unite", icon = icon("gear"),
                       status = "danger", width = "300px",
                       animate = animateOptions(
                         enter = animations$fading_entrances$fadeInLeftBig,
                         exit = animations$fading_exits$fadeOutRightBig
                       )
                     ),
                     
                     plotOutput(outputId = 'plot2')
                   )
               ))
               ),
      tabPanel("Numeric Summary",
               tags$br(),
               sidebarLayout(
                 sidebarPanel(
                   h3("Density Plot Panel"),
                   tags$br(),
                   selectInput(
                     "selectvar",
                     label = "Choose a variable to display",
                     choices = c(
                       "Basic Montly Salary (Median)" = "basic_monthly_median",
                       "Fulltime Employment Rate" = "employment_rate_ft_perm"
                     ),
                     selected = "basic monthly mean"
                   ),
                   
                   checkboxGroupInput(
                     "checkGroup",
                     label = "Select University",
                     choices = list(
                       "Nanyang Technological University" = "Nanyang Technological University",
                     ),
                     selected = list(
                       "Singapore University of Technology and Design" = "Singapore University of Technology and Design"
                     )
                   ),
                 ),
                 mainPanel(
                   h3("Distribution"),
                   plotlyOutput(outputId = "densityPlot"),
                 )
               )),
      tabPanel("Data Page")
    )
  ),
  
  
  tabPanel("Modeling",
           tabsetPanel(
             type = "tabs",
             tabPanel("Information",
                      fluidPage( withMathJax(helpText("Some math here $$\\alpha+\\beta$$")),

                      )
                      ),
             tabPanel("Fitting",
                      tabsetPanel(
                        type = "tabs",
                        tabPanel("Generalized Linear Model",
                                 tabsetPanel(              sidebarLayout(
                                   sidebarPanel(
                                     h3("Data by Year"),
                                     tags$br(),
                                     selectInput(
                                       "checkYear",
                                       "Select Year",
                                       choices = list("2018", "2017", "2016",
                                                      "2015", "2014", "2013"),
                                       selected = "2018"
                                     )
                                   ),
                                   
                                   mainPanel(
                                     tabsetPanel(
                                       type = "tabs",
                                       tabPanel("Ranking", tableOutput("datahead")),
                                       tabPanel("No. of Graduates", plotOutput(outputId = "piePlot"))
                                     ),
                                   )
                                 ))),
                        tabPanel("Classification Tree",
                                 tabsetPanel(              sidebarLayout(
                                   sidebarPanel(
                                     h3("Data by Year"),
                                     tags$br(),
                                     selectInput(
                                       "checkYear",
                                       "Select Year",
                                       choices = list("2018", "2017", "2016",
                                                      "2015", "2014", "2013"),
                                       selected = "2018"
                                     )
                                   ),
                                   
                                   mainPanel(
                                     tabsetPanel(
                                       type = "tabs",
                                       tabPanel("Ranking", tableOutput("datahead")),
                                       tabPanel("No. of Graduates", plotOutput(outputId = "piePlot"))
                                     ),
                                   )
                                 ))),
                        tabPanel("Random Forest",                       tabsetPanel(              sidebarLayout(
                          sidebarPanel(
                            h3("Data by Year"),
                            tags$br(),
                            selectInput(
                              "checkYear",
                              "Select Year",
                              choices = list("2018", "2017", "2016",
                                             "2015", "2014", "2013"),
                              selected = "2018"
                            )
                          ),
                          
                          mainPanel(
                            tabsetPanel(
                              type = "tabs",
                              tabPanel("Ranking", tableOutput("datahead")),
                              tabPanel("No. of Graduates", plotOutput(outputId = "piePlot"))
                            ),
                          )
                        ))))),
             tabPanel("Prediction",
                      tabsetPanel(              sidebarLayout(
                        sidebarPanel(
                          h3("Data by Year"),
                          tags$br(),
                          selectInput(
                            "checkYear",
                            "Select Year",
                            choices = list("2018", "2017", "2016",
                                           "2015", "2014", "2013"),
                            selected = "2018"
                          )
                        ),
                        
                        mainPanel(
                          tabsetPanel(
                            type = "tabs",
                            tabPanel("Ranking", tableOutput("datahead")),
                            tabPanel("No. of Graduates", plotOutput(outputId = "piePlot"))
                          ),
                        )
                      )))
           )
             )
           )
