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
               titlePanel(
                 uiOutput("title")
               ),
               sidebarLayout(
                 sidebarPanel(


                   prettyRadioButtons(
                     inputId = "buttonPlotType",
                     label = "Select plot type:", 
                     choices = c("Bar Plot", "2-D Count", "Histogram", "Box Plot"),
                     inline = TRUE, 
                     status = "info",
                     fill = FALSE,
                   ),
                   conditionalPanel(condition = "input.buttonPlotType" == "Histogram",
                                    checkboxInput(inputId = "rem",
                                                  label = "Also change symbol based on REM sleep?")),

                   
                   awesomeRadio(
                     inputId = "radioBar",
                     label = "Select variable for x-axis", 
                     choices = c("Severity", "Shape", "Margin", "Density"),
                     selected = "A",
                     inline = FALSE, 
                     checkbox = FALSE
                   ),
                   pickerInput(inputId = 'xVarPicker',
                               label = 'X Variable',
                               choices = c("Severity", "Margin", "Shape", "Density")),
                   
                   pickerInput(inputId = 'fillVarPicker',
                               label = 'Fill Variable',
                               choices = c("None", "Severity", "Margin", "Shape", "Density"),
                               selected = "None"),
                   
                   pickerInput(inputId = 'facetVarPicker',
                               label = 'Facet Variable',
                               choices = c("None", "Severity", "Margin", "Shape", "Density"),
                               selected = "None"),
 
 
                     sliderInput("bins", "Number of bins:",
                                 min = 1, max = 50, value = 30),
                   
                     numericInput("maxBins", label = "Set Maximum Number of Bins",
                                  value = 50, min = 1, max = 100),
# 
#                    
#                    sliderInput(inputId = 'binSlider',
#                                label = 'Bin Width Selector',
#                                value = 5,
#                                min = 0, max = 100),

                   downloadButton("downloadPlot", label = "Download Current Plot")
                   
                 ),
                 
                 mainPanel(
                   fluidPage(
                     uiOutput("title")



                   )
               ))
               ),
      tabPanel("Numeric Summary",
               tags$br(),
               sidebarLayout(
                 sidebarPanel(
                   h3("Density Plot Panel"),
                   tags$br(),
                   pickerInput(
                     inputId = "tablePicker",
                     label = "Select Variables for Contigency Table",
                     choices = c("Severity", "Margin", "Shape", "Density"),
                     multiple = TRUE,
                     options =  list(
                       "max-options" = 3
                     )
                   )
                   
                 ),
                 mainPanel(
                   verbatimTextOutput("summary"),
                   verbatimTextOutput("table")
                 )
                 
                 
                 
               )),
      tabPanel("Data Page",
               tags$br(),
               sidebarLayout(
                 sidebarPanel(
                   switchInput("switchRemoveNA",
                               label = "Remove NA",
                               value = FALSE
                   ),
                   
                   h3("Filtering options"),
                   setSliderColor(c("#2c3e50 ", "#2c3e50"), c(1, 2)),
                   sliderInput("sliderAgeFilter",
                     label = "Age Range",
                     min = 18,
                     max = 96,
                     value = c(18, 96)
                   ),
                   pickerInput( "selectShapeFilter",
                                "Select Shape",
                                choices = c("round", "oval", "lobular", "irregular"),
                                options = list(
                                  `actions-box` = TRUE), 
                                multiple = TRUE
                   ),

                   pickerInput( "selectMarginFilter",
                                "Select Margin",
                     choices = c("circumscribed","microlobulated", "obscured", "ill-defined",
                                 "spiculated"),
                     options = list(
                       `actions-box` = TRUE), 
                     multiple = TRUE
                   ),
                   pickerInput("pickDensityFilter",
                                "Select Density",
                                choices = c("high", "iso", "low", "fat-containing"),
                               options = list(
                                 `actions-box` = TRUE), 
                               multiple = TRUE
                   ),

                   tags$br(),

                h3("Subsetting options"),
                pickerInput("pickerSubset",
                            "Select variables for subset",
                            choices = c("Severity", "Margin", "Shape", "Density"),
                            options = list(
                              `actions-box` = TRUE), 
                            multiple = TRUE
                ),

                actionButton("actionData", "Apply Selections", class = "btn btn-success"),
                downloadButton("downloadData", label = "Download Data")
               ),
               mainPanel(
                 tags$br(),
                 dataTableOutput("myTable"),
               )
               ))
               
    )
  ),
  
  
  tabPanel("Modeling",
           tabsetPanel(
             type = "tabs",
             tabPanel("Information",
                      fluidPage(
                                 h3("GLM"),
                                 p("p creates a paragraph of text."),
                                 withMathJax("$$\\alpha+\\beta$$"),
                                 tags$br(),
                                 h3("Classification Tree"),
                                 p("p creates a paragraph of text."),
                                 tags$br(),
                                 h3("Random Forest"),
                                 p("p creates a paragraph of text."),
                      )
                      ),
             tabPanel("Fitting",
                      sidebarLayout(
                                   sidebarPanel(
                                     knobInput(
                                       inputId = "knobSplit",
                                       label = "Split Percentage of Training Data",
                                       value = 70,
                                       min = 10,
                                       max = 90,
                                       displayPrevious = TRUE,
                                       lineCap = "round",
                                       fgColor = "#428BCA",
                                       inputColor = "#428BCA"
                                     ),
                                     numericInput("kfolds", "k-folds for Cross-Validation", 5, min = 5, max = 10
                                                  , step = 5),

                                     setSliderColor(c("#2c3e50 ", "#2c3e50"), c(1, 2)),
                                     sliderInput("sliderCP",
                                                 label = "Complexity Paramater Range",
                                                 min = 0,
                                                 max = .5,
                                                 value = c(0, .5)
                                     ),
                                     sliderInput("mtrys",
                                                 label = "mtry Range",
                                                 min = 1,
                                                 max = 30,
                                                 value = c(1, 30)
                                     ),
                                   pickerInput("pickerModel",
                                               "Select desired model predictors",
                                               choices = c("Shape", "Margin", "Density", "Age",
                                                           "Shape:Age", "Margin:Age", "Density:Age",
                                                           "I(Age^2)", "Shape:I(Age^2)",
                                                           "Margin:I(Age^2)", "Density:I(Age^2)"),
                                               options = list(
                                                 `actions-box` = TRUE), 
                                               multiple = TRUE),
                                   
                                   actionButton("actionFit", "Apply Selections",
                                                class = "btn btn-success")
                                   ),


                                     
                                   mainPanel(
                                     tabsetPanel(
                                       type = "tabs",
                                       tabPanel("GLM",
                                                
                                       textOutput("resultsGLM"),
                                       plotOutput("varImpGLM"),
                                       tableOutput("resultstableGLM")
                                       ),
                                       tabPanel("Classification Tree",
                                                textOutput("resultsClass"),
                                                plotOutput("plotClass"),
                                                plotOutput("varImpClass"),
                                                plotOutput("rpartPlotClass"),
                                                tableOutput("resultstableClass")
                                                ),
                                       tabPanel("Random Forest", 
                                                textOutput("resultsClass"),
                                                plotOutput("varImpRF"),
                                                tableOutput("resultstableClass")
                                                )
                                     ),
                                   )
                                 )),
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
