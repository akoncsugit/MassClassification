library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(markdown)
library(DT)
library(tidyverse)

dashboardPage(skin = "blue",
              
              # Add Title
              dashboardHeader(title = "Mammographic Mass Classification Using Supervised Learning Models", titleWidth = 700),
              
              # Define Sidebar Items
              dashboardSidebar(sidebarMenu(
                menuItem("About", tabName = "Tab1"),
                menuItem("Data", tabName = "Tab2"),
                menuItem("Data Exploration: Graphical", tabName = "Tab3"),
                menuItem("Data Exploration: Numeric", tabName = "Tab4"),
                menuItem("Modeling", tabName = "Tab5")
              )),
              
              # Define the Body of the APP
              dashboardBody(
                tabItems(
                  #####################First Tab Content####################
                         tabItem(tabName = "Tab1",
                                 includeMarkdown("about.md")
                         ),
                         
                         #####################Second Tab Content####################
                         tabItem(tabName = "Tab2", fluidPage(
                           fluidRow(
                             switchInput("switchRemoveNA",
                                         label = "Remove NA",
                                         value = FALSE
                             )),
                           fluidRow(
                             h3("Filtering options"),
                             # Filtering
                             column(4,
                                    sliderInput("sliderAgeFilter",
                                                label = "Age",
                                                min = 18,
                                                max = 96,
                                                value = 40
                                    ),
                             ),
                             
                             column(4,
                                    pickerInput( "selectShapeFilter",
                                                 "Select Shape",
                                                 choices = c("round", "oval", "lobular", "irregular"),
                                                 options = list(
                                                   `actions-box` = TRUE), 
                                                 multiple = TRUE
                                    )
                             ),
                             column(4,
                                    pickerInput( "selectMarginFilter",
                                                 "Select Margin",
                                                 choices = c("circumscribed","microlobulated", "obscured", "ill-defined",
                                                             "spiculated"),
                                                 options = list(
                                                   `actions-box` = TRUE), 
                                                 multiple = TRUE
                                    ),
                             ),
                           ),
                           fluidRow(
                             column(4,
                                    pickerInput("pickDensityFilter",
                                                "Select Density",
                                                choices = c("high", "iso", "low", "fat-containing"),
                                                options = list(
                                                  `actions-box` = TRUE), 
                                                multiple = TRUE
                                    ),
                             ),
                             column(4,
                                    pickerInput("pickerSubset",
                                                "Select variables for subset",
                                                choices = c("Severity", "Margin", "Shape", "Density"),
                                                options = list(
                                                  `actions-box` = TRUE), 
                                                multiple = TRUE
                                    )
                             ),
                             column(4,
                                    actionButton("actionData", "Apply Selections", class = "btn btn-success"),
                                    downloadButton(outputId = "down_dat", label = "Download Data")
                             )
                             
                           )),
                           
                           fluidRow(
                             #Show the data frame
                             dataTableOutput("dataf")
                           )
                           ),
                  #####################Third Tab Content####################
                  tabItem(tabName = "Tab3",
                    fluidRow(
                      column(3,
                             prettyRadioButtons(
                               inputId = "buttonPlotType",
                               label = "Select plot type:", 
                               choices = c("Bar", "Box"),
                               inline = TRUE, 
                               status = "info",
                               fill = FALSE,
                             )),
                      column(3,
                             conditionalPanel(condition = "input.buttonPlotType == Box",
                                              pickerInput(inputId = 'facetVarPicker',
                                                          label = 'Facet Variable',
                                                          choices = c("None", "Severity", "Margin",
                                                                      "Shape", "Density"),
                                                          selected = "None"))),
                      column(3,
                             awesomeRadio(
                               inputId = "radioBar",
                               label = "Select variable for x-axis", 
                               choices = c("Severity", "Shape", "Margin", "Density"),
                               selected = "A",
                               inline = FALSE, 
                               checkbox = FALSE
                             )),
                      column(3,
                             pickerInput(inputId = 'fillVarPicker',
                                         label = 'Fill Variable',
                                         choices = c("None", "Severity", "Margin", "Shape", "Density"),
                                         selected = "None")
                             )
                    ),
                    fluidRow(
                      column(9, plotOutput("plot"),

                      column(3,
                             downloadButton("downloadPlot", label = "Download Current Plot")
                      )
                    ),
                  )
                  ),
                  tabItem(tabName = "Tab4",
                    fluidRow(
                      column(3, 
                             pickerInput(
                        inputId = "tablePicker",
                        label = "Select Variables for Contigency Table",
                        choices = c("Severity", "Margin", "Shape", "Density"),
                        multiple = TRUE,
                        options =  list(
                          "max-options" = 3
                        ))
                      ),
                      column(9,verbatimTextOutput("table")),
                    ),
                    fluidRow(
                      verbatimTextOutput("summary")
                    )

                    )
                  ),
                  tabItem(tabName = "Tab5",
                                  
                                  #add in latex functionality if needed
                                  withMathJax(),
                                  
                                  tabsetPanel(
                                    tabPanel("Modeling Info",
                                             fluidRow(
                                               column(width = 4,
                                                      h1("Generalized Linear Model"),
                                                      ),
                                         column(width = 4,
                                                h1("Classification Tree"),)
                                         ),
                                         column(width = 4,
                                                h1("Random Forest"),
                                                )
                                         )
                                             )
                                    ),
                                    tabPanel("Model Fitting",
                                             fluidRow(
                                               column(4,
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
                                                      )
                                                      ),
                                               column(4,
                                                 numericInput("kfolds", "k-folds for Cross-Validation", 5, min = 5, max = 10
                                                              , step = 5)
                                               ),
                                               column(4,
                                                      sliderInput("sliderCP",
                                                                  label = "Complexity Paramater Range",
                                                                  min = .1,
                                                                  max = .5,
                                                                  value = .1
                                                      )
                                                      )),
                                             fluidRow(
                                               column(4,
                                                      sliderInput("mtrys",
                                                                  label = "mtry Range",
                                                                  min = 2,
                                                                  max = 30,
                                                                  value = 15
                                                      )
                                               ),
                                               column(4,
                                                      pickerInput("pickerModel",
                                                                  "Select desired model predictors",
                                                                  choices = c("Shape", "Margin", "Density", "Age",
                                                                              "Shape:Age", "Margin:Age", "Density:Age",
                                                                              "I(Age^2)", "Shape:I(Age^2)",
                                                                              "Margin:I(Age^2)", "Density:I(Age^2)"),
                                                                  options = list(
                                                                    `actions-box` = TRUE), 
                                                                  multiple = TRUE)
                                                      ),
                                               column(4,
                                                      actionButton("actionFit", "Apply Selections",
                                                                   class = "btn btn-success"))
                                             )

                                    ),
                                    tabPanel("Model Results",
                                               fluidRow(
                                                 column(4, tableOutput("resultstableGLM")),
                                                 column(4, tableOutput("resultstableClass")),
                                                 column(4, tableOutput("resultstableRF"))
                                               ),
                                    fluidRow(
                                      column(4, plotOutput("varImpGLM")
                                      ),
                                      column(4,
                                             plotOutput("varImpClass")
                                      ),
                                      column(4,
                                             plotOutput("varImpRF")
                                      )
                                    ),
                                    fluidRow(
                                      column(6,
                                        plotOutput("rpartPlotClass")
                                      ),
                                      column(6,
                                             plotOutput("plotClass")
                                      )
                                    )
                                    
                                    ),
                                    tabPanel("Prediction", 
                                          fluidRow(
                                                   p("Please selecy the Age,
                                                     mass Shape, mass Margin, and mass Density of a 
                                                     patient you wish to predict. The prediction will apppear below.")
                                          ),
                                            fluidRow(
                                            column(3,
                                                   sliderInput(inputId = 'predAge',
                                                               label = 'Age',
                                                               value = 35,
                                                               min = 18, max = 100),
                                          ),
                                          column(3,
                                                 pickerInput( "predShape",
                                                              "Shape",
                                                              choices = c("round", "oval", "lobular", "irregular"),
                                                              options = list(
                                                                `actions-box` = TRUE), 
                                                              multiple = TRUE
                                                 )
                                                 ),
                                          column(3,
                                                 pickerInput( "preMargin",
                                                              "Margin",
                                                              choices = c("circumscribed","microlobulated", "obscured", "ill-defined",
                                                                          "spiculated"),
                                                              options = list(
                                                                `actions-box` = TRUE), 
                                                              multiple = TRUE
                                                 )),
                                                 column(3,
                                                 pickerInput("pickDensityFilter",
                                                             "Select Density",
                                                             choices = c("high", "iso", "low", "fat-containing"),
                                                             options = list(
                                                               `actions-box` = TRUE), 
                                                             multiple = TRUE
                                                 )
                                                 ),
                                      
                                          
                                            ),
                                          fluidRow(
                                            column(3,
                                                   actionButton("prebutton", "Predict")
                                                   ),
                                            column(9,
                                                   p("A mass with the above is predicted as follows:"),
                                                   tableOutput("datahead")
                                                   )

                                          )
                                          ))
                                    )
              )
)
