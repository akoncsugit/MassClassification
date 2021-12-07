library(shiny)
library(shinythemes)
library(shinyWidgets)
library(markdown)

ui <- navbarPage(theme = shinytheme("cosmo"),
                 "My Application",
                 tabPanel("About",
                          includeMarkdown("about.md")),
                 tabPanel("Data",
                          fluidRow(
                            column(2,
                                   prettyRadioButtons(
                                     inputId = "filtVar",
                                     label = "Filter Variable",
                                     choices = c("None" = "None", "Severity", "Margin",
                                                 "Shape", "Density"), selected = "None",
                                     inline = TRUE, status = "info", fill = FALSE
                                     ),
                                   conditionalPanel(condition = "input.filtVar == 'Severity'",
                                                    pickerInput(inputId = 'filSeverity',
                                                                label = 'Select level of Severity',
                                                                choices = c("benign", "maligant"),
                                                                selected = "maligant"
                                                    )
                                   ),
                                   conditionalPanel(condition = "input.filtVar == 'Margin'",
                                                    pickerInput(inputId = 'filMargin',
                                                                label = 'Select level of Margin',
                                                                choices = c("circumscribed",
                                                                            "microlobulated",
                                                                            "obscured",
                                                                            "ill-defined",
                                                                            "spiculated"),
                                                                selected = "circumscribed"
                                                    )
                                   ),
                                   conditionalPanel(condition = "input.filtVar == 'Shape'",
                                                    pickerInput(inputId = 'filShape',
                                                                label = 'Select level of Shape',
                                                                choices = c("round", "oval",
                                                                            "lobular", "irregular"),
                                                                selected = "round"
                                                    )
                                   ),
                                   conditionalPanel(condition = "input.filtVar == 'Density'",
                                                    pickerInput(inputId = 'filDens',
                                                                label = 'Select level of Density',
                                                                choices = c("high", "iso", "low",
                                                                            "fat-containing"),
                                                                selected = "high"
                                                    )
                                   ),
                                   pickerInput("pickerSubset",
                                               "Select variables for subset",
                                               choices = c("Age", "Shape", "Margin", "Density"),
                                               selected = "Age",
                                               multiple = TRUE
                                               ),


                                   downloadButton(outputId = "dataDownload", label = "Download Data")
                                   ),
                            column(10,
                                   dataTableOutput("myTable")
                                   )
                            
                          )),
                 navbarMenu("Data Exploration",
                            tabPanel("Graphical Summary",
                                     fluidRow(
                                       column(3,
                                              prettySwitch("switchColors",
                                                           label = "Select plot color palette?",
                                                           value = FALSE),
                                              conditionalPanel(condition = "input.switchColors",
                                                             pickerInput(
                                                               inputId = "color",
                                                               label = "Select color palate",
                                                               choices = c("Accent", "Dark2",
                                                                           "Paired", "Set1",
                                                                           "Set2", "Set3",
                                                                           "Pastel1", "Pastel2"))),
                                              prettyRadioButtons(
                                                inputId = "plotType",
                                                label = "Select plot type:", 
                                                choices = c("Box", "Histogram", "Bar"),
                                                selected = "Box",
                                                inline = TRUE, 
                                                status = "info",
                                                fill = FALSE
                                              ),

                                              pickerInput(inputId = 'fill',
                                                          label = 'Fill Variable',
                                                          choices = c("Age", "Severity", "Margin", "Shape", "Density"),
                                                          selected = "Severity"
                                                          ),
                                              conditionalPanel(condition = "input.plotType == 'Histogram'",
                                                               sliderInput("histBins", "Number of bins:", 
                                                                           min = 1, max = 50, value = 30),
                                                               numericInput("maxBins", label = "Set Maximum Number of Bins",
                                                                            value = 50, min = 1, max = 100)
                                              ),
                                              
                                              conditionalPanel(condition = "input.plotType != 'Histogram'",
                                                               prettyRadioButtons(
                                                                 inputId = "xaxis",
                                                                 label = "Select variable for x-axis", 
                                                                 choices = c("Severity", "Shape", "Margin", "Density"),
                                                                 selected = "Severity",
                                                                 inline = TRUE, 
                                                                 status = "info",
                                                                 fill = FALSE
                                                               )
                                              ),
                                              
                                              conditionalPanel(condition = "input.plotType == 'Box'",
                                                               pickerInput(inputId = 'facet',
                                                                           label = 'Facet Variable',
                                                                           choices = c("Severity", "Margin",
                                                                                       "Shape", "Density"),
                                                                           selected = "Margin"
                                                               )
                                              )
                                              
                                              
                                              #,downloadButton("downloadPlot", label = "Download Current Plot")
                                       ),
                                       column(9,
                                              #plotOutput("sumPlot")
                                              plotOutput("distPlot")
                                       )
                                     )
                                     
                                     ),
                            tabPanel("Numeric Summary",
                                     fluidRow(
                                       column(3,
                                              "sidebar",
                                              pickerInput(
                                                inputId = "conPick",
                                                label = "Select Variables for Contigency Table",
                                                choices = c("Severity", "Margin", "Shape", "Density"),
                                                multiple = TRUE,
                                                options =  list(
                                                  "max-options" = 3
                                                )),
                                              actionBttn("conTab", "Run")

                                       ),
                                       column(9,
                                              "Contingency Tab",
                                              #verbatimTextOutput("con"),
                                              "Summary"#,
                                              #verbatimTextOutput("summary")
                                              )

                                       )

                                     )
                                     ),
                 navbarMenu("Modeling",
                            tabPanel("Modeling Information"
                                     ),

                            tabPanel("Fitting",
                                     fluidRow(
                                       column(3,
                                              prettyRadioButtons(
                                                inputId = "modSelect",
                                                label = "Select a model fit",
                                                choices = c("GLM", "Classification Tree",
                                                            "Random Forest"), selected = "GLM",
                                                inline = TRUE, status = "info", fill = FALSE
                                              )
                                              ,
                                              numericInput("percent", "Data Split Ratio", 0.70, min = 0.40, max = 0.80
                                                           , step = .05),
                                              numericInput("kfolds", "k-folds for Cross-Validation", 5, min = 5, max = 10
                                                           , step = 5),
                                              sliderInput("sliderCP",
                                                          label = "Complexity Paramater Range",
                                                          min = .01,
                                                          max = .1,
                                                          value = .1
                                              ),
                                              sliderInput("mtrys",
                                                          label = "mtry Range",
                                                          min = 2,
                                                          max = 20,
                                                          value = 3
                                              ),
                                              pickerInput("modVar",
                                                          "Select desired model predictors",
                                                          choices = c("Shape", "Margin", "Density", "Age",
                                                                      "Shape:Age", "Margin:Age", "Density:Age",
                                                                      "I(Age^2)", "Shape:I(Age^2)",
                                                                      "Margin:I(Age^2)", "Density:I(Age^2)"),
                                                          selected = c("Shape", "Margin", "Density", "Age",
                                                                       "Shape:Age", "Margin:Age", "Density:Age",
                                                                       "I(Age^2)", "Shape:I(Age^2)",
                                                                       "Margin:I(Age^2)", "Density:I(Age^2)"),
                                                          options = list(
                                                            `actions-box` = TRUE), 
                                                          multiple = TRUE
                                                          ),
                                              helpText("See the 'Results' page for additonal output."),
                                              helpText("Note: Page may take a few seconds to load.")
                                       ),
                                       column(9,
                                              verbatimTextOutput("fitSummary")
                                              )
                                     )
                                     ),
                            tabPanel("Results",
                                     fluidRow(
                                       column(2,
                                              prettyRadioButtons(
                                                inputId = "resultsSelect",
                                                label = "Select a confusion matrix",
                                                choices = c("GLM", "Classification Tree",
                                                            "Random Forest"), selected = "GLM",
                                                inline = TRUE, status = "info", fill = FALSE
                                              ),
                                              helpText("Note: Page may take a few seconds to load.")
                                              ),
                                       column(9,
                                              uiOutput("confuName"),
                                              verbatimTextOutput("confusion")
                                              
                                              )
                                       ),
                                     fluidRow(
                                       column(6,
                                              h6("Classification Tree Plot"),
                                              plotOutput("treePlot")
                                              ),
                                       column(6,
                                              h6("Random Forest Variable of Importance Plot"),
                                              plotOutput("varImpPlot")
                                              )
                                     )
                                     ),
                            tabPanel("Predition",
                                     titlePanel(
                                       uiOutput("ptInfo")
                                     ),
                                     fluidRow(
                                       column(3,
                                              numericInput(inputId = 'predAge',
                                                          label = 'Age',
                                                          value = 35,
                                                          min = 18, max = 100),
                                              selectInput("predShape",
                                                           "Shape",
                                                           choices = c("round", "oval", "lobular",
                                                                       "irregular"),
                                                           selected = "round"
                                              ),
                                              selectInput("predMarg",
                                                           "Margin",
                                                           choices = c("circumscribed",
                                                                       "microlobulated",
                                                                       "obscured", "ill-defined",
                                                                       "spiculated"),
                                                          selected = "circumscribed"
                                              ),
                                              selectInput("predDens",
                                                          "Select Density",
                                                          choices = c("high", "iso", "low",
                                                                      "fat-containing"),
                                                          selected = "high"
                                                          ),
                                              ),
                                       column(3,
                                              h6("GLM Prediction"),
                                              verbatimTextOutput("predResGLM"),
                                              ),
                                       column(3,
                                            h6("Classification Tree Prediction"),
                                             verbatimTextOutput("predResCla")
                                             ),
                                       column(3,
                                            h6("Random Forest Prediction"),
                                             verbatimTextOutput("predResRF")
                                       )
                                     )))
)