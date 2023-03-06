########################################################################;
#Author:  Ashley Ko                                                     ;
#Title: Mass Classifier UI                                              ;
#Program Purpose: Demonstrate skill data visualization, ML, & R Shiny   ;
#Date: 2021/12/05                                                       ;
########################################################################;


# Required packages
library(shiny)
library(shinythemes)
library(shinyWidgets)

# Establishes a navbar page ui
ui <- navbarPage(theme = shinytheme("cosmo"),
                 "Mammographic Mass Classification",
                 # About page panel
                 tabPanel("About",
                          includeMarkdown("about.md")),
                 # Data page panel
                 tabPanel("Data",
                          fluidRow(
                            column(2,
                                   # Radio Button for selecting variables to filter data
                                   prettyRadioButtons(
                                     inputId = "filtVar",
                                     label = "Filter Variable:",
                                     choices = c("None" = "None", "Severity", "Margin",
                                                 "Shape", "Density"), selected = "None",
                                     inline = TRUE, status = "info", fill = FALSE
                                     ),
                                   # Conditonal Panel for selecting variable levels for filtering
                                   # data based on previously selected filter variable Severity
                                   conditionalPanel(condition = "input.filtVar == 'Severity'",
                                                    pickerInput(inputId = 'filSeverity',
                                                                label = 'Select level of Severity',
                                                                choices = c("benign", "maligant"),
                                                                selected = "maligant"
                                                    )
                                   ),
                                   # Conditonal Panel for selecting variable levels for filtering
                                   # data based on previously selected filter variable Margin
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
                                   # Conditonal Panel for selecting variable levels for filtering
                                   # data based on previously selected filter variable Shape
                                   conditionalPanel(condition = "input.filtVar == 'Shape'",
                                                    pickerInput(inputId = 'filShape',
                                                                label = 'Select level of Shape',
                                                                choices = c("round", "oval",
                                                                            "lobular", "irregular"),
                                                                selected = "round"
                                                    )
                                   ),
                                   # Conditonal Panel for selecting variable levels for filtering 
                                   # data based on previously selected filter variable Density
                                   conditionalPanel(condition = "input.filtVar == 'Density'",
                                                    pickerInput(inputId = 'filDens',
                                                                label = 'Select level of Density',
                                                                choices = c("high", "iso", "low",
                                                                            "fat-containing"),
                                                                selected = "high"
                                                    )
                                   ),
                                   # Picker Input for selecting subseting variable
                                   pickerInput("pickerSubset",
                                               "Select variables for subset:",
                                               choices = c("Age", "Shape", "Margin", "Density"),
                                               selected = "Age",
                                               multiple = TRUE
                                               ),
                                   # Button which prompts download handeler to render a csv file
                                  downloadButton(outputId = "dataDownload", label = "Download Data")
                                   ),
                            column(10,
                                   # Outputs raw data set & updates based on filter and subset input
                                   dataTableOutput("myTable")
                                   )
                            
                          )),
                 # Data Exploration page panel
                 navbarMenu("Data Exploration",

                            # Graphical Summary tab
                            tabPanel("Graphical Summary",
                                     fluidRow(
                                       column(3,
                                              # Switch to allow for fill color palette selection
                                              prettySwitch("switchColors",
                                                           label = "Change color palette?",
                                                           value = FALSE),
                                              # Radio Button to allow for plot type selection
                                              prettyRadioButtons(
                                                inputId = "plotType",
                                                label = "Plot Type:", 
                                                choices = c("Box", "Histogram", "Bar"),
                                                selected = "Box",
                                                inline = TRUE, 
                                                status = "info",
                                                fill = FALSE
                                              ),
                                              # Conditional Panel that displays color palette
                                              #  options
                                              conditionalPanel(condition = "input.switchColors",
                                                               pickerInput(
                                                                 inputId = "color",
                                                                 label = "Color Palatte:",
                                                                 choices = c("Accent", "Dark2",
                                                                             "Paired", "Set1",
                                                                             "Set2", "Set3",
                                                                             "Pastel1", "Pastel2"))
                                                               ),
                                              # Conditional Panel that displays bin width for the
                                              #  histogram
                                              conditionalPanel(condition = "input.plotType == 
                                                               'Histogram'",
                                                               sliderInput("histBins", "Bin Width:",
                                                                           min = 1, max = 50,
                                                                           value = 30)
                                              ),
                                              # Conditional Panel that plot options for bar and box
                                              # plots (x variable and fill variable)
                                              conditionalPanel(condition = "input.plotType != 
                                                               'Histogram'",
                                                               prettyRadioButtons(
                                                                 inputId = "xaxis",
                                                                 label = "Select variable for 
                                                                 x-axis", 
                                                                 choices = c("Severity", "Shape",
                                                                             "Margin", "Density"),
                                                                 selected = "Severity",
                                                                 inline = TRUE, 
                                                                 status = "info",
                                                                 fill = FALSE
                                                               ),
                                                               pickerInput(inputId = 'fill',
                                                                           label = 'Fill Variable:',
                                                                           choices = c("Severity", 
                                                                                       "Margin", 
                                                                                       "Shape", 
                                                                                       "Density"),
                                                                           selected = "Severity"
                                                               )
                                              ),
                                              # Condional panel that renders options for 
                                              # facet selection for Box plot
                                              conditionalPanel(condition = "input.plotType == 
                                                               'Box'",
                                                               pickerInput(inputId = 'facet',
                                                                           label = "Facet 
                                                                           Variable:",
                                                                           choices = c("Severity",
                                                                                       "Margin",
                                                                                       "Shape", 
                                                                                       "Density"),
                                                                           selected = "Margin"
                                                               )
                                              )
                                       ),
                                       column(9,
                                              # Renders custom plot
                                              plotOutput("sumPlot")
                                       )
                                     )
                                     
                                     ),
                            # Tab panel for numeric summary
                            tabPanel("Numeric Summary",
                                     fluidRow(
                                       column(3,
                                              # Radio Buttons for selecting type of contigency
                                              # table
                                              prettyRadioButtons(
                                                inputId = "editConTab",
                                                label = "Edit contingency table:", 
                                                choices = c("One-Way", "Two-Way", "Three-Way"),
                                                selected = "Two-Way",
                                                inline = TRUE, 
                                                status = "info",
                                                fill = FALSE
                                              ),
                                              # Conditional Panel for one-way table, variable picker
                                              conditionalPanel(condition = "input.editConTab ==
                                                               'One-Way'",
                                                               pickerInput(inputId = 'oneWay',
                                                                           label = "Select one-way 
                                                                           variable:",
                                                                           choices = c("Severity",
                                                                                       "Margin",
                                                                                       "Shape", 
                                                                                       "Density"),
                                                                           selected = "Margin"
                                                               )
                                              ),
                                              # Conditional Panel for two-way table, variable picker
                                              conditionalPanel(condition = "input.editConTab == 
                                                               'Two-Way'",
                                                               pickerInput(inputId = 'twoWay',
                                                                           label = "Select two-way 
                                                                           variables:",
                                                                      choices = c("Severity|Margin",
                                                                                 "Severity|Density",
                                                                                  "Severity|Shape"),
                                                                        selected = "Severity|Shape"
                                                               )
                                              ),
                                              # Conditional Panel for 3-way table, variable picker
                                              conditionalPanel(condition = "input.editConTab == 
                                                        'Three-Way'",
                                                        pickerInput(inputId = 'threeWay',
                                                                    label = "Select two-way 
                                                                    variables:",
                                                              choices = c("Margin|Density|Severity",
                                                                            "Margin|Shape|Severity",
                                                                           "Shape|Density|Severity",
                                                                              "Shape|Margin|Density"
                                                                                ),
                                                               selected = "Margin|Density|Severity"
                                                        )
                                              )
                                       ),
                                       column(9,
                                              "Contingency Table",
                                              # Displays contingency table results
                                              verbatimTextOutput("conTab"),
                                              "Simple Summary",
                                              # Displays simple data summary
                                              verbatimTextOutput("generalSum")
                                              )
                                       )
                                     )
                                     ),
                 # Modeling page
                 navbarMenu("Modeling",
                            
                            # Tab for Modeling Information
                            tabPanel("Modeling Information",
                                     h4("Supervised Learning"),
                                     tags$br(),

                                     p("Supervised learning means there are predictor variables
                                       in our data set which can be used to represent a response
                                       variable. For this app, repeated (3 times), cross-validation,
                                       was used to tune and select the optimal model for prediction.
                                       As the response, Severity, is categorical accuracy should
                                       be used to make fit comparions among the three models."),

                                     tags$br(),
                                     h4("Generalized Linear Model"),
                                     tags$br(),
                                     p("Generalized Linear Models, GLM, are like linear models in
                                       that the model relies on a linear relationship between the
                                       response and predictor variables. In the GLM case, responses
                                       follow non-normal distribution. The purposes of this
                                       application is to classify mammary masses by 'Severity'
                                       either malignant or bengin. This creates a binomial model
                                       with where the bengin condition is 0 and the malignant
                                       condition is 1. This type of model uses a `logit` function
                                       (shown below) to link the proportion of maligant, in the case
                                       of the breast mass data, occurances out of total occurances
                                       back to a linear model."),

                 # Renders LaTex
                  withMathJax(helpText("$$\\text{logit = }\\log\\left( \\frac{p}{1-p} \\right)$$")),

                                     p("Pros: Unlike classification trees and random forest,
                                       the GLM approach allows for prediction and interpetation in
                                       the form of parameter estimates."),
                                     p("Cons: GLM models only fit the model provides. This means 
                                       all the predictors passed to the model are used regardless
                                       of importance. Classification trees and random forest models
                                       select 'optimal' predictors from the formula provided."),
                                     tags$br(),
                                     tags$br(),

                                     h4("Classification Tree Model"),
                                     tags$br(),
                                     p("Tree based methods create predictions by dividing predictors
                                       space into regions and produce different predictions for each
                                       region. For classification trees, the most common class is
                                       used as the prediction. The tree continues to grow as
                                       subsequent splits are made. The results are pruned back to
                                       prevent overfitting. For this app repeated, 
                                       cross-validatation with tuning parameter complexity parameter
                                       select the optimal number of nodes."),

                                     p("Pros: Easy to understand and variable selection is built in.
                                     Little background knowledge is needed follow the tree plot."),
                                     p("Cons: Prone to fluacations based on small changes in the
                                       data. As each split is only made by looking one step ahead,
                                       it is possible that this greedy algorithm misses the optimal
                                       path."),

                                     tags$br(),
                                     tags$br(),
                                     h4("Random Forest Model"),
                                     tags$br(),

                                     p("Random forest models use bootstrapping to create multiple
                                       trees, in this case classification trees, and average over
                                       the results of all bootstrap samples. Predictors are randomly
                                       selected for each tree and all predictors supplied to the
                                       model are used at once. This app applies cross-validation,
                                       to select the optimal 'mtrys'. M (mtrys) is the number
                                       of randomly selected predictors to be used by the model.
                                       For classification random forest models m is normally the
                                       square root of the number of predictors."),

                                     # Renders LaTex
                                     withMathJax(helpText("$$\\text{m = }\\sqrt{p}$$")),

                                     p("Pros: Not restricted to using all the predictors.
                                       Randomly selects predictors unlike classification tree's
                                       greedy algorithm."),
                                     p("Cons: Results allow for prediction but not interpretation.
                                       This model is prone to overfitting. Computationally 
                                       intensive in comparions to GLM.")
                            ),

                            # Model fitting tab
                            tabPanel("Fitting",
                                     fluidRow(
                                       column(3,
                                              # Radio Button to talk fit output
                                              prettyRadioButtons(
                                                inputId = "modSelect",
                                                label = "Select a model fit:",
                                                choices = c("GLM", "Classification Tree",
                                                            "Random Forest"), selected = "GLM",
                                                inline = TRUE, status = "info", fill = FALSE
                                              ),
                                              # Data split ratio  numeric input selector
                                              numericInput("percent", "Data Split Ratio", 0.70,
                                                           min = 0.40, max = 0.80
                                                           , step = .05),
                                              # k-folds numeric input selector
                                              numericInput("kfolds", "k-folds for Cross-Validation",
                                                           5, min = 5, max = 10
                                                           , step = 5),
                                              # cp slider
                                              sliderInput("sliderCP",
                                                          label = "Complexity Paramater Range",
                                                          min = .01,
                                                          max = .1,
                                                          value = .1
                                              ),
                                              # mtrys slider
                                              sliderInput("mtrys",
                                                          label = "mtry Range",
                                                          min = 2,
                                                          max = 20,
                                                          value = 3
                                              ),

                                              # Variable selection box for model call
                                              pickerInput("modVar",
                                                          "Select desired predictors:",
                                                          choices = c("Shape", "Margin", "Density",
                                                                      "Age",  "Shape:Age",
                                                                      "Margin:Age", "Density:Age",
                                                                      "I(Age^2)", "Shape:I(Age^2)",
                                                                      "Margin:I(Age^2)",
                                                                      "Density:I(Age^2)"),
                                                          selected = c("Shape", "Margin", "Density",
                                                                       "Age", "Shape:Age",
                                                                       "Margin:Age", "Density:Age",
                                                                       "I(Age^2)", "Shape:I(Age^2)",
                                                                       "Margin:I(Age^2)",
                                                                       "Density:I(Age^2)"),
                                                          options = list(
                                                            `actions-box` = TRUE), 
                                                          multiple = TRUE
                                                          ),
                                              
                                             helpText("Note: Page may take a few seconds to load."),
                                             helpText("To avoid errors,
                                                       you must select at least one predictor."),
                                            helpText("See the 'Results' page for additonal output.")
                                       ),
                                       column(9,
                                              # Renders fit summary
                                              verbatimTextOutput("fitSummary")
                                              )
                                     )
                                     ),

                            # Model (additonal) results tab
                            tabPanel("Results",
                                     fluidRow(
                                       column(2,
                                              # Radio buttons to select model for confusion matrix
                                              prettyRadioButtons(
                                                inputId = "resultsSelect",
                                                label = "Select a model for the confusion matrix:",
                                                choices = c("Generalized Linear",
                                                            "Classification Tree",
                                                            "Random Forest"),
                                                selected = "Generalized Linear",
                                                inline = TRUE, status = "info", fill = FALSE
                                              ),
                                              helpText("Note: Page may take a few seconds to load.")
                                              ),
                                       column(9,
                                              # Renders confusion matrix
                                              uiOutput("confuName"),
                                              verbatimTextOutput("confusion")
                                              
                                              )
                                       ),
                                     fluidRow(
                                       column(6,
                                              h6("Classification Tree Plot"),
                                              # Renders classification tree plot
                                              plotOutput("treePlot")
                                              ),
                                       column(6,
                                              h6("Random Forest Variable of Importance Plot"),
                                              # Renders random forest variable of importance plot
                                              plotOutput("varImpPlot")
                                              )
                                     )
                                     ),

                            # Model prediction tab
                            tabPanel("Predition",
                                     titlePanel(
                                       uiOutput("ptInfo")
                                     ),
                                     fluidRow(
                                       column(3,
                                              helpText("Note: Prediction updates after each new
                                                       selection."),
                                              # Selector for new patient age
                                              numericInput(inputId = 'predAge',
                                                          label = 'Age',
                                                          value = 35,
                                                          min = 18, max = 100),
                                              # Selector for new mass shape
                                              selectInput("predShape",
                                                           "Shape",
                                                           choices = c("round", "oval", "lobular",
                                                                       "irregular"),
                                                           selected = "round"
                                              ),
                                              # Selector for new mass margin
                                              selectInput("predMarg",
                                                           "Margin",
                                                           choices = c("circumscribed",
                                                                       "microlobulated",
                                                                       "obscured", "ill-defined",
                                                                       "spiculated"),
                                                          selected = "circumscribed"
                                              ),
                                              # Selector for new mass density
                                              selectInput("predDens",
                                                          "Density",
                                                          choices = c("high", "iso", "low",
                                                                      "fat-containing"),
                                                          selected = "high"
                                                          )
                                              ),
                                       column(3,
                                              h6("GLM Prediction"),
                                              # Renders prediction from the glm
                                              verbatimTextOutput("predResGLM")
                                              ),
                                       column(3,
                                            h6("Classification Tree Prediction"),
                                            # Renders classification tree prediction
                                             verbatimTextOutput("predResCla")
                                             ),
                                       column(3,
                                            h6("Random Forest Prediction"),
                                            # Renders random forest prediction
                                             verbatimTextOutput("predResRF")
                                       )
                                     )))
)
