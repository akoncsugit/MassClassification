library(shiny)
library(shinythemes)
library(shinyWidgets)
library(markdown)

ui <- navbarPage(theme = shinytheme("slate"),
                 "My Application",
                 tabPanel("About",
                          includeMarkdown("about.md")),
                 tabPanel("Data",
                          fluidRow(
                            column(3, "sidebar",
                                   switchInput("switchRemoveNA",
                                               label = "Remove NA",
                                               value = FALSE
                                   ),
                                   sliderInput("sliderAgeFilter",
                                               label = "Filter by Age",
                                               min = 18,
                                               max = 96,
                                               value = c(40, 75)
                                   ),
                                   pickerInput( "selectShapeFilter",
                                                "Filter by Shape",
                                                choices = c("round", "oval", "lobular", "irregular"),
                                                options = list(
                                                  `actions-box` = TRUE), 
                                                multiple = TRUE
                                   ),
                                   pickerInput( "selectMarginFilter",
                                                "Filter by Margin",
                                                choices = c("circumscribed","microlobulated", "obscured", "ill-defined",
                                                            "spiculated"),
                                                options = list(
                                                  `actions-box` = TRUE), 
                                                multiple = TRUE
                                   ),
                                   pickerInput("pickDensityFilter",
                                               "Filter by Density",
                                               choices = c("high", "iso", "low", "fat-containing"),
                                               options = list(
                                                 `actions-box` = TRUE), 
                                               multiple = TRUE
                                   ),
                                   pickerInput("pickerSubset",
                                               "Select variables for subset",
                                               choices = c("Severity", "Margin", "Shape", "Density"),
                                               options = list(
                                                 `actions-box` = TRUE), 
                                               multiple = TRUE
                                   ),
                                   actionBttn("actionData", "Apply"),
                                   downloadButton(outputId = "dataDownload", label = "Download Data")
                                   ),
                            column(9, "main",
                                   #dataTableOutput("myTable")
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
                                                               inputId = "plotColorPicker",
                                                               label = "Select color palate",
                                                               choices = c("Accent", "Dark2",
                                                                           "Paired", "Set1",
                                                                           "Set2", "Set3",
                                                                           "Pastel1", "Pastel2"))),
                                              prettyRadioButtons(
                                                inputId = "buttonPlotType",
                                                label = "Select plot type:", 
                                                choices = c("Box", "Histogram", "Bar"),
                                                selected = "Box",
                                                inline = TRUE, 
                                                status = "info",
                                                fill = FALSE
                                              ),

                                              pickerInput(inputId = 'fillVarPicker',
                                                          label = 'Fill Variable',
                                                          choices = c("Age", "Severity", "Margin", "Shape", "Density"),
                                                          selected = "Severity"
                                                          ),
                                              conditionalPanel(condition = "input.buttonPlotType == 'Histogram'",
                                                               sliderInput("histBins", "Number of bins:", 
                                                                           min = 1, max = 50, value = 30),
                                                               numericInput("maxBins", label = "Set Maximum Number of Bins",
                                                                            value = 50, min = 1, max = 100)
                                              ),
                                              
                                              conditionalPanel(condition = "input.buttonPlotType != 'Histogram'",
                                                               prettyRadioButtons(
                                                                 inputId = "radioBar",
                                                                 label = "Select variable for x-axis", 
                                                                 choices = c("Severity", "Shape", "Margin", "Density"),
                                                                 selected = "Severity",
                                                                 inline = TRUE, 
                                                                 status = "info",
                                                                 fill = FALSE
                                                               )
                                              ),
                                              
                                              conditionalPanel(condition = "input.buttonPlotType == 'Box'",
                                                               pickerInput(inputId = 'facetVarPicker',
                                                                           label = 'Facet Variable',
                                                                           choices = c("Severity", "Margin",
                                                                                       "Shape", "Density"),
                                                                           selected = "Margin"
                                                               )
                                              )
                                              
                                              
                                              #,downloadButton("downloadPlot", label = "Download Current Plot")
                                       ),
                                       column(9,
                                              plotOutput("distPlot")
                                       )
                                     )
                                     
                                     ),
                            tabPanel("Numeric Summary",
                                     fluidRow(
                                       column(3,
                                              "sidebar",
                                              pickerInput(
                                                inputId = "tablePicker",
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
                                              #verbatimTextOutput("table"),
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
                                       column(4, "sidebar",
                                              knobInput(
                                                inputId = "knobSplit",
                                                label = "Split Percentage of Training Data",
                                                value = 70,
                                                min = 20,
                                                max = 80,
                                                displayPrevious = TRUE,
                                                lineCap = "round",
                                                fgColor = "#428BCA",
                                                inputColor = "#428BCA"
                                              ),
                                              numericInput("kfolds", "k-folds for Cross-Validation", 5, min = 5, max = 10
                                                           , step = 5),
                                              sliderInput("sliderCP",
                                                          label = "Complexity Paramater Range",
                                                          min = .1,
                                                          max = .5,
                                                          value = .1
                                              ),
                                              sliderInput("mtrys",
                                                          label = "mtry Range",
                                                          min = 2,
                                                          max = 30,
                                                          value = 15
                                              ),
                                              pickerInput("pickerModel",
                                                          "Select desired model predictors",
                                                          choices = c("Shape", "Margin", "Density", "Age",
                                                                      "Shape:Age", "Margin:Age", "Density:Age",
                                                                      "I(Age^2)", "Shape:I(Age^2)",
                                                                      "Margin:I(Age^2)", "Density:I(Age^2)"),
                                                          options = list(
                                                            `actions-box` = TRUE), 
                                                          multiple = TRUE
                                                          ),
                                              actionBttn("actionFit", "Fit")
                                       ),
                                       column(8, "main",
                                              # tableOutput("resultstable"),
                                              # plotOutput("varImpGLM"),
                                              # plotOutput("varImpClass"),
                                              # plotOutput("varImpRF"),
                                              # plotOutput("rpartPlotClass")
                                              # plotOutput("plotClass")
                                              )
                                       
                                     )
                                     ),
                            tabPanel("Predition",
                                     fluidRow(
                                       column(3,
                                              numericInput(inputId = 'predAge',
                                                          label = 'Age',
                                                          value = 35,
                                                          min = 18, max = 100),
                                              selectInput( "predShape",
                                                           "Shape",
                                                           choices = c("round", "oval", "lobular", "irregular")
                                              ),
                                              selectInput("preMargin",
                                                           "Margin",
                                                           choices = c("circumscribed","microlobulated", "obscured", "ill-defined",
                                                                       "spiculated")
                                              ),
                                              selectInput("preDensity",
                                                          "Select Density",
                                                          choices = c("high", "iso", "low", "fat-containing")
                                                          ),

                                              actionBttn("prebutton", "Predict")
                                              ),
                                       column(9,
                                              p("A mass with the above is predicted as follows:")
                                              )
                                     )))
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  observe({updateSliderInput(session, "histBins", max = input$maxBins)})
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    Age <- faithful[, 2]
    bins <- seq(min(Age), max(Age), length.out = input$histBins + 1)
    
    # draw the histogram with the specified number of bins
    hist(Age, breaks = bins, col = 'darkgray', border = 'white')
  })
  
  
  
  
  
  
  # Render Filetered data set
  output$dataf <- renderDataTable({
    index <- cityinput()
    if (index == "All"){
      air_data %>%
        select(city, date, input$var_selecte) %>%
        datatable()
    } else{
      air_data %>%
        filter(city==index) %>%
        select(city, date, input$var_selecte) %>%
        datatable()
    }
  })
  
  

  
  # Download Handler for data frame
  output$dataDownload <- downloadHandler(
    filename = function(){"Dataframe.csv"},
    content = function(fname){
      index <- cityinput()
      if (index == "All"){
        temp <- air_data %>%
          select(city, date, input$var_selecte)
        write.csv(temp, fname)
      } else{
        temp <- air_data %>%
          filter(city==index) %>%
          select(city, date, input$var_selecte)
        write.csv(temp, fname)
      }
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)