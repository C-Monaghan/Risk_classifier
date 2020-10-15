source("Main.R")

# All the libraries required for th shiny app 
library(shiny)
library(shinyBS)
library(shinythemes)
library(shinyjs)
library(shinyalert)
library(shinyWidgets)


shinyUI(ui = tagList(
  tags$head(
    tags$style(HTML("
      .shiny-output-error-validation {
        color: red;
      }
    "))),
  useShinyjs(),
  withMathJax(),
  
  navbarPage(

    theme = shinytheme("slate"),  # <--- To use a theme, 
    strong("MI based classifier"), # Main title name

    
    
    # Home --------------------------------------------------------------------
    tabPanel("Home",
             column(12, align="center",
                    # img(src='logo.png', align = "right", height=120, width=100),
                    h2(strong("Welcome to the Shiny App for using MI based classifier in R")),
                    tags$img(src = "classifier.png",height=500,width=550),
                    h4(strong("Note: For more help on usage, please look into the 'Guide' tab and 'About us' in detail."))
             )
    ),
    
    # Guides ------------------------------------------------------------------
    tabPanel("Guides", 
             br(),
             bsCollapse(id = "Machine_Learning_based_Classifier_desc", open = "Machine Learning based Classifier Usage Guide",
                        bsCollapsePanel("Machine Learning based Classifier Usage Guide",
                                        
                                        includeHTML("Machine_Learning_based_Classifier_desc_ug.html"),
                                        style = "primary"),
                        bsCollapsePanel("Machine Learning based Classifier Conceptual Explanation",
                                        
                                        includeHTML("Machine_Learning_based_Classifier_desc_ce.html"),
                                        style = "info"),
                        bsCollapsePanel("Machine Learning based Classifier Technical Details",
                                        
                                        includeHTML("Machine_Learning_based_Classifier_desc_td.html"),
                                        style = "info")
             )
    ), 
    
    # Dataset Input ----------------------------------------------------------------
    tabPanel("Dataset",  sidebarPanel(width = 3,
                                      
                                      fileInput("sample_file", h4("File input:", bsButton("main_data_tooltip", label = "",
                                                icon = icon("question"), size = "extra-small")),
                                                multiple = F, accept = c("text/csv", "text/comma-separated-values, text/plain", ".csv"),
                                                placeholder = "Enter Your Data Here"),
                                          bsPopover("main_data_tooltip", title="File format",
                                                 content="Please make sure: rows are customers/observations, columns are different variables with first column named as Class specifying the variable to be estimated.",
                                                 trigger = "hover"),
                                      #tags$hr(),
                                      h4(helpText("Is there a header in the data?")),
                                      checkboxInput('header', 'Header', TRUE),
                                      radioButtons('sep', 'Separator',
                                                   c(Comma=',',
                                                     Semicolon=';',
                                                     Tab='\t'),
                                                   'Comma'),
                                      radioButtons('quote', 'Quote',
                                                   c(None='',
                                                     'Double Quote'='"',
                                                     'Single Quote'="'"),
                                                   'Double Quote'),
                                      
                                      
                                      h4("Please Select Input Values:",align="centre"),
                                      # Input: Variable Selection Method ----
                                      radioButtons("method", "Step 1: Type of Variable Selection Method:",choices = list("Ridge Regression","Lasso Regression")),
                                  
                                      
                                      # Input: No. of Decision Trees ----

                                      sliderInput("trees", "Step 2: How many Decision Trees user wants to select for Evolutionary Algorithm?",
                                                 min = 20, max=800,
                                                 step = 4,animate = TRUE,value = 200),
                                      
                                      # Input: Which Decision Trees to be viewed ----
                                      sliderInput("viewtree", "Step 3: Which Decision Tree the user wants to view with its various associative results?",
                                                  min = 1, max=800,
                                                  step = 1,animate = TRUE,value = 1),
                                      
                                      # Downloading the file type
                                      h5("3.Select the type of plot to be downloaded:",align="centre"),
                                      radioButtons("filetype","Select the file type",choices = list("png","pdf"))
                                      
                                      
    ),  # Main panel for displaying outputs ----
    mainPanel(
      tabsetPanel(
        type="tab",
        tabPanel(" View Input Dataset ",dataTableOutput("dataset"), tableOutput("col")),
        tabPanel("User Selection Input",tableOutput("values"),br(),useShinyalert() ,h5("Click Calculate button after selecting the desired inputs!!"),actionButton("button", "Calculate")),
        tabPanel("Reduced Dataset after variable selection",downloadButton(outputId="down",label ="Download the reduced data in .csv"),dataTableOutput("Reduced_data"), tableOutput("colred")),
        tabPanel("Classifier",br(),useShinyalert() ,h5("Click Calculate button after selecting the decision tree to be viewed in inputs!!"),actionButton("button1", "Calculate"),addSpinner(plotOutput("plot",  width = "120%"), spin = "circle", color = "#E41A1C"),downloadButton(outputId="down1",label ="Download the plot"),tableOutput("res"))
      )
       )
    ),
    
    # Evolutionary Algorithm ----------------------------------------------------------------
    tabPanel("Evolutionary Algorithm", 
              sidebarPanel(width = 3,
                          sliderInput("generations", 
                                      "Generations",
                                      min = 10, 
                                      max=200,
                                      step = 10,
                                      animate = TRUE,
                                      value = 10),
                          actionButton("evolve", 
                                       "Start evolution"),
                          actionButton("stop", 
                                       "Stop evolution")
                          ),
              mainPanel(tabsetPanel(type="tab",
                                    tabPanel("Hyperparameters",
                                             sliderInput("tournament_size", 
                                                         "Tournament size",
                                                         min = 1, 
                                                         max=10,
                                                         step = 1,
                                                         animate = TRUE,
                                                         value = 3),
                                             sliderInput("mutation_rate", 
                                                         "Mutation rate",
                                                         min = 0, 
                                                         max=1,
                                                         step = 0.05,
                                                         animate = TRUE,
                                                         value = 0.5),
                                             sliderInput("crossover_rate", 
                                                         "Crossover rate",
                                                         min = 0, 
                                                         max=1,
                                                         step = 0.05,
                                                         animate = TRUE,
                                                         value = 0.5),
                                             ),
                                    tabPanel("Set of available rules"),
                                    tabPanel("View trees")
                                    )
                        )
             
            ),
    
    
    
    # About Us ----------------------------------------------------------------
    tabPanel("About Us",
             
            
             
             
    )
    
  )))
