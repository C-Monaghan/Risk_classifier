source("Main.R")

# All the libraries required for th shiny app
library(shiny)
library(shinyBS)
library(shinythemes)
library(shinyjs)
library(shinyalert)
library(shinyWidgets)
#library(shinycssloaders)

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
    #flatly
    theme = shinytheme("flatly"),  # <--- To use a theme, uncomment this
    strong("MI based classifier"), # Main title name
    
    
    
    # Home --------------------------------------------------------------------
    tabPanel("Home",
             column(12, align="center",
                    # img(src='logo.png', align = "right", height=120, width=100),
                    h2(strong("Welcome to the Shiny App for using MI based classifier in R")),
                    tags$img(src = "classifier.png",height=500,width=550),
                    h4(strong("Note: For more help on usage, please look into the 'Guide' tab and 'About us' in detail.")),
                    
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
    tabPanel("Dataset", conditionalPanel(condition="input.tabs!='steps'",
                                         sidebarPanel(width = 3,
                                                      conditionalPanel(condition="input.tabs=='data'",
                                                                       textOutput("arg"),
                                                                       
                                                                       fileInput("sample_file", h4("File input:", bsButton("main_data_tooltip", label = "",
                                                                                                                           icon = icon("question"), size = "extra-small")),
                                                                                 multiple = F, accept = c("text/csv", "text/comma-separated-values, text/plain", ".csv"),
                                                                                 placeholder = "Enter Your Data Here"),
                                                                       bsPopover("main_data_tooltip", title="File format",
                                                                                 content="Please make sure: rows are customers/observations, columns are different variables with first column specifying the variable to be estimated.",
                                                                                 trigger = "hover"),
                                                                       #tags$hr(),
                                                                       
                                                                       radioButtons(inputId = 'data_choice', label = NULL,
                                                                                    choiceNames =  c("Use Your Own Data", "Use Example Data"),
                                                                                    choiceValues = c(TRUE, FALSE),
                                                                                    selected = TRUE, inline = TRUE),
                                                                       
                                                                       h4(helpText("Is there a header in the data?")),
                                                                       checkboxInput('header', 'Tick for Yes', TRUE),
                                                                       
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
                                                                       
                                                                       bsPopover("main_data_tooltip", title="",
                                                                                 content="Please make sure: rows are customers/observations, columns are different variables with first column named as 'Class' specifying the variable to be estimated",
                                                                                 trigger = "hover"),
                                                                       tags$hr(),
                                                                       
                                                                       actionButton("parameter_button", "Go to Method Specification",
                                                                                    width = "100%", class = "btn-default"),
                                                                       
                                                      ),
                                                      conditionalPanel(condition="input.tabs=='download'",
                                                                       downloadButton(outputId="down",label ="Download the reduced data in .csv")
                                                      ),
                                                      conditionalPanel(condition="input.tabs=='specification'",
                                                                       
                                                                       h4("Please Select Input Values:",align="centre"),
                                                                       # Input: Variable Selection Method ----
                                                                       radioButtons("method", "Step 1: Type of Variable Selection Method:",choices = list("Lasso Regression","Ridge Regression"),selected = "Lasso Regression"),
                                                                       
                                                                       
                                                                       # Input: No. of Decision Trees ----
                                                                       sliderInput("trees", "Step 2: How many Decision Trees user wants to select for Evolutionary Algorithm?",
                                                                                   min = 20, max=800,
                                                                                   step = 4,animate = TRUE,value = 100),
                                                      ),
                                                      conditionalPanel(condition="input.tabs=='plot'",
                                                                       
                                                                       # Input: Which Decision Trees and its associative results to be viewed ----
                                                                       radioButtons("option","Select the decision tree to be viewed with the below property:",choiceNames = c('Max. Accuracy',
                                                                                                                                                                              'Min. Gini Index',
                                                                                                                                                                              'Max. AUROC'),choiceValues = c('ind_max_acc','ind_min_gini','ind_max_AUROC'),selected = 'ind_max_acc'),
                                                                       
                                                                       # Downloading the file type
                                                                       h5("3.Select the type of plot to be downloaded:",align="centre"),          
                                                                       radioButtons("filetype","Select the file type",choices = list("png","pdf"),selected = "pdf"),
                                                                       actionButton("return_parameter_button", "Return to Method Specification",
                                                                                    width = "100%", class = "btn-default")
                                                      ),
                                         )
    ),  # Main panel for displaying outputs ----
    mainPanel(width = 9, id = "main_layout",
              tabsetPanel(id = "tabs",  selected = "steps",
                          #  type="tab",
                          tabPanel("Quick Start", value = "steps",
                                   column(width = 5, offset = 0.5,
                                          br(),
                                          h1(strong(tags$u("Usage Guide:"))),
                                          h3(
                                            tags$div(
                                              tags$ol(
                                                tags$li("Enter your data in the 'View Dataset' tab."),
                                                br(),
                                                tags$li("Specify the method and its parameters to be selected in the 'Method Specification' tab and after that click 'Calculate' button for the method to run."),
                                                br(),
                                                tags$li("In the 'Reduced Dataset after variable selection' tab, the user can view the reduced dataset and download it in csv format."),
                                                br(),
                                                tags$li("Once the method finishes running, decision tree will appear under the 'Classifier' tab which enables user to view tree with maximum accuracy, minimum gini index and maximum AUROC plus they can download the specified tree in pdf or png format."),
                                                br(),
                                                actionButton("data", " View Dataset", class = "btn-default"),
                                                actionButton("method_spec", "Method Specification", class = "btn-default")
                                              )
                                            )
                                          ),
                                          br(),
                                   ) , column(width = 7, offset = 0.5,
                                              br(),
                                              h4(tags$u("Note: App demo below, click play to animate")),
                                              sliderInput("gif_slider", label=NULL, min = 0, max=218, value=0, ticks = FALSE,
                                                          animate = animationOptions(interval = 300)),
                                              #hidden(imageOutput("gif_demo")),
                                              br(),
                                              br(),
                                   )),
                          tabPanel(" View Dataset ", value = "data",dataTableOutput("dataset"),tableOutput("col")),
                          tabPanel("Method Specification", value="specification",tableOutput("values"),br(),useShinyalert() ,h5("Click Calculate button after selecting the desired inputs!!"),actionButton("button", "Calculate")),
                          tabPanel("Reduced Dataset after variable selection",value="download",dataTableOutput("Reduced_data"), tableOutput("colred")),
                          tabPanel("Classifier",value="plot",br(),useShinyalert() ,h5("Click Calculate button after selecting the decision tree to be viewed in inputs!!"),actionButton("button1", "Show desired plot"),addSpinner(plotOutput("plot",  width = "120%"), spin = "circle", color = "#E41A1C"),downloadButton(outputId="down1",label ="Download the plot"),tableOutput("res"))
              )
    )
    ),
    
    ######################################################################################
    ##########  EVOLUTIONARY ALGORITHM ###################################################
    ######################################################################################
    
    tabPanel("Evolutionary Algorithm",
             sidebarPanel(width = 3,
                          sliderInput("generations",
                                      "Generations",
                                      min = 5,
                                      max=100,
                                      step = 5,
                                      animate = TRUE,
                                      value = 10),
                          actionButton("seed",
                                       "Initiate Genetic Program"),
                          actionButton("restart_evolution",
                                       "Restart evoution"),
                          actionButton("evolve",
                                       "Evolve"),
                          actionButton("save_tree_python",
                                       "Save current best tree"),
                          actionButton("loade_tree_python",
                                       "Load tree"),
                          checkboxInput("accuracy_checkbox", "Accuracy", value = TRUE, width = NULL),
                          checkboxInput("nodes_checkbox", "Nodes", value = TRUE, width = NULL),
                          checkboxInput("sensitivity_checkbox", "Sensitivity", value = FALSE, width = NULL),
                          checkboxInput("specificity_checkbox", "Specificity", value = FALSE, width = NULL)
             ),
             mainPanel(tabsetPanel(type="tab",
                                   tabPanel("Set of available splits",
                                            dataTableOutput("crucial_values")),
                                   actionButton("remove_split",
                                                "Remove selected values"),
                                   #dataTableOutput("crucial_values")),
                                   
                                   tabPanel("Progress charts",
                                            plotOutput("pareto_front"),
                                            plotOutput("evolution_progress"),
                                            
                                            plotOutput("evolution_progress_nodes")),
                                   tabPanel("View trees",
                                            actionButton("update_tree",
                                                         "View best tree"),
                                            visNetworkOutput("network", height = "800px", width = "800px"),
                                            DTOutput("tree_partitions"))
             )
             )
             
    ),
    
    
    
    # About Us ----------------------------------------------------------------
    tabPanel("About Us",
             
             
             
             
    )
    
  )))