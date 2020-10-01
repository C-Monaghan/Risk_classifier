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
    theme = shinytheme("lumen"),  # <--- To use a theme, uncomment this
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
    
    # Dataset Input ----------------------------------------------------------------
    tabPanel("Dataset", "This panel is intentionally left blank",
             
             selectInput("dataset", label = "Dataset", choices = ls("package:datasets")),
             verbatimTextOutput("summary"),
             tableOutput("table")
    ),
    
    # Classifier (Analysis) ----------------------------------------------------------------
    tabPanel("Classifier", "This panel is intentionally left blank",
             
             selectInput("dataset", label = "Dataset", choices = ls("package:datasets")),
             verbatimTextOutput("summary"),
             tableOutput("table")
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
    
    # About Us ----------------------------------------------------------------
    tabPanel("About Us", "This panel is intentionally left blank",
             
             selectInput("dataset", label = "Dataset", choices = ls("package:datasets")),
             verbatimTextOutput("summary"),
             tableOutput("table")
    )
    
    
    
    
  )))
