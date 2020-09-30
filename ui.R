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
    
    # Analysis ----------------------------------------------------------------
    tabPanel("Analysis", "This panel is intentionally left blank",
             
             selectInput("dataset", label = "Dataset", choices = ls("package:datasets")),
             verbatimTextOutput("summary"),
             tableOutput("table")
    ),
    # Guides ------------------------------------------------------------------
    tabPanel("Guides", "This panel is intentionally left blank",
             mainPanel(width = 9,
                       tabsetPanel(
                         tabPanel("Tab 3", "This panel is intentionally left blank",
                                  textInput("txt", "Text input:", "general"),
                                  tags$h5("Default actionButton:"),
                                  actionButton("action", "Search"),
                                  
                                  tags$h5("actionButton with CSS class:"),
                                  actionButton("action2", "Action button", class = "btn-primary")
                         )
                       )
             ),
    ),
    
    # About Us ----------------------------------------------------------------
    tabPanel("About Us", "This panel is intentionally left blank",
             
             selectInput("dataset", label = "Dataset", choices = ls("package:datasets")),
             verbatimTextOutput("summary"),
             tableOutput("table")
    )
    
    
    
    
  )))
