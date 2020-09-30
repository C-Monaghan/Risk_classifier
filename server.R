# Library for running shinyapp
library(shiny)

# Library for using python scripts in shiny
library(reticulate)

default_data <- load("GermanCredit.Rdata")

# use_python("/Users/sajalkaurminhas/Documents/PhD/Group Project/Risk_classifier",required=T)
# source_python("Source_EA.py")
# x <- func()
# #py_config()

# Run in the shell to find your python directory:
# import os
# import sys
# os.path.dirname(sys.executable)

# ui<-fluidPage(toString(x))
shinyServer(function(input, output, session) { })
#shinyApp(ui=ui,server=server)