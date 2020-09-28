library(shiny)
library(reticulate)

use_python("C:/Users/fredx/Anaconda3",required=T)
source_python("Source_EA.py")
x <- func()
#py_config()

# Run in the shell to find your python directory:
# import os
# import sys
# os.path.dirname(sys.executable)

ui<-fluidPage(toString(x))
server<-function(input, output){
  
}

shinyApp(ui=ui,server=server)
