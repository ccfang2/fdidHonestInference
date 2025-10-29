
# Options
options(warn = -1)
options(spinner.type=5)

# Import Libraries
library(bslib)
library(fdid)
#library(did)
library(dplyr)
#library(mvtnorm)
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(shinydashboard)
library(shinyBS)
library(shinydisconnect)
library(shinycssloaders)
library(shinyjs)
#library(lubridate)
#library(Rglpk)
#library(WriteXLS)
#library(readxl)
library(tools)
library(grDevices)
library(graphics)

source("Tabs.R")

# UI 
UI <- shinyUI({
  fluidPage(
    
    # Isend messages
    tags$head(tags$script(HTML('Shiny.addCustomMessageHandler("jsCode",function(message) {eval(message.value);});')),
              tags$style(HTML("hr {border-top: 1px solid #000000;}"))),

    tags$head(tags$style(HTML("#example_sidebar_panel {height: 90vh; overflow-y: auto; padding: 10px; background-color: #f8f9fa;}
                               #owndata_sidebar_panel {height: 90vh; overflow-y: auto; padding: 10px; background-color: #f8f9fa;}   
                               #help_sidebar_panel {height: 90vh; overflow-y: auto; padding: 10px; background-color: #f8f9fa;}"))),
    
    tags$head(tags$style(HTML(".control-label {font-size: 15px !important; font-weight: bold;}
                               .shiny-input-container .checkbox label, .shiny-input-container .radio label {font-size: 14px;}"))),
    
    disconnectMessage(
      text = "Your session timed out, reload the application.",
      refresh = "Reload now",
      background = "#f89f43",
      colour = "white",
      overlayColour = "grey",
      overlayOpacity = 0.3,
      refreshColour = "brown"),

    # Navbar structure for UI
    navbarPage(strong("Making Event Study Plots Honest: A Functional Data Approach to Causal Inference"), theme = shinytheme("yeti"),
               Home,
               Examples,
               OwnData,
               Help,
               More)
    )
  })
  
  