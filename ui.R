# ui.R
library(shiny)
library(ggplot2)
library(extrafont)
library(showtext)
library(patchwork)
library(here)

# Define UI
ui <- fluidPage(
  titlePanel("Forest Plot Generator for Stefania - data from GEO2R"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose GEO2R Excel File",
                accept = c(".xlsx")),
      
      checkboxInput("facet_wrap", "Apply Facet Wrap", FALSE),
      
      uiOutput("facet_var_ui"),
      
      numericInput("font_size", "Theme font size", value = 12, step = 1),
      
      numericInput("xlim_min", "X-axis Limit Min", value = 0, step = 0.1),
      numericInput("xlim_max", "X-axis Limit Max", value = 4.5, step = 0.1),
      
      downloadButton("savePlot", "Save Plot as PDF")
    ),
    
    mainPanel(
      plotOutput("forestPlot")
    )
  )
)
