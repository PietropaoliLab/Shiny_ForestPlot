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
      
      checkboxInput("show_pvalue", "Show P-value", FALSE),
      
      checkboxInput("facet_wrap", "Apply Facet Wrap", FALSE),
      
      uiOutput("facet_var_ui"),
      
      selectInput("yvar", "Variable Y axis:", choices = NULL),
      
      selectInput("colorby", "Color by:", choices = NULL),
      
      
      selectInput(inputId = "legend_position",
                  label = "Legend position",
                  choices = c("none","right", "left", "bottom","inside", "top")),
      
        
      numericInput("font_size", "Theme font size", value = 12, step = 1),
      
      numericInput("aspect_ratio", "Plot aspect ratio", value = 0.5, step = 0.1),
      
      numericInput("xlim_min", "X-axis Limit Min", value = 0, step = 0.1),
      numericInput("xlim_max", "X-axis Limit Max", value = 4.5, step = 0.1),
      
      downloadButton("savePlot", "Save Plot as PDF")
    ),
    
    mainPanel(
      plotOutput("forestPlot")
    )
  )
)
