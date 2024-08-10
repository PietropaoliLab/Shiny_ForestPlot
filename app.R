# Load required libraries
library(shiny)
library(readxl)
library(ggplot2)
library(extrafont)
library(showtext)
library(patchwork)
library(here)

MyTheme <- theme_bw(13) +
  theme(axis.text = element_text(colour = "black"),
        aspect.ratio = 1/2,
        panel.grid = element_blank(),
        legend.key = element_blank(),
        strip.text = element_text(size = 12),
        legend.background = element_blank(),
        strip.background = element_rect(fill = NA, colour = NA),
        text = element_text(family = "Helvetica"))

# Define UI
ui <- fluidPage(
  titlePanel("Forest Plot Generator for Stefania"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose Excel File",
                accept = c(".xlsx")),
      
      checkboxInput("facet_wrap", "Apply Facet Wrap", FALSE),
      
      uiOutput("facet_var_ui"),
      
      numericInput("xlim_min", "X-axis Limit Min", value = -0.5, step = 0.1),
      numericInput("xlim_max", "X-axis Limit Max", value = 4.5, step = 0.1),
      
      downloadButton("savePlot", "Save Plot as PDF")
    ),
    
    mainPanel(
      plotOutput("forestPlot")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive expression to read the Excel file
  df <- reactive({
    req(input$file1)
    read_excel(input$file1$datapath)
  })
  
  # Render UI for selecting facet variable(s) if facet_wrap is selected
  output$facet_var_ui <- renderUI({
    req(input$facet_wrap)
    df <- df()
    
    selectInput("facet_var", "Select Facet Variable",
                choices = names(df), multiple = TRUE)
  })
  
  # Generate the forest plot
  output$forestPlot <- renderPlot({
    req(df())
    data <- df()
    
  
    p <- ggplot(data, aes(x = log2FoldChange, y = Comparison, color = Comparison)) +
      geom_vline(xintercept = 0, linewidth = .3, linetype = 2, color = "gray35") +
      geom_errorbar(aes(xmin = log2FoldChange - lfcSE, xmax = log2FoldChange + lfcSE), width = .1, color = "gray35") +
      geom_point(size = 2) +
      geom_text(aes(x = 3, y = Comparison, label = paste0("P = ", padj)), size = 3, colour = "black", hjust = -0.5) +
      scale_y_discrete(limits = rev) +
      coord_fixed(xlim = c(input$xlim_min, input$xlim_max)) +
      labs(y = "", x = "Log2 fold change") +
      MyTheme
    
    if (input$facet_wrap) {
      p <- p + facet_wrap(as.formula(paste("~", paste(input$facet_var, collapse = " + "))))
    } else {
      p <- p + facet_grid(Symbol ~ .)
    }
    
    print(p)
  })
  
  # Save the plot as a PDF
  output$savePlot <- downloadHandler(
    filename = function() {
      paste("forest_plot", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      pdf(file, width = 8, height = 6)
      print(ggplot(df(), aes(x = log2FoldChange, y = Comparison, color = Comparison)) +
              geom_vline(xintercept = 0, linewidth = .3, linetype = 2, color = "gray35") +
              geom_errorbar(aes(xmin = log2FoldChange - lfcSE, xmax = log2FoldChange + lfcSE), width = .1, color = "gray35") +
              geom_point(size = 2) +
              geom_text(aes(x = 3, y = Comparison, label = paste0("P = ", padj)), size = 3, colour = "black", hjust = -0.5) +
              scale_y_discrete(limits = rev) +
              coord_fixed(xlim = c(input$xlim_min, input$xlim_max)) +
              labs(y = "", x = "Log2 fold change") +
              MyTheme +
              (if (input$facet_wrap) {
                facet_wrap(as.formula(paste("~", paste(input$facet_var, collapse = " + "))))
              } else {
                facet_grid(Symbol ~ .)
              }))
      dev.off()
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

